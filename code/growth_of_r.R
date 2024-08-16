

# load("data/cran_log.rdata")

fst::write_fst(daily_pkg_download,"data/daily_pkg_download.fst")
save(cran_archive_download,cran_pkg_download,cran_R_download,file = "data/cran_download_info.rdata")

##
load("data/cran_download_info.rdata")

library(pacman)
p_load(tidyverse,dtplyr,lubridate,patchwork,changepoint)

cran_R_download %>% 
  lazy_dt() %>% 
  group_by(date) %>% 
  summarise(count = sum(count)) %>% 
  as_tibble() %>% 
  mutate(year = year(date),month = month(date),day = day(date)) %>% 
  unite("ym",year:month,remove = F) %>% 
  group_by(year) %>% 
  add_count(wt = count,name = "yearly_count") %>% 
  mutate(yearly_count = yearly_count/n()) %>% 
  ungroup -> df

# df %>% 
#   group_by(year,month) %>% 
#   summarise(count = sum(count)) %>% 
#   ungroup() %>% 
#   pull(count) %>% 
#   cpt.mean() -> df_cpt
# 
# df %>% 
#   group_by(year,month) %>% 
#   summarise(count = sum(count)) %>% 
#   ungroup() %>% 
#   slice(df_cpt@cpts[1]) 
  
p1 = df %>% 
  mutate_at(vars(count,yearly_count),list(~./1e3)) %>% 
  ggplot(aes(date,count)) +
  geom_boxplot(aes(group = ym)) +
  geom_vline(xintercept = ymd("2018/09/01"),color = "red",linetype = "dashed",size = 1) +
  geom_line(aes(y = yearly_count,group = year),color = "blue",size = 1) +
  ylab("Daily download times (K)\n") + ggtitle("R Software")

######
cran_pkg_download %>% 
  lazy_dt() %>% 
  group_by(date) %>% 
  summarise(count = sum(count)) %>% 
  as_tibble() %>% 
  mutate(year = year(date),month = month(date),day = day(date)) %>% 
  unite("ym",year:month,remove = F) %>% 
  group_by(year) %>% 
  add_count(wt = count,name = "yearly_count") %>% 
  mutate(yearly_count = yearly_count/n()) %>% 
  ungroup -> df

df %>%
  group_by(year,month) %>%
  summarise(count = sum(count)) %>%
  ungroup() %>%
  pull(count) %>%
  cpt.mean() -> df_cpt

df %>%
  group_by(year,month) %>%
  summarise(count = sum(count)) %>%
  ungroup() %>%
  slice(df_cpt@cpts[1])

p2 = df  %>% 
  mutate_at(vars(count,yearly_count),list(~./1e6)) %>%  
  ggplot(aes(date,count)) +
  geom_boxplot(aes(group = ym)) +
  geom_vline(xintercept = ymd("2018/09/01"),color = "red",linetype = "dashed",size = 1) +
  geom_line(aes(y = yearly_count,group = year),color = "blue",size = 1) +
  ylab("Daily download times (M)\n") + ggtitle("R Packages")

p_load(extrafont)

windowsFonts(HEL=windowsFont("Helvetica CE 55 Roman"),
             RMN=windowsFont("Times New Roman"),
             ARL=windowsFont("Arial"))
(p1/p2 * 
  xlab("") * 
  scale_x_date(limits = c(min(df$date),max(df$date))) *
  cowplot::theme_minimal_grid(
    font_size = 12,
    color = "grey70"
  ) *
  theme(plot.title=element_text(family="ARL", size=15, face="bold", colour="black"),
        axis.title = element_text(family="RMN", face="bold",size = 13),
        axis.text = element_text(family="RMN", face="bold",size = 13)) +
  #plot_annotation(tag_levels = "A",tag_prefix = "(",tag_suffix = ")")) *
  plot_annotation(tag_levels = "a")) *
  theme(plot.tag = element_text(size = 20))

## theme test
# cowplot::theme_minimal_grid(
#   font_size = 12,
#   color = "grey70"
# )
# ggthemes::theme_pander()
# ggthemes::theme_gdocs()
# ggthemes::theme_igray() 
# ggthemes::theme_foundation()

############

# why is the change point lie there?

rm(list = ls())

library(pacman)
p_load(dplyr,dtplyr,data.table,lubridate,stringr)

## linux
setwd("/home/tyHuang/workdir/")

system.time({
  fst::read_fst("data/daily_pkg_download.fst",
                #from = 1,to = 10000,
                as.data.table = T)  -> dpd
})


system.time({
  dpd %>% 
    lazy_dt() %>% 
    group_by(package) %>% 
    mutate(cum_count = cumsum(count)) %>% 
    filter(cum_count != 0) %>% 
    select(-cum_count) %>% 
    as_tibble()-> dpd2
})

dpd2 %>% 
  lazy_dt() %>% 
  mutate(year = year(date),
         month = month(date),
         day = day(date)) %>% 
  as.data.table() -> dpd2

dpd2 %>% 
  lazy_dt() %>% 
  mutate(tag = ifelse(year >= 2018 & month >= 9,"after","before")) %>% 
  as_tibble -> dpd2
  
dpd2 %>% 
  lazy_dt() %>% 
  group_by(package,tag) %>% 
  summarise(avg = mean(count)) %>% 
  as_tibble() -> dpd3

dpd3 %>% 
  as.data.table() %>% 
  dcast(package ~tag,fill = 0,value.var = "avg") -> dpd4

dpd4 %>% 
  lazy_dt() %>% 
  mutate(delta = after - before) %>%
  arrange(-delta) %>% 
  as_tibble() -> dpd5

cran_archive_download %>% 
  setNames(c("Name","Date","Size")) %>% 
  mutate(Date = ymd_hm(Date)) %>% 
  mutate(year = lubridate::year(Date),
         month = lubridate::month(Date),
         day = lubridate::day(Date)) %>% 
  filter(year == 2018,month == 8) %>% 
  mutate(package =str_extract(Name,".+?_") %>% str_sub(1,-2)) %>% 
  inner_join(dpd5) %>% 
  arrange(-delta) %>% 
  select(package,Date,delta) %>% 
  distinct(package,.keep_all = T) %>% 
  print(n = 50)





