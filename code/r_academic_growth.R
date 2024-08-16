
library(pacman)
p_load(tidyverse,rscopus,patchwork)
# p_load(dplyr,stringr,rscopus,readr)

time_query = str_c("PUBYEAR = ",2005:2019," AND")
subject_query = str_c("SUBJAREA(",subject_areas(),")")
r_query = "REF({R: A Language and Environment for Statistical Computing}) AND DOCTYPE(ar)"

search_list = do.call(paste,expand.grid(time_query,subject_query)) %>% 
  str_c(" AND ",r_query)

set_api_key("50bcdd2037473d276841efd6c2f6296e")

all = tibble()
for(i in search_list){
  res = scopus_search(query = i,max_count = 0,count = 1)
  tibble(query = i,no = res$total_results) %>% 
    bind_rows(all,.) -> all
}
all %>% 
  mutate(year = str_extract(query,"20..")) %>% 
  mutate(subj = str_extract(query,"\\(....\\)")) %>% 
  mutate(subj = str_sub(subj,2,-2)) -> subject_year
# write_rds(subject_year,"/home/tyHuang/workdir/data/subject_year.rds")
###
search_list = do.call(paste,expand.grid(time_query,subject_query)) %>% 
  str_c(" AND DOCTYPE(ar)")
all = tibble()
for(i in search_list){
  res = scopus_search(query = i,max_count = 0,count = 1)
  tibble(query = i,no = res$total_results) %>% 
    bind_rows(all,.) -> all
}
all %>% 
  mutate(year = str_extract(query,"20..")) %>% 
  mutate(subj = str_extract(query,"\\(....\\)")) %>% 
  mutate(subj = str_sub(subj,2,-2)) %>% 
  mutate(total = no)-> subject_year2

subject_year %>% 
  inner_join(subject_year2,by = c("year","subj")) -> subject_year_total

# write_rds(subject_year_total,"/home/tyHuang/workdir/data/subject_year_total.rds")
write_rds(subject_year,"data/subject_year.rds")

read_rds("data/subject_year.rds") -> subject_year
read_rds("data/subject_year_total.rds") -> subject_year_total
## only article


subject_year %>%
  group_by(year) %>% 
  summarise(sum = sum(no)) %>% 
  ungroup() %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(sum = sum/1000) %>% 
  ggplot(aes(year,sum)) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  xlab("") + ylab("No. of articles (K)") +
  theme_bw() -> p1


subject_year %>% 
  mutate(no = no/1000) %>% 
  #mutate(year = as.numeric(year)) %>% 
  #mutate(subj = fct_reorder(subj,no)) %>% 
  ggplot(aes(year,subj,fill = no)) +
  geom_tile()+ ylab("") + xlab("") +
  scale_y_discrete(position = "right") +
  scale_fill_distiller(name = "No. of articles (K)",palette = "Spectral") +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 90,hjust = 1,vjust = .5)) -> p2

subject_year %>% 
  group_by(subj) %>% 
  summarise(count = sum(no)) %>% 
  ungroup() %>% 
  top_n(10,count) %>% 
  mutate(subj = fct_reorder(subj,count)) %>% 
  mutate(count = count/1000) %>% 
  ggplot(aes(subj,count)) +
  geom_col(fill = "orange") +
  xlab("") + ylab("\nNo. of articles (K)") +
  coord_flip() +
  theme_bw() -> p3

subject_year_total %>% 
  group_by(subj) %>% 
  summarise(no = sum(no.x),total = sum(total)) %>% 
  mutate(prop = no/total) %>% 
  arrange(-prop) %>% 
  top_n(10,prop) %>% 
  select(subj,prop) %>%
  mutate(subj = fct_reorder(subj,prop)) %>% 
  ggplot(aes(subj,prop)) +
  geom_segment(aes(xend=subj, yend=0)) +
  geom_point(size=4, color="orange") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bw() +
  xlab("") + ylab("\nProportion") +
  coord_flip() -> p4

windowsFonts(HEL=windowsFont("Helvetica CE 55 Roman"),
             RMN=windowsFont("Times New Roman"),
             ARL=windowsFont("Arial"))
(p1 + p2 +p3+p4 +plot_layout(nrow = 2) + plot_annotation(tag_levels = "a"))*
  theme(plot.title=element_text(family="ARL",  face="bold", colour="black"),
        axis.title = element_text(family="RMN", face="bold",size = 14),
        axis.text.x = element_text(family="RMN", face="bold",size = 11),
        axis.text.y = element_text(family="RMN", face="bold"),
        legend.title = element_text(family="RMN", face="bold"),
        legend.text = element_text(family="RMN", face="bold"))


