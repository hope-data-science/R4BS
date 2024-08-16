
library(pacman)
p_load(tidyverse,patchwork,data.table)

load("data/cran_meta.rdata")

## p1 distribution of author no.(only 0-15 showed)

package_db %>% 
  select(package,author) %>% 
  mutate(no = map_int(author,length)) %>%
  filter(no != 0) %>% 
  ggplot(aes(no)) + 
  geom_histogram(binwidth = 1) + 
  labs(x = "\nNo. of authors",y = "No. of packages\n") +
  coord_cartesian(xlim = c(1,15)) +  # show only
  theme_bw() -> p1

package_db %>% 
  select(package,author) %>% 
  mutate(no = map_int(author,length)) %>%
  filter(no != 0) %>% 
  count(no) %>% print(n = Inf)

package_db %>% 
  select(package,author) %>% 
  mutate(no = map_int(author,length)) %>% 
  filter(no > 5) %>%  #
  count(no) %>% 
  summarise(sum(n))

package_db %>% 
  select(package,author) %>% 
  mutate(no = map_int(author,length)) %>% filter(no == 0)

package_db %>% 
  select(package,author) %>% 
  mutate(no = map_int(author,length)) %>% 
  filter(no != 0) %>% 
  pull(no) %>% summary()

###################################################################################

##p2 import  v. author no.
## author no. larger than 50 or equal to 0 are excluded
package_db %>% 
  select(package,author,imports,depends) %>% 
  mutate(au.no = map_int(author,length),
         import = map_int(imports,length),
         depend = map_int(depends,length)) %>% 
  mutate(im_de = import + depend) %>% 
  #select(package,au.no,im_de) %>% 
  filter(import != 0) %>%  # when import = 0, they are more likely to use depends
  filter(between(au.no,1,50)) %>% 
  ggplot(aes(au.no,import)) +
  geom_hex() + scale_fill_viridis_c() +
  # geom_smooth(method = "lm",colour = "red",linetype = "dashed") +  
  theme_bw()+
  theme(legend.position = c(1, 1), legend.justification = c(1, 1),
        legend.background = element_rect(fill = "white", colour = "black")) +
  labs(x = "\nNo. of authors",y = "No. of imported packages\n",
       fill = NULL) -> p2

package_db %>% 
  select(package,author,imports,depends) %>% 
  mutate(au.no = map_int(author,length),
         import = map_int(imports,length),
         depend = map_int(depends,length)) %>% 
  mutate(im_de = import + depend) %>% 
  #select(package,au.no,im_de) %>% 
  filter(import != 0) %>%  # when import = 0, they are more likely to use depends
  filter(between(au.no,1,50)) -> dt
cor.test(dt$au.no,dt$import)

###################################################################################
## p3 downloads v. author no.

read_csv("data/pkg_count_202001.csv") -> pkg_count_202001
package_db %>% 
  select(package,author) %>% 
  mutate(au.no = map_int(author,length)) %>% 
  filter(between(au.no,1,50)) %>% 
  inner_join(pkg_count_202001) %>% 
  select(au.no,count) %>% 
  ggplot(aes(au.no,count/31/1e3)) +
  geom_hex() + scale_fill_viridis_c() +
  # geom_smooth(method = "lm",colour = "red",linetype = "dashed") +  
  theme_bw()+
  theme(legend.position = c(1, 1), legend.justification = c(1, 1),
        legend.background = element_rect(fill = "white", colour = "black")) +
  labs(x = "\nNo. of authors",y = "Daily download times (K)\n",
       fill = NULL) -> p3

package_db %>% 
  select(package,author) %>% 
  mutate(au.no = map_int(author,length)) %>% 
  filter(between(au.no,1,50)) %>% 
  inner_join(pkg_count_202001) %>% 
  select(au.no,count) -> dt
cor.test(dt$au.no,dt$count)

## p4 update frequency v. author no.

package_db %>% 
  select(package,published) %>% 
  setNames(c("pkg","ymd")) -> dt1

fst::fst("data/washed_cran_archive_download.fst") %>% 
  as_tibble() %>% 
  select(pkg,ymd) %>% 
  na.omit() -> dt2

dt1 %>% 
  bind_rows(dt2) %>% 
  unique() %>% 
  mutate(year = year(ymd)) %>% 
  count(pkg,year) %>% 
  group_by(pkg) %>% 
  summarise(year_avg = mean(n)) %>% 
  arrange(-year_avg) %>% 
  as_tibble() -> year_update_times

package_db %>% 
  select(package,author) %>% 
  mutate(au.no = map_int(author,length)) %>% 
  filter(between(au.no,1,50)) %>% 
  inner_join(year_update_times,by = c("package"="pkg")) %>% 
  select(-author) %>% 
  ggplot(aes(au.no,year_avg)) +
  geom_hex() + scale_fill_viridis_c() +
  # geom_smooth(method = "lm",colour = "red",linetype = "dashed") +  
  theme_bw()+
  theme(legend.position = c(1, 1), legend.justification = c(1, 1),
        legend.background = element_rect(fill = "white", colour = "black")) +
  labs(x = "\nNo. of authors",y = "Update times per year\n", 
       fill = NULL) -> p4


package_db %>% 
  select(package,author) %>% 
  mutate(au.no = map_int(author,length)) %>% 
  filter(between(au.no,1,50)) %>% 
  inner_join(year_update_times,by = c("package"="pkg")) %>% 
  select(-author)-> dt
cor.test(dt$au.no,dt$year_avg)



(p1 + p2 + p3 + p4) * theme(legend.key.size = unit(0.3, "cm")) + 
  plot_layout(nrow = 2) + plot_annotation(tag_levels = "a")

#  width 650 height 600

