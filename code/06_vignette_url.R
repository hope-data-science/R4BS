


library(tidyverse)

load("data/cran_meta.rdata")

read_csv("data/pkg_count_202001.csv") -> pkg_count_202001
package_db %>% 
  select(package,vignettebuilder,url) %>% 
  inner_join(pkg_count_202001) %>% 
  mutate(vignette = !is.na(vignettebuilder)) %>% 
  mutate(url = !is.na(url)) %>% 
  select(-vignettebuilder) -> dt

dt %>% 
  group_by(url) %>% 
  summarise(avg = mean(count),sd = sd(count))

dt %>% 
  group_by(vignette) %>% 
  summarise(avg = mean(count),sd = sd(count))


