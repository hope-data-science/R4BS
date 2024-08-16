
library(pacman)
p_load(tidyverse,data.table,patchwork)

# setwd("/home/tyHuang/workdir")

# load("data/contents.rdata")
load("data/contents2.rdata")


top = 10
total_sum_pkg %>% 
  rename(total = count) %>% 
  inner_join(month_mean_pkg %>% 
                  rename(month = count)) -> plot_table

plot_table %>% 
  arrange(-total) %>% 
  slice(1:top) %>% 
  ggplot(aes(package %>% fct_reorder(total),total/1e6)) +
  geom_col(fill = "orange") +
  xlab("") + ylab("\nTotal downloads (M)") +
  coord_flip() +
  theme_bw() -> p1

plot_table %>% 
  arrange(-month) %>% 
  slice(1:top) %>% 
  ggplot(aes(package %>% fct_reorder(month),month/1e3)) +
  geom_segment(aes(xend=package, yend=0)) +
  geom_point(size=4, color="orange") +
  theme_bw() +
  xlab("") + ylab("\nMonthly downloads (K/month)") +
  coord_flip() -> p2

p1 + p2 + plot_annotation(tag_levels = 'a')

########################################

load("data/cran_meta.rdata")

crandb
package_db %>% 
  select(package,imports) %>% 
  unnest(imports) -> pkg_imports

plot_table %>% 
  as_tibble() %>% 
  mutate(rank1 = min_rank(-total),rank2 = min_rank(-month)) %>% 
  filter(rank1 <= 10 | rank2 <= 10) -> sel_pkg

plot_table %>% 
  as_tibble() %>% 
  mutate(rank1 = min_rank(-total),rank2 = min_rank(-month)) %>% 
  filter(rank1 <= 10 ) -> sel_pkg

pkg_imports %>% count(imports,sort = T) %>%
  mutate(rank = min_rank(-n)) %>% 
  inner_join(sel_pkg,by = c("imports"="package"))

total_sum_pkg %>% 
  mutate(prop = count/sum(count)) %>% 
  as_tibble %>% 
  arrange(desc(prop)) %>% 
  mutate(cum_prop = cumsum(prop)) %>% 
  filter(cum_prop<=.1) 



