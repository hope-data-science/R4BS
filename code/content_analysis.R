

library(pacman)
p_load(tidyverse,data.table,patchwork,tidyfst,
       widyr,ggraph,tidygraph,igraph,graphlayouts)

# setwd("/home/tyHuang/workdir")

load("data/contents.rdata")
load("data/contents2.rdata")

pkg_key %>% 
  count(keyword,sort = T,name = "freq") -> pkg_key_freq



total_sum_pkg %>% 
  mutate(prop = count/sum(count)) %>% 
  as_tibble %>% 
  arrange(desc(prop)) %>% 
  mutate(cum_prop = cumsum(prop)) %>% 
  filter(cum_prop<=.8) %>% tail



pkg_key %>% 
  pairwise_count(keyword, package, upper = FALSE) %>%
  graph_from_data_frame(directed = FALSE) %>% 
  as_tbl_graph() %>% 
  mutate(degree = centrality_degree()) %>% 
  arrange(desc(degree)) -> g

extrafont::loadfonts()

g %>% 
  inner_join(pkg_key_freq,by = c("name" = "keyword")) %>% 
  top_n(100,freq) %>% 
  ggraph(layout = "stress") + 
  geom_edge_link0(aes(width = n),edge_colour = "grey66",show.legend = F)+
  geom_node_point(aes(fill = degree,size = freq),shape=21)+
  #geom_node_text(aes(filter = freq >= 100, label = name),family="serif",repel = T)+
  geom_node_text(aes(label = name),family="serif",repel = T)+
  scale_edge_width(range = c(0.2,3))+
  scale_size(name = "Frequency",range = c(1,6))+
  scale_fill_distiller(name = "Degree",palette = "RdYlGn")+
 # theme(legend.position = "top") +
  theme_graph() -> p1

top = 10
total_sum_pkg %>% 
  rename_dt(total = count) %>% 
  inner_join_dt(month_mean_pkg %>% 
                  rename_dt(month = count)) -> plot_table

plot_table %>% 
  arrange_dt(-total) %>% 
  slice_dt(1:top) %>% 
  ggplot(aes(package %>% fct_reorder(total),total/1e6)) +
  geom_col(fill = "orange") +
  xlab("") + ylab("\nTotal downloads (M)") +
  coord_flip() +
  theme_bw() -> p2

plot_table %>% 
  arrange_dt(-month) %>% 
  slice_dt(1:top) %>% 
  ggplot(aes(package %>% fct_reorder(month),month/1e3)) +
  geom_segment(aes(xend=package, yend=0)) +
  geom_point(size=4, color="orange") +
  theme_bw() +
  xlab("") + ylab("\nAverage monthly downloads (K/month)") +
  coord_flip() -> p3

p1/(p2+p3) + 
  plot_annotation(tag_levels = "a") -> final

ggsave(file = "figure/fig3.tiff",final,dpi = 400,width = 12,height = 11,
       limitsize = F)

# windowsFonts(HEL=windowsFont("Helvetica CE 55 Roman"),
#              RMN=windowsFont("Times New Roman"),
#              ARL=windowsFont("Arial"))
# p1 /((p2 + p3))*
#   theme(plot.title=element_text(family="ARL",  face="bold", colour="black"),
#         axis.title = element_text(family="RMN", face="bold",size = 14),
#         axis.text.x = element_text(family="RMN", face="bold",size = 11),
#         axis.text.y = element_text(family="RMN", face="bold"),
#         legend.title = element_text(family="RMN", face="bold"),
#         legend.text = element_text(family="RMN", face="bold")) 









 #load("/home/tyHuang/workdir/data/contents.rdata")

# pkg_key %>% distinct(keyword)

# yearly_pkg_download %>% 
#   inner_join(pkg_key) %>% 
#   count(keyword,wt = count,sort = T) %>%
#   filter(n >= 5e6) %>% 
#   filter(str_detect(keyword,"^r ",negate = T)) %>% 
#   select(keyword) -> selected_key
#   #filter(str_detect(keyword,"^r ")) %>% 
# 
# yearly_pkg_download %>% 
#   group_by(year) %>% 
#   top_n(10,count) %>% 
#   arrange(year,-count) %>% 
#   print(n = Inf)
# 
# yearly_pkg_download %>% 
#   group_by(year) %>% 
#   top_n(10,count) %>% 
#   arrange(year,-count) %>% 
#   ungroup %>% 
#   distinct(package) %>% 
#   print(n = Inf)

#################################################
# yearly_pkg_download %>% 
#   inner_join(pkg_key) %>% 
#   inner_join(selected_key)-> tidy_table
# 
# tidy_table %>% 
#   unite("id",package:count)  -> raw_table
#   
# raw_table %>% 
#   cast_sparse(id,keyword) -> m
# 
# rownames(m) %>% 
#   str_extract("_[0-9]+$") %>% 
#   str_sub(2) %>% 
#   as.numeric() %>% 
#   scale()-> target
# 
# cbind(target,m) -> m
# 
# as.data.table(as.matrix(m)) -> m2
# 
# ge <- mRMR.data(data = m2)
# 
# p_load(doParallel)
# (cl = detectCores() %>%
#     makeCluster()) %>%
#   registerDoParallel()
# 
# ft = 8
# sc = 100
# 
# mRMR.ensemble(data = ge, target_indices = c(1),
#               solution_count = sc,
#               feature_count = ft) -> mrmre_model
# mrmre_model@filters$`1` -> ensemble_vec
# 
# system.time({
#   all = foreach(i = 1:sc,.combine = rbind,
#                 .packages = c("broom","dplyr","mRMRe")) %dopar% {
#     featureData(ge)[,c(1,ensemble_vec[,i])] %>% 
#       lm(V1~.,data=.) %>% 
#       broom::glance() %>% 
#       bind_cols(id = i,.)
#   }
# })
# 
# all %>% arrange(-adj.r.squared,AIC,BIC) %>% slice(1) %>% pull(id) -> best_id
# featureNames(ge)[ensemble_vec[,best_id]] -> sel_names
# 
# sel_names
# 
# tidy_table %>% filter(keyword %in% sel_names) %>% 
#   count(package,keyword,wt = count) %>% 
#   arrange(keyword) %>% 
#   print(n = Inf)
#   















