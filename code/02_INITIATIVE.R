
library(pacman)
p_load(tidyverse,data.table,patchwork,tidytext,
       widyr,ggraph,tidygraph,igraph,graphlayouts)

load("data/contents.rdata")
# load("data/contents2.rdata")

pkg_key %>% 
  count(keyword,sort = T,name = "freq") -> pkg_key_freq

extrafont::loadfonts()

pkg_key %>% 
  pairwise_count(keyword, package, upper = FALSE) %>%
  graph_from_data_frame(directed = FALSE) %>% 
  as_tbl_graph() %>% 
  mutate(degree = centrality_degree()) %>% 
  arrange(desc(degree)) -> g

# g %>% 
#   inner_join(pkg_key_freq,by = c("name" = "keyword")) %>% 
#   top_n(100,freq) %>% 
#   ggraph(layout = "stress") + 
#   geom_edge_link0(aes(width = n),edge_colour = "grey66",show.legend = F)+
#   geom_node_point(aes(fill = degree,size = freq),shape=21)+
#   #geom_node_text(aes(filter = freq >= 100, label = name),family="serif",repel = T)+
#   geom_node_text(aes(label = name),family="serif",repel = T)+
#   scale_edge_width(range = c(0.2,3))+
#   scale_size(name = "Frequency",range = c(1,6))+
#   scale_fill_distiller(name = "Degree",palette = "RdYlGn")+
#   # theme(legend.position = "top") +
#   theme_graph() 

g %>% 
  inner_join(pkg_key_freq,by = c("name" = "keyword")) %>% 
  top_n(50,freq) %>% 
  ggraph(layout = "focus",focus = 1) + 
  #draw_circle(use = "focus",max.circle = 2)+
  #ggraph(layout = "nicely") + 
  geom_edge_link0(aes(width = n,alpha = n),
                  edge_colour = "grey66",
                  #edge_colour = "grey66",
                  show.legend = F)+
  geom_node_point(aes(fill = degree,size = freq),shape=21)+
  #geom_node_text(aes(filter = freq >= 100, label = name),family="serif",repel = T)+
  geom_node_text(aes(label = name),family="serif",fontface = "bold",repel = T)+
  scale_edge_width(range = c(0.2,3))+
  scale_size(name = "Frequency",range = c(1,6))+
  scale_fill_distiller(name = "Degree",palette = "RdYlGn")+
  # theme(legend.position = "top") +
  theme_graph() 

# 600 * 500

