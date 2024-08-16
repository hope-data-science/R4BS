
library(pacman)
p_load(tidyverse,rscopus,patchwork)

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

library(readxl)
field_code_raw <- read_excel("data/Scopus_field_code.xlsx", 
                             col_names = FALSE)

field_code_raw %>% 
  select(5) %>% 
  rename(text = `...5`) %>% 
  na.omit() %>% 
  filter(str_detect(text,"\\(")) %>% 
  filter(str_detect(text,"\\([A-Z]{4}\\)")) %>% 
  filter(!str_detect(text,"^Entering")) %>% 
  separate(text,c("subject","abbr"),sep = "\\(") %>% 
  mutate(subject = str_squish(subject)) %>% 
  mutate(abbr = str_remove(abbr,"\\)")) %>% 
  arrange(abbr) %>% 
  transmute(Abbreviation=abbr,Subject=subject) %>% 
  write_csv("data/subj_abbr.csv")

field_code_raw %>% 
  select(5) %>% 
  rename(text = `...5`) %>% 
  na.omit() %>% 
  filter(str_detect(text,"\\(")) %>% 
  filter(str_detect(text,"\\([A-Z]{4}\\)")) %>% 
  filter(!str_detect(text,"^Entering")) %>% 
  separate(text,c("subject","abbr"),sep = "\\(") %>% 
  mutate(subject = str_squish(subject)) %>% 
  mutate(abbr = str_remove(abbr,"\\)")) %>% 
  arrange(abbr) %>% 
  unite("text",2:1,sep = ": ") %>% 
  summarise(text = str_c(text,collapse = "; ")) %>% 
  pull(text)


