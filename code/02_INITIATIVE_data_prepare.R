

library(pacman)
p_load(tidyverse,data.table,fst,tidyfst,akc)

load("data/cran_meta.rdata")
crandb %>% 
  select(Package,Description) -> raw_corpus

parse_fst("data/R_df.fst") -> ft
ft %>% select_fst("keyword|eid") -> dict_raw

dict_raw %>% 
  keyword_clean(id = "eid",keyword = "authkeywords",sep = "\\|") -> cleaned_keywords
cleaned_keywords %>% 
  distinct(keyword,.keep_all = T) -> unique_keywords
unique_keywords %>% 
  pull(keyword) %>% 
  make_dict() -> dict

# unique_keywords %>% mutate(count = str_count(keyword," ") +1) %>% pull(count) %>% table()
extracted_keyword <- raw_corpus %>% 
  keyword_extract(id = "Package",text = "Description",dict = dict,n_max = 5,n_min = 2)

extracted_keyword %>% 
  keyword_merge(reduce_form = "lemma") %>% 
  keyword_merge(reduce_form = "partof") -> merged_keyword

# write_csv(merged_keyword,"data/r_pkg_keyword.csv")

# linux

library(pacman)
p_load(fst,data.table,tidyverse)

############ yearly
read_fst("/home/tyHuang/workdir/data/daily_pkg_download.fst",as.data.table = T) -> ft
ft[,year:=year(date)]
ft
ft[,.(count = sum(count)),keyby = .(package,year)][count!=0] -> res

fwrite(res,"/home/tyHuang/workdir/data/yearly_pkg_download.csv") # general

########## montly
pacman::p_load(fst,data.table,maditr,tidyverse)

read_fst("/home/tyHuang/workdir/data/daily_pkg_download.fst",as.data.table = T) -> ft
ft %>% 
  dt_mutate(year = year(date),month = month(date)) -> ft
ft %>% dt_filter(count != 0) -> ft
ft %>% 
  dt_summarise(count = sum(count),by = .(package,year,month)) -> monthly_pkg_download

monthly_pkg_download %>% 
  dt_summarise(count = mean(count),by = package) -> month_mean_pkg

ft %>% 
  dt_summarise(count = sum(count),by = package) -> total_sum_pkg

# load("/home/tyHuang/workdir/data/cran_log.rdata")
# cran_archive_download %>% 
#   write_fst("/home/tyHuang/workdir/data/cran_archive_download.fst",100)
read_fst("/home/tyHuang/workdir/data/cran_archive_download.fst",
         as.data.table = T) -> cran_archive_download

copy(cran_archive_download) %>% setNames(c("name","date","size"))-> dt
dt[,`:=`(year = year(date), month = month(date),day = mday(date),
         ymd = as.IDate(date),
         pkg = str_extract(name,"^.+?_") %>% str_sub(end = -2))]
write_fst(dt,"/home/tyHuang/workdir/data/washed_cran_archive_download.fst",100)

na.omit(dt) -> washed_cran_archive

na.omit(dt)[order(pkg,year)][,.SD[1],by = pkg] %>% 
  dt_select(pkg,year) %>% 
  dt_mutate(pkg = str_sub(pkg,end = -2)) %>% 
  as.data.table-> pkg_birth_year

save(monthly_pkg_download,month_mean_pkg,total_sum_pkg,pkg_birth_year,washed_cran_archive,
     file = "/home/tyHuang/workdir/data/contents2.rdata")

##
parse_fst("data/R_df.fst") -> ft
names(ft)

ft %>% select_fst("eid|coverDate|keyword|issn") %>% names()  # academic

ft %>% select_fst("eid|coverDate|keyword|issn") %>% 
  setNames(c("eid","issn","date","keyword")) -> scopus_key

##
read_csv("data/r_pkg_keyword.csv") -> pkg_key

read_csv("data/yearly_pkg_download.csv") -> yearly_pkg_download

save(scopus_key,pkg_key,yearly_pkg_download,file = "data/contents.rdata")




