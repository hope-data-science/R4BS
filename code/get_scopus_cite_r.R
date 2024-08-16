
library(pacman)
# p_load(tidyverse,rvest)

p_load(readr,dplyr,stringr,rvest,purrr,rscopus)

set_api_key("2e68f67a32e5919e33a2b995c0a3d1e1")

setwd("/home/tyHuang/workdir")
read_csv("data/main_journal_cite_r.csv") -> journal_list
journal_list

journal_list %>% 
  filter(count < 5000) %>% 
  pull(source_id) %>% 
  str_c("SRCID(",.,")") %>% 
  str_c("REF ({R: A Language and Environment for Statistical Computing}) AND ",.) -> search_list

setwd("data/raw")


error_record = tibble()

for(i in seq_along(search_list)){
  
  # get data
  tryCatch(scopus_search(query = search_list[i], 
                         max_count = Inf,
                         count = 25,
                         view = "COMPLETE"),
           error = function(e){conditionMessage(e)}) -> res
  
  if(is_character(res)){
    
    tibble(search_txt = search_list[i],error_message = res) %>% 
      bind_rows(error_record,.) -> error_record
    next
    
  }else if(res$total_results == 0){
    
    tibble(search_txt = search_list[i],error_message = "Result set was empty") %>% 
      bind_rows(error_record,.) -> error_record
    next
    
  }else{
    
    tibble(search_txt = search_list[i],error_message = "Success") %>% 
      bind_rows(error_record,.) -> error_record
    
    df = gen_entries_to_df(res$entries)
    
    file_year = ""
    file_subject = search_list[i] %>% str_extract("SRCID\\(.+?\\)")
    if(is.na(file_subject)) file_subject = ""
    file_name = str_c(file_year,file_subject,sep = "_")
    
    df$df %>% 
      mutate(search_txt = search_list[i]) %>% 
      write_csv(str_c(file_name,"_df.csv"))
    
    df$author %>% 
      mutate(search_txt = search_list[i]) %>% 
      write_csv(str_c(file_name,"_au.csv"))
    
    df$affiliation %>% 
      mutate(search_txt = search_list[i]) %>% 
      write_csv(str_c(file_name,"_aff.csv"))
    
  }
  
}

error_record %>% print(n = Inf)

###################################################################################################

journal_list %>% 
  filter(count > 5000) %>% 
  pull(source_id) %>% 
  str_c("SRCID(",.,")") %>% 
  str_c("REF ({R: A Language and Environment for Statistical Computing}) AND ",.) %>% 
  str_c(c(" AND PUBYEAR < 2015"," AND PUBYEAR > 2014 AND PUBYEAR < 2018",
          " AND PUBYEAR > 2017")) -> search_list

error_record = tibble()

for(i in seq_along(search_list)){
  
  # get data
  tryCatch(scopus_search(query = search_list[i], 
                         max_count = Inf,
                         count = 25,
                         view = "COMPLETE"),
           error = function(e){conditionMessage(e)}) -> res
  
  if(is_character(res)){
    
    tibble(search_txt = search_list[i],error_message = res) %>% 
      bind_rows(error_record,.) -> error_record
    next
    
  }else if(res$total_results == 0){
    
    tibble(search_txt = search_list[i],error_message = "Result set was empty") %>% 
      bind_rows(error_record,.) -> error_record
    next
    
  }else{
    
    tibble(search_txt = search_list[i],error_message = "Success") %>% 
      bind_rows(error_record,.) -> error_record
    
    df = gen_entries_to_df(res$entries)
    
    file_year = search_list[i] %>% str_extract("20.{2}$")
    file_subject = search_list[i] %>% str_extract("SRCID\\(.+?\\)")
    if(is.na(file_subject)) file_subject = ""
    file_name = str_c(file_year,file_subject,sep = "_")
    
    df$df %>% 
      mutate(search_txt = search_list[i]) %>% 
      write_csv(str_c(file_name,"_df.csv"))
    
    df$author %>% 
      mutate(search_txt = search_list[i]) %>% 
      write_csv(str_c(file_name,"_au.csv"))
    
    df$affiliation %>% 
      mutate(search_txt = search_list[i]) %>% 
      write_csv(str_c(file_name,"_aff.csv"))
    
  }
  
}

error_record %>% print(n = Inf)
###################################################################################################

library(pacman)
p_load(doParallel,dplyr,rscopus,data.table,stringr)



journal_list %>% 
  pull(source_id)->b

a %>% setdiff(b) -> a_b

a_b %>% 
  str_c("SRCID(",.,")") %>% 
  str_c("REF ({R: A Language and Environment for Statistical Computing}) AND ",.) -> search_list

write_rds(search_list,"otherjournal.rds")
read_rds("otherjournal.rds") -> search_list

search_list = search_list[-(1:(939+644))]


search_list %>% 
  str_extract("AND SRCID(.+)") %>% 
  split(ceiling(seq_along(.)/50)) %>% 
  lapply(str_replace_all,pattern = "AND","OR") %>% 
  lapply(str_c,collapse = " ") %>% 
  str_c("REF ({R: A Language and Environment for Statistical Computing}) AND ",.) %>% 
  str_remove("OR")-> search_list


error_record = tibble()

for(i in seq_along(search_list)){
  
  # get data
  tryCatch(scopus_search(query = search_list[i], 
                         max_count = Inf,
                         count = 25,
                         view = "COMPLETE"),
           error = function(e){conditionMessage(e)}) -> res
  
  if(is_character(res)){
    
    tibble(search_txt = search_list[i],error_message = res) %>% 
      bind_rows(error_record,.) -> error_record
    next
    
  }else if(res$total_results == 0){
    
    tibble(search_txt = search_list[i],error_message = "Result set was empty") %>% 
      bind_rows(error_record,.) -> error_record
    next
    
  }else{
    
    tibble(search_txt = search_list[i],error_message = "Success") %>% 
      bind_rows(error_record,.) -> error_record
    
    df = gen_entries_to_df(res$entries)
    
    file_year = ""
    file_subject = search_list[i] %>% str_extract("SRCID\\(.+?\\)")
    if(is.na(file_subject)) file_subject = ""
    file_name = str_c(file_year,file_subject,sep = "_")
    
    df$df %>% 
      mutate(search_txt = search_list[i]) %>% 
      write_csv(str_c(file_name,"_df.csv"))
    
    df$author %>% 
      mutate(search_txt = search_list[i]) %>% 
      write_csv(str_c(file_name,"_au.csv"))
    
    df$affiliation %>% 
      mutate(search_txt = search_list[i]) %>% 
      write_csv(str_c(file_name,"_aff.csv"))
    
  }
  
}


#####################################save
library(pacman)
p_load(dplyr,data.table,stringr,readr)

setwd("/home/tyHuang/workdir/data")

setwd("/home/tyHuang/workdir/data/raw")

dir() %>% str_subset("_df\\.csv$") -> df_names
lst <- lapply(df_names, fread)
dt <- rbindlist(lst,fill = T)
fwrite(dt,"/home/tyHuang/workdir/data/R_df.csv")

dir() %>% str_subset("_au\\.csv$") -> df_names
lst <- lapply(df_names, fread)
dt <- rbindlist(lst,fill = T)
fwrite(dt,"/home/tyHuang/workdir/data/R_au.csv")

dir() %>% str_subset("_aff\\.csv$") -> df_names
lst <- lapply(df_names, fread)
dt <- rbindlist(lst,fill = T)
fwrite(dt,"/home/tyHuang/workdir/data/R_aff.csv")




