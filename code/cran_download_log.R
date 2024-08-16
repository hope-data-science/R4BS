
# library(pacman)
# p_load(tidyverse,cranlogs,data.table,rvest)

## linux
setwd("/home/tyHuang/workdir")

library(pacman)
p_load(doParallel,dplyr,cranlogs,data.table,stringr)

load("data/cran_meta.rdata")
crandb %>% 
  pull(Package) %>% unique() -> pkg_names

# cran_downloads(from = "2019-12-30",to = "2019-12-31")

system.time({
  cran_downloads(from = "2000-01-01", to = "2019-12-31") %>% 
    as_tibble() -> cran_pkg_download
})

# user  system elapsed 
# 0.07    0.00   45.08 

# 用户   系统   流逝
# 0.070  0.005 44.604

system.time({
  cran_downloads("R",from = "2000-01-01", to = "2019-12-31") %>% 
    as_tibble() -> cran_R_download
})

# user  system elapsed 
# 2.53    0.16   20.00 

# 用户   系统   流逝
# 1.168  0.111 23.063

(detectCores() %>% 
  makeCluster() -> cl) %>% 
  registerDoParallel()

is.error = function(x) inherits(x,"try-error") #it can catch an error but still run the codes
system.time({
  foreach(i = pkg_names,
          .packages = c("data.table","cranlogs","magrittr")) %dopar% {
            repeat {
                try({
                  cran_downloads(packages = i,from = "2000-01-01",to = "2019-12-31")
                },silent=T) -> results
                ifelse(is.error(results),Sys.sleep(2),break)
              } 
            
            results %>% as.data.table()
          } %>% 
    rbindlist() %>% 
    as_tibble()-> daily_pkg_download
})

# 用户     系统     流逝
# 70.434   20.946 1732.039

##########################
urls = str_c("https://cran.r-project.org/src/contrib/Archive/",
             pkg_names,"/")

system.time({
  foreach(i = 1:length(urls),
          .packages = c("rvest","data.table","dplyr","stringr")) %dopar% {
    try({
      urls[i] %>%
        read_html() 
    },silent = T) -> results
    
    if(class(results) == "try-error"){
      
        data.table(Name = pkg_names[i],
          `Last modified`=NA,
          Size=NA)
      
    } else
    {
      results %>% 
        html_table %>% 
        .[[1]] %>% 
        select(Name,`Last modified`,Size) %>% 
        filter(str_detect(Name,"_")) %>% 
        as.data.table()
    }
  } %>% 
    rbindlist %>% as_tibble() -> cran_archive_download
})

# 用户    系统    流逝
# 17.075   1.876 352.882

stopCluster(cl)

ls(pattern = "download$")-> to_save
save(list = ls(pattern = "download$"), file = "data/cran_log.rdata")

############################################################

