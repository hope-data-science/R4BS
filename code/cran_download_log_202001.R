
# library(pacman)
# p_load(tidyverse,cranlogs,data.table,rvest)

## linux
setwd("/home/tyHuang/workdir")

library(pacman)
p_load(doParallel,dplyr,cranlogs,data.table,stringr)

load("data/cran_meta.rdata")
crandb %>% 
  pull(Package) %>% unique() -> pkg_names

(detectCores() %>% 
  makeCluster() -> cl) %>% 
  registerDoParallel()

is.error = function(x) inherits(x,"try-error") #it can catch an error but still run the codes
system.time({
  foreach(i = pkg_names,
          .packages = c("data.table","cranlogs","magrittr")) %dopar% {
            repeat {
                try({
                  cran_downloads(packages = i,from = "2020-01-01",to = "2020-01-31")
                },silent=T) -> results
                ifelse(is.error(results),Sys.sleep(2),break)
              } 
            
            results %>% as.data.table()
          } %>% 
    rbindlist() %>% 
    as_tibble()-> daily_pkg_download_202001
})

# 用户    系统    流逝
# 16.384   1.808 239.940


stopCluster(cl)

fwrite(daily_pkg_download_202001,"data/daily_pkg_download_202001.csv") 
fst::write_fst(daily_pkg_download_202001,"data/daily_pkg_download_202001.fst",compress = 100)

daily_pkg_download_202001 %>% 
  as.data.table() %>% 
  .[,.(count = sum(count)),by = package] -> pkg_count_202001

fwrite(pkg_count_202001,"data/pkg_count_202001.csv")


############################################################

