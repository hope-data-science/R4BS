
# 2020.1.1

library(pacman)
p_load(RWsearch,cranly,tidyverse)

system.time({
  crandb_down()
})
# user  system elapsed 
# 28.01    0.43   57.40 

system.time({
  p_db <- tools::CRAN_package_db()
})
package_db <- clean_CRAN_db(p_db) 

# user  system elapsed 
# 0.54    0.12   19.94 

crandb %>% as_tibble() -> crandb
package_db %>% as_tibble() -> package_db

# pryr::object_size(crandb)
# pryr::object_size(package_db)
# 
# names(crandb) %>% tolower() -> a
# names(package_db) -> b
# 
# a %>% setdiff(b)
# b %>% setdiff(a)
# 
# crandb %>% select(Imports)
# package_db %>% select(imports) 

save(crandb,package_db,file = "data/cran_meta.rdata")

## 把该文件放到"/home/tyHuang/workdir/data"目录下

