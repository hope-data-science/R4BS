
pacman::p_load(cranlogs,RWsearch,rscopus,dplyr,data.table,patchwork,
               changepoint,lubridate,fst,dtplyr,akc,tidyfst)

knitr::write_bib(c(
  .packages()
), file =  'packages.bib')
