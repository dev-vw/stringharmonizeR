library(tidyverse)
library(sf)

load("../pepfar-census_poly-match/outputs/objects/shp_lsts.rda")

cvec <- census_shp_lst$burundi$adm1 %>% pull(ADM1_NAME)
pvec <- cou_shp_lst$burundi$adm1 %>% pull(name)

ovec <- run_harmonizeR(pvec, cvec)

pjoined <- left_join(cou_shp_lst$burundi$adm1, ovec$vec_tbl, join_by(name == polyname))
cjoined <- left_join(census_shp_lst$burundi$adm1, ovec$compvec_tbl, join_by(ADM1_NAME == polyname))
