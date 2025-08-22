library(tidyverse)
library(sf)

load("../pepfar-census_poly-match/outputs/objects/shp_lsts.rda")

# burundi -----------------------------------------------------------------

cvec <- census_shp_lst$`dominican republic`$adm1 %>% pull(AREA_NAME)
pvec <- cou_shp_lst$`dominican republic`$adm2 %>% pull(name)

ovec <- run_harmonizeR(pvec, cvec)

pjoined <- left_join(cou_shp_lst$`dominican republic`$adm2, ovec$vec_tbl, join_by(name == polyname))
cjoined <- left_join(census_shp_lst$`dominican republic`$adm1, ovec$compvec_tbl, join_by(ADM1_NAME == polyname))
