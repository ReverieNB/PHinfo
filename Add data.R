library(tidyverse)
library(sf)

mpls_nbhds <- read_sf("C:/Users/NAIM001/Downloads/Minneapolis_Neighborhoods/Minneapolis_Neighborhoods.shp") |>
  janitor::clean_names()

#Save data to package
usethis::use_data(mpls_nbhds)

#Document data
usethis::use_r("data")
