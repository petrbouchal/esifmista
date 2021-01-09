#!/usr/bin/env Rscript

library(here)

source(here::here("01_load-data.R"))
source(here::here("02_create-meta-header.R"))
source(here::here("03_load-metadata.R"))
source(here::here("04_load-geodata.R"))

rmarkdown::render_site(quiet = T)

source(here::here("export_samples.R"))
source(here::here("export_pro_vyzvu.R"))
