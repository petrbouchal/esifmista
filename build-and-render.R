#!/usr/bin/env Rscript

library(here)

source(here::here("01_load-data.R"))
source(here::here("02_create-meta-header.R"))
source(here::here("03_load-metadata.R"))

rmarkdown::render_site(quiet = T)
