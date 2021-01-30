#!/usr/bin/env Rscript

library(here)
library(config)

cnf <- config::get()

source(here::here("01_load-data.R"))
source(here::here("02_create-meta-header.R"))
source(here::here("03_load-metadata.R"))
source(here::here("04_load-geodata.R"))

# rmarkdown::render("05_resolve-simple.Rmd")
# rmarkdown::render("07_resolve-obecless-projects.Rmd")
# rmarkdown::render("08_resolve-placeless.Rmd")
# rmarkdown::render("09_resolve-complicated.Rmd")

rmarkdown::render_site(quiet = F)

if(cnf$export_excel) source(here::here("export-excel.R"))
