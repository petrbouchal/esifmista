#!/usr/bin/env Rscript

library(here)

cnf <- config::get()

source(here::here("01_load-data.R"))
source(here::here("02_create-meta-header.R"))
source(here::here("03_load-metadata.R"))
source(here::here("04_load-geodata.R"))

rmarkdown::render("05_resolve-simple.Rmd")
rmarkdown::render("07_resolve-obecless-projects.Rmd")
rmarkdown::render("08_resolve-placeless.Rmd")
rmarkdown::render("09_resolve-complicated.Rmd")
rmarkdown::render("10_compile-export.Rmd")


if(cnf$export_excel) source(here::here("export-excel.R"))

rmarkdown::render_site(quiet = F)

# ds <- arrow::open_dataset(cnf$arrow_output_dir)
#
# ds_prj <- ds |> distinct(prj_id) |> pull()
#
# xl <- readxl::read_excel("data-input/sestavy-20210603/E007 Místo realizace žádosti nebo projektu.xlsx")
# prj_regional_new <- xl |> distinct(`Registrační číslo projektu`) |> pull()
#
# xl |>
#   filter(!`Registrační číslo projektu` %in% ds_prj)

