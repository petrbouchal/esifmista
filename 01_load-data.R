library(tidyverse)
library(readxl)
library(skimr)
library(visdat)
library(naniar)
library(arrow)
library(janitor)

source(here::here("shared.R"))

read_xlsx("data-input/misto/IROP.xlsx")

files <- list.files(here::here("data-input/misto"))
files_paths <- str_glue("data-input/misto/{files}")
names(files_paths) <- tools::file_path_sans_ext(files)
files
files_paths
names(files_paths)
dt0 <- map_dfr(files_paths, read_xlsx, skip = 1, .id = "op_id")

geounits_pattern <- str_c("^", geounits, collapse = "|")
geounits_name_pattern <- str_c(paste0(geounits, "$"), collapse = "|")

dt <- dt0 %>%
  janitor::clean_names() %>%
  rename(prj_nazev = nazev_projektu_cz,
         prj_id = registracni_cislo_projektu,
         prj_anotace = anotace_projektu,
         p_nazev = nazev_subjektu,
         p_sidlo_nazev = sidlo_nazev,
         p_sidlo_id = sidlo_kod,
  ) %>%
  rename_all(str_replace, "(?!_)cislo$", "_id") %>%
  rename_at(vars(matches(geounits_pattern)), ~paste0("g_", .)) %>%
  rename_at(vars(matches(geounits_name_pattern)), ~paste0(., "_nazev")) %>%
  group_by(prj_id) %>%
  mutate(prj_radek = row_number()) %>%
  ungroup()

write_parquet(dt0, here::here("data-processed","misto_raw.parquet"))
write_parquet(dt, here::here("data-processed","misto_renamed.parquet"))
