library(tidyverse)
library(readxl)
library(skimr)
library(visdat)
library(naniar)
library(arrow)
library(janitor)

source(here::here("shared.R"))

read_xlsx("data-input/misto-ico/OP D.xlsx", skip = 1)

# pth <- "data-input/misto/"
pth <- "data-input/misto-ico"

files <- list.files(here::here(pth), full.names = T)
names(files) <- tools::file_path_sans_ext(files) %>% basename()
files
names(files)

files <- str_replace(files,
                     "data-input/misto-ico/OP PIK.xlsx",
                     "data-input/misto/OP PIK.xlsx")
files

dt0 <- map_dfr(files[c(1:2, 4:8)], read_xlsx, skip = 1, .id = "op_id")

geounits_pattern <- str_c("^", geounits, collapse = "|")
geounits_name_pattern <- str_c(paste0(geounits, "$"), collapse = "|")

dt <- dt0 %>%
  janitor::clean_names() %>%
  rename(prj_nazev = nazev_projektu_cz,
         prj_id = registracni_cislo_projektu,
         prj_anotace = anotace_projektu,
         p_nazev = nazev_subjektu,
         p_sidlo_nazev = sidlo_nazev,
         p_ico = ic,
         p_sidlo_id = sidlo_kod,
  ) %>%
  rename_all(str_replace, "(?!_)cislo$", "_id") %>%
  rename_at(vars(matches(geounits_pattern)), ~paste0("g_", .)) %>%
  rename_at(vars(matches(geounits_name_pattern)), ~paste0(., "_nazev")) %>%
  group_by(prj_id) %>%
  mutate(prj_radek = row_number()) %>%
  ungroup()

write_parquet(dt0, here::here("data-processed","misto_raw-mix.parquet"))
write_parquet(dt, here::here("data-processed","misto_renamed-mix.parquet"))
