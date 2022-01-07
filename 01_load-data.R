library(tidyverse)
library(readxl)
library(skimr)
library(visdat)
library(naniar)
library(arrow)
library(janitor)

source(here::here("shared.R"))

# Core data ---------------------------------------------------------------

# read_xlsx("data-input/misto-ico/OP D.xlsx", skip = 1)

# pth <- "data-input/misto/"
pth <- cnf$input_dir

files <- list.files(here::here(pth), full.names = T)
names(files) <- tools::file_path_sans_ext(files) %>% basename()
files
names(files)

# files <- str_replace(files,
#                      "data-input/misto-ico/OP PIK.xlsx",
#                      "data-input/misto/OP PIK.xlsx")
files

dt0 <- map_dfr(files, read_xlsx, skip = 0)

geolevels_pattern <- str_c("^", geolevels, collapse = "|")
geolevels_name_pattern <- str_c(paste0(geolevels, "$"), collapse = "|")

dt <- dt0 %>%
  janitor::clean_names() %>%
  rename(prj_nazev = nazev_projektu,
         prj_id = registracni_cislo_projektu,
         prj_anotace = anotace_projektu,
         p_nazev = nazev_zadatele,
         p_sidlo_nazev = sidlo_nazev,
         p_ico = ic,
         p_sidlo_id = sidlo_kod,
  ) %>%
  rename(g_zuj_id = cislo_zuj,
         g_zuj_nazev = zuj,
         g_obec_nazev = obec,
         g_obec_id = cislo_obce,
         g_orp_id = cislo_rozsirene_obce,
         g_orp_nazev = rozsirena_obec,
         g_orp_id = cislo_rozsirene_obce,
         g_kraj_id = cislo_kraje,
         g_kraj_nazev = kraj,
         g_okres_nazev = okres,
         g_okres_id = cislo_okresu) |>
  group_by(prj_id) %>%
  mutate(prj_radek = row_number()) %>%
  ungroup() |>
  add_op_labels() |>
  select(-op_id, -op_nazev, -op_nazev_zkr) |>
  rename(op_id = op_zkr)

write_parquet(dt0, here::here("data-processed","misto_raw-n.parquet"))
write_parquet(dt, here::here("data-processed","misto_renamed-n.parquet"))


# Projects in NP/CHKO -----------------------------------------------------

prj_chu <- read_excel(here::here(cnf$input_chu_xlsx), skip = 1) %>%
  set_names(c("prj_id", "prj_nazev", "chkonp_nazev"))

write_parquet(prj_chu, here::here("data-processed","prj_chkonp.parquet"))
