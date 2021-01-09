library(tidyverse)
library(arrow)
library(writexl)

source("read_metadata.R")

nasekraje_ids <- ids_and_names %>%
  distinct(kraj_id, kraj_nazev) %>%
  filter(kraj_nazev %in% c("Karlovarský kraj", "Středočeský kraj")) %>%
  pull(kraj_id)


obceazujvkrajich <- ids_short %>%
  select(id = zuj, kraj) %>%
  bind_rows(ids_short %>%
              select(id = obec, kraj)) %>%
  distinct(id, kraj) %>%
  filter(kraj %in% nasekraje_ids)

kraj_adder <- ids_and_names %>%
  select(value = obec_id, nazev = obec_nazev, kraj_id, kraj_nazev) %>%
  bind_rows(ids_and_names %>%
              select(value = zuj_id, nazev = zuj_nazev, kraj_id, kraj_nazev)) %>%
  distinct()


dta <- open_dataset("data-output/dtl-all-arrow//")

names(dta)

relevant_prj_ids <- dta %>%
  filter(op_id %in% c("OP ZP", "OP Z"),
         value %in% obceazujvkrajich$id) %>%
  select(prj_id) %>%
  collect() %>%
  distinct() %>%
  pull(prj_id)

length(relevant_prj_ids)

dta_sample <- dta %>%
  filter(op_id %in% c("OP ZP", "OP Z")) %>%
  filter(prj_id %in% relevant_prj_ids) %>%
  collect()

dta_sample %>%
  distinct(prj_id) %>%
  count()

dta_sample %>%
  filter(value %in% obceazujvkrajich$id) %>%
  distinct(prj_id) %>%
  count()

relevant_prj_ids_nootherkraj <- dta_sample %>%
  left_join(obceazujvkrajich %>% rename(value = id)) %>%
  group_by(prj_id) %>%
  summarise(has_other_kraj = any(is.na(kraj))) %>%
  filter(!has_other_kraj) %>%
  distinct(prj_id) %>%
  pull()

dta_sample_nootherkraj <- dta_sample %>%
  filter(prj_id %in% relevant_prj_ids_nootherkraj)

dta_sample_nootherkraj %>%
  distinct(prj_id, op_id) %>%
  count(op_id)

dta_sample %>%
  distinct(prj_id, op_id) %>%
  count(op_id)

write_xlsx(tibble(cislo_projektu = relevant_prj_ids_nootherkraj),
           here::here("data-export", "vyber_KVK-SCK_OPZ-OPZP_cisla.xlsx"))

dta_sample_nootherkraj %>%
  select(prj_id, radek, uroven = level, kod = value, op_id) %>%
  left_join(kraj_adder %>% rename(kod = value)) %>% View()

write_xlsx(dta_sample_nootherkraj %>%
             select(prj_id, radek, uroven = level, kod = value, op_id) %>%
             left_join(kraj_adder %>% rename(kod = value)),
           here::here("data-export", "vyber_KVK-SCK_OPZ-OPZP_obce-realizace.xlsx"))
