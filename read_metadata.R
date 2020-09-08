# cz <- read_parquet(here::here("data-processed", "cz_geo_meta.parquet"))
csu_orp <- read_parquet(here::here("data-processed", "orp-csu.parquet"))
csu_kraj <- read_parquet(here::here("data-processed", "kraj-csu.parquet"))
csu_okres <- read_parquet(here::here("data-processed", "okres-csu.parquet"))
csu_zuj <- read_parquet(here::here("data-processed", "zuj-csu.parquet"))
csu_obec <- read_parquet(here::here("data-processed", "obec-csu.parquet"))
obecnuts <- read_parquet(here::here("data-processed", "obec-nuts-csu.parquet"))
zujnuts <- read_parquet(here::here("data-processed", "zuj-nuts-csu.parquet"))

ops <- read_parquet(here::here("data-processed", "op-codes.parquet"))
vyzvy <- read_parquet(here::here("data-processed", "vyzvy-codes.parquet"))

ids_and_names <- read_parquet(here::here("data-processed",
                                         "czso-ids-all.parquet"))
ids <- ids_and_names %>%
  select(ends_with("_id")) %>%
  rename_with(.fn = str_remove, pattern = "_id") %>%
  mutate(zuj =  paste0(kraj, zuj),
         obec = paste0(kraj, obec))

ids_short <- ids_and_names %>%
  select(ends_with("_id")) %>%
  rename_with(.fn = str_remove, pattern = "_id")

mas_all <- read_parquet(here::here("data-processed", "mas-metadata.parquet"))
