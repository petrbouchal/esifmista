library(CzechData)
library(arrow)
library(czso)
library(dplyr)
library(xml2)

cz <- load_RUIAN_state("obce")
write_parquet(cz, here::here("data-processed", "cz_geo_meta.parquet"))

metaciselnik <- c(obec = 43,
                  zuj = 51,
                  okres = 109,
                  orp = 65,
                  kraj = 100)

orp_csu <- czso_get_codelist("cis65")
okres_csu <- czso_get_codelist("cis109")
kraj_csu <- czso_get_codelist("cis100")
zuj_csu <- czso_get_codelist("cis51")
obec_csu <- czso_get_codelist("cis43")

zujnuts <- czso_get_codelist("cis100vaz51") %>%
  left_join(kraj_csu, by = c("CHODNOTA1" = "CHODNOTA"))

obecnuts <- czso_get_codelist("cis100vaz43") %>%
  left_join(kraj_csu, by = c("CHODNOTA1" = "CHODNOTA"))

obecorp <- czso_get_codelist("cis65vaz43")
# obecokres <- czso_get_codelist("cis109vaz43") # vraci 0 radku
orpkraj <- czso_get_codelist("cis100vaz65")
zujokres <- czso_get_codelist("cis109vaz51")
zujobec <- czso_get_codelist("cis43vaz51")

write_parquet(orp_csu, here::here("data-processed", "orp-csu.parquet"))
write_parquet(okres_csu, here::here("data-processed", "okres-csu.parquet"))
write_parquet(kraj_csu, here::here("data-processed", "kraj-csu.parquet"))
write_parquet(zuj_csu, here::here("data-processed", "zuj-csu.parquet"))
write_parquet(obec_csu, here::here("data-processed", "obec-csu.parquet"))

write_parquet(zujnuts, here::here("data-processed", "zuj-nuts-csu.parquet"))
write_parquet(obecnuts, here::here("data-processed", "obec-nuts-csu.parquet"))
write_parquet(obecorp, here::here("data-processed", "obec-orp-csu.parquet"))
write_parquet(zujokres, here::here("data-processed", "zuj-okres-csu.parquet"))

czsoids_all <- zuj_csu %>%
  select(zuj_id = CHODNOTA, zuj_nazev = TEXT) %>%
  full_join(zujobec %>%
              select(obec_id = CHODNOTA1, zuj_id = CHODNOTA2)) %>%
  left_join(obecorp %>%
              select(orp_id = CHODNOTA1, orp_nazev = TEXT1,
                     obec_id = CHODNOTA2)) %>%
  left_join(zujokres %>%
              select(okres_id = CHODNOTA1, okres_nazev = TEXT1,
                     zuj_id = CHODNOTA2)) %>%
  left_join(obecnuts %>%
              select(kraj_id = CZNUTS, kraj_nazev = TEXT1,
                     obec_id = CHODNOTA2))


write_parquet(czsoids_all, here::here("data-processed", "czso-ids-all.parquet"))


# ESIF metadata -----------------------------------------------------------

ops <- tribble(~op_id, ~op_abbrev, ~op_name, ~op_num,
               "OP PIK", "PIK", "", 1,
               "OP ZP", "ŽP", "", 5,
               "OP D", "D", "", 4,
               "OP PPR", "PPR", "", 7,
               "IROP", "IROP", "", 6,
               "OP Z", "Z", "", 3,
               "OP TP", "TP", "", 8,
               "OP VVV", "VVV", "", 2,
               ) %>%
  mutate(op_tnum = str_pad(op_num, width = 2, pad = "0"))

write_parquet(ops, here::here("data-processed", "op-codes.parquet"))


# Metadata vyzev ----------------------------------------------------------

library(xml2)
library(tidyverse)

xmldoc <- xml2::read_xml("https://ms14opendata.mssf.cz/SeznamVyzev.xml")

xmldoc

vyzvyxml <- xmldoc %>%
  xml2::xml_children()

vyzvy <- purrr::map_df(vyzvyxml, function(x) {
  ids <- x %>% xml2::xml_child(3) %>% xml2::xml_text()
  descs <- x %>% xml2::xml_child(4) %>% xml2::xml_text()

  tibble(vyzva_id = ids, vyzva_nazev = descs)

})

write_parquet(vyzvy, here::here("data-processed", "vyzvy-codes.parquet"))


# org metadata from statnipokladna ----------------------------------------

library(statnipokladna)

if(!file.exists("data-input/orgs.rds")) {
  orgs_raw <- sp_get_codelist("ucjed", dest_dir = "data-input")
  druhuj <- sp_get_codelist("druhuj", dest_dir = "data-input")

  orgs <- orgs_raw %>%
    left_join(druhuj)

  write_rds(orgs, "data-input/orgs.rds", compress = "gz")

} else {
  orgs <- read_rds("data-input/orgs.rds")
}

orgs %>%
  distinct(druhuj_id, poddruhuj_id, druhuj_nazev) %>% View()

poddruhy <- statnipokladna::sp_get_codelist("poddruhuj")
druhy <- statnipokladna::sp_get_codelist("poddruhuj")

poddruhy_joinable <- poddruhy %>%
  mutate(across(c(poddruhuj_id, druhuj_id), ~str_remove(., "^0")),
         poddruhuj_id = paste0(druhuj_id, poddruhuj_id)) %>%
  select(-druhuj_id)

orgs_detail <- orgs %>%
  left_join(poddruhy_joinable)

write_parquet(orgs_detail, here::here("data-processed", "orgs_sp.parquet"))


