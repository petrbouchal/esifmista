library(CzechData)
library(arrow)
library(czso)
library(dplyr)
library(xml2)
library(readxl)

source("shared.R")

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

  tibble(vyzva_id = ids, vyzva_nazev = descs) %>%
    mutate(vyzva_nazev = str_remove(vyzva_nazev, "\\n$"))

})

write_parquet(vyzvy, here::here("data-processed", "vyzvy-codes.parquet"))


# org metadata from statnipokladna ----------------------------------------

library(statnipokladna)

if(!file.exists(here::here("data-input", "orgs.parquet"))) {
  orgs_raw <- sp_get_codelist("ucjed", dest_dir = "data-input")
  druhuj <- sp_get_codelist("druhuj", dest_dir = "data-input")

  orgs <- orgs_raw %>%
    left_join(druhuj)

  write_parquet(orgs, here::here("data-input", "orgs.parquet"))

} else {
  orgs <- read_parquet(here::here("data-input", "orgs.parquet"))
}

orgs %>%
  count(druhuj_id, poddruhuj_id, druhuj_nazev)

poddruhy <- statnipokladna::sp_get_codelist("poddruhuj")
druhy <- statnipokladna::sp_get_codelist("druhuj")

poddruhy_joinable <- poddruhy %>%
  mutate(across(c(poddruhuj_id, druhuj_id), ~str_remove(., "^0")),
         poddruhuj_id = paste0(druhuj_id, poddruhuj_id)) %>%
  select(-druhuj_id)

orgs_detail <- orgs %>%
  mutate(across(c(poddruhuj_id, druhuj_id), ~str_pad(., 2, pad = "0"))) %>%
  left_join(poddruhy)

orgs_detail %>%
  count(druhuj_id, poddruhuj_id, druhuj_nazev, poddruhuj_nazev, sort = T)

write_parquet(orgs_detail, here::here("data-processed", "orgs_sp.parquet"))


# MAS metadata ------------------------------------------------------------

masczsofile <- tempfile()
download.file("https://www.czso.cz/documents/10180/23194580/data_pro_mas_2014_2019_b_aktualizace_k_30_6_2020.xlsx/3e6bf657-507b-4592-a6b9-959392e934ba?version=1.1",
              masczsofile)

readxl::excel_sheets(masczsofile)

get_mas_year <- function(year) {
  read_excel(masczsofile, sheet = as.character(year)) %>%
    select(1:3) %>%
    set_names(c("obec_kod", "obec_nazev", "mas_nazev")) %>%
    mutate(across(dplyr::everything(), as.character)) %>%
    mutate(year = as.character(year))
}

# NB looking at 2014 shows that some MAS renamed into 2015
# plus there were no projects in 2014 anyway

mas_all <- map_dfr(2015:2019, get_mas_year) %>%
  mutate(mas_nazev_simple = str_remove_all(mas_nazev, mas_pravniformy_regex)) %>%
  filter(mas_nazev != "(obec vznikla k 1.1.2016)")

mas_all %>%
  count(mas_nazev_simple, year) %>%
  spread(year, n)

p_nazev_simple <- dtl %>%
  distinct(p_nazev) %>%
  mutate(p_nazev_simple = str_remove_all(p_nazev, mas_pravniformy_regex)) %>%
  pull()

(unique(mas_all$mas_nazev) %in% unique(dtl$p_nazev)) %>% table()
(unique(mas_all$mas_nazev_simple) %in% unique(dtl$p_nazev)) %>% table()
(unique(mas_all$mas_nazev_simple) %in% p_nazev_simple) %>% table()

mas_all %>%
  filter(!mas_nazev_simple %in% p_nazev_simple) %>%
  distinct(mas_nazev_simple)

write_parquet(mas_all, here::here("data-processed", "mas-metadata.parquet"))


# Zrizovatele prispevkovek ------------------------------------------------

orgs_progper <- orgs_detail %>%
  filter(end_date > "2015-01-01") %>%
  # filter(end_date > lubridate::now()) %>%
  select(-start_date, -end_date, -datumakt, -isektor_id, -ulice, -sidlo, -psc, -pocob) %>%
  distinct()

orgs_progper %>%
  group_by(ico) %>%
  mutate(pocet_radku = n()) %>%
  filter(pocet_radku > 1) %>%
  arrange(pocet_radku, ico)

length(unique(orgs_progper$ico))
length(unique(orgs_current$ico))

table(dt$p_ico %in% orgs_progper$ico)

zrizovani <- orgs_progper %>%
  filter(poddruhuj_nazev %in% c("Příspěvkové organizace zřízené obcí",
                                "Příspěvkové organizace zřízené MČ",
                                "Příspěvkové organizace zřízené krajem")) %>%
  distinct(ico, zrizovatel_id, druhuj_nazev, poddruhuj_nazev, ucjed_nazev) %>%
  mutate(zrizovatel_id = na_if(zrizovatel_id, "")) %>%
  drop_na(zrizovatel_id)

zrizovani %>% count(poddruhuj_nazev)

zrizovatele <- orgs_progper %>%
  filter(druhuj_nazev %in% c("Obce", "Kraje") | poddruhuj_nazev == "Městská část") %>%
  distinct(csuis_ucjed_id, ico, ico_lau1, nuts_id, ucjed_nazev, druhuj_nazev, poddruhuj_nazev) %>%
  distinct(csuis_ucjed_id, ico, ico_lau1, nuts_id,  druhuj_nazev, poddruhuj_nazev, .keep_all = T)

zrizovaci_vztahy <- zrizovani %>%
  left_join(zrizovatele %>%
              rename(zrizovatel_spid = csuis_ucjed_id,
                     zrizovatel_ico = ico,
                     zrizovatel_obec_kod = ico_lau1,
                     zrizovatel_nuts = nuts_id,
                     zrizovatel_nazev = ucjed_nazev,
                     zrizovatel_druh = druhuj_nazev,
                     zrizovatel_poddruh = poddruhuj_nazev),
            by = c("zrizovatel_id" = "zrizovatel_spid")) %>%
  replace_na(list(zrizovatel_poddruh = "")) %>%
  mutate(zrizovatel_obec_kod = na_if(zrizovatel_obec_kod, "000000"),
         zrizovatel_typ = if_else(zrizovatel_poddruh == "Městská část", zrizovatel_poddruh, zrizovatel_druh)
  ) %>%
  select(-zrizovatel_poddruh, -zrizovatel_druh) %>%
  mutate(zrizovatel_nazev = str_remove(zrizovatel_nazev, "Městská část ") %>%
           str_replace(" - ", "-"))

write_parquet(zrizovaci_vztahy, here::here("data-processed", "zrizovatele.parquet"))
