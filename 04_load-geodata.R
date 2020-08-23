library(sf)
library(czso)
library(CzechData)

library(tidyverse)
library(arrow)


# czk <- czso_get_catalogue()

# sf::st_layers("/Users/petr/Documents/Research/Geodata/AdministrativniCleneni_v13.gdb")

# zsjp <- st_read("/Users/petr/Documents/Research/Geodata/AdministrativniCleneni_v13.gdb",
#                 layer = "ZakladniSidelniJednotkyPolygony")

# obce_p200 <- CzechData::load_Data200("AdministrativniUzemiObce")
# write_parquet(here::here("data-input", "obce_data200.parquet"))
obce_pr <- CzechData::load_RUIAN_state("obce")
write_parquet(here::here("data-input", "obce_ruian.parquet"))

# setdiff(obce_p200$NAMN, obce_pr$nazev)
# setdiff(obce_pr$nazev, obce_p200$NAMN)

# plot(obce_p200, max.plot = 1)

# unique(obce_p200$FCSUBTYPE)
# unique(obce_p200$DESN)
table(obce_pr$status_kod)
table(obce_pr$sm_roz_kod)
table(obce_pr$sm_typ_kod)

CzechData::obce

cisobce <- czso::czso_get_codelist(43)
ciszuj <- czso::czso_get_codelist(51)

table(cisobce$SM_ROZSAH)
table(cisobce$SM_TYP)

obce_clenene <- obce_pr %>%
  filter(!is.na(sm_typ_kod)) %>%
  pull(kod)

obce_neclenene <- obce_pr %>%
  filter(is.na(sm_typ_kod)) %>%
  pull(kod)

obce_momc <- map_dfr(obce_clenene, load_RUIAN_settlement, "MOMC_P")

zuj <- obce_pr %>%
  filter(kod %in% obce_neclenene) %>%
  mutate(typuj = "obec") %>%
  bind_rows(obce_momc %>%
              mutate(typuj = "momc"))

write_rds(zuj, here::here("data-processed", "geo_zuj-all.rds"))
