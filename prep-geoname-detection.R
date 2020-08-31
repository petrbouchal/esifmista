#!/usr/bin/env Rscript


library(tidyverse)
library(arrow)
library(sf)
library(progressr)
library(beepr)
library(tictoc)
library(furrr)

set.seed(10)


dtl <- read_parquet(here::here("data-processed",
                               "misto_fix-02-gnames_long-geo.parquet"))

zuj_geo <- read_rds(here::here("data-processed", "geo_zuj-all.rds"))
obce_geo <- read_rds(here::here("data-input", "obce_ruian.rds"))
prj_no_obec <- read_parquet(here::here("data-processed", "prj_id_noobec.parquet"))

prj_meta <- read_parquet(here::here("data-processed", "prj-esif-meta.parquet"))

prijemci_public <- read_rds(here::here("data-processed", "prijemci_public.rds"))
vyzvy <- read_parquet(here::here("data-processed", "vyzvy-codes.parquet"))

texty <- dtl %>%
  filter(prj_id %in% prj_no_obec$prj_id,
         !p_ico %in% prijemci_public) %>%
  distinct(prj_nazev, prj_anotace, p_ico, p_nazev, prj_id) %>%
  # sample_n(1000) %>%
  left_join(prj_meta) %>%
  left_join(vyzvy)

zuj_nazvy <- zuj_geo %>%
  st_set_geometry(NULL) %>%
  bind_rows(obce_geo %>%
              st_set_geometry(NULL)) %>%
  select(kod, nazev, starts_with("mluv_pad_")) %>%
  pivot_longer(c(nazev, starts_with("mluv"))) %>%
  separate_rows(value, sep = ";") %>%
  drop_na(value) %>%
  distinct(kod, value, .keep_all = T) %>%
  mutate(value = str_squish(value),
         value = paste0("\\b", value, "\\b")) %>%
  ungroup()

detect_any_name <- function(txt) {
  check_rslt <- map_lgl(zuj_nazvy$value, ~grepl(.x, txt))

  nazvyn <- zuj_nazvy %>%
    filter(check_rslt) %>%
    group_by(kod) %>%
    filter(nchar(value) == max(nchar(value)))

  # nazvyn <- zuj_nazvy[check_rslt,]
  # nazvyn <- nazvyn[nchar(nazvyn$value) == max(nchar(nazvyn$value)),]

  as.list(unique(nazvyn$kod))
}

# detect_any_name("chomutov a most")
# detect_any_name("Most")
# detect_any_name("Chomutov")
# detect_any_name("Most, Chomutov")
# detect_any_name("Chomutov, Most")
#
# detect_any_name("Praha Brno")
# detect_any_name("Brno Praha")
# detect_any_name("Praha Opava")
#
# detect_any_name("Bohuňovice")
# detect_any_name("Bohuňov")
# detect_any_name("Brně")
# detect_any_name("Bohuňovicemi")
# detect_any_name("Bohuňovic")

library(progressr)
library(tidyverse)
library(furrr)
library(arrow)
library(tictoc)
handlers(handler_progress(format = "[:bar] :percent ETA: :eta",
                          complete = "◼",
                          incomplete = " ",
                          current = "▸"))

plan(multiprocess)
tictoc::tic()
with_progress({
  dts <- texty
  # dts <- texty[1:200,]
  p <- progressor(along = dts$prj_id)
  prj_geoname_matched <- dts %>%
    mutate(geonames_in_prjtitle = future_map(prj_nazev,
                                             function(x) {
                                               p(sprintf("x=%s", x))
                                               detect_any_name(x)
                                             }),
           geonames_in_prjtitle = map(geonames_in_prjtitle, as.character),
           geonames_in_prjanot = future_map(prj_anotace,
                                            function(x) {
                                              p(sprintf("x=%s", x))
                                              detect_any_name(x)
                                            }),
           geonames_in_prjanot = map(geonames_in_prjanot, as.character),
           )
  })
tictoc::toc()

prj_geoname_matched <- prj_geoname_matched %>%
  select(prj_id, geonames_in_prjanot, geonames_in_prjtitle)

write_parquet(prj_geoname_matched,
              here::here("data-processed", "geonames_in_prjtexts.parquet"))

prj_geoname_matched %>%
  mutate(gn_len = map_int(geonames_in_prjtitle, length)) %>%
  count(gn_len)

prj_geoname_matched %>%
  mutate(gn_len = map_int(geonames_in_prjanot, length)) %>%
  filter(gn_len > 10) %>% View()
  count(gn_len)

prj_geoname_matched %>%
  mutate(nn = map(geonames_in_prjtitle, as.character)) %>%
  select(-geonames_in_prjtitle) %>%
  write_parquet("x.parquet") %>%
  mutate(gn_len = map_int(nn, length)) %>% View()
  count(gn_len)
