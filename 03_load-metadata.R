library(CzechData)
library(arrow)
library(czso)
library(dplyr)

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
obce_csu <- czso_get_codelist("cis43")

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
write_parquet(obce_csu, here::here("data-processed", "obce-csu.parquet"))

write_parquet(zujnuts, here::here("data-processed", "zuj-nuts-csu.parquet"))
write_parquet(obecnuts, here::here("data-processed", "obec-nuts-csu.parquet"))
write_parquet(obecorp, here::here("data-processed", "obec-orp-csu.parquet"))
write_parquet(zujokres, here::here("data-processed", "zuj-okres-csu.parquet"))
