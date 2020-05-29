library(CzechData)
library(arrow)
library(czso)

cz <- load_RUIAN_state("obce")
write_parquet(cz, here::here("data-processed", "cz_geo_meta.parquet"))

library(czso)

czso_get_codelist <- function(dataset_id, resource_num = 1) {
  czso_meta <- czso:::get_czso_resource_pointer(dataset_id, resource_num)
  cis_url <- czso_meta$url
  cis_url_new <- str_replace(cis_url, "format\\=0$", "format=2&separator=,")
  dt <- read_csv(cis_url_new, locale = locale(encoding = "Windows-1250"),
                 col_types = cols(.default = "c"))
  return(dt)
}

orp_csu <- czso_get_codelist("cis65")
okres_csu <- czso_get_codelist("cis109")
kraj_csu <- czso_get_codelist("cis100")
zuj_csu <- czso_get_codelist("cis51")
obce_csu <- czso_get_codelist("cis51")

zujnuts <- czso_get_codelist("cis100vaz51") %>%
  left_join(kraj_csu, by = c("CHODNOTA1" = "CHODNOTA"))

obecnuts <- czso_get_codelist("cis100vaz43") %>%
  left_join(kraj_csu, by = c("CHODNOTA1" = "CHODNOTA"))

write_parquet(orp_csu, here::here("data-processed", "orp-csu.parquet"))
write_parquet(okres_csu, here::here("data-processed", "okres-csu.parquet"))
write_parquet(kraj_csu, here::here("data-processed", "kraj-csu.parquet"))
write_parquet(zujnuts, here::here("data-processed", "zuj-csu.parquet"))
write_parquet(obecnuts, here::here("data-processed", "obec-csu.parquet"))
