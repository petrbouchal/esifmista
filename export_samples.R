library(tidyverse)
library(writexl)
library(arrow)

dta <- open_dataset(here::here("data-output", "dtl-all-arrow"))

op_z_onechunk <- dta %>%
  filter(op_id == "OP Z", chunk == 1) %>%
  collect()

typy_duvody <- dta %>%
  select(obec_puvod, rozpad_typ, rozpad_duvod) %>%
  collect() %>%
  count(obec_puvod, rozpad_typ, rozpad_duvod)

dta_proj <- dta %>%
  select(prj_id, op_id, obec_puvod, rozpad_typ, rozpad_duvod) %>%
  collect() %>%
  group_by(op_id, obec_puvod, rozpad_typ, rozpad_duvod) %>%
  sample_n(10, replace = T) %>%
  distinct(prj_id) %>%
  pull(prj_id)

dt_sample <- dta %>%
  filter(prj_id %in% dta_proj) %>%
  collect()

writexl::write_xlsx(dt_sample, here::here("data-export", "sample_export.xlsx"))
