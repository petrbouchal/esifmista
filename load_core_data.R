dt <- read_parquet(here::here("data-processed",
                              "misto_fix-02-gnames.parquet"))
dtl <- read_parquet(here::here("data-processed",
                               "misto_fix-02-gnames_long-geo.parquet"))

prj_meta <- read_parquet(here::here("data-processed", "prj-esif-meta.parquet"))
dt_geostructure_byprj <- read_parquet(here::here("data-processed", "dt_geostructure_by-prj.parquet"))
dt_geostructure_bylvl <- read_parquet(here::here("data-processed", "dt_geostructure_by-lvl.parquet"))

dts <- dt %>%
  select(op_id, starts_with("prj_"), starts_with("p_"), -prj_radek) %>%
  distinct()

orgs <- read_parquet(here::here("data-processed", "orgs_sp.parquet"))
