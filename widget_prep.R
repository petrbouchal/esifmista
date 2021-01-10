
gorp <- CzechData::load_RUIAN_state("orp") %>%
  left_join(csu_orp %>% select(kod = KOD_RUIAN, kod_csu = CHODNOTA)) %>%
  select(kod = kod_csu, nazev) %>%
  mutate(typuj = "orp")

gokres <- CzechData::load_RUIAN_state("okresy") %>%
  select(kod = lau1_kod, nazev) %>%
  mutate(typuj = "okres")

gkraj <- CzechData::load_RUIAN_state("kraje") %>%
  select(kod = nuts3_kod, nazev) %>%
  mutate(typuj = "kraj")

write_rds(gkraj, "gkraj.rds")

geo_zuj <- read_rds("data-processed/geo_zuj-all.rds") %>%
  mutate(typuj = recode(typuj, momc = "zuj")) %>%
  select(kod, nazev, typuj)

gobce <- read_rds("data-input/obce_ruian.rds") %>%
  filter(!kod %in% geo_zuj$kod) %>%
  select(kod, nazev) %>%
  mutate(typuj = "obec")

gall <- rbind(gorp, gokres, gkraj, geo_zuj, gobce) %>%
  sf::st_cast("POLYGON") %>%
  sf::st_simplify(preserveTopology = TRUE, dTolerance = 500) %>%
  sf::st_transform(4326)


gall %>%
  ggplot() +
  geom_sf(aes(colour = typuj)) +
  facet_wrap(~typuj)

prjs_geostatus <- read_parquet(here::here("data-processed",
                                          "projects-geo-check-groups.parquet"))
places_geostatus <- read_parquet(here::here("data-processed",
                                            "places-geo-check.parquet"))

prj_id_nh <- prjs_geostatus %>% filter(geostatus == "více míst nehierarchicky") %>%
  pull(prj_id)
plcnh <- places_geostatus %>% filter(prj_id %in% prj_id_nh)

prjgeo <- plcnh %>%
  mutate(level = if_else(level == "zuj" & value %in% ids$obec,
                         "obec", as.character(level)),
         value = if_else(level %in% c("obec", "zuj"), str_sub(value, 6, 11), value)) %>%
  select(prj_id, kod = value, typuj = level) %>%
  left_join(gall) %>%
  sf::st_as_sf() %>%
  filter(!sf::st_is_empty(geometry)) %>%
  mutate(numbr = as.numeric(as.factor(prj_id)))

write_rds(prjgeo, "prjgeo.rds")


library(crosstalk)
library(leaflet)
library(plotly)

pplot_s <- prjgeo %>%
  # filter(!sf::st_is_empty(geometry)) %>%
  ggplot() +
  geom_sf(aes(colour = typuj)) +
  facet_wrap(~typuj)
ggplotly(pplot_s)

ddd <- highlight_key(prjgeo, ~numbr)
pplot <- ddd %>%
  # filter(!sf::st_is_empty(geometry)) %>%
  ggplot() +
  geom_sf(aes(colour = numbr)) +
  facet_wrap(~typuj)

ggplotly(pplot)

bscols(
  filter_slider("xxx", "yyy", ddd, ~numbr),
  ggplotly(pplot),
  DT::datatable(ddd)
)


prjs <- SharedData$new(prjgeo)
factpal <- colorFactor(topo.colors(5), prjs$typuj)
c(
  leaflet(prjs, width = "100%") %>%
    addTiles() %>%
    addPolygons(data = prjs, color = ~factpal(typuj)),
  filter_select(id = "sldr", label = "lbl", sharedData = prjs, group = ~typuj)
)



