library(magrittr)
library(forcats)

cnf <- config::get()

geolevels <- c("zuj", "obec", "orp", "okres", "kraj")
geolevels_recoder <- c("zuj", "obec", "orp", "okres", "kraj")
names(geolevels_recoder) <- 1:5

# factor(c("okres", "kraj")) %>% fct_relevel(geolevels)

col_blue <- "#428bca"
col_red <- "#d9534f"
col_orange <- "#f0ad4e"
col_green <- "#5cb85c"

Sys.setlocale("LC_ALL", locale = "cs_CZ.UTF-8")

make_long_geo <- function(data) {
  data %>%
    group_by(prj_id) %>%
    pivot_longer(cols = starts_with("g_")) %>%
    separate(name, c("g", "level", "typ")) %>%
    select(-g) %>%
    mutate(level = as_factor(level) %>%
             fct_relevel(geolevels) %>%
             factor(ordered = T)) %>%
    ungroup()
    # ungroup() %>%
    # spread(typ, value)
}

ptrr::set_ptrr_ggplot_fonts()
ggplot2::update_geom_defaults(geom = "bar", new= list(fill = "darkblue"))
ggplot2::update_geom_defaults(geom = "line", new = list(colour = "darkblue"))
ggplot2::update_geom_defaults(geom = "point", new = list(colour = "darkblue",
                                                         fill = "darkblue"))

ggplot2::theme_set(ptrr::theme_ptrr("both"))

mas_pravniformy_regex <- "[,]?\\s?(([zo]\\.\\s?[ús]\\.\\s?)|(([o]\\.\\s?[p]\\.\\s?[s]\\.\\s?)))"

add_long_geoid <- function(data, ids) {

  stopifnot("geo_id" %in% names(data))
  stopifnot("level" %in% names(data))

  zuj_obce_adder <- bind_rows(
    ids %>%
      filter(!zuj %in% ids$obec) %>%
      select(geo_id_long = zuj) %>%
      mutate(level = "zuj"),
    ids %>%
      distinct(obec) %>%
      rename(geo_id_long = obec) %>%
      mutate(level = "obec")
  ) %>%
    mutate(geo_id = str_sub(geo_id_long, 6, 11)) %>%
    select(-level)

  data %>%
    left_join(zuj_obce_adder)
}

add_chunk_number <- function(data, group = op_id) {
  data <- ungroup(data)
  groups_orig <- group_vars(data)
  row_nums <- data %>%
    count({{group}}, prj_id) %>%
    group_by({{group}}) %>%
    mutate(runsum = cumsum(n),
           chunk = floor(runsum/4.5e5) + 1,
           chunk = as.integer(chunk))

  dt <- data %>%
    left_join(row_nums %>%
                select(prj_id, chunk)) %>%
    group_by(across(all_of(groups_orig)))

  rslt <- dt

  return(rslt)
}
