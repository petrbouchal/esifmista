library(magrittr)
library(forcats)


geounits <- c("zuj", "obec", "rozobec", "okres", "kraj")
geounits_recoder <- c("zuj", "obec", "rozobec", "okres", "kraj")
names(geounits_recoder) <- 1:5

factor(c("okres", "kraj")) %>% fct_relevel(geounits)

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
             fct_relevel(geounits)) %>%
    ungroup()
}

ptrr::set_ptrr_ggplot_fonts()
ggplot2::update_geom_defaults(geom = "bar", new= list(fill = "darkblue"))
ggplot2::update_geom_defaults(geom = "line", new = list(colour = "darkblue"))
ggplot2::update_geom_defaults(geom = "point", new = list(colour = "darkblue",
                                                         fill = "darkblue"))

ggplot2::theme_set(ptrr::theme_ptrr("both"))
