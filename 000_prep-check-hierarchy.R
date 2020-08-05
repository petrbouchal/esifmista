library(tidyverse)
library(arrow)

ids_and_names <- read_parquet(here::here("data-processed", "czso-ids-all.parquet"))
ids <- ids_and_names %>%
  select(ends_with("_id")) %>%
  rename_with(.fn = str_remove, pattern = "_id") %>%
  mutate(zuj =  paste0(kraj, zuj),
         obec = paste0(kraj, obec))


#' Check geographical hierarchy
#'
#' Check whether a given ID of a given level is within a parent of another level.
#'
#' Not vectorised, so must be applied using `pmap_lgl()`.
#' Relies on an object `ids` defined above, which contains a complete table of
#' geographical IDs, defining the hierarchy within which checking should be done.
#' The table is in wide format, i.e. each level is defined by a column.
#' It has a row for each lowest-level unit (ZUJ) and columns defining the IDs of
#' all its parents.
#'
#' @param id the ID of the unit to be checked (string, full NUTS form).
#' @param parent the ID of the (supposed) parent (string, full NUTS form).
#' @param level level of the checked ID.
#' @param parent_level level of the (supposed) parent ID.
#'
#' @return boolean of length one
is_parent <- function(id, parent, level, parent_level) {

  stopifnot(level != parent_level)
  stopifnot(level != "kraj")

  filter_var <- sym(as.character(level))
  pull_var <- sym(as.character(parent_level))

  parents <- ids %>%
    filter(!!filter_var == id) %>%
    pull(!!pull_var) %>%
    unique()
  # print(rslt)
  # print(id2)
  rslt <- parent %in% parents

  return(rslt)
}

is_parent("CZ010554782", "CZ010", "obec", "kraj") # Praha
is_parent("CZ010554782", "CZ010", "kraj", "kraj") # should fail
is_parent("CZ010582786", "CZ010", "obec", "kraj") # Brno
is_parent("CZ010582786", "CZ010", "obec", "obec") # should fail

is_parent("CZ0513", "CZ051", "okres", "kraj") # Brno

# load sample data
dfs <- read_parquet("data-processed/sample_multilevel.parquet") %>%
  group_by(prj_id) %>%
  filter(n_distinct(level) > 1) %>%
  mutate(level_num = as.numeric(level),
         # change the name of ORP which is named oddly in original data
         level = fct_recode(level, orp = "rozobec"))

unique(dfs$level)
levels(dfs$level)

# derive single-project sample datasets
oneproj_false <- dfs[dfs$prj_id == dfs$prj_id[320],]
max(as.numeric(oneproj_false$level))

oneproj_true <- dfs[dfs$prj_id == dfs$prj_id[10003],]
max(as.numeric(oneproj_true$level))

projids <- unique(dfs$prj_id)

oneproj_rand <- dfs[dfs$prj_id == sample(projids, size = 1),]
max(as.numeric(oneproj_rand$level))


#' Check each geo ID against any parent geo IDs in a project
#'
#' Must be applied using `group_map()`
#'
#' @param df tibble, with columns `value` and `level`. One `df` per project.
#'
#' @return a tibble derived from `df` with row for all unit-parent combinations
#' and a column indicating whether the combination exists in the real hierarchy.
check_all_parents <- function(df) {

  # create numeric level for unambiguous comparison
  df <- df %>% mutate(level_num = as.numeric(level))

  # print(df)

  # select distinct values, exclude kraj (top level with no need to check)

  unique_values_nokraj <- df %>%
    distinct(value, level, level_num) %>%
    filter(level != "kraj")

  # print(unique_values_nokraj)

  #' FUNCTION_TITLE
  #'
  #' Find all IDs against which a given ID can be checked
  #'
  #' To be called inside parent fn, so relies on `df` object existing in
  #' the parent environment.
  #'
  #' @param checked_value DESCRIPTION.
  #' @param checked_val_level DESCRIPTION.
  #'
  #' @return RETURN_DESCRIPTION
  #' @examples
  #' # ADD_EXAMPLES_HERE
  get_relevant_parents <- function(val, lev) {
    lev_num <- as.numeric(lev)

    # recall `df` contains cols
    # - value
    # - level
    # - level_num

    df %>%
      # only levels higher than that of the checked value
      filter(level_num > lev_num) %>%
      # distinct
      distinct(value, level, level_num) %>%
      # rename cols
      mutate(this_value = val,
             this_val_level = lev,
             this_val_level_num = lev_num)
  }


  relevant_parents <- map2_dfr(unique_values_nokraj$value,
                               unique_values_nokraj$level,
                               get_relevant_parents)
  print("Relevant parents")
  print(relevant_parents)

  # print(checkable_superiors)

  relevant_parents %>%
    mutate(levels_ok = pmap_lgl(list(this_value,
                                     value,
                                     this_val_level,
                                     level),
                                is_parent)) %>%
    select(-ends_with("_num"))

}


check_all_parents(oneproj_true)
check_all_parents(oneproj_false)
check_all_parents(oneproj_rand)

is_parent("CZ053574449", "CZ053574449", "zuj", "obec")
