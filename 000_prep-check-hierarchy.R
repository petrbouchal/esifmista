# TO DO
# [x] clean up column names in output, preferably internally too
# [-] handle kraj in input - should be TRUE or NA on output
# [x] handle reliance on unstated inputs - df and ids
# [x] apply to multiple projects
#

library(tidyverse)
library(arrow)
library(progress)
# library(furrr)

library(tictoc)
library(beepr)

# load sample data --------------------------------------------------------

ids_and_names <- read_parquet(here::here("data-processed", "czso-ids-all.parquet"))
ids <- ids_and_names %>%
  select(ends_with("_id")) %>%
  rename_with(.fn = str_remove, pattern = "_id") %>%
  mutate(zuj =  paste0(kraj, zuj),
         obec = paste0(kraj, obec))

make_pb <- function(n = 1000) {
  progress_bar$new(
    format = "  checking spatial hierarchy [:bar] :percent ETA: :eta",
    complete = "◼",
    incomplete = " ",
    current = "▸",
    total = n, clear = FALSE, width= 60)
}

pb <- make_pb()



#' Check geographical hierarchy
#'
#' Check whether a given ID of a given level is within a parent of another level.
#'
#' Not vectorised, so must be applied using `pmap_lgl()`.
#'
#' @param id the ID of the unit to be checked (string, full NUTS form).
#' @param parent the ID of the (supposed) parent (string, full NUTS form).
#' @param level level of the checked ID.
#' @param parent_level level of the (supposed) parent ID.
#' @param id_table table containing complete spatial hierarchy - a complete table of
#' geographical IDs, defining the hierarchy within which checking should be done.
#' The table is in wide format, i.e. each level is defined by a column.
#' It has a row for each lowest-level unit (ZUJ) and columns defining the IDs of
#' all its parents
#'
#' @return boolean of length one
is_parent <- function(id, parent, level, parent_level, id_table) {

  stopifnot(level != parent_level)
  # stopifnot(level != "kraj")

  filter_var <- sym(as.character(level))
  pull_var <- sym(as.character(parent_level))

  parents <- id_table %>%
    filter(!!filter_var == id) %>%
    pull(!!pull_var) %>%
    unique()
  # print(rslt)
  # print(id2)
  rslt <- parent %in% parents

  return(rslt)
}

is_parent("CZ010554782", "CZ010", "obec", "kraj", ids) # Praha v Praze
# is_parent("CZ010554782", "CZ010", "kraj", "kraj", ids) # should fail
is_parent("CZ010582786", "CZ010", "obec", "kraj", ids) # Brno v Praze
# is_parent("CZ010582786", "CZ010", "obec", "obec", ids) # should fail

is_parent("CZ0513", "CZ051", "okres", "kraj", ids) # Brno

# load sample data
dfs <- read_parquet("data-processed/sample_multilevel.parquet") %>%
  group_by(prj_id) %>%
  filter(n_distinct(level) > 1) %>%
  mutate(level_num = as.numeric(level))

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

twoproj_rand <- dfs[dfs$prj_id %in% sample(projids, size = 2),]
thousandproj_rand <- dfs[dfs$prj_id %in% sample(projids, size = 1000),]


#' Check each geo ID against any parent geo IDs in a project
#'
#' Must be applied using `group_map()`
#'
#' @param df tibble, with columns `value` and `level`. One `df` per project.
#'
#' @return a tibble derived from `df` with row for all unit-parent combinations
#' and a column indicating whether the combination exists in the real hierarchy.
check_all_parents <- function(df, id_table) {

  pb$tick()

  # print(unique_values_nokraj)

  #' Find checkable parent IDs
  #'
  #' Find all IDs against which a given ID can be checked
  #'
  #' To be called inside parent fn, so relies on `df` object existing in
  #' the parent environment.
  #'
  #' @param checked_value DESCRIPTION.
  #' @param checked_val_level DESCRIPTION.
  #' @param data data, same shape as `df` arg in `check_all_parents()`
  #'
  #' @return RETURN_DESCRIPTION
  #' @examples
  #' # ADD_EXAMPLES_HERE
  get_relevant_parents <- function(val, lev, data) {
    lev_num <- as.numeric(lev)

    # recall `df` contains cols
    # - value
    # - level
    # - level_num

    data %>%
      # only levels higher than that of the checked value
      filter(level_num > lev_num) %>%
      # distinct
      distinct(value, level, level_num) %>%
      # rename cols
      rename(parent = value,
             parent_level = level,
             parent_level_num = level_num) %>%
      mutate(value = val,
             level = lev,
             level_num = lev_num)
  }

  # create numeric level for unambiguous comparison
  df <- df %>% mutate(level_num = as.numeric(level))

  # print(df)

  # select distinct values, exclude kraj (top level with no need to check)

  unique_values_nokraj <- df %>%
    distinct(value, level, level_num) %>%
    filter(level != "kraj")

  relevant_parents <- map2_dfr(unique_values_nokraj$value,
                               unique_values_nokraj$level,
                               get_relevant_parents, df)
  # print("Relevant parents: ")
  # print(relevant_parents)

  # print(checkable_superiors)

  relevant_parents %>%
    mutate(levels_ok = pmap_lgl(list(value,
                                     parent,
                                     level,
                                     parent_level),
                                is_parent, id_table)) %>%
    select(-ends_with("_num"))

}

# test --------------------------------------------------------------------

check_all_parents(oneproj_true, ids)
check_all_parents(oneproj_false, ids)
check_all_parents(oneproj_rand, ids)

# test multi-project ------------------------------------------------------

twoproj_rand_checked <- twoproj_rand %>%
  ungroup() %>%
  group_by(prj_id) %>%
  group_modify(~check_all_parents(.x, ids)) %>%
  group_by(prj_id, value, level) %>%
  nest(geocheck = c(parent, parent_level, levels_ok))

twoproj_rand %>%
  left_join(twoproj_rand_checked)

# test many-project ------------------------------------------------------

pb <- make_pb()

message("1k, group_modify")

beep()
tic()
thousandproj_rand_checked <- thousandproj_rand %>%
  ungroup() %>%
  group_by(prj_id) %>%
  group_modify(~check_all_parents(.x, ids)) %>%
  group_by(prj_id, value, level) %>%
  nest(geocheck = c(parent, parent_level, levels_ok))
toc()
beep()


# Test many project with nest-map logic -----------------------------------

pb <- make_pb()

twoproj_rand_checked <- twoproj_rand %>%
  ungroup() %>%
  group_by(prj_id) %>%
  nest(geodata = c(level, value, level_num)) %>%
  mutate(geocheck = map(geodata, check_all_parents, ids)) %>%
  unnest(c(geocheck))

message("1k, nest-map")

pb <- make_pb()

tic()
thousandproj_rand_checked <- thousandproj_rand %>%
  ungroup() %>%
  group_by(prj_id) %>%
  nest(geodata = c(level, value, level_num)) %>%
  mutate(geocheck = map(geodata, check_all_parents, ids)) %>%
  unnest(c(geocheck))
toc()
beep()

pb <- make_pb(n_distinct(dfs$prj_id))

tic()
dfs_checked <- dfs %>%
  ungroup() %>%
  group_by(prj_id) %>%
  nest(geodata = c(level, value, level_num)) %>%
  mutate(geocheck = map(geodata, check_all_parents, ids)) %>%
  unnest(c(geocheck))
toc()
beep()

# Test many project with nest-map logic and multicore ---------------------

# message("1k, nest-map, multicore")
#
# plan(multiprocess)
#
# pb <- make_pb()
#
# tic()
# thousandproj_rand_checked <- thousandproj_rand %>%
#   ungroup() %>%
#   group_by(prj_id) %>%
#   nest(geodata = c(level, value, level_num)) %>%
#   mutate(geocheck = future_map(geodata, check_all_parents, ids)) %>%
#   unnest(c(geocheck))
# toc()
# beep()


# Compile data and check result -------------------------------------------

thousandproj_with_check <- thousandproj_rand %>%
  left_join(thousandproj_rand_checked %>% select(-geodata))

thousandproj_with_check %>%
  group_by(prj_id) %>%
  summarise(all_ok = all(levels_ok, na.rm = T)) %>%
  count(all_ok)

dfs_with_check <- dfs %>%
  left_join(dfs_checked %>% select(-geodata))

dt <- read_parquet(here::here("data-processed",
                              "misto_fix-02-gnames.parquet"))

progs <- distinct(dt, prj_id, op_id)

dfs_with_check %>%
  left_join(progs) %>%
  group_by(op_id, prj_id) %>%
  summarise(all_ok = all(levels_ok, na.rm = T)) %>%
  summarise(mean = 1-mean(all_ok), count = n())

