library(arrow)
library(writexl)
library(purrr)
library(here)
library(stringr)
library(tidyr)
library(dplyr)

ds <- open_dataset(here::here("data-output", "dtl-all-arrow"))

ops_chunks <- ds %>% select(op, chnk) %>% collect() %>% distinct()

dir <- file.path("data-export", "excel_all_chunked")
dir.create(dir, showWarnings = F, recursive = T)

save_one_excel <- function(ds, op, chnk, dir) {
  # print(op)
  # print(chnk)
  df_op_chunk <- ds %>%
    filter(chunk == chnk, op_id == op) %>%
    collect()

  # print(df_op_chunk)

  chunk_padded <- str_pad(chnk, width = 2, pad = "0")
  filename <- file.path(dir, paste0(op, "_", chunk_padded, ".xlsx"))
  write_xlsx(df_op_chunk, filename)
}

save_one_excel(ds = ds, op = "IROP", chnk = 1, dir = dir)
walk2(ops_chunks$op_id, ops_chunks$chunk, ~save_one_excel(ds, .x, .y, dir))

ds %>%
  filter(op_id == "OP PPR", chunk == 1) %>%
  collect()

ds

