suppressPackageStartupMessages({
  library(tercen)
  library(data.table)
  library(dplyr, warn.conflicts = FALSE)
  library(tidyr)
})

source("./utils.R")

ctx = tercenCtx()

df_long <- ctx$select(c(".ci", ".ri", ".y")) %>%
  as.data.table()

# Checks
if(df_long[, .N, by = .(.ci, .ri)][N > 1][, .N, ] > 0) {
  stop("Multiple values found in at least a cell.")
}

# Settings
format <- ctx$op.value('format', as.character, "CSV")
filename <- ctx$op.value('filename', as.character, "Exported_Table_WORKFLOW_GROUP_DATASTEP")
export_to_project <- ctx$op.value('export_to_project', as.logical, FALSE)
na_encoding <- ctx$op.value('na_encoding', as.numeric, "")
time_stamp <- ctx$op.value('time_stamp', as.logical, FALSE)
decimal_character <- ctx$op.value('decimal_character', as.character, ".")
data_separator <- ctx$op.value('data_separator', as.character, ",")

if(time_stamp) {
  ts <- format(Sys.time(), "-%D-%H:%M:%S")
} else {
  ts <- ""
}

if(grepl("WORFKLOW|GROUP|DATASTEP", filename)) {
  nms <- get_names(ctx)
  if(!is.null(nms$GRP)) {
    filename <- gsub("GROUP", nms$GRP, filename)
  } else {
    filename <- gsub("GROUP", "", filename)
  }
  filename <- gsub("WORKFLOW", nms$WF, filename)
  filename <- gsub("DATASTEP", nms$DS, filename)
}

filename <- paste0(filename, ts, ".csv")

df_wide <- dcast(df_long, .ri ~ .ci, value.var = ".y")
data <- df_wide[order(.ri)][, !".ri"]

rnames <- ctx$rselect() %>% as.data.table()
cnames <- ctx$cselect() %>% tidyr::unite(col = "name")

colnames(data) <- cnames$name
data <- cbind(rnames, data)

# create temp file
tmp_file = tempfile(fileext = ".csv")
on.exit(unlink(tmp_file))

fwrite(
  data,
  file = tmp_file,
  append = FALSE,
  quote = "auto",
  sep = data_separator,
  na = na_encoding,
  dec = decimal_character,
  row.names = FALSE,
  col.names = TRUE,
  verbose = FALSE
)

if(export_to_project) {
  upload_df(as_tibble(data), ctx, filename = filename, output_folder = "Exported Data")
}

file_to_tercen(file_path = tmp_file, filename = filename) %>%
  ctx$addNamespace() %>%
  as_relation() %>%
  as_join_operator(list(), list()) %>%
  save_relation(ctx)
