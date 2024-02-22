suppressPackageStartupMessages({
  library(tercen)
  library(data.table)
  library(dplyr, warn.conflicts = FALSE)
  library(tidyr)
})

source("./utils.R")

ctx = tercenCtx()

subfolders <- ctx$client$projectDocumentService$getParentFolders(ctx$workflowId)
root_path <- paste0("/", paste0(unlist(lapply(subfolders, "[[", "name")), collapse = "/"))

df_long <- ctx$select(c(".ci", ".ri", ".y")) %>%
  as.data.table()

# Checks
if(df_long[, .N, by = .(.ci, .ri)][N > 1][, .N, ] > 0) {
  stop("Multiple values found in at least a cell.")
}

# Settings
format <- ctx$op.value('format', as.character, "CSV")
prefix <- ctx$op.value('filename_prefix', as.character, "Exported_Table")
export_to_project <- ctx$op.value('export_to_project', as.logical, FALSE)
na_encoding <- ctx$op.value('na_encoding', as.numeric, "")
decimal_character <- ctx$op.value('decimal_character', as.character, ".")
data_separator <- ctx$op.value('data_separator', as.character, ",")

ts <- format(Sys.time(), "%Y-%m-%d-%H%M%S")

nms <- get_names(ctx)
if(!is.null(nms$GRP)) {
  filename <- paste(prefix, nms$WF, nms$GRP, nms$DS, ts, sep = "_")
} else {
  filename <- paste(prefix, nms$WF, nms$DS, ts, sep = "_")
}

filename <- paste0(filename, ".csv")

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
  upload_df(as_tibble(data), ctx, filename = filename, output_folder = paste0(root_path, "/Exported Data"))
}

file_to_tercen(file_path = tmp_file, filename = filename) %>%
  ctx$addNamespace() %>%
  as_relation(relation_name = "CSV Export") %>%
  as_join_operator(list(), list()) %>%
  save_relation(ctx)
