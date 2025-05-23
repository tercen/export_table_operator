suppressPackageStartupMessages({
  library(tercen)
  library(data.table)
  library(dplyr, warn.conflicts = FALSE)
  library(tidyr)
  library(forcats)
})

source("./utils.R")

ctx <- tercenCtx()

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
na_encoding <- ctx$op.value('na_encoding', as.character, "")
decimal_character <- ctx$op.value('decimal_character', as.character, ".")
data_separator <- ctx$op.value('data_separator', as.character, ",")
export_subfolder_name <- ctx$op.value('export_subfolder_name', as.character, "")
export_subfolder_id <- ctx$op.value('export_subfolder_id', as.character, "")
if(export_subfolder_id == "") export_subfolder_id <- NULL
ts <- format(Sys.time(), "%Y-%m-%d-%H%M%S")

wfId <- get_workflow_id(ctx)
if(is.null(wfId)) { # unit test condition
  filename <- prefix
} else {
  nms <- get_names(ctx)
  
  if(!is.null(nms$GRP)) {
    filename <- paste(prefix, nms$WF, nms$GRP, nms$DS, ts, sep = "_")
  } else {
    filename <- paste(prefix, nms$WF, nms$DS, ts, sep = "_")
  }
}

df_wide <- dcast(df_long, .ri ~ .ci, value.var = ".y")
data <- df_wide[order(.ri)][, !".ri"]

rnames <- ctx$rselect() %>% as.data.table()
cnames <- ctx$cselect() %>% tidyr::unite(col = "name")

if((length(ctx$cnames) == 1) & (ctx$cnames[[1]] == "")) {
  colnames(data) <- ctx$yAxis[[1]]
} else {
  colnames(data) <- cnames$name
}

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
  subfolders_list <- ctx$client$projectDocumentService$getParentFolders(wfId)
  
  if(length(subfolders_list) == 0) {
    subfolders <- ""
  } else {
    subfolders <- unlist(lapply(subfolders_list, "[[", "name"))
  }
  
  root_path <- do.call(file.path, as.list(c(subfolders, export_subfolder_name)))
  data_out <- replace_na_custom(data, new_na = na_encoding)

  upload_df(
    as_tibble(data_out),
    ctx,
    filename = filename,
    output_folder = root_path,
    output_folder_id = export_subfolder_id
  )
}

file_to_tercen(file_path = tmp_file, filename = paste0(filename, ".csv")) %>%
  ctx$addNamespace() %>%
  as_relation(relation_name = "CSV Export") %>%
  as_join_operator(list(), list()) %>%
  save_relation(ctx)
