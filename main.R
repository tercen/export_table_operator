suppressPackageStartupMessages({
  library(tercen)
  library(data.table)
  library(dplyr, warn.conflicts = FALSE)
  library(tidyr)
  library(forcats)
  library(writexl)
})

source("./utils.R")

ctx <- tercenCtx()

df_long <- ctx$select(c(".ci", ".ri", ".y")) %>%
  as.data.table()

if(df_long[, .N, by = .(.ci, .ri)][N > 1][, .N, ] > 0) {
  stop("Multiple values found in at least a cell.")
}

# Settings
format <- ctx$op.value('format', as.character, "CSV")
collapse_cols <- ctx$op.value('collapse_cols', as.logical, FALSE)
collapse_rows <- ctx$op.value('collapse_rows', as.logical, FALSE)
prefix <- ctx$op.value('filename_prefix', as.character, "Exported_Table")
export_to_project <- ctx$op.value('export_to_project', as.logical, FALSE)
na_encoding <- ctx$op.value('na_encoding', as.character, "")
decimal_character <- ctx$op.value('decimal_character', as.character, ".")
data_separator <- ctx$op.value('data_separator', as.character, ",")
export_subfolder_name <- ctx$op.value('export_subfolder_name', as.character, "")
export_subfolder_id <- ctx$op.value('export_subfolder_id', as.character, "")
if(export_subfolder_id == "") export_subfolder_id <- NULL

format <- toupper(format)
if (!(format %in% c("CSV", "TSV", "XLSX"))) stop("Unsupported format: ", format)
ext <- switch(format, CSV = ".csv", TSV = ".tsv", XLSX = ".xlsx")
# TSV always uses tab; CSV honours data_separator. XLSX is text-format-agnostic.
text_sep <- if (format == "TSV") "\t" else data_separator

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

row_values <- as.data.table(ctx$rselect())
col_values <- as.data.table(ctx$cselect())

# dcast only emits a row/column per .ri/.ci value that actually occurs, so a crosstab
# row or column holding no observations at all would silently drop out and leave the
# data block smaller than the headers built from rselect()/cselect() - which then fails
# in colnames<- ("Can't assign N names to an M-column data.table"). Pin the levels to
# the full index range and keep the empty ones so the block always lines up.
n_ri <- max(nrow(row_values), max(df_long$.ri) + 1L)
n_ci <- max(nrow(col_values), max(df_long$.ci) + 1L)
df_long[, .ri := factor(.ri, levels = seq_len(n_ri) - 1L)]
df_long[, .ci := factor(.ci, levels = seq_len(n_ci) - 1L)]

df_wide <- dcast(df_long, .ri ~ .ci, value.var = ".y", drop = FALSE)
raw_data <- df_wide[order(.ri)][, !".ri"]
yaxis_names <- unlist(ctx$yAxis)
if (length(yaxis_names) == 0) yaxis_names <- ""
row_names_in <- names(ctx$rnames)
if (length(row_names_in) == 0) row_names_in <- ""

no_col_factors <- (length(ctx$cnames) == 1) && (ctx$cnames[[1]] == "")
no_row_factors <- (ncol(row_values) == 0) ||
  ((ncol(row_values) == 1) && all(as.character(row_values[[1]]) == ""))

build_collapsed_col_names <- function(col_values, yaxis_names, no_col_factors) {
  if (no_col_factors) return(yaxis_names[1])
  if (ncol(col_values) == 1) return(as.character(col_values[[1]]))
  apply(col_values, 1, paste, collapse = "_")
}

build_row_block <- function(row_values, row_names_in, collapse_rows, no_row_factors) {
  if (no_row_factors) {
    return(list(block = NULL, names = character(0)))
  }
  if (collapse_rows && ncol(row_values) >= 1) {
    joined <- apply(row_values, 1, paste, collapse = "_")
    joined_name <- paste(row_names_in, collapse = "_")
    list(
      block = setNames(data.table(joined), joined_name),
      names = joined_name
    )
  } else {
    list(block = row_values, names = row_names_in)
  }
}

row_block <- build_row_block(row_values, row_names_in, collapse_rows, no_row_factors)

# header_block is non-NULL only for crosstab-view output (collapse_cols == FALSE).
# Each element is a row of cells written verbatim above the data block.
header_block <- NULL

if (collapse_cols || no_col_factors) {
  new_col_names <- build_collapsed_col_names(col_values, yaxis_names, no_col_factors)
  if (is.null(row_block$block)) {
    df_out <- raw_data
    colnames(df_out) <- new_col_names
  } else {
    df_out <- cbind(row_block$block, raw_data)
    colnames(df_out) <- c(row_block$names, new_col_names)
  }
} else {
  if (is.null(row_block$block)) {
    df_out <- raw_data
  } else {
    df_out <- cbind(row_block$block, raw_data)
  }
  empty_cols <- rep("", max(length(row_block$names) - 1, 0))
  header_rows <- vector("list", ncol(col_values) + 1)
  for (i in seq_len(ncol(col_values))) {
    cf_name <- colnames(col_values)[i]
    cf_vals <- as.character(col_values[[i]])
    header_rows[[i]] <- c(empty_cols, cf_name, cf_vals)
  }
  yaxis_line <- rep(yaxis_names[1], nrow(col_values))
  header_rows[[ncol(col_values) + 1]] <- c(empty_cols, "Y-axis", yaxis_line)
  header_block <- header_rows
}

tmp_file <- tempfile(fileext = ext)
on.exit(unlink(tmp_file))

write_text_output <- function(con, df_out, header_block, sep, na, dec) {
  if (!is.null(header_block)) {
    header_text <- paste(
      vapply(header_block, function(row) paste(row, collapse = sep), character(1)),
      collapse = "\n"
    )
    cat(header_text, "\n", file = con, sep = "")
    fwrite(
      df_out, file = con, append = TRUE,
      quote = "auto", sep = sep, na = na, dec = dec,
      row.names = FALSE, col.names = FALSE
    )
  } else {
    fwrite(
      df_out, file = con,
      quote = "auto", sep = sep, na = na, dec = dec,
      row.names = FALSE, col.names = TRUE
    )
  }
}

if (format %in% c("CSV", "TSV")) {
  write_text_output(tmp_file, df_out, header_block, text_sep, na_encoding, decimal_character)
} else {
  if (is.null(header_block)) {
    write_xlsx(as.data.frame(df_out), tmp_file)
  } else {
    # Crosstab header rows can't be expressed as a tidy data.frame's colnames, so we
    # write a TSV first and re-read it without a header row — every cell ends up in
    # the workbook as-is. Same approach as the Shiny operator.
    tmp_tsv <- tempfile(fileext = ".tsv")
    on.exit(unlink(tmp_tsv), add = TRUE)
    write_text_output(tmp_tsv, df_out, header_block, "\t", na_encoding, decimal_character)
    xlsx_df <- read.table(tmp_tsv, sep = "\t", header = FALSE,
                          check.names = FALSE, stringsAsFactors = FALSE,
                          quote = "\"", comment.char = "", fill = TRUE,
                          colClasses = "character")
    write_xlsx(xlsx_df, tmp_file)
  }
}

if(export_to_project) {
  subfolders_list <- ctx$client$projectDocumentService$getParentFolders(wfId)
  if(length(subfolders_list) == 0) {
    subfolders <- character(0)
  } else {
    subfolders <- unlist(lapply(subfolders_list, "[[", "name"))
  }
  # When no folder picker id and no subfolder name, default to "Exported Data"
  # so getOrCreate has a non-empty terminal segment (matches pre-1.0.0 behavior).
  effective_subfolder_name <- if (nzchar(export_subfolder_name)) {
    export_subfolder_name
  } else if (is.null(export_subfolder_id)) {
    "Exported Data"
  } else {
    ""
  }
  path_parts <- c(subfolders, effective_subfolder_name)
  path_parts <- path_parts[nzchar(path_parts)]
  root_path <- if (length(path_parts) == 0) "" else do.call(file.path, as.list(path_parts))

  upload_file_to_folder(
    file_path = tmp_file,
    ctx = ctx,
    filename = paste0(filename, ext),
    output_folder = root_path,
    output_folder_id = export_subfolder_id
  )
}

file_to_tercen(file_path = tmp_file, filename = paste0(filename, ext)) %>%
  ctx$addNamespace() %>%
  as_relation(relation_name = paste(format, "Export")) %>%
  as_join_operator(list(), list()) %>%
  save_relation(ctx)
