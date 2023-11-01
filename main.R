suppressPackageStartupMessages({
  library(tercen)
  library(data.table)
  library(dplyr, warn.conflicts = FALSE)
  library(tidyr)
})

ctx = tercenCtx()

df_long <- ctx$select(c(".ci", ".ri", ".y")) %>%
  as.data.table()

# Checks
if(df_long[, .N, by = .(.ci, .ri)][N > 1][, .N, ] > 0) {
  stop("Multiple values found in at least a cell.")
}

# Settings
na_encoding <- ctx$op.value('na_encoding', as.numeric, "")
filename <- ctx$op.value('filename', as.character, "Exported_Table")
time_stamp <- ctx$op.value('time_stamp', as.logical, FALSE)
decimal_character <- ctx$op.value('decimal_character', as.character, ".")
data_separator <- ctx$op.value('data_separator', as.character, ",")

if(time_stamp) {
  ts <- format(Sys.time(), "-%D-%H:%M:%S")
} else {
  ts <- ""
}
filename <- paste0(filename, ts, ".csv")

df_wide <- dcast(df_long, .ri ~ .ci, value.var = ".y")
data <- df_wide[order(.ri)][, !".ri"]

rnames <- ctx$rselect() %>% tidyr::unite(col = "name")
cnames <- ctx$cselect() %>% tidyr::unite(col = "name")

row.names(data) <- rnames$name
colnames(data) <- cnames$name

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
  row.names = TRUE,
  col.names = TRUE,
  verbose = FALSE
)

file_to_tercen <- function(file_path, chunk_size_bits = 1e6, filename = NULL) {
  
  if (is.null(filename)) {
    filename <- basename(file_path)
  }

  mimetype <- switch(
    tools::file_ext(file_path),
    png = "image/png",
    svg = "image/svg+xml", 
    csv = "unknown",
    pdf = "application/pdf",
    "unknown"
  )
  
  raw_vector <- readBin(
    file_path,
    "raw",
    file.info(file_path)[1, "size"]
  )
  
  splitted <- split(
    x = raw_vector, 
    f = ceiling((seq_along(raw_vector) * 8) / chunk_size_bits)
  )
  
  output_txt <- unlist(
    x = lapply(X = splitted, FUN = base64enc::base64encode, "txt")
  )
  
  df <- tibble::tibble(
    filename = filename,
    mimetype = mimetype,
    .content = output_txt
  )
  
  return(df)

}

file_to_tercen(file_path = tmp_file, filename = filename) %>%
  ctx$addNamespace() %>%
  as_relation() %>%
  as_join_operator(list(), list()) %>%
  save_relation(ctx)
