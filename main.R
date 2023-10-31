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
output_folder <- ctx$op.value('output_folder', as.character, "Exported data")
na_encoding <- ctx$op.value('na_encoding', as.numeric, "")
filename <- ctx$op.value('filename', as.character, "")
timestamp <- ctx$op.value('timestamp', as.logical, TRUE)
decimal_character <- ctx$op.value('decimal_character', as.character, ".")
data_separator <- ctx$op.value('data_separator', as.character, ",")

df_wide <- dcast(df_long, .ri ~ .ci, value.var = ".y")
data <- df_wide[order(.ri)][, !".ri"]

rnames <- ctx$rselect() %>% tidyr::unite(col = "name")
cnames <- ctx$cselect() %>% tidyr::unite(col = "name")

row.names(data) <- rnames$name
colnames(data) <- cnames$name

# create temp file

fwrite(
  data,
  file = "test.csv",
  append = FALSE,
  quote = "auto",
  sep = ",",
  sep2 = c("","|",""),
  na = "",
  dec = ".",
  row.names = TRUE,
  col.names = TRUE,
  yaml = FALSE,
  bom = FALSE,
  verbose = FALSE
)


upload_df(df_tmp, ctx, project, filename, folder)

ctx$save(list())
