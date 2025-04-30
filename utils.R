library(tercenApi)
upload_df <- function(df, ctx, filename, output_folder, output_folder_id) {
  
  # create output folder
  project   <- ctx$client$projectService$get(ctx$schema$projectId)
  folder    <- NULL
  # if ID specified, get from ID
  if (!is.null(output_folder_id)) {
    folder  <- ctx$client$folderService$get(id = output_folder_id)
  } else {
    if (output_folder != "") {
      folder  <- ctx$client$folderService$getOrCreate(project$id, output_folder)
    }
  }
  
  tbl = tercen::dataframe.as.table(df)
  bytes = memCompress(teRcenHttp::to_tson(tbl$toTson()),
                      type = 'gzip')
  
  fileDoc = FileDocument$new()
  fileDoc$name = filename
  fileDoc$projectId = project$id
  fileDoc$acl$owner = project$acl$owner
  fileDoc$metadata$contentEncoding = 'gzip'
  
  if (!is.null(folder)) {
    fileDoc$folderId = folder$id
  }
  
  fileDoc = ctx$client$fileService$upload(fileDoc, bytes)
  
  task = CSVTask$new()
  task$state = InitState$new()
  task$fileDocumentId = fileDoc$id
  task$owner = project$acl$owner
  task$projectId = project$id
  
  task = ctx$client$taskService$create(task)
  ctx$client$taskService$runTask(task$id)
  task = ctx$client$taskService$waitDone(task$id)
  if (inherits(task$state, 'FailedState')){
    stop(task$state$reason)
  }
  
  if (!is.null(folder)) {
    schema = ctx$client$tableSchemaService$get(task$schemaId)
    schema$folderId = folder$id
    ctx$client$tableSchemaService$update(schema)
  }
  
  return(NULL)
}


get_workflow_id <- function(ctx) {
  if (is.null(ctx$task)) {
    return(ctx$workflowId)
  } else {
    workflowIdPair <-
      Find(function(pair)
        identical(pair$key, "workflow.id"),
        ctx$task$environment)
    workflowId <- workflowIdPair$value
    return(workflowId)
  }
}

get_step_id <- function(ctx) {
  if (is.null(ctx$task)) {
    return(ctx$stepId)
  } else {
    stepIdPair <-
      Find(function(pair)
        identical(pair$key, "step.id"),
        ctx$task$environment)
    stepId <- stepIdPair$value
    return(stepId)
  }
}

get_names <- function(ctx) {
  wf <- ctx$client$workflowService$get(get_workflow_id(ctx))
  ds <-
    Find(function(s)
      identical(s$id, get_step_id(ctx)), wf$steps)
  grp <-
    Find(function(s)
      identical(s$id, ds$groupId), wf$steps)
  return(list(WF = wf$name, DS = ds$name, GRP = grp$name))
}


replace_na_custom <- function(data, new_na) {
  # Convert to tibble for consistency
  data <- as_tibble(data)
  if(new_na == "") return(data)
  # Check if new_na is a string-encoded number
  is_numeric_string <- !is.na(as.numeric(new_na))
  
  if (is_numeric_string) {
    # If new_na is a string-encoded number (e.g., "0", "42"), preserve numeric columns
    data %>%
      mutate_if(is.factor, ~fct_explicit_na(., na_level = new_na)) %>%
      mutate_if(is.numeric, ~replace(., is.na(.), as.numeric(new_na))) %>%
      mutate_if(is.character, ~replace(., is.na(.), new_na))
  } else {
    # If new_na is not a number (e.g., "missing"), convert only columns with NA to character
    data %>%
      mutate(across(where(is.factor), ~{
        if (any(is.na(.))) {
          as.character(fct_explicit_na(., na_level = new_na))
        } else {
          as.character(.)
        }
      })) %>%
      mutate(across(where(is.numeric), ~{
        if (any(is.na(.))) {
          as.character(replace(., is.na(.), new_na))
        } else {
          .
        }
      })) %>%
      mutate_if(is.character, ~replace(., is.na(.), new_na))
  }
}
