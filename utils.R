library(tercenApi)
upload_df <- function(df, ctx, filename, output_folder) {
  
  # create output folder
  project   <- ctx$client$projectService$get(ctx$schema$projectId)
  folder    <- NULL
  if (output_folder != "") {
    folder  <- ctx$client$folderService$getOrCreate(project$id, output_folder)
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

