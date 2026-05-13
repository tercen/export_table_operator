library(tercenApi)
upload_file_to_folder <- function(file_path, ctx, filename, output_folder, output_folder_id) {
  project <- ctx$client$projectService$get(ctx$schema$projectId)
  folder  <- NULL
  if (!is.null(output_folder_id)) {
    folder <- ctx$client$folderService$get(id = output_folder_id)
  } else if (output_folder != "") {
    folder <- ctx$client$folderService$getOrCreate(project$id, output_folder)
  }

  bytes <- readBin(file_path, what = "raw", n = file.info(file_path)$size)

  fileDoc <- FileDocument$new()
  fileDoc$name <- filename
  fileDoc$projectId <- project$id
  fileDoc$acl$owner <- project$acl$owner
  if (!is.null(folder)) fileDoc$folderId <- folder$id

  ctx$client$fileService$upload(fileDoc, bytes)
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


