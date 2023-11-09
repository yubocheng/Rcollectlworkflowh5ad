#' @rdname summary_result
#' @name summary_result
#' @title Parse workflow results 
NULL

#' @rdname summary_result
#' @description `summary_result_dataset()` returns csv files of parsing 
#'   workflow results for different datasets of each workflow steps 
#' @importFrom AnVIL avworkspace avworkflow_localize
#' @importFrom Rcollectl cl_parse
#' @importFrom tibble tibble
#' @importFrom utils read.delim write.csv
#' @importFrom dplyr select arrange
#' @param inputTribble submission IDs (and workflow IDs) of terra job and 
#'   corresponding name
#' @param dir directory for generated csv files
#' @examples
#' library(AnVIL)
#' library(Rcollectl)
#' avworkspace("bioconductor-rpci-yubo/Rcollectlworkflowh5ad")
#' 
#' # inputTribble contains only submission IDs
#' inputTribble <- tribble(
#'   ~name, ~submissionId,
#'   "dgCMatrix_medium", "17efa80e-3ef1-429d-8359-58c7cd653310",
#'   "DA_medium", "2f0cc8ee-ae53-44e6-a6c0-31e7089f0d42",
#'   "dgCMatrix_large", "c3c147eb-2ff8-4cf0-ba55-5708fa9c0ae5",
#'   "DA_large", "8be18771-f06d-4256-ac13-6566a3053f14"
#' )
#' 
#' # inputTribble contains submission IDs and workflow IDs
#' inputTribble <- tribble(
#'   ~name, ~submissionId, ~workflowId,
#'   "dgCMatrix_medium", "123895c9-6ad1-4258-b3d4-526e057c5cba", 
#'     "923c360d-a30d-4013-b9f3-80058be1b34a",
#'   "DA_medium", "123895c9-6ad1-4258-b3d4-526e057c5cba", 
#'     "c7d5a05a-a275-411d-9908-3e344200cd4e",
#'   "dgCMatrix_large", "9385fd75-4cb7-470f-9e07-1979e2c8f193", 
#'     "31c69e74-12d4-40e7-b593-0f59006aca37",
#'   "DA_large", "9385fd75-4cb7-470f-9e07-1979e2c8f193", 
#'     "1410f324-021a-43f0-90bd-78fbb6a25c50"
#' )
#' 
#' summary_result_dataset(inputTribble)
#' @export
summary_result_dataset <- function(inputTribble, dir = tempdir()) {
  for (i in 1:dim(inputTribble)[1]) {
    if (dim(inputTribble)[2] == 2) {
      assign(paste0("data_", i), 
             .parse_Rcollectl_result_name(as.character(inputTribble[i,2]), 
                                          NULL)
             )
    } else if (dim(inputTribble)[2] == 3) {
      assign(paste0("data_", i), 
           .parse_Rcollectl_result_name(as.character(inputTribble[i,2]), 
                                       as.character(inputTribble[i,3])
                                       )
           )
    }
  }

  CPU_mean <- tibble(.rows = 7)
  for (i in 1:dim(inputTribble)[1]) {
    CPU_mean <- cbind(CPU_mean, 
                      get(paste0("data_", i))[, "CPU_mean"])
  }
  colnames(CPU_mean) <- as.vector(inputTribble[,1])$name
  rownames(CPU_mean) <- as.vector(get(paste0("data_", i))[, "Step"])$Step
  write.csv(CPU_mean, paste0(dir, "/CPU_mean.csv"))

  MEM_mean <- tibble(.rows = 7)
  for (i in 1:dim(inputTribble)[1]) {
    MEM_mean <- cbind(MEM_mean, 
                      get(paste0("data_", i))[, "MEM_mean"])
  }
  colnames(MEM_mean) <- as.vector(inputTribble[,1])$name
  rownames(MEM_mean) <- as.vector(get(paste0("data_", i))[, "Step"])$Step
  write.csv(MEM_mean, paste0(dir, "/MEM_mean.csv"))

  Time <- tibble(.rows = 7)
  for (i in 1:dim(inputTribble)[1]) {
    Time <- cbind(Time, 
                  get(paste0("data_", i))[, "Time"])
  }
  colnames(Time) <- as.vector(inputTribble[,1])$name
  rownames(Time) <- as.vector(get(paste0("data_", i))[, "Step"])$Step
  write.csv(Time, paste0(dir, "/Time.csv"))
}

# parse workflow result from Rcollectl
.parse_Rcollectl_result_name <- function(submissionId, workflowId) {
  stat <- tibble(Step = character(),
                 CPU_max = numeric(),
                 CPU_mean = numeric(),
                 MEM_max = numeric(),
                 MEM_mean = numeric(),
                 Time = character())

  fls <- .get_files(submissionId, workflowId) 
  tab_fls_Rcollectl <- fls[endsWith(fls, ".tab.gz")]
  tab_fls_timestamp <- fls[endsWith(fls, ".timestamp.txt")]
  a <- cl_parse(tab_fls_Rcollectl)
  t <- read.delim(tab_fls_timestamp, 
                  header = FALSE, 
                  col.names = c("Step", "sampdate"), 
                  sep = "\t")

  for (i in 1:dim(t)[1]) {
    if (i == 1) {
      cpu <- 100 - a[1:rownames(a[substr(a$sampdate, 1, 19) == substr(t$sampdate[i+1], 1, 19), ])-1, "CPU_Idle%"]
      mem <- a[1:rownames(a[substr(a$sampdate, 1, 19) == substr(t$sampdate[i+1], 1, 19), ])-1, "MEM_Used"]/1024/1024
      time <- as.numeric(round(
        difftime(
          as.POSIXct(t$sampdate[2], tz = "EST"),
          as.POSIXct(a[1, "sampdate"]),
          units='secs')
      ))
    } else if (i == dim(t)[1]) {
      cpu <- 100 - a[rownames(a[substr(a$sampdate, 1, 19) == substr(t$sampdate[i], 1, 19), ]):dim(a)[1], "CPU_Idle%"]
      mem <- a[rownames(a[substr(a$sampdate, 1, 19) == substr(t$sampdate[i], 1, 19), ]):dim(a)[1], "MEM_Used"]/1024/1024
      time <- as.numeric(round(
        difftime(
          as.POSIXct(a$sampdate[dim(a)[1]]),
          as.POSIXct(t$sampdate[i], tz = "EST"),
          units='secs')
      ))
    } else {
      cpu <- 100 - a[rownames(a[substr(a$sampdate, 1, 19) == substr(t$sampdate[i], 1, 19), ]):rownames(a[substr(a$sampdate, 1, 19) == substr(t$sampdate[i+1], 1, 19), ])-1, "CPU_Idle%"]
      mem <- a[rownames(a[substr(a$sampdate, 1, 19) == substr(t$sampdate[i], 1, 19), ]):rownames(a[substr(a$sampdate, 1, 19) == substr(t$sampdate[i+1], 1, 19), ])-1, "MEM_Used"]/1024/1024
      time <- as.numeric(round(
        difftime(
          as.POSIXct(t$sampdate[i+1]),
          as.POSIXct(t$sampdate[i]),
          units='secs')
      ))
    }

    time <- sprintf("%d:%02d:%02d", 
                    floor(time / (60 * 60)), 
                    floor((time %% 3600) / 60), 
                    time %% 60)

    tmp <- tibble(Step = t$Step[i],
                  CPU_max = max(cpu),
                  CPU_mean = mean(cpu),
                  MEM_max = max(mem),
                  MEM_mean = mean(mem),
                  Time = time)

    stat = rbind(stat,tmp)
  }
  stat
}

# get all files for submissionId (and workflowId)
.get_files <- function(submissionId, workflowId) {
  path <- paste0(tempdir(), "/", submissionId)
  avworkflow_localize(submissionId, 
                      destination = path, 
                      type = "output", 
                      dry=FALSE)
  fls = paste0(path, "/", dir(path, recursive = TRUE))
  if (!is.null(workflowId)) {
    fls <- fls[grepl(workflowId, fls, fixed=TRUE)]
    if (length(fls) == 0) {
      stop("Workflow not available\n",
        "  submissionId: '", submissionId, "'")
    }
  }
  fls
}

#' @rdname summary_result
#' @description `summary_result()` returns csv files of parsing workflow 
#'   results for different autoBlockSize as well as cpu
#' @param submissionId character(1) submission ID of terra job
#' @param workflowId character(1) workflow ID within the submission ID of 
#'   terra job if multiple workflows in one submission
#' @examples
#' submissionId <- "8ca42a70-d2e9-421f-9f51-f42601348a82"
#' summary_result(submissionId)
#' @export
summary_result <- function(submissionId, workflowId = NULL, dir = tempdir()) {
  fls <- .get_files(submissionId, workflowId) 
  tab_fls <- fls[endsWith(fls, ".tab.gz")]
  tab_fls_AutoBlockSize <- 
    tab_fls[startsWith(basename(tab_fls), "AutoBlockSize")]
  tab_fls_cpu <- 
    tab_fls[startsWith(basename(tab_fls), "cpu")]

  stat <- tibble(autoBlockSize = character(),
                 elapsed = character(),
                 CPU_max = numeric(),
                 CPU_mean = numeric(),
                 MEM_max = numeric(),
                 MEM_mean = numeric())

  for (i in tab_fls_AutoBlockSize) {
    tmp <- strsplit(basename(i), "-|s")[[1]][1]
    autoBlockSize <- strsplit(tmp, "_|s")[[1]][2]
    tmp <- .parse_Rcollectl_result(i)
    tmp <- cbind(autoBlockSize,tmp)
    stat <- rbind(stat,tmp)
  }
  stat <- arrange(stat, as.numeric(autoBlockSize))
  write.csv(stat, paste0(dir, "/", submissionId, "_autoBlockSize.csv"))

  stat <- tibble(ncpus = character(),
                 elapsed = character(),
                 CPU_max = numeric(),
                 CPU_mean = numeric(),
                 MEM_max = numeric(),
                 MEM_mean = numeric())

  for (i in tab_fls_cpu) {
    tmp <- strsplit(basename(i), "-|s")[[1]][1]
    ncpus <- strsplit(tmp, "_|s")[[1]][2]
    tmp <- .parse_Rcollectl_result(i)
    tmp <- cbind(ncpus,tmp)
    stat <- rbind(stat,tmp)
  }
  stat <- arrange(stat, as.numeric(ncpus))
  write.csv(stat, paste0(dir, "/", submissionId, "_cpu.csv"))
}

# parse workflow result from Rcollectl
.parse_Rcollectl_result <- function(file) {
  a <- cl_parse(file)
  time <- as.numeric(round(
    difftime(
      as.POSIXct(a$sampdate[dim(a)[1]]),
      as.POSIXct(a$sampdate[1]),
      units='secs')))
  res <- tibble(elapsed = sprintf("%d:%02d:%02d", 
                                  floor(time / (60 * 60)), 
                                  floor((time %% 3600) / 60), 
                                  time %% 60), 
                MEM_max = max(a$MEM_Used/1024/1024), 
                MEM_mean = mean(a$MEM_Used/1024/1024), 
                CPU_max = max(100 - a[, "CPU_Idle%"]), 
                CPU_mean = mean(100 - a[, "CPU_Idle%"]))
  res
}