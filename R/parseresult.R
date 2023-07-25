#' @rdname summary_result_dataset
#' @title Function to parse workflow results for different datasets of each workflow steps and save to csv files
#' @importFrom AnVIL Rcollectl dplyr
#' @param inputTribble submission IDs of terra job and corresponding name 
#' @examples
#' inputTribble <- tribble(
#'   ~name, ~submissionId,
#'   "dgCMatrix_medium", "53b146eb-eccc-470f-bd3a-aee0e2be2bce",
#'   "DA_medium", "4c66be7f-2d8a-4e80-917f-8872faf66377",
#'   "dgCMatrix_large", "874043e7-a3f5-4a0b-b120-ded15a41a2ef",
#'   "DA_large", "316c604d-4ce6-49fc-8b16-d6414136ea9d"
#' )
#' summary_result_dataset(inputTribble)
#' @export
summary_result_dataset <- function(inputTribble) {
  for (i in 1:dim(inputTribble)[1]) {
    assign(paste0("data_", i), parse_Rcollectl_result_name(as.character(inputTribble[i,2])))
  }
  
  CPU_mean <- tibble(.rows = 7)
  for (i in 1:dim(inputTribble)[1]) {
    CPU_mean <- cbind(CPU_mean, get(paste0("data_", i))[, "CPU_mean"])
  }
  colnames(CPU_mean) <- as.vector(inputTribble[,1])$name
  rownames(CPU_mean) <- as.vector(get(paste0("data_", i))[, "Step"])$Step
  write.csv(CPU_mean, "CPU_mean.csv")

  MEM_mean <- tibble(.rows = 7)
  for (i in 1:dim(inputTribble)[1]) {
    MEM_mean <- cbind(MEM_mean, get(paste0("data_", i))[, "MEM_mean"])
  }
  colnames(MEM_mean) <- as.vector(inputTribble[,1])$name
  rownames(MEM_mean) <- as.vector(get(paste0("data_", i))[, "Step"])$Step
  write.csv(MEM_mean, "MEM_mean.csv")

  Time <- tibble(.rows = 7)
  for (i in 1:dim(inputTribble)[1]) {
    Time <- cbind(Time, get(paste0("data_", i))[, "Time"])
  }
  colnames(Time) <- as.vector(inputTribble[,1])$name
  rownames(Time) <- as.vector(get(paste0("data_", i))[, "Step"])$Step
  write.csv(Time, "Time.csv")
}


# parse workflow result from Rcollectl
parse_Rcollectl_result_name <- function(submissionId) {
  stat <- tibble(Step = character(),
                 CPU_max = numeric(),
                 CPU_mean = numeric(),
                 MEM_max = numeric(),
                 MEM_mean = numeric(),
                 Time = character())
  
  setwd("~")
  avworkflow_localize(submissionId, type = "output", dry=FALSE)
  fls = dir(submissionId, recursive = TRUE)
  tab_fls_Rcollectl <- paste0(submissionId, "/", fls[endsWith(fls, ".tab.gz")])
  tab_fls_timestamp <- paste0(submissionId, "/", fls[endsWith(fls, ".timestamp.txt")])
  a <- cl_parse(tab_fls_Rcollectl)
  t <- read.delim(tab_fls_timestamp, header = FALSE, col.names = c("Step", "sampdate"), sep = "\t")
  
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
    
    time <- sprintf("%d:%02d:%02d", floor(time / (60 * 60)), floor((time %% 3600) / 60), time %% 60)
    
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


#' @rdname summary_result
#' @title Function to parse workflow results for different autoBlockSize as well as cpu and save to csv files
#' @importFrom AnVIL Rcollectl dplyr
#' @param submissionId submission ID of terra job
#' @examples
#' submissionId <- "1bc21284-be3e-4520-9783-5a0824754b40"
#' summary_result(submissionId)
#' @export
summary_result <- function(submissionId) {
  setwd("~")
  avworkflow_localize(submissionId, type = "output", dry=FALSE)
  dir(submissionId)
  fls = dir(submissionId, recursive = TRUE)
  tab_fls <- fls[endsWith(fls, ".tab.gz")]
  tab_fls_AutoBlockSize <- tab_fls[startsWith(basename(tab_fls), "AutoBlockSize")]
  tab_fls_cpu <- tab_fls[startsWith(basename(tab_fls), "cpu")]
  
  stat <- tibble(autoBlockSize = character(),
                 elapsed = character(),
                 CPU_max = numeric(),
                 CPU_mean = numeric(),
                 MEM_max = numeric(),
                 MEM_mean = numeric())
  
  for (i in tab_fls_AutoBlockSize) {
    tmp <- strsplit(basename(i), "-|s")[[1]][1]
    autoBlockSize <- strsplit(tmp, "_|s")[[1]][2]
    tmp <- parse_Rcollectl_result(submissionId, i)
    tmp <- cbind(autoBlockSize,tmp)
    stat <- rbind(stat,tmp)
  }
  stat <- arrange(stat, as.numeric(autoBlockSize))
  write.csv(stat, paste0(submissionId, "_autoBlockSize.csv"))
  
  stat <- tibble(ncpus = character(),
                 elapsed = character(),
                 CPU_max = numeric(),
                 CPU_mean = numeric(),
                 MEM_max = numeric(),
                 MEM_mean = numeric())
  
  for (i in tab_fls_cpu) {
    tmp <- strsplit(basename(i), "-|s")[[1]][1]
    ncpus <- strsplit(tmp, "_|s")[[1]][2]
    tmp <- parse_Rcollectl_result(submissionId, i)
    tmp <- cbind(ncpus,tmp)
    stat <- rbind(stat,tmp)
  }
  stat <- arrange(stat, as.numeric(ncpus))
  write.csv(stat, paste0(submissionId, "_cpu.csv"))
}


# parse workflow result from Rcollectl
parse_Rcollectl_result <- function(submissionId, file) {
  a <- cl_parse(paste0(submissionId, "/", file))
  time <- as.numeric(round(
    difftime(
      as.POSIXct(a$sampdate[dim(a)[1]]),
      as.POSIXct(a$sampdate[1]),
      units='secs')))
  return(tibble(elapsed = sprintf("%d:%02d:%02d", floor(time / (60 * 60)), floor((time %% 3600) / 60), time %% 60), 
                MEM_max = max(a$MEM_Used/1024/1024), 
                MEM_mean = mean(a$MEM_Used/1024/1024), 
                CPU_max = max(100 - a[, "CPU_Idle%"]), 
                CPU_mean = mean(100 - a[, "CPU_Idle%"])))
}
