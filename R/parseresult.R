#' @rdname summary_result
#' @title Function to parse workflow result and save to csv files
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
  return(tibble(elapsed = sprintf("%d:%02d:%02d", floor(time / (60 * 60)), floor(time / 60) - floor(time / (60 * 60)) * 60, time %% 60), 
                MEM_max = max(a$MEM_Used/1024/1024), 
                MEM_mean = mean(a$MEM_Used/1024/1024), 
                CPU_max = max(100 - a[, "CPU_Idle%"]), 
                CPU_mean = mean(100 - a[, "CPU_Idle%"])))
}
