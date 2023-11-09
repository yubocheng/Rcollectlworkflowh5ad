#' @rdname update_data_table
#' @name update_data_table
#' @title Update Terra data table
NULL

#' @rdname update_data_table
#' @description `get_manifest_tibble()` retrieve cellxgene data
#' @importFrom cellxgenedp datasets files db 
#' @importFrom dplyr select mutate left_join
#' @return `get_manifest_tibble()` returns a tibble
#' @examples
#' library(cellxgenedp)
#' manifest_tibble <- get_manifest_tibble() |>
#'   filter(
#'   title == "Immunophenotyping of COVID-19 and influenza highlights the role 
#'     of type I interferons in development of severe COVID-19",
#'   filetype == "H5AD"
#' )
#' @export
get_manifest_tibble <- function() {
  manifest_tibble <- left_join(
    datasets(db()) |> 
      select("dataset_id", "collection_id", "title", "donor_id", "cell_count"),
    files(db()) |> 
      select("dataset_id", "url", "filetype"),
    by = "dataset_id"
  ) 
}


#' @rdname update_data_table
#' @description `update_file_avtable()` imports cellxgene data to Terra data 
#'   table file
#' @param manifest_tibble character() a tibble contains result of cellxgene 
#'   data 
#' @param id character(1) unique id of file table
#' @param sample character(1) column to analyze
#' @param dgCMatrix logical(1) If TRUE, read in assay as in-memory `dgCMatrix`
#' @param core integer() number of cpus
#' @param mem_gb numeric() memory (GB) to allocate for running the workflow, 
#'   default: 60
#' @importFrom AnVIL avworkspace avtable avtable_import
#' @examples
#' library(AnVIL)
#' avworkspace("bioconductor-rpci-yubo/Rcollectlworkflowh5ad")
#' manifest_tibble <- get_manifest_tibble() |>
#'   filter(
#'   title == "Immunophenotyping of COVID-19 and influenza highlights the role 
#'     of type I interferons in development of severe COVID-19",
#'   filetype == "H5AD"
#' )
#' sample = "Disease.group"
#' id = sprintf("%04d", dim(avtable("file"))[1] + 1)
#' update_file_avtable(manifest_tibble, id, sample, TRUE, 3, 60)
#' @export
update_file_avtable <- 
  function(manifest_tibble, id, sample, dgCMatrix, core, mem_gb) {
  manifest_tibble |> 
    mutate(
      file = id,
      file_url = url,
      knitr_eval = TRUE,
      sample = sample,
      dgCMatrix = dgCMatrix,
      core = core,
      mem_gb = mem_gb
    ) |>
    select(
      file, 
      "file_url",
      "knitr_eval",
      sample,
      dgCMatrix,
      core,
      mem_gb,
      dataset_title = "title",
      "cell_count"
    ) |>
    avtable_import("file")
}
