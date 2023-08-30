#' @rdname get_manifest_tibble
#' @title Functions to retrieve cellxgene data and import to terra
#' @importFrom cellxgenedp dplyr
#' @return `get_manifest_tibble()` returns a tibble
#' @examples
#' manifest_tibble <- get_manifest_tibble() |>
#'   filter(
#'   title == "Immunophenotyping of COVID-19 and influenza highlights the role of type I interferons in development of severe COVID-19",
#'   filetype == "H5AD"
#' )
#' @export
get_manifest_tibble <- function() {
  manifest_tibble <- left_join(
    datasets(db()) |> select(dataset_id, collection_id, title, donor_id, cell_count),
    files(db()) |> select(dataset_id, url, filetype),
    by = "dataset_id"
  ) 
}


#' @rdname update_file_avtable
#' @importFrom cellxgenedp AnVIL dplyr
#' @param manifest_tibble result of cellxgene data
#' @param id unique id of file table
#' @param sample column to analyze
#' @param core number of cpus
#' @param mem_gb memory to allocate for running the workflow, default: 60 GB
#' @examples
#' sample = "Disease.group"
#' id = sprintf("%04d", dim(avtable("file"))[1] + 1)
#' update_file_avtable(manifest_tibble, id, sample, dgCMatrix, core, mem_gb)
#' @export
update_file_avtable <- function(manifest_tibble, id, sample, dgCMatrix, core, mem_gb) {
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
      file_url,
      knitr_eval,
      sample,
      dgCMatrix,
      core,
      mem_gb,
      dataset_title = title,
      cell_count
    ) |>
    avtable_import("file")
}
