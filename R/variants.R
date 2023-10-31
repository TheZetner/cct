#' Get CoVariants.org Tracked Variants
#'
#' Grab tracked variants table and a cleaned up tibble version of it.
#'
#' @param variants_url Point to a different version if necessary
#' @return table of lineage, clade, who (designation)
#' @export
#'
getVariants <-
  function(variants_url = "https://raw.githubusercontent.com/hodcroftlab/covariants/master/web/data/nameTable.json") {
    ## Get Data and test for errors
    v.raw <-
      tryCatch(
        jsonlite::fromJSON(txt = variants_url, flatten = T)$nameTable,
        error = function(e) {
          e
        }
      )

    if (inherits(v.raw, "error")) {
      message(paste0(
        "Error acquiring remote file: \n",
        "variants - ",
        dplyr::if_else(inherits(v.raw, "error"), "FAILED", "SUCCEEDED")
      ))
      message("This often occurs when the target file has bad formatting")
      message("Perhaps point to the previous commit if this error is new")
      return(NULL)
    }

    v.raw %>%
      dplyr::as_tibble() %>%
      tidyr::unnest(lineages) %>%
      dplyr::select(lineage = name, clade, who)
  }

#' Parse Website Constellations from
#'
#' https://github.com/cov-lineages/constellations/
#'
#' @param x location of cloned constellations root dir
#'
#' @return a table of constellation mutations with WHO label, variants, and mutations
#' @export

constellationToTable <- function(x) {
  f <- jsonlite::fromJSON(txt = x, simplifyVector = F)
  tibble::tibble(
    label = f[["label"]],
    who = f[["variant"]][["WHO_label"]],
    variant = list(f[["variant"]][["Pango_lineages"]]),
    mutations = f[["sites"]]
  ) %>%
    tidyr::separate("mutations",
      into = c("Gene", "Mutation"),
      sep = ":",
      extra = "merge"
    ) %>%
    tidyr::unnest(variant)
}
