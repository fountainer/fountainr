#' Clean the BibTex exported from mendeley
#'
#' @param file_name characters, the name of the bib file
#'
#'
#' @export
#'
#'
clean_bib <- function(file_name) {
  bib <- readLines(file_name)
  bib <- bib[-grep("abstract|doi|isbn|memdeley-groups|keywords|url|file|issn|pmid", bib)]
  writeLines(bib, paste0("cleaned_", file_name))
}