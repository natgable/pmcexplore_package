#' Get references by paper PMCID
#' 
#' This is just a wrapper function for `tidypmc::pmc_reference()` by Chris Stubben
#' to get all reference information from a PMCID.
#'
#' @param id A string of PMCID number of the document
#' 
#' @return A tibble with id, pmid, authors, year, title, journal, 
#' volume pages, and doi.
#' 
#' @examples 
#' get_references("PMC4161373")
#' 
#' @importFrom magrittr "%>%"
#' 
#' @export

get_references <- function(id) {
  xml <- xml_by_id(id, "pmc")
  
  xml %>% 
    tidypmc::pmc_reference()
}