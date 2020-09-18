#' Get abstract of a paper by PMCID
#' 
#' This is just a wrapper function for `tidypmc::pmc_text` by Chris Stubben
#' to get just the abstract text of a document.
#'
#' @param id A string of PMCID number of the document
#' 
#' @return A string that is the full abstract text.
#' 
#' @examples 
#' get_abstract("PMC4161373")
#' 
#' @importFrom magrittr "%>%"
#' 
#' @export

get_abstract <- function(id) {
  xml <- xml_by_id(id, "pmc")
  full_text <- tidypmc::pmc_text(xml)

  full_text %>% 
    dplyr::filter(section == "Abstract") %>% 
    dplyr::pull(text) %>% 
    paste(sep = "", collapse = " ")
}