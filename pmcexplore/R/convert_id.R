#' Convert between PMCID (PubMed Central) and PMID (PubMed)
#' 
#' This function utilizes the ID converter provided by the NCBI
#' to switch between a PMCID to PMID or PMID to PMCID. 
#'
#' @param id A string of ID number
#' @param from The database that the ID number is from
#' @param to The database we want to the convert the ID number to
#' 
#' @return A string of the ID number for the correct database
#' 
#' @examples 
#' convert_id("3256301", from = "pmc", to = "pubmed")
#' convert_id("3256301", from = "pmcid", to = "pmid")
#' convert_id("22253597", from = "pubmed", to = "pmc")
#' convert_id("22253597", from = "pmcid", to = "pmid")
#' 
#' @importFrom magrittr "%>%"
#' 
#' @export

convert_id <- function(id, from, to) {
  
  if (from == "pmc" | from == "pmcid") {
    from_url <- "pmcid"
  } else if (from == "pubmed" | from == "pmid") {
    from_url <- "pmid"
  } else {
    print("from must be either pmcid or pmid.")
  }
  
  if (to == "pmc" | to == "pmcid") {
    to_url <- "pmcid"
  } else if (to == "pubmed" | to == "pmid") {
    to_url <- "pmid"
  } else {
    print("to must be either pmcid or pmid.")
  }
  
  service_root <- "https://www.ncbi.nlm.nih.gov/pmc/utils/idconv/v1.0/"
  url <-
    paste0(service_root, "?ids=", id, "&idtype=", from_url)
  
  xml2::read_xml(url) %>% 
    xml2::xml_find_all("//record") %>% 
    xml2::xml_attr(attr = to_url)
}