#' Get grant information of a paper by PMCID
#' 
#' This function uses the ID converstion API provided by the NCBI
#' to get the PubMed document associated with a PubMed Central document.
#' It then searches the full PubMed XML document for the tagged Grants 
#' section and returns the grant information.
#'
#' @param id A string of the PMCID number of the document
#' 
#' @return A tibble with one column containing the grant agency.
#' 
#' @examples 
#' get_funding("PMC4161373")
#' 
#' @importFrom magrittr "%>%"
#' 
#' @export

get_funding <- function(id) {

  if (!stringr::str_detect(id, "^(PMC\\d{7})$|^(\\d{7})$")) {
    stop("Please provide a valid PMCID")
  }
    
  service_root <- "https://www.ncbi.nlm.nih.gov/pmc/utils/idconv/v1.0/"
  url <- paste0(service_root, "?ids=", id, "&idtype=pmcid")
  
  # get PMID from PMCID using the ID converter
  pmid <- 
    xml2::read_xml(url) %>% 
    xml2::xml_find_all("//record[@pmid]") %>% 
    xml2::xml_attr(attr = "pmid")
  
  # get full text PubMed xml
  pubmed_xml<- 
    paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&id=", pmid, "&retmode=xml") %>% 
    xml2::read_xml()
  
  # get information under field "Grants" from PubMed full text XML
  grants <-
    pubmed_xml %>%
    xml2::xml_find_all("//GrantList") %>%
    xml2::xml_children() %>%
    xml2::xml_find_all("//Agency") %>%
    purrr::map_chr(xml2::xml_text) %>%
    tibble::tibble() %>%
    dplyr::rename(grant_agency = ".")
  
  grants
}