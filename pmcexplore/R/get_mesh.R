#' Get Medical Subject Heading (MeSH) terms of a document by PMCID
#' 
#' This function uses the ID converstion API provided by the NCBI
#' to get the PubMed document associated with a PubMed Central document.
#' It then searches the full PubMed XML document for tagged MeSH terms and returns
#' the MeSH terms associated with the document.
#'
#' @param id A string of the PMCID number of the document
#' 
#' @return A tibble with one column containing the associated MeSH terms.
#' 
#' @examples 
#' get_mesh("PMC4161373")
#' 
#' @importFrom magrittr "%>%"
#' 
#' @export

get_mesh <- function(id) {
  service_root <- "https://www.ncbi.nlm.nih.gov/pmc/utils/idconv/v1.0/"
  if (!stringr::str_detect(id, "^(PMC\\d{7})$|^(\\d{7})$")) {
    stop("Please provide a valid PMCID")
  }
  
  url <- paste0(service_root, "?ids=", id, "&idtype=pmcid")
  
  # get PMID from PMCID using the ID converter
  pmid <- 
    xml2::read_xml(url) %>% 
    xml2::xml_find_all("//record[@pmid]") %>% 
    xml2::xml_attr(attr = "pmid")
  
  # get full text PubMed xml
  efetch_url <- 
    paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&id=", pmid, "&retmode=xml")
  
  mesh_terms <-
    xml2::read_xml(efetch_url) %>% 
    xml2::xml_find_all("//MeshHeadingList//DescriptorName") %>% 
    xml2::xml_text() %>% 
    tibble::tibble() %>% 
    dplyr::rename(MeSH_terms = ".")
  
  mesh_terms
}