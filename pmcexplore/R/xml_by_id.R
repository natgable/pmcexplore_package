#' Download full text XML from PubMed Central by ID number
#' 
#' Fetches a full text XML specified by ID number from the E-utilities API. 
#'
#' @param id A string specifying the ID number corresponding to a document.
#' @param db A string specifying the dataset to pull the XML from, defaults to "pmc" (PubMed Central).
#' Other database options include "pubmed" (PubMed), "mesh" (MeSH), and "nlmcatalog" (NLM Catalog).
#' The full list of database options are available on the NCBI E-utilities guide.
#' 
#' @return an XML document nodeset (as defined by the `xml2` package).
#' 
#' @examples 
#' full_text_xml <- xml_by_id("PMC7344926")
#' 
#' @importFrom magrittr "%>%"
#' 
#' @export

xml_by_id <- function(id, db = "pmc") {
  base_url <- 
    "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?"
  if (!grepl("^PMC[0-9]+$", id)) {
    stop("id should be a valid PMC id like PMC2231364")
  }
  url <- paste0(base_url, "db=", db, "&id=", id)

  xml2::read_xml(url)
}