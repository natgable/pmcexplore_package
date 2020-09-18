#' Search a database by keyword
#' 
#' Search a database by a keyword or keywords to get a list of documents 
#' that correspond to the input keyword. Uses the ESearch functionality of
#' E-utilities to search a specified database. This function currently only 
#' returns documents that are NIH-funded and open access.
#'
#' @param keyword A string of one of multiple keywords, separated by a space.
#' @param n An integer number of document IDs to return, defaults to 100.
#' @param db A string specifying the dataset to pull the XML from, defaults to "pmc" (PubMed Central).
#' Other database options include "pubmed" (PubMed), "mesh" (MeSH), and "nlmcatalog" (NLM Catalog).
#' The full list of database options are available on the NCBI E-utilities guide.
#' 
#' @return A tibble with one column (labeled PMCID) that contains all the PMCID
#' numbers returned from the database search
#' 
#' @examples 
#' search_keyword("protein")
#' search_keyword("data", n = 20)
#' 
#' @importFrom magrittr "%>%"
#' 
#' @export

search_keyword <- function(keyword, n = 100, db = "pmc") {
  term <- "open+access[filter]+AND+nih+funded[filter]"
  base_url <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?"
  
  if (is.null(keyword)) stop("Need to provide keyword.")
  
  # get keyword into url format for search
  keyword_url <- 
    keyword %>% 
    stringr::str_replace_all("\\s{2,}", " ") %>% # catch extra whitespace
    stringr::str_trim() %>% 
    stringr::str_replace_all(" ", "+")
    
  # now do search via E-utilities to get just PMCIDs
  url <- paste0(base_url, "db=", db, "&term=", keyword_url, "+AND+", term, "&retmax=", n)
  
  print(url)
  
  xml <- xml2::read_xml(url)

  # now take out just PMCIDs and put into a tibble
  if (class(xml)[1] != "xml_document") {
    stop("Error. Did not return a valid search.")
  }
  
  id_list <-
    xml %>% 
    xml2::as_list()
  
  ids <-
    id_list[[1]]["IdList"] %>% 
    c()
  
  # get into tibble with proper PMCID naming convention
  ids[[1]] %>% 
    as.character() %>% 
    purrr::map_chr(as.character()) %>% 
    tibble::tibble() %>% 
    dplyr::mutate(
      PMCID = stringr::str_extract(., "[:digit:]{7}"),
      PMCID = stringr::str_c("PMC", PMCID)
    ) %>% 
    dplyr::select(PMCID)
}
