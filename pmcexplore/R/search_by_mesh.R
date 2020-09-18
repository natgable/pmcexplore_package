#' Search a database by a MeSH term
#' 
#' Search a database by a MeSH term to get a list of documents 
#' that correspond to the input keyword. Uses the ESearch functionality of
#' E-utilities to search PubMed Central by MeSH.
#'
#' @param keyword A string of a MeSH term.
#' @param n An integer number of document IDs to return, defaults to 20.
#' 
#' @return A tibble with one column (labeled PMCID) that contains all the PMCID
#' numbers returned from the database search.
#' 
#' @examples 
#' search_by_mesh("Oceania")
#' search_by_mesh("Multiple Birth Offspring")
#' 
#' @importFrom magrittr "%>%"
#' 
#' @export

search_by_mesh <- function(mesh_term, n = 20) {
  
  term_formatted <-
    mesh_term %>% 
    stringr::str_trim() %>% 
    stringr::str_replace_all(" ", "+")
  
  url <- 
    paste0(
      "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pmc&term=",
      term_formatted, 
      "&field=mh",
      "&retmax=",
      n
    )
  
  xml <- xml2::read_xml(url)
  
  # now take out just PMCIDs and put into a tibble
  if (class(xml)[1] != "xml_document") {
    stop("Error. Did not return a valid search.")
  }
  
  id_list <-
    xml %>% 
    as_list()
  
  id_list[[1]]["IdList"][[1]] %>% 
    c() %>% 
    as.character() %>% 
    tibble::tibble() %>% 
    dplyr::mutate(
      PMCID = stringr::str_extract(., "[:digit:]{7}")
    ) %>%
    dplyr::select(PMCID)
}