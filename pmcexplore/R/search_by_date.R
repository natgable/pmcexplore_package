#' Search a database by date
#' 
#' Search PubMed Central by a start and end date to get a list of documents 
#' that were published in PMC within the date window. Uses the ESearch 
#' functionality of E-utilities to search a specified database. This function 
#' returns a maximum of 100,000 documents that fall within the window.
#'
#' @param start A string specifying the start date. The string must be formatted as
#' YYYY, YYY/MM, or YYYY/MM/DD.
#' @param end A string specifying the end date. The string must be formatted as
#' YYYY, YYY/MM, or YYYY/MM/DD.
#' 
#' @return A tibble with one column (labeled PMCID) that contains all the PMCID
#' numbers returned from the database search.
#' 
#' @examples 
#' search_by_date("2020/01/01", "2020/01/31")
#' search_by_date("2020/01/01", "2020/01/01")
#' 
#' @importFrom magrittr "%>%"
#' 
#' @export

search_by_date <- function(start, end) {
  term <- "open+access[filter]+AND+nih+funded[filter]"
  date_format <- "\\d{4}|\\d{4}/\\d{2}|\\d{4}/\\d{2}/\\d{2}"
  if (stringr::str_length(start) != stringr::str_length(end)) {
    stop("Start and end dates must be in the same format")
  }
  if (!stringr::str_detect(start, date_format)) {
    stop("Input start date not in a proper format")
  }
  if (!stringr::str_detect(end, date_format)) {
    stop("Input end date not in a proper format")
  }
  base_url <- 
    paste0(
      "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pmc&term=",
      term,
      "&retmax=100000&"
    )
  
  url <-
    paste0(base_url, "mindate=", start, "&maxdate=", end)
  
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