#' Get a document's metadata in a tidy format
#' 
#' Wrapper function for `tidypmc::pmc_metadata()` to return metadata in a tidy format.  
#'
#' @param id A string specifying the ID number corresponding to a document.
#' 
#' @return a tibble with two columns: the metadata field and the value.
#' 
#' @examples 
#' test_id <- "PMC7344926"
#' tidy_metadata(test_id)
#' 
#' @importFrom magrittr "%>%"
#' 
#' @export

tidy_metadata <- function(id) {
  
  xml <-
    xml_by_id(id, db = "pmc")
    
  xml %>% 
    tidypmc::pmc_metadata() %>% 
    tibble::as_tibble() %>% 
    tidyr::gather(key = "metadata_field", value = "value")
}