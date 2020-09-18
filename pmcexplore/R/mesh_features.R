#' Create binary encoding of a paper's MeSH terms
#' 
#' This function uses the assigns a numerical embedding of MeSH 
#' (Medical Subject Heading) terms. MeSH terms form a tree structure,
#' where some tags are sub-categories of a more broad tag. MeSH tags range from
#' high-level terms like "Metabolism (G03)" to "DNA Methylation (G03.059.538.161)".
#' This function focuses on the 111 high level terms, from "Body Regions (A01)"
#' to "Geographic Locations (Z01)". This function takes all the more specific tags
#' of a document (represented by ID number) and matches them to their parent/higher
#' level term. It then uses binary encoding to numerically embed the MeSH terms,
#' where it will have a 1 if the document contains the MeSH term and 0 otherwise.
#'
#' @param id A string of the PMCID number of the document
#' 
#' @return A 1 x 111 matrix that has 1's corresponding to the included MeSH terms.
#' 
#' @examples 
#' mesh_features("PMC4161373")
#' 
#' @importFrom magrittr "%>%"
#' 
#' @export

mesh_features <- function(id) {
  mesh_names <- get_mesh(id)
  
  id_mesh_table <- 
    dplyr::inner_join(mesh_table, mesh_names, by = c("description" = "MeSH_terms")) %>% 
    dplyr::select(node1, description) %>% 
    unique() %>% 
    dplyr::rename(top_level = node1) %>% 
    dplyr::inner_join(mesh_top_level, by = c("top_level" = "branch")) %>% 
    dplyr::rename(big_umbrella = descriptor)
  
  mesh_top_level %>% 
    dplyr::mutate(
      is_included = descriptor %in% id_mesh_table$big_umbrella 
    ) %>% 
    dplyr::select(is_included) %>% 
    dplyr::mutate(is_included = if_else(is_included, 1, 0)) %>% 
    t()
}