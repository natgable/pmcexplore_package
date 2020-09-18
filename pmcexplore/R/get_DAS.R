#' Get data sharing and accessibility information of a document
#' 
#' This function uses the PMCID to get the full-text PMC XML document. It then
#' uses text mining, regular expressions, and XML tagging to return data sharing
#' information. The possible ways that this function detects data sharing information:
#' (1) A specific Data Availability Statement section is tagged in the XML, 
#' (2) A Supplementary Materials section is tagged in the XML,
#' (3) Any section of the document has a title matching a list of titles included below,
#' (4) Any inline tables or figures
#'
#' @param id A string of the PMCID number of the document
#' 
#' @return A tibble with two columns: tagging method (matching one of the three
#' methods above) and content. If there is no data sharing information in the document
#' that matches the methods above, return the string "No data availability information".
#' 
#' @examples 
#' get_DAS("PMC4161373")
#' 
#' @importFrom magrittr "%>%"
#' 
#' @export

get_DAS <- function(id) {
  
  possible_titles <- # provided by Rebecca Orris
    c(
      "data availability",
      "data availability statement",
      "data accessibility",
      "data accessibility statement",
      "availability of data and material",
      "data and code availability",
      "code and data availability",
      "data and software availability",
      "software and data availability",
      "availability and data format",
      "availability of data",
      "availability of supporting data",
      "correlates of data availability",
      "data archiving statement",
      "data deposition and access",
      "direct link to deposited data",
      "raw data availability"
    )
  
  # source("./xml_by_id.R")
  xml <- xml_by_id(id, "pmc") # retrieves XML from PMC
  
  if (class(xml)[1] != "xml_document") {
    stop("not a proper XML document")
  }
  
  doc_list <-
    vector("list")
  
  # tag 1: data availability statements (specific tag)
  das <-
    xml %>% 
    xml2::xml_find_all(
      "//sec[@sec-type='data-availability']|
        //notes[@notes-type='data-availability']|
      //custom-meta[@id='data-availability']"
    )
  
  # tag 2: data citations
  cits <-
    xml %>% 
    xml2::xml_find_all(
      "//ref-list//element-citation[@publication-type='data']|
      //ref-list//mixed-citation[@publication-type='data']"
    )
  
  # tag 3: supplementary materials
  supp <- 
    xml %>% 
    xml2::xml_find_all(
      "//body//sec[@sec-type='supplementary-material'] |
      //body//supplementary-material"
    )
  
  # tag 4: inline/embedded tables and figures
  included_df <-
    xml %>% 
    xml2::xml_find_all("//xref") %>% 
    xml2::xml_parent() %>% 
    purrr::map_dfr(
      ~list(
        parent_val = list(purrr::map(xml2::xml_children(.x), xml2::xml_text)),
        ref_attr = list(purrr::map(xml2::xml_children(.x), xml2::xml_attrs))
      )
    )
  
  if (nrow(included_df) > 0 ){
    included_df <-
      included_df %>% 
      tidyr::unnest(ref_attr) %>% 
      dplyr::mutate(
        attr_len = purrr::map_int(ref_attr, length)
      ) %>% 
      dplyr::filter(attr_len > 0) %>% 
      tidyr::unnest(ref_attr) %>% 
      dplyr::filter(ref_attr %in% c("fig", "table")) 
  }
  
  if(nrow(included_df) > 0 ){
    included_df <-
      included_df %>% 
      tidyr::unnest(parent_val) %>% 
      tidyr::unnest(parent_val) %>% 
      unique() %>% 
      dplyr::group_by(ref_attr) %>% 
      dplyr::mutate(
        parent_val_concat = stringr::str_c(parent_val, sep = " ", collapse = " ")
      ) %>% 
      dplyr::ungroup() %>% 
      dplyr::select(ref_attr, parent_val_concat) %>% 
      unique() %>% 
      dplyr::mutate(
        ref_attr = dplyr::case_when(
          ref_attr == "fig" ~ "Inline Figure", 
          ref_attr == "table" ~ "Inline Table",
          TRUE ~ ref_attr
        )
      ) %>% 
      dplyr::rename(
        tagging_method = ref_attr,
        content = parent_val_concat
      )
  }
  
  # specifically tagged DAS
  if (length(das) > 0) {
    doc_list[["Data Availability Statement"]] <- das
  } 
  # data citation section
  if (length(cits) > 0) {
    doc_list[["Data Citations"]] <- cits
  } 
  # supplementary materials
  if (length(supp) > 0) {
    doc_list[["Supplementary Materials"]] <- supp
  } 
  # if all else fails, use any with the provided titles (see above)
  if (length(das) == 0 & length(cits) == 0 & length(supp) == 0) {
    das_titles <-
      xml %>% 
      xml2::xml_find_all("//title")
    
    for (i in seq_len(length(das_titles))) {
      curr_node <-
        das_titles[i]
      
      # standardize title to compare to provided
      curr_title <- 
        curr_node %>% 
        xml2::xml_text() %>% 
        stringr::str_trim() %>%
        stringr::str_to_lower() %>% 
        stringr::str_replace("s(?=$)", "") # take out plural s
      
      if (curr_title %in% possible_titles) {
        title_das <- 
          curr_node %>% 
          xml2::xml_parent()
        
        doc_list[[paste0("Title: ", stringr::str_to_title(curr_title))]] <- 
          title_das
      }
    }
  }
  
  # now tidy everything
  # everything is currently a list of nodesets
  # we want everything to be in a tibble of format:
  # Tagging type | Content
  format_node <- function(node) {
    # return non-text elements on supplementary data (e.g. a pdf download)
    if(length(supp) > 0) { 
      node %>% 
        xml2::xml_children() %>% 
        xml2::xml_text() %>% 
        paste0(" ") %>% 
        paste(
          node %>% 
            xml2::xml_parent() %>% 
            xml2::xml_find_all("//media")
        ) 
    } else {
      node %>% 
        xml2::xml_children() %>% 
        xml2::xml_text()
    }
  }
  
  if (length(doc_list) > 0) {
    doc_list %>% 
      names() %>% 
      tibble::tibble() %>% 
      dplyr::rename(tagging_method = ".") %>% 
      dplyr::mutate(
        content = purrr::map(doc_list, format_node)
      ) %>% 
      tidyr::unnest(content) %>% 
      dplyr::slice(2:nrow(.)) %>% 
      dplyr::mutate(
        content = stringr::str_remove(content, "^.*[:upper:][:digit:](?=[:upper:])")
      ) %>% 
      bind_rows(included_df)
  } else if (length(doc_list) <= 0 & nrow(included_df) > 0) {
    included_df
  } else {
    "No data availability information."
  }
  
}