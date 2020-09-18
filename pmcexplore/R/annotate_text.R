#' Use text mining and regular expressions to extract data sharing information from a document
#' 
#' This function uses text mining and regular expressions to determine, sentence
#' by sentence, if there is a match with specific data availability keywords.
#' The algorithm used in this function is inspired by the ODDPub package, which
#' is available on GitHub at github.com/quest-bih/oddpub. There are vectors of
#' keywords which are grouped as words associated with: available, was_available,
#' not_available, repository, data, not_data, source_code, supplements_and_files,
#' and data_availability. An input document is tokenized by sentences and each 
#' sentence is marked if it matches one of the five following combinations:
#' 
#' 1) is_available = data NEAR (available OR was_available)
#' 2) not_available = not_data OR not_available
#' 3) external_source = repository OR source_code
#' 4) supplementary = supplements_and_files
#' 5) has_das = data_availability
#' 
#' x NEAR y means that keyword x is detected in the same sentence, the following sentence,
#' or the previous sentence as keyword y.
#'
#' @param pmcid A string of the PMCID number of the document
#' 
#' @return A tibble with the tokenized sentences and logical columns is_available,
#' not_available, external_source, supplementary, and has_das that are TRUE or FALSE
#' for each sentence.
#' 
#' @examples 
#' annotate_text("PMC7437466")
#' 
#' @importFrom magrittr "%>%"
#' 
#' @export

annotate_text <- function(pmcid) {
  
  # helper function to format the search term keywords
  format_keyword_vectors <- function(keyword_list, to_lower) {
    if(to_lower) {
      keyword_list %>% 
        purrr::map_chr(
          ~ paste0("\\b", ., "\\b") %>% 
            stringr::str_to_lower()
        ) %>% 
        stringr::str_c(collapse = "|")
    } else {
      keyword_list %>% 
        purrr::map_chr(
          ~ paste0("\\b", ., "\\b")
        ) %>% 
        stringr::str_c(collapse = "|")
    }
  }
  
  # for formatting purposes 
  possible_titles <-
    c(
      "Background and Aims",
      "Background/Aims",
      "Background",
      "(?<!Graphical )Abstract",
      "Conclusions",
      "Conclusion",
      "Materials",
      "Methods/Results",
      "Methods",
      "Results",
      "Objectives",
      "Objective"
    ) %>% 
    format_keyword_vectors(to_lower = FALSE)
  
  # first specify all keyword combinations
  # this is taken from ODDPub
  available <- 
    c(
      "included",
      "deposited",
      "released",
      "is provided",
      "are provided",
      "contained in",
      "available",
      "reproduce",
      "accessible",
      "can be accessed",
      "submitted",
      "can be downloaded",
      "reported in",
      "uploaded",
      "are public on"
    ) %>% 
    format_keyword_vectors(to_lower = TRUE)
  
  was_available <- 
    c(
      "was provided",
      "were provided",
      "was contained in",
      "were contained in",
      "was available",
      "were available",
      "was accessible",
      "were accessible",
      "deposited by",
      "were reproduced"
    ) %>% 
    format_keyword_vectors(to_lower = TRUE)
  
  not_available <-
    c(
      "not included",
      "not deposited",
      "not released",
      "not provided",
      "not contained in",
      "not available",
      "not accessible",
      "not submitted"
    ) %>% 
    format_keyword_vectors(to_lower = TRUE)
  
  url1 <- "https://www.nlm.nih.gov/NIHbmic/domain_specific_repositories.html"
  table1 <-
    xml2::read_html(url1) %>% 
    rvest::html_table()
  
  url2 <- "https://www.nlm.nih.gov/NIHbmic/other_data_resources.html"
  table2 <-
    xml2::read_html(url2) %>% 
    rvest::html_table()
  
  other_repositories <-
    tribble(
      ~repository_name, ~abbreviation,
      "Dataverse", NA_character_,
      "Dryad", NA_character_,
      "Figshare", NA_character_,
      "Mendeley Data", NA_character_,
      "Open Science Framework", NA_character_,
      "Vivli", NA_character_,
      "Zenodo", NA_character_
    )
  
  get_repository_names <- function(table) {
    table[[1]] %>% 
      dplyr::transmute(
        repository_name = `Repository Name`
      ) %>% 
      dplyr::mutate(
        abbreviation = dplyr::if_else(
          stringr::str_detect(repository_name, "\\("),
          stringr::str_extract(repository_name, "\\(.+\\)") %>% 
            str_remove("\\(") %>% 
            str_remove("\\)"),
          NA_character_
        ),
        repository_name = stringr::str_remove(repository_name, "\\(.+\\)")
      )
  }
  
  repository1 <- get_repository_names(table1)
  repository2 <- get_repository_names(table2)
  
  repository <-
    c(
      repository1$repository_name,
      repository2$repository_name,
      other_repositories$repository_name,
      repository1 %>% 
        dplyr::filter(!is.na(abbreviation)) %>% 
        dplyr::pull(abbreviation),
      repository2 %>% 
        dplyr::filter(!is.na(abbreviation)) %>% 
        dplyr::pull(abbreviation)
    ) %>% 
    format_keyword_vectors(to_lower = FALSE)
  
  data <- 
    c(
      "data",
      "dataset",
      "datasets",
      "all data",
      "all array data",
      "raw data",
      "full data set",
      "full dataset",
      "crystallographic data",
      "subject-level data"
    ) %>% 
    format_keyword_vectors(to_lower = TRUE)
  
  not_data <- 
    c(
      "not all data",
      "not all array data",
      "no raw data",
      "no full data set",
      "no full dataset"
    ) %>% 
    format_keyword_vectors(to_lower = TRUE)
  
  source_code <- 
    c(
      "source code",
      "analysis script",
      "github",
      "SAS script",
      "SPSS script",
      "R script",
      "R code",
      "\\.R\\b",
      "python",
      "python script",
      "python code",
      "matlab script",
      "matlab code"
    ) %>% 
    format_keyword_vectors(to_lower = TRUE)
  
  supplement_and_files <- 
    c(
      "supporting information",
      "supplement",
      "supplementary data",
      "(\\b|[:punct:])csv(\\b|[:punct:])",
      "(\\b|[:punct:])zip(\\b|[:punct:])",
      "(\\b|[:punct:])xls(\\b|[:punct:])",
      "(\\b|[:punct:])xlsx(\\b|[:punct:])",
      "(\\b|[:punct:])sav(\\b|[:punct:])",
      "(\\b|[:punct:])cif(\\b|[:punct:])",
      "(\\b|[:punct:])fasta(\\b|[:punct:])", 
      "(\\b|[:punct:])doc(\\b|[:punct:])",
      "(\\b|[:punct:])docx(\\b|[:punct:])",
      "supplementary table",
      "supplementary tables",
      "supplemental table",
      "supplemental tables",
      "table", 
      "tables",
      "additional file",
      "file", 
      "files",
      "supplementary data",
      "supplementary dataset",
      "supplementary data set"
    ) %>% 
    format_keyword_vectors(to_lower = TRUE)
  
  data_availability <- 
    c(
      "Data sharing",
      "Data Availability Statement",
      "Data Availability",
      "Data deposition",
      "Deposited Data",
      "Data Archiving",
      "Availability of data and materials",
      "Availability of data",
      "Data Accessibility",
      "Accessibility of data"
    ) %>% 
    format_keyword_vectors(to_lower = TRUE)
  
  parse_sentences <- function(section_nodes) {
    if(length(section_nodes) > 0) {
      section_nodes %>% 
        xml2::xml_contents() %>% 
        xml2::xml_text() %>% 
        tibble::tibble() %>% 
        dplyr::rename(text = ".") %>%
        dplyr::mutate(
          text = gsub("([a-z])([A-Z])", "\\1 \\2", text),
          text = gsub("([a-z])(\\.)([A-Z])", "\\1\\2 \\3", text),
        ) %>% 
        dplyr::mutate(
          sentences = purrr::map(
            text, 
            ~ tokenizers::tokenize_sentences(.) %>% 
              unlist() %>% 
              tibble::tibble() %>% 
              dplyr::rename(sentence = ".")
          )
        ) %>% 
        dplyr::select(sentences) %>% 
        tidyr::unnest(sentences) %>% 
        transmute(
          title_sentence = str_extract(sentence, possible_titles) %>%
            str_to_title() %>% 
            paste0(":::"),
          sub_sentence = str_remove(sentence, possible_titles)
        ) %>%
        unite(title_sentence, sub_sentence, col = "full_sentence") %>%
        separate_rows(full_sentence, sep = ":::") %>%
        transmute(
          sentence = str_remove(full_sentence, "\\b_"),
          sentence = if_else(sentence == "NA", NA_character_, sentence)
        ) %>% 
        drop_na()
    } else {
      tibble()
    }
  }
  
  if (stringr::str_detect(pmcid, "\\bPMC\\d{7}\\b")) {
    xml <- pmcexplore::xml_by_id(pmcid)
  } else {
    stop("Please provide a proper 7-digit PMCID.")
  }
  
  xml_nodes <- 
    xml %>% 
    xml2::xml_children() %>% 
    xml2::xml_children()
  
  front <-
    xml_nodes %>%
    xml2::xml_find_all("//abstract") %>%
    parse_sentences() %>% 
    dplyr::mutate(
      section = "Front"
    )
  
  body <-
    xml_nodes %>%
    xml2::xml_find_all("//body") %>%
    xml2::xml_contents() %>%
    parse_sentences() %>%
    dplyr::mutate(
      section = "Body"
    )
  
  full_doc <-
    rbind(front, body)
  
  full_doc %>% 
    dplyr::mutate(
      lowercase_sentence = stringr::str_to_lower(sentence),
      prev_sentence = sentence %>% stringr::str_to_lower() %>% dplyr::lag(),
      next_sentence = sentence %>% stringr::str_to_lower() %>% dplyr::lead()
    ) %>% 
    mutate(
      is_available = stringr::str_detect(lowercase_sentence, data) & 
        (
          (stringr::str_detect(lowercase_sentence, available) | 
             stringr::str_detect(prev_sentence, available) |
             stringr::str_detect(next_sentence, available)) |
            (stringr::str_detect(lowercase_sentence, was_available) |
               stringr::str_detect(prev_sentence, was_available) |
               stringr::str_detect(next_sentence, was_available))
        ),
      not_available = stringr::str_detect(lowercase_sentence, not_data) |
        stringr::str_detect(lowercase_sentence, not_available),
      external_source = stringr::str_detect(sentence, repository) | # capitalization matters for repo names
        stringr::str_detect(lowercase_sentence, source_code),
      supplementary = stringr::str_detect(lowercase_sentence, supplement_and_files),
      has_das = stringr::str_detect(lowercase_sentence, data_availability)
    )
  
}

