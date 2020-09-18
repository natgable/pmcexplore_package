## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(tidyverse)
library(pmcexplore)

## ----search database----------------------------------------------------------
my_ids <- search_by_date(start = "2019/01/01", end = "2019/01/02")

head(my_ids)

## ----pull text----------------------------------------------------------------
my_text <-
  my_ids %>% 
  pull(PMCID) %>% # this takes out just the character vector
  map(xml_by_id) %>% # the map function applies pmcexplore::xml_by_id to retrieve xmls for each ID number
  tibble() %>% # wrangle into a tibble datatype
  rename(xml = ".") %>% 
  transmute(
    PMCID = my_ids %>% pull(PMCID),
    text = map(xml, tidypmc::pmc_text) # uses the tidypmc function to parse text
  )

head(my_text)

## ----text data----------------------------------------------------------------
my_text %>% 
  dplyr::slice(1) %>% 
  tidyr::unnest(text) %>% 
  select(text) %>% 
  head()

## ----features-----------------------------------------------------------------
my_features <-
  generate_features(my_text, lem_stem = "stem", mode = "tm 50")

dim(my_features)

## -----------------------------------------------------------------------------
my_features_processed <-
  preprocess_text(my_features, mode = "none", type = "tm")

## -----------------------------------------------------------------------------
my_das <-
  my_ids %>% 
  mutate(
    das = purrr::map(PMCID, get_DAS)
  )

head(my_das)

## -----------------------------------------------------------------------------
my_das_cleaned <-
  clean_DAS(my_das)

head(my_das_cleaned)

## -----------------------------------------------------------------------------
my_labels <-
  label_DAS(my_das_cleaned)

head(my_labels)

## -----------------------------------------------------------------------------
my_data <-
  bind_cols(my_labels, as_tibble(my_features_processed)) %>% 
  select(-PMCID) %>% 
  mutate(
    label = factor(label)
  )

head(my_data)

## -----------------------------------------------------------------------------
train_index <- 
  caret::createDataPartition(
    my_data$label, 
    p = 0.8, 
    list = FALSE, 
    times = 1
  )

train_data <- my_data[train_index,]
test_data <- my_data[-train_index,]

## -----------------------------------------------------------------------------
train_data %>% 
  count(label)

## -----------------------------------------------------------------------------
balanced_train_data <-
  train_data %>% 
  balance_labels(p = 0.87)

balanced_train_data %>% 
  count(label)

