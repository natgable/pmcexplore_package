####################
# PMC Explorer App #
# Natalie Gable    #
####################

library(shiny)
library(shinydashboard)
library(tidyverse)
library(pmcexplore)
library(plotly)
library(tidypmc)
library(lubridate)
library(xml2)
library(wordcloud)
source("../experiments/annotation.R")

ui <-
  dashboardPage(
    header = dashboardHeader(title = "PMC Explorer"),
    skin = "blue",
    
    # Sidebar includes tabs for ID and Data Sharing Trends
    sidebar = dashboardSidebar(
      sidebarMenu(
        menuItem("Explore by ID", tabName = "by_id", icon = icon("id-card")),
        menuItem("Data Sharing Trends", tabName = "das", icon = icon("chart-bar")),
        menuItem("Annotate Document", tabName = "annotations", icon = icon("pencil"))
      )
    ),
    
    # Plots and interface
    body = dashboardBody(
      tabItems(
        
        # first tab with PMC search
        tabItem(
          tabName = "by_id",
          fluidRow(
            column(
              width = 12,
              box(
                textInput("pmcid", label = "Input a 7-digit PMCID", value = "PMC"),
                textOutput("valid_pmcid"),
                p(" "),
                actionButton("submit", "Search")
              ),
              box(title = "Title", textOutput("title"))
            ),
            column(
              width = 12,
              box(title = "Abstract", textOutput("abstract")),
              box(title = "MeSH Terms", tableOutput("mesh_tab"))
            ),
            column(
              width = 12,
              box(title = "Data Sharing", tableOutput("das_tab"))
            )
          )
        ),
        
        # second tab with DAS plots and graphics
        tabItem(
          tabName = "das",
          fluidRow(
            box(
              title = "Document Date Range",
              dateRangeInput(
                "date_range", 
                label = "Input a date range (month and year) between 2017 and 2019",
                start = "2018-01-01",
                end = "2018-12-31",
                format = "yyyy-mm",
                min = "2017-01-01", 
                max = "2019-12-31"
              ),
              p("(Plots might take a moment to load)")
            )
          ),
          h4(textOutput("das_title")),
          h5(textOutput("number_docs")),
          p(""),
          plotOutput("has_das_plot"),
          p(""),
          plotOutput("tagging_method_plot"),
          p(""),
          plotOutput("label_plot"),
          p(""),
          plotOutput("time_series"),
          p(""),
          h4("Word Cloud of DAS Content"),
          plotOutput("wordcloud")
        ),
        
        # third tab with annotated text
        tabItem(
          tabName = "annotations",
          fluidRow(
            box(
              textInput("pmcid_anno", label = "Input a 7-digit PMCID", value = "PMC"),
              textOutput("valid_pmcid_anno")
            ),
            box(
              h2("Annotated Text"),
              htmlOutput("annotated_text")
            )
          )
        )
      )
    )
  )

server <- function(input, output) {
  
  full_xml <-
    reactive({
      xml_by_id(input$pmcid)
    })
  
  output$title <-
    renderText({
      if(input$submit) {
        if(str_detect(input$pmcid, "PMC\\d{7}")) {
          full_xml() %>% 
            pmc_text() %>% 
            filter(section == "Title") %>% 
            pull(text)
        }
      }
    })
  
  output$valid_pmcid <-
    renderText({
      if(str_detect(input$pmcid, "PMC\\d{7}")) {
        "This is a valid PMCID. Click Search."
      } else {
        "Please enter a valid PMCID (PMC followed by 7 digits)."
      }
    })
  
  output$abstract <-
    renderText({
      if(input$submit) {
        if(str_detect(input$pmcid, "PMC\\d{7}")) {
          full_xml() %>% 
            pmc_text() %>% 
            filter(section == "Abstract") %>% 
            pull(text) %>% 
            paste(sep = "", collapse = " ")
        }
      }
    })
  
  output$das_tab <-
    renderTable({
      if(input$submit) {
        if(str_detect(input$pmcid, "PMC\\d{7}")) {
          full_xml_len <-
            input$pmcid %>% 
            xml_by_id() %>% 
            xml_children() %>% 
            xml_children() %>% 
            length()
          
          if(full_xml_len < 3) {
            tibble(
              `Data Availability` = "The full XML of this document is not available on PubMed Central."
            )
          } else {
            tibble(
              PMCID = input$pmcid,
              das = map(input$pmcid, get_DAS)
            ) %>% 
              clean_DAS() %>% 
              select(tagging_method, content) %>% 
              rename(
                `Data Tagging Method` = tagging_method,
                `Content` = content
              ) %>% 
              unique()
          }
        }
      }
    })
  
  output$mesh_tab <-
    renderTable({
      if(input$submit) {
        if(str_detect(input$pmcid, "PMC\\d{7}")) {
          get_mesh(input$pmcid)
        }
      }
    })
  
  filtered_by_date <-
    reactive({
      all_das %>% 
        mutate(
          day = 01,
          date = dmy(paste0(day, month, year))
        ) %>% 
        filter(
          date >= input$date_range[1] %>% ymd() %>% floor_date(unit = "month") &
            date <= input$date_range[2] %>% ymd() %>% ceiling_date(unit = "month") %>% rollback()
        )
    })
  
  label_filtered_by_date <-
    reactive({
      all_labeled_das %>% 
        mutate(
          day = 01,
          date = dmy(paste0(day, month, year))
        ) %>% 
        filter(
          date >= input$date_range[1] %>% ymd() %>% floor_date(unit = "month") &
            date <= input$date_range[2] %>% ymd() %>% ceiling_date(unit = "month") %>% rollback()
        )
    })
  
  output$das_title <- 
    renderText({
      paste0(
        "Data sharing plots of documents from ",
        input$date_range[1] %>% ymd() %>% floor_date(unit = "month"),
        " to ",
        input$date_range[2] %>% ymd() %>% ceiling_date(unit = "month") %>% rollback()
      )
    })
  
  output$number_docs <- 
    renderText({
      ndocs <- 
        filtered_by_date() %>% 
        select(PMCID) %>% 
        unique() %>% 
        nrow()
      
      paste0("There are ", ndocs, " PMC documents in this date range.")
    })
  
  output$has_das_plot <-
    renderPlot({
      filtered_by_date() %>% 
        select(PMCID, das, has_das) %>% 
        mutate(has_das = if_else(has_das, "Yes", "No")) %>% 
        count(has_das) %>% 
        ggplot(mapping = aes(x = has_das, y = n)) +
        geom_col() +
        geom_label(aes(label = n), size = 4) +
        theme_bw() +
        labs(
          title = "Data Sharing Breakdown",
          x = "Does this document share data in some way?",
          y = "Count"
        )
    })
  
  output$tagging_method_plot <-
    renderPlot({
      filtered_by_date() %>% 
        filter(has_das) %>% 
        unnest(das) %>%
        select(PMCID, tagging_method) %>%
        mutate(
          tagging_method = if_else(
            str_detect(tagging_method, "Title"),
            "Section with Data-Title",
            tagging_method
          )
        ) %>% 
        group_by(PMCID) %>% 
        unique() %>% 
        ungroup() %>% 
        count(tagging_method) %>% 
        ggplot(mapping = aes(x = fct_reorder(tagging_method, n), y = n)) +
        geom_col() + 
        geom_label(aes(label = n), size = 4) +
        theme_bw() +
        labs(
          title = "Data Tagging Methods in PubMed Central",
          x = "Tagging Method",
          y = "Count"
        )
    })
  
  output$label_plot <- 
    renderPlot({
      label_exp <-
        c(
          "0) No data sharing information.",
          "1) Supplementary materials for download.",
          "2) Data citation or link to repository.",
          "3) Data upon request.",
          "4) Restricted access.",
          "5) Supplementary materials, but no downloadable file.",
          "6) Other.",
          "7) Inline figures or tables.",
          "8) Two or more categories."
        )
      
      label_filtered_by_date() %>% 
        count(label) %>% 
        mutate(label_exp = label_exp) %>% 
        ggplot(mapping = aes(x = label, y = n)) +
        geom_col(aes(fill = label_exp)) +
        geom_label(aes(label = n), size = 4) +
        theme_bw() +
        theme(
          legend.position = c(0.3,0.65),
          legend.background = element_rect(color = "gray")
        ) +
        labs(
          title = "Data Sharing in PubMed Central",
          subtitle = "For NIH-funded Publications",
          x = "Label",
          y = "Count",
          fill = "Label Details"
        )
    })
  
  output$time_series <- 
    renderPlot({
      filtered_by_date() %>% 
        filter(has_das) %>%
        unnest(das) %>% 
        select(PMCID, date, tagging_method) %>% 
        mutate(
          tagging_method = if_else(
            str_detect(tagging_method, "Title"),
            "Titled Section Related to Data",
            tagging_method
          )
        ) %>% 
        unique() %>% 
        group_by(date) %>% 
        count(date, tagging_method) %>% 
        ggplot(mapping = aes(x = date, y = n, color = tagging_method)) +
        geom_point() +
        geom_line() +
        theme_bw() +
        theme(
          legend.position = "bottom"
        ) +
        labs(
          title = "Data Sharing Methods over Time",
          x = "Date",
          y = "Count",
          color = "Tagging Method Details"
        )
    })
  
  wordcloud_rep <- repeatable(wordcloud)
  
  output$wordcloud <- 
    renderPlot({
      word_freq <- 
        filtered_by_date() %>% 
        select(PMCID) %>% 
        left_join(das_words, by = "PMCID") %>% 
        count(word)
      
      wordcloud_rep(
        word_freq$word, 
        word_freq$n, 
        scale = c(4, 0.5),
        min.freq = 1, 
        max.words= 30,
        colors = brewer.pal(8, "Dark2")
      )
    })
  
  output$valid_pmcid_anno <-
    renderText({
      if(str_detect(input$pmcid_anno, "PMC\\d{7}")) {
        "This is a valid PMCID."
      } else {
        "Please enter a valid PMCID (PMC followed by 7 digits)."
      }
    })
  
  output$annotated_text <-
    renderText({
      annotate_text(input$pmcid_anno) %>%
        mutate(
          title = if_else(
            str_to_title(sentence) == sentence & !str_detect(sentence, "[:punct:]"),
            TRUE,
            FALSE
          ),
          annotated = if_else(
            is_available|has_data|external_source|supplementary|has_das,
            paste0("<mark>", sentence, "</mark>"),
            sentence
          ),
          annotated = if_else(
            title, 
            paste0("<h4>", sentence, "</h4>"), 
            annotated
          )
        ) %>%
        pull(annotated) %>%
        paste(collapase = " ")
    })
  
}

shinyApp(ui = ui, server = server)