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
library(rintrojs)
library(tokenizers)

# helper function to add class to a shiny element 
# this is used in order to access individual tabs in introjs
add_class <- function(x, class) {
  x$attribs <- append(x$attribs, list(class = class))
  x
}

ui <-
  dashboardPage(
    header = dashboardHeader(title = "PMC Explore"),
    skin = "blue",
    
    # Sidebar includes tabs for ID and Data Sharing Trends
    sidebar = dashboardSidebar(
      introjsUI(),
      sidebarMenu(
        menuItem("Explore by ID", tabName = "by_id", icon = icon("id-card")) %>% add_class("by_id"),
        menuItem("Data Sharing Trends", tabName = "das", icon = icon("chart-bar")) %>% add_class("das"),
        menuItem("Annotate Document", tabName = "annotations", icon = icon("pencil")) %>% add_class("annotations")
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
                textOutput("valid_pmcid")
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
              box(title = "Data Sharing", tableOutput("das_tab")),
              box(title = "Other Metadata", tableOutput("metadata"))
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
          plotOutput("time_series_percent"),
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
              checkboxGroupInput(
                "anno_type",
                label = "Highlight Inputs",
                choiceNames= list(
                  tags$span("Contains words related to 'availability'", style = "background-color: #FFFF00"),
                  tags$span("No availability", style = "background-color: #00FF00"),
                  tags$span("Contains external source(s)", style = "background-color: #00FFFF"),
                  tags$span("Contains words related to supplementary materials and files", style = "background-color: #FFB6C1"),
                  tags$span("Contains data availability statement", style = "background-color: #FFA900")
                ),
                choiceValues = c("avail_check", "no_avail_check", "external_check", "supp_check", "das_check"),
                selected = c("avail_check", "no_avail_check", "external_check", "supp_check", "das_check")
              )
            ),
          ),
          fluidRow(
            column(
              width = 12, 
              offset = 0.2,
              h2("Annotated Text"),
              htmlOutput("annotated_text")
            )
          )
        )
      )
    )
  )

server <- function(input, output, session) {
  
  # intro pop up box
  observeEvent("", {
    showModal(
      modalDialog(
        h4("Welcome to the PubMed Central Explore App!", style = "text-align: center"),
        p(strong("What is this project? Why is data sharing important?")),
        p("This app lets us explore text data from PubMed Central, and specifically data sharing information."),
        p("Good data sharing and stewardship practices are important in the future of biomedical 
          and scientific research. The FAIR (findable, accessible, interoperable, and reusable) 
          data guiding approach allows for better data sharing and reuse, study reproducibility, 
          and collaboration between researchers. The goal of this project is to understand what 
          data sharing looks like for papers published in PubMed Central. 
          To learn more about data sharing in general, check out the ", 
          a(
            href = 'https://www.nature.com/articles/sdata201618', 
            "FAIR Guiding Principles for scientific data management and stewardship"
          ), 
          " and the ", 
          a(
            href = "https://digitalscience.figshare.com/articles/The_State_of_Open_Data_Report_2019/9980783", 
            "State of Open Data Report 2019"
          )
        ),
        p("This app allows us to look at the data sharing information of individual papers
          as well as see trends in data sharing practices from 2017 to 2019."),
        p("Click the Introduction button to walk through the app and learn more!"),
        p(""),
        p("Questions? Comments? Contact natalie.gable@nih.gov"),
        easyClose = TRUE,
        footer = tagList(
          actionButton(inputId = "intro_button", label = "Introduction", icon = icon("info-circle"))
        )
      )
    )
  })
  
  # set up logic for intro step through menu on the server side
  steps <-
    reactive(
      data.frame(
        element = c(".by_id", ".das", ".annotations"),
        intro = c(
          "Here we can search PubMed Central by PMCID. 
          A PMCID is a unique 7-digit ID number associated with a document. 
          After inputting the PMCID, we can view the document title, abstract, 
          associated Medical Subject Heading (MeSH) terms, 
          and the associated data sharing practices if applicable. The data sharing 
          practices include inline figures and tables, data availability statements,
          supplementary materials, data citations, and sections with titles related to 
          data availability. The information returned is also dependent on the availability 
          of the full XML document in the PubMed Central database.", 
          "Here we can see trends in data sharing information for documents published
          in PMC between January 2017 and December 2019. Input a date range 
          (with month and year) and view the following visualizations: a bar chart of 
          whether documents have any data sharing information at all, a bar chart with
          counts of the different tagging methods, a bar chart with counts of documents
          categorized by different labels, a time series showing tagging methods over time,
          and a wordcloud of commonly used words in data sharing sections of papers.",
          "Here we can test another way to look at data sharing information for individual
          documents. This page implements an annotation algorithm, that searches through the 
          full text of a document and highlights sections that might have
          relevance to data sharing. This annotation algorithm uses text mining and regular
          expressions to determine whether a phrase is associated with data sharing. 
          If there are no highlighted sections of the document, 
          there is no data sharing information detected by the algorithm."
        )
      )
    )
  
  # close the pop up box when introduction button is pressed
  observeEvent(
    input$intro_button,
    {
      removeModal()
    }
  )
  
  # walk through app sequence
  observeEvent(
    input$intro_button,
    introjs(
      session,
      options = list(
        steps = steps(),
        "nextLabel" = "Next",
        "prevLabel" = "Go Back",
        "doneLabel" = "Let's go!"
      )
    )
  )
  
  # download full xml to use in first tab
  full_xml <-
    reactive({
      if(str_detect(input$pmcid, "\\bPMC\\d{7}\\b")) {
        xml_by_id(input$pmcid)
      }
    })
  
  # first tab: by id, get title
  output$title <-
    renderText({
      if(str_detect(input$pmcid, "\\bPMC\\d{7}\\b")) {
        full_xml() %>% 
          pmc_text() %>% 
          filter(section == "Title") %>% 
          pull(text)
      }
    })
  
  # first tab: check if the input ID is a 7 digit PMCID
  output$valid_pmcid <-
    renderText({
      if(str_detect(input$pmcid, "\\bPMC\\d{7}\\b")) {
        "This is a valid PMCID."
      } else {
        "Please enter a valid PMCID (PMC followed by 7 digits)."
      }
    })
  
  # first tab: get abstract by id
  output$abstract <-
    renderText({
      if(str_detect(input$pmcid, "\\bPMC\\d{7}\\b")) {
        full_xml() %>% 
          pmc_text() %>% 
          filter(section == "Abstract") %>% 
          pull(text) %>% 
          paste(sep = "", collapse = " ")
      }
    })
  
  # first tab: get DAS information by ID, return an error message if the full XML isn't available
  output$das_tab <-
    renderTable({
      if(str_detect(input$pmcid, "\\bPMC\\d{7}\\b")) {
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
    })
  
  # first tab: get MeSH tags by ID
  output$mesh_tab <-
    renderTable({
      if(str_detect(input$pmcid, "\\bPMC\\d{7}\\b")) {
        get_mesh(input$pmcid) %>% 
          rename(`MesH Terms` = MeSH_terms)
      }
    })
  
  output$metadata <- 
    renderTable({
      if(str_detect(input$pmcid, "\\bPMC\\d{7}\\b")) {
        tidy_metadata(input$pmcid) %>% 
          filter(metadata_field != "Title") %>% 
          rename(
            `Metadata Field` = metadata_field,
            `Value` = value
          )
      }
    })
  
  # second tab: get input dates, filter our dataset by that 
  filtered_by_date <-
    reactive({
      pmcexplore::all_das %>% 
        mutate(
          day = 01,
          date = dmy(paste0(day, month, year))
        ) %>% 
        filter(
          date >= input$date_range[1] %>% ymd() %>% floor_date(unit = "month") &
            date <= input$date_range[2] %>% ymd() %>% ceiling_date(unit = "month") %>% rollback()
        )
    })
  
  # second tab: filter labeled dataset by date
  label_filtered_by_date <-
    reactive({
      pmcexplore::all_labeled_das %>% 
        mutate(
          day = 01,
          date = dmy(paste0(day, month, year))
        ) %>% 
        filter(
          date >= input$date_range[1] %>% ymd() %>% floor_date(unit = "month") &
            date <= input$date_range[2] %>% ymd() %>% ceiling_date(unit = "month") %>% rollback()
        )
    })
  
  # second tab: print the dates chosen
  output$das_title <- 
    renderText({
      paste0(
        "Data sharing plots of documents from ",
        input$date_range[1] %>% ymd() %>% floor_date(unit = "month"),
        " to ",
        input$date_range[2] %>% ymd() %>% ceiling_date(unit = "month") %>% rollback()
      )
    })
  
  # second tab: report how many documents were published in date range
  output$number_docs <- 
    renderText({
      ndocs <- 
        filtered_by_date() %>% 
        select(PMCID) %>% 
        unique() %>% 
        nrow()
      
      paste0("There are ", ndocs, " PMC documents in this date range.")
    })
  
  # second tab: check how many papers have some data sharing within date range
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
  
  # second tab: find tagging methods of papers within date range
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
  
  # second tab: plot categories of labeled dataset
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
  
  # second tab: plot time series of data tagging methods across time, raw count
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
          title = "Data Sharing Methods over Time, Raw Count",
          x = "Date",
          y = "Count",
          color = "Tagging Method Details"
        )
    })
  
  # second tab: plot time series of data tagging methods across time, percent
  output$time_series_percent <- 
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
        mutate(total_papers = n()) %>% 
        ungroup() %>%
        group_by(date, tagging_method) %>% 
        summarize(percentage = n() / total_papers) %>% 
        unique() %>% 
        ggplot(mapping = aes(x = date, y = percentage, color = tagging_method)) +
        geom_point() +
        geom_line() +
        theme_bw() +
        theme(
          legend.position = "bottom"
        ) +
        labs(
          title = "Data Sharing Methods over Time, Percentage",
          x = "Date",
          y = "Percentage",
          color = "Tagging Method Details"
        )
    })
  
  # second tab: display wordcloud of words found in DAS tags of the papers
  wordcloud_rep <- repeatable(wordcloud)
  
  output$wordcloud <- 
    renderPlot({
      word_freq <- 
        filtered_by_date() %>% 
        select(PMCID) %>% 
        left_join(pmcexplore::das_words, by = "PMCID") %>% 
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
  
  # third tab: check if the PMCID is valid
  output$valid_pmcid_anno <-
    renderText({
      if(str_detect(input$pmcid_anno, "\\bPMC\\d{7}\\b")) {
        "This is a valid PMCID."
      } else {
        "Please enter a valid PMCID (PMC followed by 7 digits)."
      }
    })
  
  # third tab: print annotated text, using annotate_text function
  output$annotated_text <-
    renderText({
      if(str_detect(input$pmcid_anno, "\\bPMC\\d{7}\\b")) {
        annotate_text(input$pmcid_anno) %>%
          mutate(
            title = (str_to_title(sentence) == sentence & !str_detect(sentence, "[:punct:]")),
            annotated = if_else(
              is_available & ("avail_check" %in% input$anno_type),
              paste0("<span style = 'background-color: #FFFF00'>", sentence, "</span>"),
              sentence
            ),
            annotated = if_else(
              not_available & ("no_avail_check" %in% input$anno_type),
              paste0("<span style = 'background-color: #00FF00'>", sentence, "</span>"),
              annotated
            ),
            annotated = if_else(
              external_source & ("external_check" %in% input$anno_type),
              paste0("<span style = 'background-color: #00FFFF'>", sentence, "</span>"),
              annotated
            ),
            annotated = if_else(
              supplementary & ("supp_check" %in% input$anno_type),
              paste0("<span style = 'background-color: #FFB6C1'>", sentence, "</span>"),
              annotated
            ),
            annotated = if_else(
              has_das & ("das_check" %in% input$anno_type),
              paste0("<span style = 'background-color: #FFA900'>", sentence, "</span>"),
              annotated
            ),
            annotated = if_else(
              title,
              paste0("<h4>", sentence, "</h4>"),
              annotated
            )
          ) %>%
          pull(annotated) %>%
          paste(collapase = " ")
      }
    })
  
}

# run Shiny App
shinyApp(ui = ui, server = server)