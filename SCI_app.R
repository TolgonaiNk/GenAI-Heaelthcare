

library(shinythemes)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(dplyr)
library(glue)
library(lubridate)
library(countrycode)
library(tidytext)
library(wordcloud)
library(RColorBrewer)
library(gganimate)
library(gtrendsR)
library(maps)
library(stringr)
library(viridis)
library(pals)
library(scico)
library(ggrepel)
library(ggdark)
library(sf)
library(tmap)
library(tmaptools)
library(leaflet)
library(reshape2)
library(grid)
library(gridExtra)
library(ggspatial)
library(rio)
library(RColorBrewer)### TODO
library(tm)### TODO
library(textstem)### TODO
library(tidyverse)### TODO
library(DT)

# Set working directory (here modify with your own path)
setwd("/Users/tolgonai/Desktop/Extra/Passed Courses/Strategic and Competitive Intelligence /New SCI/")

# Read the datasets
dataset <- read.csv("Scopus_values.csv", header = TRUE)
my_data <- read.csv("gp-search-20240501-100105.csv", skip = 1, header = TRUE) ### TODO
patents_data <- read.csv("gp-search.csv", sep = ";")
patents_data$state <- substr(patents_data$id, 1, 2)
patents_data$publication_date <- as.Date(patents_data$publication.date, "%d/%m/%Y")
patents_data$country <- countrycode(patents_data$state, "iso2c", "country.name")

data_int <- readRDS("data_int_gtrends.rds")
data_country <- readRDS("data_country.rds")
data_sentiment_total <- readRDS("data_sentiment_total.rds")
rev_tot <- readRDS("reviews_total.rds")
term_freq <- readRDS("term_freq.rds")
term_use <- readRDS("publicity_enterprises.rds")
countries_coord <-  readRDS("countries_coord.rds")

# Define UI
ui <- dashboardPage(
  dashboardHeader(
    title = "GenAI & Healthcare", #tags$span(style = "color:#ffffff; font-size: 24px;", "GenAI & Healthcare"),
    titleWidth = 300
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Patent Information", tabName = "patent", icon = icon("chart-bar")),
      menuItem("Research", tabName = "research", icon = icon("book")),
      menuItem("GenAI Apps", tabName = "trends", icon = icon("line-chart"))
      
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        /* Customize the body background */
        body {
          background-color: #f2f2f2 !important;
        }
        /* Make the sidebar background dark */
        .skin-blue .main-sidebar {
          background-color: #222d32;
        }
        /* Make the sidebar text white */
        .skin-blue .main-sidebar .sidebar-menu li a {
          color: #fff !important;
        }
        /* Make the sidebar icons white */
        .skin-blue .main-sidebar .sidebar-menu li a:hover {
          background-color: #1e282c !important;
        }
        /* Make the header background dark */
        .skin-blue .main-header {
          background-color: #007bff;
        }
        /* Make the header text white */
        .skin-blue .main-header .navbar {
          background-color: #222d32;
        }
        .skin-blue .main-header .navbar .navbar-custom-menu > ul > li > a {
          color: #fff;
        }
        .skin-blue .main-header .navbar .navbar-custom-menu > ul > li > a:hover {
          background-color: #0056b3 !important;
        }
        /* Stylish header titles */
        .content-header h1 {
          font-size: 24px;
          font-weight: 400;
          color: #007bff;
        }
      "))
    ),
    tabItems(
      tabItem(tabName = "patent",
              fluidRow(
                valueBoxOutput("topCompany", width = 6),
                valueBoxOutput("totalCountries", width = 3),
                valueBoxOutput("totalCompanies", width = 3),
                valueBoxOutput("topTechs")
              ),
              fluidRow(
                column(width = 12, plotlyOutput("timePlot"))
              ),
              fluidRow(
                box(width = 6,
                    height = 200,
                       title = "Instructions",
                       tags$div(
                         #style = "position:relative; margin-top:2px;",
                         
                         tags$div(
                           style = "position:absolute; top:3; right:2;",
                           
                           tags$ol(
                             tags$li("Use the 'Select Country' dropdown to choose a specific country to view patents."),
                             tags$li("Adjust the 'Patent Threshold' slider to filter companies based on the number of patents."),
                             tags$li("Click on 'Update View' after making any selections or adjustments."),
                             tags$li(tags$b("Data Source: Google Patent"), style = "list-style-type: none;")
                           )
                         )
                       )
                ),
                box(width = 6,
                    selectInput("selected_country", "Select Country:", choices = unique(patents_data$country)),
                    sliderInput("patent_threshold", "Select Patent Threshold:", min = 0, max = 150, value = 10),
                    actionButton("go", "Update View")
                    )
              ),
              fluidRow(
                
                box(width = 6, plotOutput("wordcloud2"),
                    title = "Top frequent words used in patents related to GenAi and Mental Healthcare"),
                box(
                  width = 6, 
                  plotOutput("company_chart"))
              ),
              fluidRow(
                column(width = 6, plotlyOutput("techPlot")),
                column(width = 6, DTOutput("tech_table")) ### TODO
              )
      ),
      tabItem(tabName = "trends",
              fluidRow(
                column(width = 12,
                       selectInput("keyword", "Select Keyword:", choices = unique(data_sentiment_total$keyword), selected = "woebot"),
                       sliderInput("dateRange", "Select Date Range:", min = as.Date("2019-04-21"), max = as.Date("2024-04-21"), value = c(as.Date("2023-01-01"), as.Date("2024-04-21")), step = 1, animate = TRUE),
                       numericInput("numTerms", "Select the number of search queries terms to display:", value = 10, min = 1, max = 25, step = 1),
                       valueBoxOutput("box")
                )
              ),
              fluidRow(
                box(title = "Search Popularity Index of leading generative AI solutions for mental health care.",
                    width = 12, plotOutput("linePlot"))
              ),
              fluidRow(
                box(title = "Top frequent words related to google search queries",
                    width = 6, plotOutput("termFreqPlot")),
                box(title = "Top frequent positive and negative sentiments related to app's reviews",
                    width = 6, plotOutput("sentimentPlot"))
              ),
              fluidRow(
                box(title = "Popularity search World Map",
                    width = 12, leafletOutput("worldMap"))
              ),
              fluidRow(
               # box(title = "Most important features regarding the promotion and development of apps",
                #    width = 12, plotOutput("wordCloudPlot"))
              )
      ),
      tabItem(tabName = "research",
              fluidRow(
                box(width = 6,
                    sliderInput("top_n", "Number of Top Entries to Display:", min = 5, 
                                   max = 50, value = 10, step = 1)
                ),
                box(width = 6,
                    height = 123,
                    title = "Instructions",
                    tags$div(
                      #style = "position:relative; margin-top:2px;",
                      
                      tags$div(
                        style = "position:absolute; top:3; right:2;",
                        
                        tags$ol(
                          tags$li("Slide the threshold on 'Number of top entries to display' the top researchers based on country (top right) and institutions (botton left)"),
                          tags$li(tags$b("Data Source: Scopus"), style = "list-style-type: none;")
                        )
                      )
                    ))
                       ),
              fluidRow(
                column(width = 6,
                      # h4("Publications Through the Years (Top N)"),
                       plotlyOutput("plot1")
                ),
                column(width = 6,
                      # h4("Publications by Countries (Top N)"),
                       plotlyOutput("plot2")
                )
              ),
              fluidRow(
                box(width = 6,
                    title = glue::glue("Top Publications by Funding Sponsors"),
                     #  h4("Publications by Funding Sponsors (Top N)"),
                       plotlyOutput("plot3")
                ),
                column(width = 6,
                       h4("Keyword Word Cloud"),
                       plotOutput("wordcloud"))
              )
      )      
    )
  )
)

# Server logic
server <- function(input, output) {
  
  #research tab starts
  
  # Plot 1: Publications through the years (interactive line plot)
  output$plot1 <- renderPlotly({
    top_n <- input$top_n
    dataset_top <- dataset %>%
      filter(!is.na(YEAR), !is.na(X.1)) %>%
      arrange(desc(X.1)) %>%
      head(top_n)
    
    p <- ggplot(dataset_top, aes(x = YEAR, y = X.1, group = 1)) +
      geom_line(color = "skyblue") +
      geom_point(color = "lightcoral") +
      labs(title = paste("Publications Through the Years (Top", top_n, ")"),
           x = "Year",
           y = "Number of Publications") +
      theme_minimal()
    
    ggplotly(p)
  })
  # Plot 2: Publications by countries
  output$plot2 <- renderPlotly({
    top_n <- input$top_n
    dataset_top <- dataset %>%
      filter(!is.na(COUNTRY), !is.na(X.10)) %>%
      arrange(desc(X.10)) %>%
      head(top_n)
    p <- ggplot(dataset_top, aes(x = reorder(COUNTRY, -X.10), y = X.10)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      labs(x = "Country",
           y = "Number of Publications") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    ggplotly(p)
  })
  
  # Plot 3: Publications by funding sponsors (horizontal bar chart)
  output$plot3 <- renderPlotly({
    top_n <- input$top_n
    dataset_filtered <- dataset %>%
      filter(!is.na(FUNDING.SPONSOR), (FUNDING.SPONSOR != "Undefined"), !is.na(X.9)) %>%
      arrange(desc(X.9)) %>%
      head(top_n)
    
    p <- ggplot(dataset_filtered, aes(x = X.9, y = reorder(FUNDING.SPONSOR, X.9))) +
      geom_bar(stat = "identity", fill = "lightcoral") +
      labs(x = "Number of Publications",
           y = "Funding Sponsor" ) +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 10))
    ggplotly(p)
  })
  
  # Generate the word cloud
  output$wordcloud <- renderPlot({
    dataset_keyword <- dataset %>%
      select(KEYWORD, X.7) %>%
      na.omit()
    keywords_to_remove <- c("Article", "Review", "Human","Humans", "Artificial Intelligence","AI","Animals","Animal")
    dataset_filtered <- dataset_keyword %>%
      filter(!KEYWORD %in% keywords_to_remove)
    
    # Define a mapping for keywords to merge
    keyword_mapping <- c("Large Language Models" = "Large Language Model", "Generative Artificial Intelligence" = "Generative AI", "Chatbots" = "Chatbot")  # Replace with actual keywords and their new names
    
    # Apply the mapping to merge keywords
    dataset_filtered <- dataset_filtered %>%
      mutate(KEYWORD = ifelse(KEYWORD %in% names(keyword_mapping), keyword_mapping[KEYWORD], KEYWORD)) %>%
      group_by(KEYWORD) %>%
      summarise(X.7 = sum(X.7, na.rm = TRUE))
    
    wordcloud(words = dataset_filtered$KEYWORD, freq = dataset_filtered$X.7, min.freq = 1,
              max.words = 200, random.order = FALSE, rot.per = 0.35,
              colors = brewer.pal(8, "Dark2"))
  })
  #researchtab ends
  
  # Calculate total number of countries
  output$totalCountries <- renderValueBox({
    total_countries <- length(unique(patents_data$country))
    valueBox(total_countries, "Total Countries", icon = icon("globe"), color = "blue")
  })
  
  # Calculate total number of companies
  output$totalCompanies <- renderValueBox({
    total_companies <- patents_data %>% distinct(assignee) %>% nrow()
    valueBox(total_companies, "Total Companies", icon = icon("building"), color = "blue")
  })
  
  # Calculate company with the largest number of patents
  output$topCompany <- renderValueBox({
    top_company <- patents_data %>%
      group_by(assignee) %>%
      summarise(PatentCount = n(), .groups = 'drop') %>%
      arrange(desc(PatentCount)) %>%
      slice(1) %>%
      pull(assignee)
    valueBox(top_company, "Top Company", icon = icon("chart-line"), color = "blue", width = 6)
  })
  
  ### TODO
  # Calculate techs
  my_group <- list( c("amazon","amazon"), 
                    c("verily","google"),
                    c("wing aviation","google"),
                    c("google","google"),
                    c("Deepmind","google"),
                    c("International Business Machines","ibm"),
                    c("microsoft","microsoft"),
                    c("nuance","microsoft"),
                    c("oracle","microsoft")
  )
  
  classify_assignee <- function(x) {
    if(is.null(x)) {
      return("other")
    }
    if(is.na(x)) {
      return("other")
    }
    for( i in my_group ) {
      if( grepl(i[1], x, ignore.case = TRUE) ) {
        return(i[2])
      }
    }
    return("other")
  }
  
  extract_year <- function(text) {
    first_four <- substr(text, 1, 4)
    as.integer(first_four)
  }
  
  my_data <- my_data %>%
    rowwise() %>%
    mutate(group = classify_assignee(assignee), 
           year = extract_year(filing.creation.date))
  
  
  
  
  data_by_year<- my_data[my_data$year >= 2015, ] %>% 
    group_by(year) %>% 
    summarize(count = n()) 
  
  
  # ggplot(data_by_year, aes(x = year, y = count)) +
  #   geom_col(fill="blue")  +
  #   labs(title = "Filling by year", x = "Year", y = "Numbero of filling") 
  ####### END  ######
  
  # Patent Dashboard
  ### TODO
  output$techPlot <- renderPlotly({
    data_by_group <- my_data[my_data$group != "other", ] %>%
      group_by(group,year) %>% 
      summarize(count = n())
    
    a <-   ggplot(na.omit(data_by_group), aes(x = year, y = count, fill = group)) +
      geom_col(position = "stack") +  # Equivalent to geom_bar(position = "stack")
      labs(title = "Big Techs Patent Count", x = "Year",y = "Count of patents")+
      scale_x_continuous(breaks = c(2012:2024))
    
    ggplotly(a) %>%
      layout(hovermode = "closest",
             annotations = list(
               x = 0.5, y = -0.2,  # Adjusting the position to avoid overlapping
               text = "Data source: Google Patents",
               showarrow = F,
               xref = "paper",
               yref = "paper",
               xanchor = "center",
               yanchor = "top",
               font = list(size = 12)
             ))
  })
  ####### END  ######
  output$timePlot <- renderPlotly({
    data <- patents_data %>%
      group_by(Year = year(publication_date), Country = country) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      arrange(Year)
    
    p <- ggplot(data, aes(x = Year, y = Count, color = Country)) +
      geom_line() +
      geom_point() +
      theme_minimal() +
      labs(title = "Publication Dates Over Years", x = "Year", y = "Number of Patents",
           caption = "Data source: Google Patents")
    
    ggplotly(p) %>%
      layout(hovermode = "closest",
             annotations = list(
               x = 0.5, y = -0.2,  # Adjusting the position to avoid overlapping
               text = "Data source: Google Patents",
               showarrow = F,
               xref = "paper",
               yref = "paper",
               xanchor = "center",
               yanchor = "top",
               font = list(size = 12)
             ))
  })
  
  observeEvent(input$go, {
    output$company_chart <- renderPlot({
      filtered_data <- patents_data %>%
        filter(country == input$selected_country) %>%
        group_by(assignee) %>%
        summarise(PatentCount = n(), .groups = 'drop') %>%
        filter(PatentCount >= input$patent_threshold)
      
      ggplot(filtered_data, aes(x = reorder(assignee, -PatentCount), y = PatentCount, fill = assignee)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(title = paste("Patents for", input$selected_country),
             x = "Company", y = "Number of Patents") +
        theme_minimal() +
        scale_fill_viridis_d() +
        theme(legend.position = "none")
    })
  })
  
  # output$wordcloud <- renderPlot({
  #   title_words <- patents_data %>%
  #     select(title) %>%
  #     unnest_tokens(word, title) %>%
  #     anti_join(stop_words) %>%
  #     count(word, sort = TRUE) %>%
  #     #remove words with no impact for question answering
  #     filter(!(word %in% c("method","system","device","based","methods",
  #                          "data","systems","computer","equipment"))) %>%
  #     filter(n > 2)  # Filter for words that appear more than twice
  # 
  #   wordcloud(title_words$word, title_words$n, random.order = FALSE,
  #             colors = brewer.pal(8, "Dark2"), scale = c(3, 0.5),
  #             main = "Wordcloud",
  #             caption = "Data source: Google Patents")
  # })
  output$wordcloud2 <- renderPlot({
    patents_data_usa <- patents_data %>% 
      filter(state == "US")
    corpus <- Corpus(VectorSource(patents_data_usa$title))
    # Preprocess the text
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, removeNumbers)
    corpus <- tm_map(corpus, removeWords, stopwords("english"))
    corpus <- tm_map(corpus, stripWhitespace)
    
    lemmatize_text <- function(text) {
      lemmatized_text <- lemmatize_strings(text)
      return(lemmatized_text)
    }
    corpus <- tm_map(corpus, content_transformer(lemmatize_text))
    
    custom_stopwords <- c(stopwords("english"), 
                          "claim", "claims", "embodiment", "embodiments", "invention", 
                          "patent", "description", "figure", "figures", "system", 
                          "systems", "apparatus", "example", "examples", "include", 
                          "includes", "including", "comprise", "comprises", 
                          "comprising", "provide", "provides", "providing", "may", 
                          "might", "could", "would", "will", "device", "devices", 
                          "unit", "units", "process", "processes", "control", 
                          "controls", "controlled", "operate", "operates", 
                          "operating", "operation", "operations", "one", "two", 
                          "three", "four", "first", "second", "third", "fourth", 
                          "step", "steps", "part", "parts", "portion", "portions", 
                          "element", "elements","datum","image","use", "method")
    
    corpus <- tm_map(corpus, removeWords, custom_stopwords)
    
    # Create a document-term matrix
    dtm <- TermDocumentMatrix(corpus)
    matrix <- as.matrix(dtm)
    words <- sort(rowSums(matrix), decreasing = TRUE)
    df <- data.frame(word = names(words), freq = words)
    df <- head(df, n = 50)
    # Render the word cloud
    #wordcloud2a(df, size = 0.7, shape = 'circle',ellipticity=1)
    wordcloud(df$word, df$freq, random.order = FALSE,
              colors = brewer.pal(8, "Paired"), scale = c(3, 0.5),
              family = "sans", font = 1)
    
  })
  # Add this snippet to your server logic where you render the tech_table
  
  
  ## TODO
  output$tech_table <- renderDT({
    data <- data.frame(
      Company = c("Google", "Microsoft", "IBM", "Amazon"),
      Technology = c("MedPaLM", "Nuance DAX Copilot", "IBM Watson Health (Merative)", "AWS HealthScribe"),
      Tooltip = c(
        "A large language model from Google Research, designed for the medical domain.",
        "DAX works by unobtrusively capturing the dialogue between physician and patient at the point of care. With a patient's consent, DAX records conversations as they happen in the exam room or during virtual interactions",
        "IBM Watson Health, now Merative, offers AI solutions for healthcare analytics.",
        "Transcribe patient visits, generate preliminary clinical notes, and extract insights that help clinicians to quickly revisit highlights of their patient visits and easily accept, reject, or edit content suggestions."
      ),
      URL = c(
        "https://sites.research.google/med-palm/",
        "https://www.nuance.com/healthcare/dragon-ai-clinical-solutions/dax-copilot.html",
        "https://www.merative.com/",
        "https://aws.amazon.com/healthscribe/"
      )
    )
    
    datatable(
      data,
      escape = FALSE,
      options = list(
        dom = 't',  # Hides the search box and entries dropdown
        columnDefs = list(
          list(
            targets = 3,
            render = JS(
              "function(data, type, row, meta) {",
              "return '<a href=\"' + row[3] + '\" title=\"' + row[1] + '\">' + data + '</a>';",
              "}"
            )
          )
        )
      ),
      rownames = FALSE,
      selection = 'none',
      style = "bootstrap",
      class = 'table-hover table-bordered',
      caption = htmltools::tags$caption(
        style = 'caption-side: top; text-align: center; font-size: 20px; font-weight: bold;',
        'Patented Solutions by Big Techs'
      )
    )
  })
  
  
  
  
  
  
  # Google Trends Analysis
  
  filteredDate <- reactive({
    subset(data_int, date >= input$dateRange[1] & date <= input$dateRange[2])
  })
  
  world_key <- reactive({
    countries_coord %>%
      right_join(data_country %>% 
                   filter(keyword %in% input$keyword), 
                 by = c("COUNTRY" = "location")) %>% 
      group_by(longitude, latitude, COUNTRY) %>% 
      summarise(hits = mean(hits))
  })
  
  sent_data <- reactive({
    data_sentiment_total %>%
      filter(keyword %in% input$keyword) %>%
      filter(!(Sentimiento %in% c("negative","positive"))) %>% 
      group_by(Sentimiento, ispositive) %>% 
      summarise(Count = sum(Count)) 
  })
  
  meanRating <- reactive({
    datafin = rev_tot %>% filter(keyword %in% input$keyword) %>% 
      dplyr::select(rating)
    
    round(mean(as.numeric(datafin$rating)),1)
  })
  
  output$box = renderValueBox({
    val = meanRating()
    valueBox(val,"Star(s) rating average",icon = icon("star"))
    
  })
  
  output$linePlot <- renderPlot({
    ggplot(filteredDate(), aes(x = date, y = hits, color = keyword)) + 
      geom_line(size = 0.5) +
      labs(subtitle = paste("Period between", min(filteredDate()$date), "-", max(filteredDate()$date)),
           y = "Search index", x = "",
           caption = "Source: Google Trends") +
      theme_classic() +
      theme(legend.text=element_text(size=12))
  })
  
  output$worldMap <- renderLeaflet({
    leaflet(world_key()) %>%
      addTiles() %>%
      addCircles(lng = ~longitude, lat = ~latitude, weight = 1,
                 radius = ~log(hits) * 200000,
                 popup = ~paste(COUNTRY, ":", hits)) 
  })
  
  output$termFreqPlot <- renderPlot({
    ggplot(head(term_freq, input$numTerms), aes(x = reorder(term, frequency), y = frequency)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      labs(x = "Term", y = "Frequency", 
           caption = "Source: Google Trends") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            axis.text = element_text(size = 12)) +
      coord_flip() 
  })
  
  output$sentimentPlot <- renderPlot({
    print(sent_data())
    ggplot(sent_data(), aes(x = reorder(Sentimiento, -Count), y = Count, fill = ispositive)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      facet_wrap(~ ispositive, scales = "free_x") +  # Facet por positivo y negativo
      labs(x = "Sentiment", y = "Frequency", 
           caption = "Source: App Store reviews data") +
      theme_classic() +
      theme(axis.text = element_text(size = 12)) +
      scale_fill_manual(values = c("Positive" = "skyblue", "Negative" = "lightcoral")) 
  })
  
  
  output$wordCloudPlot <- renderPlot({
    wordcloud(words = term_use$term, freq = term_use$frequency, min.freq = 1,
              max.words = 200, random.order = FALSE, rot.per = 0.35, 
              colors = brewer.pal(8, "Dark2"),
              main = "Wordcloud",
              caption = "Data source: AppStore reviews data")
  })
}

# Run the application
shinyApp(ui = ui, server = server)

