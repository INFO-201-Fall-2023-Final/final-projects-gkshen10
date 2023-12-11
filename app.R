#Albert Cheng & Gabe Shen
#INFO201 B
library(shiny)
library(shinyWidgets)
library(ggplot2)
library(shinySearchbar)
library(dplyr)
library(plotly)

data <- read.csv("Final Project - Cleaned Dataset.csv")

introPageUI <- fluidPage(
  tags$head(
    tags$style(HTML("
            body {
                background-color: #2F3D4E;
                color: #EBEBEB;
            }
            .about-section, .use-cases-section, .preview-section, .sidebar, .main {
                font-family: Arial, sans-serif;
                padding: 20px;
                background-color: #515D6B;
                margin-bottom: 10px;
                border-radius: 10px;
            }
            .use-cases-section {
                text-align: left;
            }
            h2, h3, h4, p {
                font-size: 20px;
                margin-bottom: 5px;
            }
            .main-container {
                display: grid;
                grid-template-columns: 1fr 1fr;
                grid-gap: 20px;
            }
            .full-width {
                grid-column: 1 / -1;
            }
            .preview-table {
                font-size: 14px;
            }
        "))
  ),
  titlePanel(tags$h1("Intro Page", class = "title-panel-custom"), windowTitle = "Intro Page"),
  div(class = "main-container",
      div(class = "about-section",
          h1("About"),
          p("This is a comprehensive data on DUIs, alcohol-related fatalities, and legislation across U.S. states, with age and gender-specific statistics."),
          h3("Data creation range: 2012 & 2014"),
          p("Created by: Houan Cheng & Gabe Shen"),
          p("Content: CSV files"),
          p("Source:", a(href="https://catalog.data.gov/dataset/impaired-driving-death-rate-by-age-and-gender-2012-2014-all-states", "catalog.data.gov"), " and ", a(href="https://www.kaggle.com/datasets/bryanmaloney/dui-arrests-and-population-by-state-2015-usa", "kaggle.com")),
          h1("Mapping DUI"),
          p("Our project, DUI and Car Accidents across the states, is dedicated to presenting a comprehensive view of the impact of driving under the influence (DUI) across the United States. In a nation where the car is an emblem of freedom and mobility, the shadow of DUI looms large, impacting countless lives and shaping public policy. Our goal is to bring this issue into sharper focus through a detailed, interactive map that vividly illustrates DUI-related statistics across all fifty states.
          The cornerstone of this initiative is a robust collection and analysis of data, drawing from a variety of reputable sources. We aim to gather and analyze comprehensive data sets, not merely to quantify DUI incidents but to understand them in the context of broader road safety issues. This includes examining the severity of accidents, comparing population to accidents ratio, temporal patterns of incidents, and possibly the effectiveness of local DUI laws and enforcement practices.")
      ),
      div(class = "use-cases-section",
          h1("Use Cases"),
          p("1 Assessing Regional Variations in DUI Rates"),
          p("2 Identifying Demographic Patterns in DUI Offenses"),
          p("3 Evaluating the Effectiveness of Legislation"),
          p("4 Analyzing Trends in Alcohol-Related Fatalities"),
          p("5 Studying the Impact of Alcohol Accessibility"),
          p("6 Developing Targeted Public Safety Campaigns"),
          p("7 Policy Formulation and Advocacy"),
          p("8 Academic and Health Research"),
          p("9 Community Awareness and Education Programs.")
      )
  ),
  img(src = "https://www.shutterstock.com/image-photo/driving-under-alcohol-influence-drunk-600nw-538729837.jpg"),
  img(src = "https://www.hellandlawgroup.com/media/dynamic/heroes/122_Washington%20State%20Car%20Accident%20Laws.jpeg", width = "80%", height = "80%"),  
  
)
#------------------------------------------------page 2----------------------------------------
page2UI <- fluidPage(
  tags$head(
    tags$style(HTML("
      .well {
        background-color: #515D6B;
        border: none;
      }
      .selectize-input, .selectize-control.single .selectize-input.input-active {
        background-color: #FFFFFF;
        color: #000000;
        border-radius: 4px;
      }
      .selectize-dropdown, .selectize-dropdown-content {
        background-color: #FFFFFF;
        color: #000000;
      }
      .shiny-text-output {
        color: #EBEBEB;
      }
      .sidebar {
        background-color: #515D6B;
        padding: 20px;
        border-radius: 10px;
        color: #EBEBEB;
      }
      #analysisText {
        color: #EBEBEB;
      }
      .instruction-heading {
        color: #EBEBEB;
        font-size: 24px;
      }
      .instruction-text {
        color: #EBEBEB;
        font-size: 18px;
      }
    "))
  ),
  titlePanel(tags$h1("DUI Analysis by State and Year", class = "title-panel-custom"), windowTitle = "DUI Analysis by State and Year"),
  
  sidebarLayout(
    sidebarPanel(
      tags$h1("How to use this page", class = "instruction-heading"),
      tags$p("Use the pull down menus to select a state and a year. The bar graph shown will give a breakdown of the death rate due to DUIs for the chosen state and the chosen year, for the following age groups:", class = "instruction-text"),
      tags$ul(
        tags$li("Age below 20"),
        tags$li("Age 21 to 34"),
        tags$li("Age 35 and above"),
        style = "list-style-type: none; padding-left: 0;"
      ),
      
      selectInput(
        inputId = "stateInput",
        label = "Select a state",
        choices = unique(data$State)
      ),
      selectInput(
        inputId = "yearInput",
        label = "Select a year",
        choices = c("2012", "2014")
      ),
      tags$h1("General Analysis", class = "instruction-heading"),
      tags$div(id = "analysisText", HTML("<p>Unsurprisingly, we see in most states that the highest rate of fatality occurs for people in the 21-35 year age bracket. This is likely due to the fact that 21 is the drinking age in all US states. It is around this age that most Americans begin to drink, and so they often have little to no understanding of their bodies ability to handle alcohol. Additionally, people in this age bracket with driving licenses have only been driving for a few years and are thus less confident in their driving abilities which also leads to a higher rate of fatalities.</p>"), class = "instruction-text") # Custom class for text
    ),
    mainPanel(
      plotOutput("duiBarChart", height = "400px"),
      style = "background-color: transparent; margin-top: 30px;"
    )
  )
)

#---------------------------------------------page3---------------------------------------------------
page_3 <- fluidPage(
  titlePanel(tags$h1("Rate of Deaths due to Driving Under the Influence and State Policies", class = "title-panel-custom"), windowTitle = "Rate of Deaths due to Driving Under the Influence and State Policies"),
  sidebarLayout(
    sidebarPanel(
      h1("How to use this page"),
      p("Select a region from the pull down menu to view death rate per 100,000 people due to driving under the influence (for years 2012 and 2014),
        as well as information on significant state policies."),
      selectInput(
        inputId = "region",
        label = "Select a region",
        choices = data$Region
      ),
      h1("What additional factors, such as state policies, increase risk for DUI?"),
      p("Interestingly, there appears to be NO correlation whatsoever between states with harsher drug and alcohol policies
      and those with higher rates of alcohol related fatalities. While this may seem surprising at first, upon further
      consideration, it actually goes to show that access to drugs and alcohol does not necessarily cause people to use
      controlled substances more often and in less safe ways. However, we also did not find better results for states that
      had more lenient alcohol and drug laws. This shows that most people will make a choice of whether or not to drive
      safely regardless of the laws in their state.")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Table", tableOutput(outputId = "table"))
      ),
      searchbar("sb", "table"),
      textOutput("text")
    )
  ),
)

page4UI <- fluidPage(
  titlePanel(tags$h1("States Ranked, Amount of DUIs and Fatalities", class = "title-panel-custom"), windowTitle = "States Ranked, Amount of DUIs and Fatalities"),
  tags$head(
    tags$style(HTML("
      .big-label .control-label {
        font-size: 20px;
      }
    "))
  ),
  sidebarLayout(
    sidebarPanel(
      div(class = "big-label",
          radioButtons("dataChoice", "Choose Data Type:",
                       choices = c("Fatalities" = "Fatalities",
                                   "DUI" = "DUI"))
      ),
      checkboxInput("topTen", "Show Only Top 10 States", FALSE),
      tags$p("Hover your mouse above any state too see how it is ranked among the other states, 1 being the worst state with most DUI records, or DUI related fatalities. 50 being the best state in this regard.", style = "font-size: 18px;"),
      tags$h1("What can we interpret from this visualization?"),
      tags$p("Even after taking into consideration the wide array of population sizes across the United States and comparing their deaths per capita,
             we can deduce from the data that Texas, California, and Florida in particular all have high fatality rates due to driving under the influence.
             These are the three biggest states in the union, however their amount of deaths is disproportionate even after accounting for size comparison.")
    ),
    mainPanel(
      plotlyOutput("stateBarChart", height = "800px")
    )
  )
)

ui <- navbarPage("Exploring the Impact of Alcohol Policies on Public Health and Safety Across the United States",
                 tabPanel("Introduction", introPageUI),
                 tabPanel("DUI Analysis", page2UI),
                 tabPanel("State Policies", page_3),
                 tabPanel("Every State, Ranked", page4UI),
)

server <- function(input, output, session) {
  
  
  output$duiBarChart <- renderPlot({
    filteredData <- subset(data, State == input$stateInput)
    
    yearSuffix <- ifelse(input$yearInput == "2012", "..2012", "..2014")
    ageGroups <- c("Ages.0.20", "Ages.21.34", "Ages.35.", "All.Ages")
    
    plotData <- data.frame(
      AgeGroup = ageGroups,
      DeathRate = rep(0, length(ageGroups)),
      Highest = rep(FALSE, length(ageGroups))
    )
    
    for (ageGroup in ageGroups) {
      rate <- filteredData[[paste0(ageGroup, yearSuffix)]]
      if (!is.na(rate)) {
        plotData$DeathRate[plotData$AgeGroup == ageGroup] <- rate
      }
    }
    
    highestRate <- max(plotData$DeathRate, na.rm = TRUE)
    plotData$Color <- ifelse(plotData$DeathRate == highestRate, "Highest Death Rate", "Other Age Groups")
    plotData$FillColor <- ifelse(plotData$Color == "Highest Death Rate", "darkred", "#244D87")
    
    
    gg = ggplot(plotData, aes(x = AgeGroup, y = DeathRate, fill = Color)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("Highest Death Rate" = "darkred", "Other Age Groups" = "#244D87")) +
      labs(x = "Age Group", y = "Death Rate per 100,000 population", fill = "Age Group Category") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
            axis.text.y = element_text(size = 14),
            axis.title = element_text(size = 16),
            legend.position = "right")
    
    gg
  })
  
  output$table <- renderTable({
    data <- filter(data, State != "United States")
    data_pt <- filter(data, Region == input$region)
    data_pt <- select(data_pt, c(State, All.Ages..2012, All.Ages..2014, Sunday.Sales, Types.Of.Alcohol.Purchasable.In.Grocery.Stores, Marijuana.Legalized))
    return(data_pt)
  })
  
  #--------------------------------------------page 4---------------------------------------------------
  
  output$stateBarChart <- renderPlotly({
    dataColumn <- if(input$dataChoice == "Fatalities") "Fatalities" else "DUI"
    
    sortedData <- data %>%
      arrange(desc(!!sym(dataColumn))) %>%
      mutate(Rank = paste("The current state is ranked:", row_number()))
    
    if(input$topTen) {
      sortedData <- head(sortedData, 10)
    }
    
    p <- ggplot(sortedData, aes_string(x = "State", y = dataColumn, text = "Rank")) +
      geom_bar(stat = "identity") +
      labs(x = "State", y = if(input$dataChoice == "Fatalities") "Number of Fatalities" else "Number of DUI") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = "text") %>%
      layout(hoverlabel = list(bgcolor = "white"))
  })
}

shinyApp(ui = ui, server = server)