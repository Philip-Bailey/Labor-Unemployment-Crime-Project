library(shiny)
library(DT)
library(dplyr)
library(ggplot2)

setwd("X:/Coding/Rstudio/R Projects/Data 490 Model Building/Documentation")




ui <- fluidPage(
  titlePanel("Labor, Crime, Unemployment Project"),
  tabsetPanel(
    tabPanel(
      "Overview",
      sidebarLayout(
        sidebarPanel(
          selectInput("dataset", "Choose a dataset:",
                      choices = c("New York", "Maryland", "Washington")),
          numericInput("obs", "Number of observations to view:", 10),
          helpText("These are the raw datasets used for my analysis,
                   there are lots of extra columns provided with the data which
                   may make it difficult to understand."),
          actionButton("update", "Update View"),
          uiOutput("space"),
          uiOutput("Projectquestions"),
          uiOutput("Analysis"),
          uiOutput("Datasources")
          
        ),
        mainPanel(
          h4("Summary"),
          verbatimTextOutput("summary"),
          h4("Observations"),
          tableOutput("view")
          
        )
      )
    ),
    tabPanel(
      "Visualizations",
      sidebarLayout(
        sidebarPanel(
          
          uiOutput("section_label"),
          sliderInput("population_limit_scatter", "County Population Limit",
                      min = 0, max = 1062595, value = c(345000, 1062595), step = 1000),
          selectInput("crime_type", "Select Crime Type", choices = c("MURDER", "RAPE", "ROBBERY", "AGG..ASSAULT", "B...E", "LARCENY.THEFT", "M.V.THEFT", "GRAND.TOTAL", "VIOLENT.CRIME.TOTAL"), selected = "GRAND.TOTAL"),
          dateRangeInput("date_range", "Select Date Range", format = "yyyy", startview = "year", language = "en", start = "1975"),
          
          
          uiOutput("section_label2"),
          sliderInput("population_limit_scatter2", "County Population Limit",
                      min = 0, max = 2646816, value = c(345000, 2646816), step = 1000),
          selectInput("crime_type2", "Select Crime Type", choices = c("Index_Count", "Violent.Count", "Property.Count", "Firearm.Count"), selected = "Index_Count"),
          dateRangeInput("date_range2", "Select Date Range", format = "yyyy", startview = "year", language = "en", start = "1990")
        ),
        mainPanel(
          plotOutput("new_scatterplot"),
          plotOutput("mscatterplot")
        )
      ),
      plotOutput("scatterplot"),
      DTOutput("combined_data_table"),

    ),
    tabPanel("Graphics",
             uiOutput("graphic_label1"),
             uiOutput("graphic4"),
             uiOutput("graphic_label2"),
             uiOutput("graphic3"),
             uiOutput("graphic2"),
             uiOutput("graphic_label3"),
             uiOutput("graphic1"),
             
             uiOutput("graphic")
             )
  )
)

server <- function(input, output, session) {
  
  output$combined_data_table <- renderDT({
    datatable(
      combined_data,
      options = list(pageLength = 5, selection = 'single'),
      style = "bootstrap",
      class = "display"
    )
  })
  
  
  datasetInput <- eventReactive(input$update, {
    switch(input$dataset,
           "New York" = yearly_crime_n_raw,
           "Maryland" = yearly_crime_m_raw,
           "Washington" = yearly_crime_w_raw)
  }, ignoreNULL = FALSE)
  
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  output$view <- renderTable({
    head(datasetInput(), n = isolate(input$obs))
  })
  
  output$overview_table <- renderDT({
    datatable(
      datasetInput()[, c("Year", "Population", "Index_Count", "Unemployment", "Labor")],
      options = list(pageLength = 5),
      selection = 'single'
    )
  })
  

  output$new_scatterplot <- renderPlot({
    filtered_data <- yearly_crime_m_raw %>%
      mutate(Year = as.Date(paste0(Year, "-01-01"))) %>%
      filter(Year >= input$date_range[1] & Year <= input$date_range[2],
             POPULATION >= input$population_limit_scatter[1] & POPULATION <= input$population_limit_scatter[2])
    
    ggplot(filtered_data, aes(x = Year, y = .data[[input$crime_type]], color = JURISDICTION)) +
      geom_point() +
      labs(title = "Maryland", x = "Year", y = input$crime_type)
  })
  
  output$mscatterplot <- renderPlot({
    n_data <- yearly_crime_n_raw %>%
      mutate(Year = as.Date(paste0(Year, "-01-01"))) %>%
      filter(Year >= input$date_range2[1] & Year <= input$date_range2[2],
             Population >= input$population_limit_scatter2[1] & Population <= input$population_limit_scatter2[2])
    
    ggplot(n_data, aes(x = Year, y = .data[[input$crime_type2]], color = County)) +
      geom_point() +
      labs(title = "New York", x = "Year", y = input$crime_type2)
  })
  
  
  output$scatterplot <- renderPlot({
    options(scipen = 999)
    
    base_plot <- ggplot(combined_data, aes(x = Year, y = yearly_crime, color = state)) +
      geom_point() +
      labs(title = 'Yearly Crime in Washington, Maryland, and New York', x = "Year", y = 'Yearly Crime Count')
    
    # Highlight selected rows using red points
    selected_row <- input$combined_data_table_rows_selected  # Use the correct table name
    if (length(selected_row) > 0) {
      highlighted_points <- geom_point(data = combined_data[selected_row, , drop = FALSE], 
                                       aes(x = Year, y = yearly_crime, color = state), 
                                       size = 3)
      base_plot + highlighted_points
    } else {
      base_plot
    }
  })
  
  output$graphic4 <- renderUI({
    # Use the img HTML tag to display the image
    img(src = "marylandpca.png", width = 800, height = 800)
  })
  
  output$graphic3 <- renderUI({
    # Use the img HTML tag to display the images
    tags$div(
      style = "display: flex; justify-content: space-between;",
      img(src = "noagg.png", width = 400, height = 150),
      img(src = "agg.png", width = 400, height = 150)
    )
  })
  

  output$graphic1 <- renderUI({
    # Use the img HTML tag to display the images
    tags$div(
      style = "display: flex; justify-content: space-between;",
      img(src = "your_plot2.png", width = 600, height = 400),
      img(src = "your_plot3.png", width = 600, height = 400)
    )
  })

  

  output$section_label <- renderUI({
   label <- "Section: <span style='font-size: 18px; font-weight: bold; color: green;'>Maryland</span>"
    HTML(label)
  })
  
  output$section_label2 <- renderUI({
   label <- "Section: <span style='font-size: 18px; font-weight: bold; color: blue;'>New York</span>"
    HTML(label)
  })
  
  output$graphic_label1 <- renderUI({
    label <- "<div style='line-height: .8; font-size: 11px;'>
    
    <span style='font-size: 18px; font-weight: bold; color: black;'>Maryland PCA Model Results</span>
    <br> <br>
    <br> In this machine learning model I tested to see if certain crimes could be predicted better than others.<br>
    <br>I used Unemployment, labor force percentage and population as my independent variables. My dependents were <br>
    <br> MURDER, ROBBERY, AGG..ASSAULT, B...E, LARCENY.THEFT, M.V.THEFT, GRAND.TOTAL, VIOLENT.CRIME.TOTAL. The graphic <br>
    <br> below displays the model with the highest r score and lowest r score. My control was Grand.total which was a <br>
    <br> summation of all crimes. In the end the only dependent to have a higher r score than the control was Larceny theft.<br>
    <br> Robbery had the worst score. These scores represent the models ability to explain the variablity of the <br>
    <br> data. In the end this helped me to determine that there might be some predictive power in economic variables<br>
    <br> like Unemployment and Labor for certain crimes. <br>
    <br> <br>
    </div>"
    HTML(label)
  })
  
  output$graphic_label2 <- renderUI({
    label <- "<div style= 'line-height: .8; font-size: 11px;'>
    <span style='font-size: 18px; font-weight: bold; color: black;'> New York State Crime Aggregation Results </span>
    <br> <br>
    <br> Another idea that I tested for this project was the affect that aggregating crime data would have on<br>
    <br> predicting crime. For this test I used a model built off of New York state crime data. One aggregated <br>
    <br> by year and another with all the raw county data. The models were tested on the same data set to see if <br>
    <br> there would be a difference in there ability to explain variblity. In the end the model trained on <br>
    <br> on aggregated data did the best. Though both models still explained less than half of the variability in <br>
    <br> crime across the state<br>
    <br> <br>
    </div>"
    HTML(label)
  })

  
  output$graphic_label3 <- renderUI({
    label <- "<div style= 'line-height: .8; font-size: 11px;'>
    <span style='font-size: 18px; font-weight: bold; color: black;'>Scatterplots of Economic Factors vs Crime </span>
    <br> <br>
    <br> <br>
    </div>"
    
    
    HTML(label)
  })
  
  output$Projectquestions <- renderUI({
    label <- "<div style= 'line-height: 1; font-size: 11px;'>
    <span style='font-size: 18px; font-weight: bold; color: black;'>Project Questions </span>
    <br> <br>
    For my project I came at it with one main question Can you predict future crime with Unemployment
     and Labor Force Participation? In this section I cover some questions that spun out of this
    and why they helped me answer my project question. The first questions I explored was how did
     population effect a models ability to predict crime. Another question I examined for my was project
     was the proficency of a model to predict different crimes. My thought process was maybe if these
     economic factors can't predict overall crime with accuracy they could predict certain types of crime.
     And my last question that I examined with machine learning and regressions was the ability of a
     model to predict crime if the county crime data was aggregated rather then not. I wanted to make 
     sure I was giving every chance for Unemployment and Labor to predict crime.<br>
    <br> <br>
    </div>"
    
  
    HTML(label)
  })
    output$Analysis <- renderUI({
      label <- "<div style= 'line-height: 1; font-size: 11px;'>
    <span style='font-size: 18px; font-weight: bold; color: black;'> Analysis Methods </span>
    <br> <br>
      The first question I analzed with a PCA model and used the different crimes as my 
      dependant varibles. I found that the only crime that had a higher r score than  
      overall crime was Larceny Theft. R score represents the models ability to explain 
      the variblity in the data thus providing more accurate predictions. This helped me  
      come to the conclusion that theft can be predicted by unemployment and labor more  
      accuratly than other times of crime. Which makes sense because people who are suffering 
      from a bad economy would be more likely to turn to less than legal methods to get money. 
      Rather than if the economy is going well and there are more jobs.  
      I analyzed my question about the effects of population by spliting New York state 
      into four population groups then using their related crime data to train and test a model. 
      My method of analysis was using a poisson regression model and comparing AIC which 
      is a measure of goodness of fit. The lower the score the better the model fit  
      I found that population groups that had more people has less accurate predictions. This  
      told me that predicting crime with unemployment and labor would be more effective in 
      states with a smaller population. My last question I analyzed with models and machine 
      learning was a models ability to predict crime with economic varibles based on  
      aggregation of data. For this I used a negative binomial regression model and  
      again used AIC as my way to measure accuracy. I found that aggregating data  
      provided a lower AIC thus showing me that aggregating data was a more accurate method 
      of predicting crime.
    <br> <br>
    </div>"
      
      
      HTML(label)
    })  
    
    
    output$Datasources <- renderUI({
      label <- "<div style='line-height: .8; font-size: 11px;'>
              <span style='font-size: 18px; font-weight: bold; color: black;'> Data Sources </span>
              <br> <br>
              <a href='https://fred.stlouisfed.org/series/MDUR' target='_blank'>Maryland Unemployment Rate</a>
              <br> <br>
              <a href='https://fred.stlouisfed.org/series/LBSSA24' target='_blank'>Maryland Labor Force Participation</a>
              <br> <br>
              <a href='https://catalog.data.gov/dataset/violent-crime-property-crime-by-county-1975-to-present' target='_blank'>Maryland Crime Data</a>
              <br> <br>
              <a href='https://fred.stlouisfed.org/series/NYUR' target='_blank'>New York Unemployment Rate</a>
              <br> <br>
              <a href='https://fred.stlouisfed.org/series/LBSSA36' target='_blank'>New York Labor Force Participation</a>
              <br> <br>
              <a href='https://catalog.data.gov/dataset/index-violent-property-and-firearm-rates-by-county-begin' target='_blank'>New York Crime Data</a>
              <br> <br>
              <a href='https://fred.stlouisfed.org/series/WAUR' target='_blank'>Washington Unemployment Rate</a>
              <br> <br>
              <a href='https://fred.stlouisfed.org/series/LBSSA53' target='_blank'>Washington Labor Force Participation</a>
              <br> <br>
              <a href='https://catalog.data.gov/dataset/washington-state-criminal-justice-data-book-6c019' target='_blank'>Washington Crime Data</a>
              <br> <br>
            </div>"
      HTML(label)
    })
  
      output$space <- renderUI({
        label <- "<div style= 'line-height: .8; font-size: 11px;'>
    <br> <br>
    </div>"
        
        
        HTML(label)
      })

  
}

# Run the application
shinyApp(ui = ui, server = server)
