library(shiny)
library(DT)
library(tidyverse)
library(lubridate)

data_cc <- read_csv2("temp.csv")
councilconfigurations_short <- data_cc %>% pull(abbr) %>% unique()
council_fullname_list <- data_cc %>% 
    select(abbr:date) %>% 
    group_by(date, abbr) %>% 
    unique() %>% 
    pull(full_title)


ui <- fluidPage(

    titlePanel("Discover Council Configurations"),

    sidebarLayout(
        sidebarPanel(
            h4("Choose the Council configuration"),
            radioButtons(inputId = 'configurations', 
                         label = NULL, 
                         choices = councilconfigurations_short),
            dateRangeInput(inputId = "daterange", 
                           start = "2020-01-01", 
                           end = "2021-01-01",
                           label = NULL),
            selectInput(inputId = "councillist", 
                        label = NULL,
                        choices = council_fullname_list,
                        )
        ),    
        mainPanel(
           DTOutput('participants')
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    observe({
        cc <- input$configurations
        startdate <- format(input$daterange[1])
        enddate <- format(input$daterange[2])
        select_choices_new <- data_cc %>% 
            filter(abbr == cc) %>%
            filter(date >= startdate, date <= enddate) %>% 
            group_by(date, abbr) %>% 
            unique() %>% 
            pull(full_title)
    
        if (identical(select_choices_new, character(0)))
            select_choices_new <- "No Council meetings with the specified parameters"
        
        updateSelectInput(session, "councillist",
                          choices = select_choices_new
        )
    })
    
    observe({
        output_cc <- data_cc %>% 
            filter(full_title == input$councillist)
        output$participants <- renderDT(output_cc, options = list(pageLength = 35))
        })
}

# Run the application 
shinyApp(ui = ui, server = server)
