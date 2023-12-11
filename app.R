library(shiny)
library(lubridate)
library(ggTimeSeries)
library(sonicscrewdriver)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("UNP Partner Site Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("year",
                        "Year:",
                        min = 2020,
                        max = year(today()),
                        value = year(today()),
                        timeFormat = "%Y",
                        sep = ""),
            selectInput("deployment",
                        "Deployment:",
                        choices = list(
                          "Glasgow-1" = "Glasgow-1",
                          "Glasgow-2" = "Glasgow-2",
                          "Newcastle-1" = "Newcastle-1"
                          ),
                        selected = "Glasgow-1")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("timePlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$timePlot <- renderPlot({
      year <- input$year
      deployment <- input$deployment
      data_raw <- audioblast("standalone","soundscapestats", endpoint="day_counts", source="unp", deployment=deployment, year=year)
      data <- data_raw$days
      
      
      if (!is.null(data)) {
        data <- data[!is.na(data$date),]
        data$date <- ymd(data$date)
        data$present <- rep_len(1, nrow(data))
        data$present<- as.numeric(data$present)
        
        #Check if data$date contains first and last day of year
        #data <- data[order(data$date),]
        if (data$date[1] != paste0(year,"-01-01")) {
          data <- rbind(list(date = paste0(year,"-01-01"), count=NA, duration=0, deployment=deployment, present=NA), data)
        }
        if (data$date[nrow(data)] != paste0(year,"-01-31")) {
          data <- rbind(data, list(date = paste0(year,"-12-31"), count=NA, duration=0,deployment=deployment, present=NA))
        }
        
        
        p1 = ggplot_calendar_heatmap(data, cDateColumnName = "date",
                                     cValueColumnName = "present", vcGroupingColumnNames = c("deployment"),
                                     dayBorderSize = 0.25, dayBorderColour = "white",
                                     monthBorderSize = 0.5, monthBorderColour = "white",
                                     monthBorderLineEnd = "round") +
          scale_fill_continuous(low = 'darkgreen', high = 'darkgreen') +
          theme(legend.position = "none",
                axis.title.x = element_blank(),
                axis.title.y = element_blank()
          )
        
        grid::grid.draw(p1)
      }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)