library(shiny)
library(lubridate)
library(ggTimeSeries)
library(sonicscrewdriver)


data_raw <- audioblast("standalone","soundscapestats", endpoint="day_counts", source="unp")
full_data <- data_raw$days
full_data <- full_data[!is.na(full_data$date),]
full_data <- full_data[!is.na(full_data$deployment),]
full_data <- full_data[year(full_data$date) > 2000,]
full_data$date <- ymd(full_data$date)

choices <- sort(unique(full_data$deployment))
names(choices) <- choices

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("UNP Partner Site Data"),

    # Sidebar with a slider input for number of bins 
    fluidRow(
        column(
          width=12,
            column(
              width = 6,
              selectInput("deployment",
                          "Deployment:",
                          choices = choices,
                          selected = "Glasgow-1")
            ),
            column(
              width = 6,
          
              sliderInput("year",
                          "Year:",
                          min = min(year(full_data$date)),
                          max = year(today()),
                          value = year(today()),
                          timeFormat = "%Y",
                          sep = "",
                          ticks=FALSE),
            )
        ),

        # Show a plot of the generated distribution
        column(
            width = 12,
           plotOutput("timePlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$timePlot <- renderPlot({
      year <- input$year
      deployment <- input$deployment
      
      data <- full_data[full_data$deployment == deployment,]
      data <- data[year(data$date) == year,]
      data$present <- rep_len(1, nrow(data))
      data$present<- as.numeric(data$present)
      
      if (!nrow(data) == 0) {
        
        
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
