
library(tidyverse)
library(shiny)

# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel(
        title = div(img(src="example.png", width=175, height=75),"Shiny example")
    ),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("district", "Select district", choices=c("A", "B", "C"), selected = NULL),
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30),
            downloadButton("dl_plot", "Download forecast plot"),
            downloadButton("dl_table", "Download table"),
            h5("Note: Here are some notes.")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("ts_plot"),
           tableOutput("table_out"),
           plotOutput("spc_plot")
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$ts_plot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins

        hist(x, breaks = bins, col = 'darkgray', border = 'white', main=paste0("District ", input$district))
    })
    
    output$table_out <- renderTable({
        
        tt <- data.frame(
            Week = c("2015-01-01", "2015-01-07", "2015-01-21"),
            Predicted = c(27, 21, 29),
            CI_L95 = c(25, 18, 26),
            CI_U95 = c(29, 24, 32)
        )
        tt
        
    })
    
    output$spc_plot <- renderPlot({
        set.seed(123)
        x <- rnorm(input$bins, 25, 3)
        y <- rnorm(input$bins, 3, 0.7)
        ggplot()+
            geom_point(aes(x, y))+
            theme_bw()
    })
    
    output$dl_plot <- downloadHandler(
        filename = function(){
            paste0("plot-", Sys.Date(), ".png") 
            }, # variable with filename
        content = function(file) {

            set.seed(123)
            x <- rnorm(10, 25, 3)
            y <- rnorm(10, 3, 0.7)
            p <- ggplot()+
                geom_point(aes(x, y))+
                theme_bw()+
                title("District ")
            p
            ggsave(file, p)
            
            
        })
    
    output$dl_table <- downloadHandler(
        filename = function() {
            paste("prediction-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            
            tt <- data.frame(
                Week = c("2015-01-01", "2015-01-07", "2015-01-21"),
                Predicted = c(27, 21, 29),
                CI_L95 = c(25, 18, 26),
                CI_U95 = c(29, 24, 32)
            )
            tt
            write.csv(tt, file)
        }
    )

    
}

# Run the application 
shinyApp(ui = ui, server = server)
