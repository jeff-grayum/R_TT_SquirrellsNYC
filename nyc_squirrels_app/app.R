#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

#variables you can plot
squirrel_variables <- by_hectare %>%
    select(-(hectare:lat)) %>%
    colnames() 

names(squirrel_variables) <- squirrel_variables %>%
    str_replace_all("_", " ") %>% #replaces underscores with spaces!
    str_to_title()
    

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Central Park Squirrels"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("min_squirrels",
                        "Minimum squirrels:",
                        min = 1,
                        max = 30,
                        value = 10),
            selectInput("variable",
                        "Variable:",
                        choices = squirrel_variables)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("park_plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$park_plot <- renderPlot({
        # generate bins based on input$bins from ui.R
        
        var <- sym(input$variable)
        
        by_hectare %>%
            filter(n >= input$min_squirrels) %>%
            ggplot() +
            geom_sf(data = central_park_sf) +
            geom_point(aes(long, lat, size = n, color = !!var)) +
            theme_map() +
            labs(size = "# squirrels",
                 color = "") +
            scale_color_gradient2(low = "blue", high = "red", mid = "pink",
                                  midpoint = 0.3, labels = scales::percent) +
            theme(legend.position = "right") +
            coord_sf(datum = NA)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
