#
# This is a Shiny web application. You can run the application by clicking
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(tidytuesdayR)
library(scales)
library(broom)  
library(knitr)
theme_set(theme_light())

tt <- tt_load("2020-07-07")
coffee_rating <- tt$coffee_ratings

coffee_rating <- coffee_rating %>% 
    filter(total_cup_points != 0)


ui <- dashboardPage(
    dashboardHeader(
        title = "Coffee Dashboard"
    ),
    dashboardSidebar(
        menuItem("Coffee Varieties",
                 tabName = "coffee_tab",
                 icon = icon("snowflake")
                 ),
        tabItem(
            tabName = "coffe_tab",
            selectInput("v_country", label = "Country of Origin", choices = c("India", "Ethiopia", "Canada", "Mexico")),
            sliderInput("v_coffee_points", label = "Cupper Points", min = 1, max = 10, value = 5))
        
    ),
    dashboardBody(
        
        
        fluidRow(box(plotOutput("coffee_histogram")),
                 box(plotOutput("coffee_ratings"))),
        fluidRow(box(plotOutput("coffee_scatter")),
                 box(tableOutput("coffee_table")))
    )
    
)


server <- function(input, output) {
    output$coffee_histogram <- renderPlot({
        coffee_rating %>%
            ggplot(aes(cupper_points)) +
            geom_histogram()
    })
    
    output$coffee_ratings <- renderPlot({
        coffee_rating %>%
            filter(country_of_origin == "Ethiopia") %>%
            select(owner, country_of_origin, species, aroma:moisture) %>%
            pivot_longer(aroma:moisture,
                         names_to = "characterstic",
                         values_to = "value") %>%
            mutate(characterstic = str_to_title(characterstic)) %>%
            group_by(characterstic) %>%
            summarise(mean_value = mean(value)) %>%
            ggplot(aes(fct_reorder(characterstic, mean_value), mean_value)) +
            geom_col(aes(fill = characterstic), show.legend = F) +
            coord_flip()
    })
    
    output$coffee_scatter <- renderPlot({
        coffee_rating %>%
            #  filter(country_of_origin == "Ethiopia") %>%
            select(cupper_points, aroma:moisture) %>%
            pivot_longer(aroma:moisture,
                         names_to = "characterstic",
                         values_to = "value") %>%
            ggplot(aes(value, cupper_points)) +
            geom_point() +
            facet_wrap( ~ characterstic, scales = "free")
    })
    
    output$coffee_table <- function() {
        coffee_rating %>%
            count(country_of_origin, company, sort = T) %>%
            drop_na() %>%
            head(10) %>% 
            kable()
    }
}
    
shinyApp(ui, server)
