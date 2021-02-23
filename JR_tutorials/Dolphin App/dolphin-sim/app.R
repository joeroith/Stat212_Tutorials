#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
#

library(shiny)
library(tidyverse)
library(knitr)
library(kableExtra)

data <- rep(c("Improved", "Not Improved"), times = c(13, 17))


long_from_2x2 <- function(col1_label, col2_label, row1_label, row2_label,
                          count_r1c1, count_r1c2, count_r2c1, count_r2c2)  {
    Group <- c(rep(row1_label, count_r1c1 + count_r1c2),
               rep(row2_label, count_r2c1 + count_r2c2))
    Result <- c(rep(col1_label, count_r1c1), rep(col2_label, count_r1c2),
                rep(col1_label, count_r2c1), rep(col2_label, count_r2c2))
    longdata <- data.frame(Group, Result)
    return(longdata)  }

orig_data<-long_from_2x2("Improved", "Not Improved","Control", "Dolphin" ,3,12,10,5)


orig_table<-addmargins(table(orig_data$Group,orig_data$Result))



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Randomizing the Dolphin Experiment Outcomes"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel("Hit the RANDOMIZE button to simulate shuffling subjects into the two groups randomly.",
            actionButton("rand_button",
                        "RANDOMIZE", icon = icon("random")
                        )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           h3("The Original Results"),
           htmlOutput("origTable"),
           h3("The Original Stacked Barplot"),
           plotOutput("origPlot", height = "200px", width = "75%"),
           h3("The New Randomized Results"),
           htmlOutput("randTable", width = "50%"),
           h3("Stacked Barplot of New Results"),
           plotOutput("randPlot", height = "200px", width = "75%")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    ## Show the data from the original results
    output$origTable <- renderText({
        kable(orig_table) %>%
            kable_styling(
                font_size = 15,
                bootstrap_options = c("striped", "hover", "condensed")
            ) 
    })
    
    output$origPlot <- renderPlot({
        ggplot(orig_data, aes(x = Group)) +
            geom_bar(aes(fill = Result)) +
            ylim(0,15)
    })
    
    v <- reactiveValues(new_data = NULL, table = NULL)
    
    
    observeEvent(input$rand_button, {
        v$new_data <- sample(data, 15, replace = FALSE)
        v$table <- long_from_2x2("Improved", "Not Improved","Control", "Dolphin",
                                 sum(v$new_data=="Improved"),
                                 15 - sum(v$new_data=="Improved"),
                                 13 - sum(v$new_data=="Improved"),
                                 17 - sum(v$new_data=="Not Improved"))
    })
    
    
    ## Create the new data table from randomized results
    output$randTable <- renderText({
        validate(
            need(nrow(v$table) > 0, message = FALSE)
        )
        kable(addmargins(table(v$table$Group, v$table$Result))) %>% # remove addmargins to get rid of warning
            kable_styling(
                font_size = 15,
                bootstrap_options = c("striped", "hover", "condensed")
            ) 
    })
    
    output$randPlot <- renderPlot({
        validate(
            need(nrow(v$table) > 0, message = FALSE)
        )
        ggplot(v$table, aes(x = Group)) +
            geom_bar(aes(fill = Result)) +
            ylim(0,15)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
