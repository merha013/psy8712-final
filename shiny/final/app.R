library(shiny)
library(ggplot2)
library(dplyr)
library(rsconnect)

# Define UI for application that draws a histogram
ui <- fluidPage(
  # I made Final the title in the shinyapps account and the URL.
  titlePanel("Final"),
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("Year",
                   "I would like to display:",
                   choices = c("1978", "1988", "1998", "2008", "2018"),
                   selected = c("1978", "1988", "1998", "2008", "2018")),
      checkboxGroupInput("Categories",
                   "I would like to display:",
                   choices = c("Major Companies", "Organized Religion",
                               "Education", "Press", "Medicine", "Congress", 
                               "Scientific Community", "Military"),
                   selected = c("Major Companies", "Organized Religion",
                                "Education", "Press", "Medicine", "Congress", 
                                "Scientific Community", "Military")),
      radioButtons("Color",
                   "I would like to display:",
                   choices = c("Bold Colors", "Boring Colors",
                               "Fire", "Ice")),  
      radioButtons("Pivot",
                   "I would like to rotate the data:",
                   choices = c("Yes", "No")),
      selectInput("Theme",
                  "Theme type:",
                  choices = c("Dark", "Classic", "Black and White", "Minimal"),
                  selected = c("Dark", "Classic", "Black and White", "Minimal"))
    ),
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("finalplot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$finalplot <- renderPlot({  # Render finalplot
    skinny_tbl <- readRDS("skinny.rds")
    
    filter_years <- if ("All" %in% input$Year) 
      unique(skinny_tbl$YEAR) else input$Year
    filter_categories <- if ("All" %in% input$Categories) 
      unique(skinny_tbl$Category) else input$Categories
    
    skinny_tbl %>%
      filter(YEAR %in% filter_years & Category %in% filter_categories) %>%
      ggplot(aes(x=factor(YEAR), 
                 y=Average_Confidence_Level,
                 fill = Category)) +
      geom_col(position = "dodge") +
      labs(x = "Year",
           y = "Average Confidence Level",
           fill = "Category") +
      ggtitle("Average Confidence Levels Across Organizations by Year") +
      scale_x_discrete(labels = function(x) as.character(x)) +
      scale_y_continuous(limits = c(NA, 3),
                         breaks = c(1, 2, 3),
                         labels = c("1" = "Hardly Any",
                                    "2" = "Only Some",
                                    "3" = "A Great Deal")) +
      scale_fill_brewer("Category", 
                        palette = if(input$Color=="Bold Colors"){"Dark2"}
                        else if (input$Color=="Boring Colors"){"Set3"}
                        else if (input$Color=="Fire"){"OrRd"}
                        else {"PuBu"}) +
      (if(input$Theme=="Classic"){theme_classic()}
       else if(input$Theme=="Black and White"){theme_bw()}
       else if(input$Theme=="Minimal"){theme_minimal()}
       else if(input$Theme=="Dark"){theme_dark()}) +
      if(input$Pivot=="Yes"){coord_flip()}else{}
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
