# Load R packages
library(shiny)
library(shinythemes)
library(ggplot2)
library(GGally)

# Define UI. Going to create two panels that will be clickable.
#Allows for navigation. 
# Load libraries and my data
LeagueofLegends <- read.csv("/Users/Ishar/Desktop/School Shit/Data Visualizations/Proj2RealDeal/league/LeagueofLegendsShortest.csv")


# Introduction Page 
intro_panel <- tabPanel(
  "Introduction",
  
  titlePanel("North American League of Legends eSports Stats 2015"),
  
  img(src = "leagueoflegends.jpg", style="display: block; margin-left: auto; margin-right: auto;", 
     height = 400, width = 800),
  br(), br(),    # from style to just before height is the code to center an img.
  
  p("R Shiny Web App for my Data Visualizations course. There are two panels. The introductory
    panel, which you're in right now. And then there is the actual Visualizations panel. Click it
    to be taken to the data page."),
  br(),br(),br(),br(),br(),
  
  p(a(href = "https://www.kaggle.com/chuckephron/leagueoflegends", "Data Source (Kaggle)")) #my data comes from here
)

# Page 2 - Visualization 
select_values <- colnames(LeagueofLegends) # I want these column values in my selection box
select_values <- select_values[! select_values %in% c('Team')] # r I do not want team column in my slection box

  sidebar_content <- sidebarPanel(
  selectInput(
    "y_var",
    label = "Metric To Be Viewed",
    choices = select_values,
    selected = 'SpringWins' #the first column that will show upon loading my selection box
  ),
  
  selectInput("colors",
              "Choose your panel background color",
              c('#F7A8EE', '#D1F7A8', '#AEE8E5', '#F3B6C6', '#E4BCB7'),
              selected = '#AEE8E5'), #the initial background color is set to this.


  selectInput("outline",
              "Choose your bar outline color",
              c('#141111', '#FAFA04', '#F505D6', '#05F519', '#0022FF'),
              selected = '#14111'), #the initial linetype COLOR is set to this.

  selectInput("line_type", 
              "Fancy a specific line type?",
              c("dashed", "twodash", "solid", "dotted", "dotdash", "longdash", "blank"),
              selected = "dashed") #the initial linetype is set to this.

)
main_content <- mainPanel(
  textOutput("selected_var"), #the text reactivity line above the main plot
  plotOutput("plot"), #the bar graphs will be here
)

second_panel <- tabPanel(
  "Visualization",
  titlePanel("What did the 2015 Competitive League of Legends scene look like?"),
  p("Click the box below to open different visualizations."),
  sidebarLayout(
    sidebar_content, main_content
  ), 
  br(), br(), #line breaks to make it pretty.
  p("Teams with an asterisk in their name did not play 36 games. Generally means those teams
    played poorly enough to get relegated(sent to a lower league) and replaced.")
)



# User Interface -----------------------------------------------------
ui <- navbarPage(theme = shinytheme("superhero"), #makes the background all cool.
  "League of Stats",
  intro_panel, #contains side panel.
  second_panel, #contains main panel
  verbatimTextOutput("info"), #contains panel to maintain clickability
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "fonts.css") #font chanegd to Raleway.
  )
)

# Create server -------------------------------------------------------
server <- function(input, output) {
  
  output$selected_var <- renderText({
    paste("You have selected", input$y_var) #reactive text thing. changes based on what they select to view.
  })
  
  output$plot <- renderPlot({
    # sort by Team. Each team should have a different color.
    LeagueofLegends$Team <- factor(LeagueofLegends$Team, 
                                   levels = LeagueofLegends$Team[order(LeagueofLegends$Team)])
    
    my_ggp <- ggplot(data=LeagueofLegends, aes_string(x='Team', y=input$y_var, fill="Team")) +
      geom_bar(stat="identity", width=0.85,  color = input$outline, linetype = input$line_type) + #here's where 2/3 of my reactivity stuff comes in
      labs(x="Organization", y=input$y_var) + coord_flip() #flips orientation of graph 
    
    my_ggp + theme(axis.text = element_text(size = 20))  #changes the readability of the axis text size
    my_ggp + theme(plot.background = element_rect(fill = input$colors)) # change  plot background color
  })
}


# Create Shiny object
shinyApp(ui = ui, server = server)