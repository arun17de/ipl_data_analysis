library(shiny)
library(ggplot2)
library(dplyr)

# (data source : ESPN-CricInfo)
deliveries <- read.csv("C:/Users/arunb/Desktop/IPL/deliveries.csv")
matches <- read.csv("C:/Users/arunb/Desktop/IPL/matches.csv")
matches <- matches[matches$result == "normal",]


# color palette for teams (Source : https://www.schemecolor.com)
team_colors <- c("Chennai Super Kings" = "#FFFF3C",   
                 "Delhi Daredevils" = "#00008B",      
                 "Kings XI Punjab" = "#D7D8DA",       
                 "Kolkata Knight Riders" = "#2E0854", 
                 "Mumbai Indians" = "#004BA0",        
                 "Royal Challengers Bangalore" = "#EC1C24",  
                 "Kochi Tuskers Kerala"="#632B72",
                 "Rajasthan Royals"="#254AA5",
                 "Sunrisers Hyderabad" = "#FF822A",
                 "Deccan Chargers"="#366293",
                 "Rising Pune Supergiants"="#D11D9B",
                 "Pune Warriors"="#2F9BE3",
                 "Gujarat Lions"="#E04F16")

# UI
ui <- fluidPage(
  
  # title
  titlePanel("IPL Data Analysis 2008-2016"),
  
  # Sidebar with a select input
  sidebarLayout(
    sidebarPanel(
      selectInput("analysis_type", "Choose Analysis",
                  choices = c("Matches Played in Cities" = "matches_in_cities",
                              "Toss Advantage" = "toss_advantage",
                              "Matches Played by Each Team" = "matches_by_team",
                              "Winning Percentage by Team" = "winning_percentage")),
      hr(),
      helpText("Select an analysis from the dropdown menu to view the corresponding visualization.")
    ),
    
    # Show analysis output
    mainPanel(
      plotOutput("analysis_plot")
    )
  )
)

  # server logic
server <- function(input, output) {
  
  # Matches played in different cities
  matches_in_cities <- reactive({
    ggplot(matches[!is.na(matches$city), ], aes(city, fill = city)) +
      geom_bar() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      ylab("Number of Matches Played") +
      guides(fill = FALSE)
  })
  
  # Toss advantage
  toss_advantage <- reactive({
    matches$toss_match <- ifelse(as.character(matches$toss_winner) == as.character(matches$winner), "Won", "Lost")
    ggplot(matches[!is.na(matches$toss_match), ], aes(toss_match, fill = toss_match)) +
      geom_bar() + 
      xlab("Toss") + 
      ylab("Number of matches won") +
      ggtitle("How much of an advantage is winning the toss")
  })
  
  # Matches played by each team
  matches_by_team <- reactive({
    ggplot(as.data.frame(table(matches$team2) + table(matches$team1)), aes(reorder(Var1, -Freq), Freq, fill = Var1)) +
      geom_bar(stat = "identity") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      xlab("Team") +
      ylab("Number of Matches") +
      scale_fill_manual(values = team_colors) +
      guides(fill = FALSE)
  })
  
  # Winning percentage by team
  winning_percentage <- reactive({
    matches_won <- as.data.frame(table(matches$winner))
    colnames(matches_won)[2] <- "Won"
    matches_played <- as.data.frame(table(matches$team2) + table(matches$team1))
    colnames(matches_played)[2] <- "Played"
    ggplot(left_join(matches_played, matches_won), aes(reorder(Var1, -Won/Played), Won * 100 / Played, fill = Var1)) +
      geom_bar(stat = "identity") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      xlab("Team") +
      ylab("Win Percentage") +
      scale_fill_manual(values = team_colors) +
      guides(fill = FALSE) +
      coord_cartesian(ylim = c(0, 100))
  })
  
  # output for selected analysis
  output$analysis_plot <- renderPlot({
    switch(input$analysis_type,
           "matches_in_cities" = {print(matches_in_cities())},
           "toss_advantage" = {print(toss_advantage())},
           "matches_by_team" = {print(matches_by_team())},
           "winning_percentage" = {print(winning_percentage())}
    )
  })
}

shinyApp(ui = ui, server = server)
