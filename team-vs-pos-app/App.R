# Set directory ----
setwd("C:/Users/David/OneDrive/Desktop/dataClass/04-ff-analysis/02-szn-long/2023/02-main-analysis")

# Load Shiny & other packages ----
library(shiny)
library(tidyverse)
library(ggimage)
library(scales)
library(nflreadr)
library(nflfastR)
library(gt)
library(gtExtras)
library(DT)
# library(rsconnect)

# Source helpers ----
source("helpers.R")
# source("team-vs-pos-app/helpers.R")

# Load data ----
playerScores <- read.csv("data/8. Player Scores.csv")
playerFinishes <- read.csv("data/9. Player Finishes.csv")
schedule <- read.csv("data/11. NFL Schedule.csv")

# Eddy Pineiro Week 17 Manual Adjustment
playerScores[3315, 3] <- 0

# Preprocess Scores ----
playerScores <- playerScores %>%
  left_join(playerFinishes %>% select(Player, Pos, Team), by = "Player") %>%
  left_join(schedule, by = c("Team" = "Team", "Week" = "Week")) %>%
  filter(!is.na(Score)) %>%
  arrange(Player, Week)

# Manual Score Adjustments, Accounting for Players who Changed Teams ----

# Brett Rypien
playerScores[681, 5] <- "LAR"
playerScores[681, 6] <- "DAL"
playerScores[682, 5] <- "LAR"
playerScores[682, 6] <- "GB"

# Cam Akers
playerScores[919, 5] <- "LAR"
playerScores[919, 6] <- "SEA"

# Darrynton Evans
playerScores[1790, 5] <- "CHI"
playerScores[1790, 6] <- "MIN"
playerScores[1791, 5] <- "CHI"
playerScores[1791, 6] <- "LV"
playerScores[1792, 5] <- "CHI"
playerScores[1792, 6] <- "LAC"
playerScores[1793, 5] <- "CHI"
playerScores[1793, 6] <- "NO"
playerScores[1794, 5] <- "CHI"
playerScores[1794, 6] <- "CAR"

# Joshua Dobbs
playerScores[3836, 5] <- "ARI"
playerScores[3836, 6] <- "WAS"
playerScores[3837, 5] <- "ARI"
playerScores[3837, 6] <- "NYG"
playerScores[3838, 5] <- "ARI"
playerScores[3838, 6] <- "DAL"
playerScores[3839, 5] <- "ARI"
playerScores[3839, 6] <- "SF"
playerScores[3840, 5] <- "ARI"
playerScores[3840, 6] <- "CIN"
playerScores[3841, 5] <- "ARI"
playerScores[3841, 6] <- "LAR"
playerScores[3842, 5] <- "ARI"
playerScores[3842, 6] <- "SEA"
playerScores[3843, 5] <- "ARI"
playerScores[3843, 6] <- "BAL"

# Keaontay Ingram
playerScores[4129, 5] <- "ARI"
playerScores[4129, 6] <- "WAS"
playerScores[4130, 5] <- "ARI"
playerScores[4130, 6] <- "NYG"
playerScores[4131, 5] <- "ARI"
playerScores[4131, 6] <- "DAL"
playerScores[4132, 5] <- "ARI"
playerScores[4132, 6] <- "LAR"

# Riley Patterson
playerScores[5689, 5] <- "DET"
playerScores[5689, 6] <- "KC"
playerScores[5690, 5] <- "DET"
playerScores[5690, 6] <- "SEA"
playerScores[5691, 5] <- "DET"
playerScores[5691, 6] <- "ATL"
playerScores[5692, 5] <- "DET"
playerScores[5692, 6] <- "GB"
playerScores[5693, 5] <- "DET"
playerScores[5693, 6] <- "CAR"
playerScores[5694, 5] <- "DET"
playerScores[5694, 6] <- "TB"
playerScores[5695, 5] <- "DET"
playerScores[5695, 6] <- "BAL"
playerScores[5696, 5] <- "DET"
playerScores[5696, 6] <- "LV"
playerScores[5697, 5] <- "DET"
playerScores[5697, 6] <- "LAC"
playerScores[5698, 5] <- "DET"
playerScores[5698, 6] <- "CHI"
playerScores[5699, 5] <- "DET"
playerScores[5699, 6] <- "GB"
playerScores[5700, 5] <- "DET"
playerScores[5700, 6] <- "NO"
playerScores[5701, 5] <- "DET"
playerScores[5701, 6] <- "CHI"

# Tim Boyle
playerScores[6218, 5] <- "NYJ"
playerScores[6218, 6] <- "BUF"
playerScores[6219, 5] <- "NYJ"
playerScores[6219, 6] <- "MIA"
playerScores[6220, 5] <- "NYJ"
playerScores[6220, 6] <- "ATL"

# Tony Jones Jr.
playerScores[6259, 5] <- "NO"
playerScores[6259, 6] <- "TEN"
playerScores[6260, 5] <- "NO"
playerScores[6260, 6] <- "CAR"
playerScores[6261, 5] <- "NO"
playerScores[6261, 6] <- "GB"

# Zach Ertz
playerScores[6896, 5] <- "ARI"
playerScores[6896, 6] <- "WAS"
playerScores[6897, 5] <- "ARI"
playerScores[6897, 6] <- "NYG"
playerScores[6898, 5] <- "ARI"
playerScores[6898, 6] <- "DAL"
playerScores[6899, 5] <- "ARI"
playerScores[6899, 6] <- "SF"
playerScores[6900, 5] <- "ARI"
playerScores[6900, 6] <- "CIN"
playerScores[6901, 5] <- "ARI"
playerScores[6901, 6] <- "LAR"
playerScores[6902, 5] <- "ARI"
playerScores[6902, 6] <- "SEA"



# Create TeamVsOpp Dataframe ----
TeamVsPosDF <- playerScores %>% 
  filter(Pos != "DST") %>%
  group_by(Opponent, Week, Pos) %>%
  summarise(PointsAllowed = sum(Score)) %>%
  filter(Opponent != "") %>%
  rename("Team" = "Opponent") %>%
  left_join(schedule, by = c("Team" = "Team", "Week" = "Week")) %>%
  left_join(teams_colors_logos %>% select(team_abbr, team_logo_espn, team_color), 
            by = c("Opponent" = "team_abbr"))

# gtTable of League Overview
leagueOverviewTblLeftSide <- 
  TeamVsPosDF %>%
  group_by(Team, Pos) %>%
  summarise("Total" = round(sum(PointsAllowed), 1)) %>%
  spread(key = Pos, value = Total) %>%
  left_join(teams_colors_logos %>% select(team_abbr, team_wordmark), 
            by = c("Team" = "team_abbr")) %>%
  ungroup() %>%
  select(team_wordmark, QB, RB, WR, TE, K) %>%
  mutate(qbRank = round(rank(QB), 0), 
         rbRank = round(rank(RB), 0), 
         wrRank = round(rank(WR), 0), 
         teRank = round(rank(TE), 0), 
         kRank = round(rank(K), 0)) %>%
  slice_head(n = 16) %>%
  my_gt_fn()

leagueOverviewTblRightSide <- 
  TeamVsPosDF %>%
  group_by(Team, Pos) %>%
  summarise("Total" = round(sum(PointsAllowed), 1)) %>%
  spread(key = Pos, value = Total) %>%
  left_join(teams_colors_logos %>% select(team_abbr, team_wordmark), 
            by = c("Team" = "team_abbr")) %>%
  ungroup() %>%
  select(team_wordmark, QB, RB, WR, TE, K) %>%
  mutate(qbRank = round(rank(QB), 0), 
         rbRank = round(rank(RB), 0), 
         wrRank = round(rank(WR), 0), 
         teRank = round(rank(TE), 0), 
         kRank = round(rank(K), 0)) %>%
  slice_tail(n = 16) %>%
  my_gt_fn()



# Define ui ----
ui <- fluidPage(
  
  # App title ----
  titlePanel(title = "Fantasy Football Team vs Position Analysis 2023 (PPR)"),
  
  # Navbar layout ----
  navbarPage(title = "", 
             
    # First Page ----
    tabPanel(h5("Team vs. Position Analysis"),
  
      # First row ----
      fluidRow(
        
        # First column ----
        column(2), 
        
        # Second column ----
        column(8, h3("League Overview"), 
               h4("Total (PPR) Points Allowed Weeks 1 - 17; League Rank in Blue")),
        
        # Third column ----
        column(2)
        
      ), 
      
      # Second row ----
      fluidRow(
        
        # First column ----
        column(2), 
        
        # Second column ----
        column(4, gt_output(outputId = "LeagueOverviewLeftSide")),
        
        # Third column ----
        column(4, gt_output(outputId = "LeagueOverviewRightSide")),
        
        # Fourth column ----
        column(2)
      
      ), 
      
      # Third row ----
      fluidRow(
        
        # First column ----
        column(2), 
        
        # Second column ----
        column(2, h2(textOutput(outputId = "HeadingOutput"))), 
        
        # Third column ----
        column(8, htmlOutput(outputId = "TeamLogo"))
        
      ), 
      
    
      # Fourth row ----
      fluidRow(
        
        # First column ----
        column(2, 
               
               # Sidebar panel ----
               wellPanel(
                 
                 # Team Select Box ----
                 selectInput(inputId = "TeamInput", 
                             label = h4("Team"), 
                             choices = unique(TeamVsPosDF$Team)), 
                 
                 # Position Select Box ----
                 selectInput(inputId = "PosInput", 
                             label = h4("Position"), 
                             choices = c("QB", "RB", "WR", "TE", "K")), 
                 
                 # Week range slider ----
                 sliderInput(inputId = "Weeks", label = h4("Weeks"), min = 1,
                             max = 17, value = c(1,17)), 
                 
                 # Ylim slider ----
                 sliderInput(inputId = "Ylims", label = h4("Points Allowed Limits"), 
                             min = 0, max = 120, value = c(0, 120), step = 10), 
                 
                 # Opponents-on-graph check box ----
                 checkboxInput(inputId = "Opps", "Show opponents",
                               value = FALSE))
               
        ), 
        
        # Second column ----
        column(3, h3("Overview of Points Allowed"), 
               plotOutput(outputId = "BoxPlot")), 
        
        # Third column ----
        column(5,  h3("Points Allowed by Week"), 
               plotOutput(outputId = "LinePlot")), 
        
        # Fourth column ----
        column(2)
        
      ), 
      
      # Fifth row ----
      fluidRow(
        
        # First column ----
        column(2), 
        
        # Second column ----
        column(3, h3("Summary Statistics"), 
               gt_output(outputId = "SummaryStatsOutput")), 
        
        # Third column ----
        column(5, h3("Data Table"), 
               dataTableOutput(outputId = "DTOutput")), 
        
        # Fourth column ----
        column(2)
        
      )
      
    ), 
    
    # Second Page ----
    tabPanel(h5("About"),
             
      # First row ----
      fluidRow(
       
        # First column ----
        column(12, align = "left", 
               h5("App by: David Harler Jr."), 
                br(), 
                h5("Fantasy Football Data from: FantasyPros"), 
                h5("Team Logos & Wordmarks from: nflfastR"))
       
      )
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  
  # Team vs. Pos Heading ----
  output$HeadingOutput <- renderText({ 
    paste0(input$TeamInput, " vs. ", input$PosInput, "'s")
  })
  
  # Team Logo, utilizing HTML ----
  output$TeamLogo <- renderText({
    c('<img src="', 
      head(TeamVsPosDF %>% filter(Opponent == input$TeamInput), 1)$team_logo_espn, 
      '"width="100" height="100">')
  })
  
  # League Overview ----
  output$LeagueOverviewLeftSide <- render_gt(
    leagueOverviewTblLeftSide, 
    align = "right"
  )
  
  output$LeagueOverviewRightSide <- render_gt(
    leagueOverviewTblRightSide, 
    align = "left"
  )
  
  # Box Plot of Points Allowed ----
  output$BoxPlot <- renderPlot({
    
    p1 <- TeamVsPosDF %>% 
      filter(Team == input$TeamInput & 
               Pos == input$PosInput &
               Week >= input$Weeks[1] & 
               Week <= input$Weeks[2]) %>% 
      ggplot(mapping = aes(x = Team, y = PointsAllowed), na.rm = TRUE) +
      geom_point(color = head(TeamVsPosDF %>% 
                                filter(Opponent == input$TeamInput), 1)$team_color) + 
      geom_boxplot(color = head(TeamVsPosDF %>% 
                                  filter(Opponent == input$TeamInput), 1)$team_color, 
                   alpha = 0.5) +
      ylim(input$Ylims[1], input$Ylims[2]) +
      xlab("") + ylab("(PPR) Points Allowed") +
      theme(plot.margin = margin(7.25, 12, 18.75, 12, "pt"),
            axis.text.x = element_blank(),
            axis.text.y = element_text(size = 16), 
            axis.title.y = element_text(size = 18, face = "plain"), 
            axis.ticks = element_blank(), 
            panel.border = element_rect(color = "black", 
                                        fill = NA, 
                                        linewidth = 1))
    
    p1
    
  })
  
  # Line Chart of Points Allowed vs. Week ----
  output$LinePlot <- renderPlot({
    
    if (!input$Opps) {
      p2 <- TeamVsPosDF %>% 
        filter(Team == input$TeamInput & 
                 Pos == input$PosInput &
                 Week >= input$Weeks[1] & 
                 Week <= input$Weeks[2]) %>% 
        ggplot(mapping = aes(x = Week, y = PointsAllowed), na.rm = TRUE) +
        geom_point(color = head(TeamVsPosDF %>% 
                                  filter(Opponent == input$TeamInput), 1)$team_color) + 
        geom_line(color = head(TeamVsPosDF %>% 
                                 filter(Opponent == input$TeamInput), 1)$team_color,
                  linewidth = 0.65) +
        ylim(input$Ylims[1], input$Ylims[2]) +
        scale_x_continuous(breaks = integer_breaks()) + 
        xlab("Weeks") + ylab("") +
        theme(plot.margin = margin(8, 12, 0, 12, "pt"), 
              axis.text.x = element_text(size = 16), 
              axis.text.y = element_text(size = 16),
              axis.title.x = element_text(size = 18, face = "plain"), 
              axis.ticks = element_blank(), 
              panel.border = element_rect(color = "black", 
                                          fill = NA, 
                                          linewidth = 1))
      
      p2
    }
    
    else {
      p2 <- TeamVsPosDF %>% 
        filter(Team == input$TeamInput & 
                 Pos == input$PosInput &
                 Week >= input$Weeks[1] & 
                 Week <= input$Weeks[2]) %>% 
        ggplot(mapping = aes(x = Week, y = PointsAllowed), na.rm = TRUE) +
        geom_image(aes(image = team_logo_espn), size = .125) + 
        geom_line(color = head(TeamVsPosDF %>% 
                                filter(Opponent == input$TeamInput), 1)$team_color,
                  linewidth = 0.65) +
        ylim(input$Ylims[1], input$Ylims[2]) +
        scale_x_continuous(breaks = integer_breaks()) + 
        xlab("Weeks") + ylab("") +
        theme(plot.margin = margin(8, 12, 0, 12, "pt"), 
              axis.text.x = element_text(size = 16), 
              axis.text.y = element_text(size = 16),
              axis.title.x = element_text(size = 18, face = "plain"), 
              axis.ticks = element_blank(), 
              panel.border = element_rect(color = "black", 
                                          fill = NA, 
                                          linewidth = 1))
      
      p2
    }
    
  })
  
  # Reactive conductor to hold the current team's positional rank
  currRank <- reactive({
    (TeamVsPosDF %>% 
       filter(Pos == input$PosInput &
                Week >= input$Weeks[1] & Week <= input$Weeks[2] &
                PointsAllowed >= input$Ylims[1] & PointsAllowed <= input$Ylims[2]) %>%
       group_by(Team) %>%
       summarise("Total" = round(sum(PointsAllowed), 1)) %>%
       mutate(Rank = round(rank(Total), 0)) %>% 
       filter(Team == input$TeamInput))$Rank
  })
  
  # gtTable to display the player's summary statistics ----
  output$SummaryStatsOutput <- render_gt(
    TeamVsPosDF %>% 
      filter(Team == input$TeamInput & Pos == input$PosInput &
               Week >= input$Weeks[1] & Week <= input$Weeks[2] &
               PointsAllowed >= input$Ylims[1] & PointsAllowed <= input$Ylims[2]) %>%
      ungroup() %>%
      summarise("PPG" = round(mean(PointsAllowed, na.rm = TRUE), 1), 
                "Min" = round(min(PointsAllowed, na.rm = TRUE), 1), 
                "Median" = round(median(PointsAllowed, na.rm = TRUE), 1), 
                "Max" = round(max(PointsAllowed, na.rm = TRUE), 1), 
                "Stdev" = round(sd(PointsAllowed, na.rm = TRUE), 1), 
                "Total" = round(sum(PointsAllowed, na.rm = TRUE), 1)) %>% 
      pivot_longer(cols = everything()) %>% 
      rbind(data.frame(name = c("Rank"), value = c(as.character(currRank())))) %>%
      gt() %>% 
      tab_options(column_labels.hidden = TRUE) %>%
      cols_align("left") %>% 
      cols_width(name ~ px(100), value ~ px(80)), 
    align = "left"
  )
  
  # Data Table  ----
  output$DTOutput = renderDataTable(
    TeamVsPosDF %>% 
      filter(Team == input$TeamInput & Pos == input$PosInput &
               Week >= input$Weeks[1] & Week <= input$Weeks[2] &
               PointsAllowed >= input$Ylims[1] & PointsAllowed <= input$Ylims[2]) %>%
      select(Week, PointsAllowed, Opponent) %>%
      rename("Points Allowed" = "PointsAllowed"), 
    options = list(pageLength = 25, dom = "ft")
    
  )
  
}

# Create Shiny app ----
shinyApp(ui, server)




