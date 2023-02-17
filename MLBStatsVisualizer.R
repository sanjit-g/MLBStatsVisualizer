library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)

MLB_22 <- read.csv("2022_TeamStats.csv", header = TRUE, sep = ",")
MLB_22 <- filter(MLB_22, Team != "League Average" & Team != "Total")

MLB_21 <- read.csv("2021_TeamStats.csv", header = TRUE, sep = ",")
MLB_21 <- filter(MLB_21, Team != "League Average" & Team != "Total")

MLB_20 <- read.csv("2020_TeamStats.csv", header = TRUE, sep = ",")
MLB_20 <- filter(MLB_20, Team != "League Average" & Team != "Total")

MLB_19 <- read.csv("2019_TeamStats.csv", header = TRUE, sep = ",")
MLB_19 <- filter(MLB_19, Team != "League Average" & Team != "Total")

MLB_18 <- read.csv("2018_TeamStats.csv", header = TRUE, sep = ",")
MLB_18 <- filter(MLB_18, Team != "League Average" & Team != "Total")

MLB_17 <- read.csv("2017_TeamStats.csv", header = TRUE, sep = ",")
MLB_17 <- filter(MLB_17, Team != "League Average" & Team != "Total")

MLB_16 <- read.csv("2016_TeamStats.csv", header = TRUE, sep = ",")
MLB_16 <- filter(MLB_16, Team != "League Average" & Team != "Total")

MLB_15 <- read.csv("2015_TeamStats.csv", header = TRUE, sep = ",")
MLB_15 <- filter(MLB_15, Team != "League Average" & Team != "Total")

seasons_vector <- c("MLB_18","MLB_19","MLB_20","MLB_21","MLB_22")
team_color_vector <- 
  c("ARI"="#A71930","ATL"="#13274F","BAL"="#DF4601","BOS"="#BD3039","CHC"="#0E3386","CWS"="#27251F","CIN"="#C6011F","CLE"="#00385D","COL"="#333366","DET"="#0C2340","HOU"="#002D62","KCR"="#004687","LAA"="#BA0021","LAD"="#005A9C"
             ,"MIA"="#00A3E0","MIL"="#FFC52F","MIN"="#002B5C","NYM"="#002D72","NYY"="#0C2340","OAK"="#003831","PHI"="#E81828","PIT"="#27251F","SDP"="#2F241D","SEA"="#005C5C", "SFG"="#FD5A1E","STL"="#C41E3A","TBR"="#8FBCE6"
             ,"TEX"="#003278","TOR"="#134A8E","WSH"="#AB0003")

selection_vector <- c(MLB_22$Div, MLB_22$League, "Total")
selection_vector <- selection_vector[!duplicated(selection_vector)]

  # Define UI
  ui <- fluidPage(theme = shinytheme("cerulean"),
    navbarPage(
      # theme = "cerulean",  # <--- To use a theme, uncomment this
      "MLBStatsVisualizer",
      tabPanel("Point Plot",
               sidebarPanel(
                 tags$h3("Scatter plot generator"),
                 selectInput(inputId = "x", label = "X Axis", choices = names(MLB_22), selected = "WAA"),
                 selectInput(inputId = "y", label = "Y Axis", choices = names(MLB_22), selected = "PWAA"),
                 selectInput(inputId = "z", label = "Season", choices = seasons_vector, selected = "MLB_22"),
                 
               ), # sidebarPanel
               mainPanel(
                 plotOutput(outputId = "scatterPlot"),

               ) # mainPanel
               
      ), # Navbar 1, tabPanel
      tabPanel("Bar Plot",
               sidebarPanel(
                 tags$h3("Bar plot generator"),
                 selectInput(inputId = "a", label = "Season", choices = seasons_vector, selected = "MLB_22"),
                 
                 
               ), # sidebarPanel
               mainPanel(
                 plotOutput(outputId = "barPlot"),
                 plotOutput(outputId = "barPlot2"),
                 
               ) # mainPanel
      ),
      tabPanel("Line Plot", 
               sidebarPanel(
                 tags$h3("Team Progress Over Time"),
                 selectInput(inputId = "f", label = "Team", choices = MLB_22$Team, selected = "NYM"),
               ),
               mainPanel(
                 plotOutput(outputId = "linePlot"),
                 
               )
               ),
      tabPanel("Total Line Plot",
               sidebarPanel(
                 tags$h3("Team Progress Over Time"),
                 #input_a <- filter(names(MLB_18), )
                 selectInput(inputId = "total", label = "Stat", choices = selection_vector, selected = "Total"),
               ),
               mainPanel(
                 plotOutput(outputId = "total_lineplot", height = "800px")
               )
               )
  
    ) # navbarPage
  ) # fluidPage

  
  # Define server function  
  server <- function(input, output) {
    data <- reactive({get(input$z)})
    #eval(parse(text = input$a))
    data2 <- reactive({get(input$a)})
    output$scatterPlot <- renderPlot({
      ggplot(data = data(), aes_string(x = input$x, y = input$y)) + 
        geom_point(size = 2.5, aes(color = Team)) + #aes(size = qsec, color = factor(cyl))
        geom_text(size = 3.5, aes(label=Team),hjust=0.5, vjust=-0.5)+
        scale_color_manual(values = team_color_vector)+
        geom_vline(xintercept = 0)+
        geom_hline(yintercept = 0)+
        geom_line(aes(PWAA, PWAA))+
        #scale_color_manual(values = c("#3C6E71", "#70AE6E", "#BEEE62")) +
        theme_classic() 
        #theme(legend.position = "none")
    })

    output$barPlot <- renderPlot({
      ggplot(data = data2(), aes(Team, PWAA))+
        geom_bar(stat = "identity", alpha = 0.7, color = "transparent", aes(y = PWAA, fill = Team))+#replace alpha val with 0.7
        geom_bar(stat = "identity",alpha = 0.5, color = "#353d3d", aes(y = WAA, fill = Team), size = 1)+
        scale_fill_manual(values = team_color_vector)+
        #geom_bar(stat = "identity", alpha = 0.5, aes(y = PosWAA, fill = Team))+
        labs(title = "Wins Above Average(WAA) For All Teams in 2022 MLB Season", x = "Teams", y = "WAA(Grey Box)")+
        scale_y_continuous(
          "WAA(Grey Box)", 
          sec.axis = sec_axis(~ . * 1, name = "PWAA(Darker Shade)")
        )
    })
    
    output$barPlot2 <- renderPlot({
      ggplot()
      team_avg_WAA <- data.frame(Team = character(), WAA = numeric())
      team_avg_WAA[1,] <- c(NA,NA)
      colnames(team_avg_WAA) <- c("Team", "WAA")
      for (j in MLB_22$Team){
        result <- data.frame(Year = numeric(), WAA = numeric())
        result[1,] <- c(NA,NA)
        colnames(result) <- c("Year", "WAA")
        for (i in seasons_vector){
          k <- get(i)
          k <- filter(k, Team == j)
          temp_bind <- c(k$Year, k$WAA)
          result <- rbind(result, temp_bind)
        }
        result <- na.omit(result)
        avg <- mean(result$WAA)
        temp_bind2 <- c(j, avg)
        team_avg_WAA  <- rbind(team_avg_WAA, temp_bind2)
      }
      team_avg_WAA <- na.omit(team_avg_WAA)
      ggplot(data = team_avg_WAA, aes(x = Team, y = WAA))+
        geom_bar(stat = "identity", alpha = 0.7, color = "transparent", aes(y = WAA, fill = Team))
    })
    
    output$linePlot <- renderPlot({
      g <- ggplot()
      result <- data.frame(Year = numeric(), WAA = numeric())
      result[1,] <- c(NA,NA)
      colnames(result) <- c("Year", "WAA")
      for (i in seasons_vector){
        k <- get(i)
        k <- filter(k, Team == input$f)
        temp_bind <- c(k$Year, k$WAA)
        result <- rbind(result, temp_bind)
        #g <- g + geom_point(data = k, aes(x = Year, y = WAA))
      }
      result <- na.omit(result)
      g <- g + geom_point(data = result, aes(x = Year, y = WAA))
      g <- g + geom_line(data = result, aes(x = Year, y = WAA))
      g
    })
    
    output$total_lineplot <- renderPlot({
      g <- ggplot()
      WAA_over_time <- data.frame(Team = character(), Year = numeric(), WAA = numeric())
      WAA_over_time[1,] <- c(NA,NA,NA)
      colnames(WAA_over_time) <- c("Team", "Year", "WAA")
      div_or_league <- input$total
      print(class(div_or_league))
      #print(MLB_22)
      if (div_or_league == "Total"){
        desired_teams <- MLB_22
        print("a")
      } else {
        desired_teams <- filter(MLB_22, League == as.character(input$total) | Div == as.character(input$total))
      }
      print(desired_teams)
      for (j in desired_teams$Team){
        result <- data.frame(Team = character(), Year = numeric(), WAA = numeric())
        result[1,] <- c(NA,NA,NA)
        colnames(result) <- c("Team","Year", "WAA")
        for (i in seasons_vector){
          k <- get(i)
          k <- filter(k, Team == j)
          fr <- reactive({k}) 
          temp_bind <- c(j, fr()$Year, as.numeric(fr()$WAA))
          result <- rbind(result, temp_bind)
          result$WAA <- as.numeric(as.character(result$WAA))
          result$Team <- as.character(result$Team)
        }
        result <- na.omit(result)
        g <- g + geom_point(data = result, aes(x = Year, y = WAA, color = Team))
        #g <- g + geom_text(position = "identity", size = 3.5, aes(label=result$Team),hjust=0.5, vjust=-0.5)
        g <- g + geom_text(data = result, aes(x = Year[1], y = WAA[1], label = Team), hjust = 13.25, vjust = 0, nudge_x = 2, size = 3.5, show.legend = TRUE)
        g <- g + geom_path(data = result, aes(x = Year, y = WAA, color = Team), group = 1)
        g <- g + scale_x_discrete(expand = expansion(add = 1.2))
        g <- g + scale_color_manual(values = team_color_vector)
        
        
      }
      g <- g + theme(plot.margin = unit(c(0,1,3,1),"cm"))
      g
    })
  } # server
  

  # Create Shiny object
  shinyApp(ui = ui, server = server)