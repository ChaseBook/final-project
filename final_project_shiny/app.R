
library(shiny)
library(graphics)
library(shinythemes)
library(tidyverse)
library(gt)


master <- read_csv("csv/project_master.csv")
redraft_table <- read_csv("csv/redraft_table.csv")

ui <- fluidPage(theme = shinytheme("flatly"),
                
            
                navbarPage("Exploring the MLB Draft",
 
 # Introduction tab -------------------------------                                                    
                           tabPanel("Introduction", align = "center",
                                    
                              titlePanel("High School Players and The Major League Baseball Draft"),
                              
                              br(),
                              
                              tags$img(src = "draft_2020.jpg", width = "540px", height = "292.5px"),
                              
                              br(),
                              br(),
                              
                              h2("About the Project", align = "center"),
                              
                              htmlOutput("intro")
                           ),
                           
 # Turning Down the Draft Tab -------------------------
                           tabPanel("Turning Down the Draft", align = "center",
                                    
                                    
                                    tabsetPanel(
                                      
                                      tabPanel("Chances of Getting Redrafted",
                                                
                                                titlePanel("What are the Chances of Getting Redrafted?"),
                                                
                                                br(),
                                                br(),
                                    
                                                sidebarLayout(
                                                  sidebarPanel(
                                                    
                                                    checkboxGroupInput("redraft_year",
                                                                       "Start of Five Year Period:",
                                                                       unique(redraft_table$start_year), 
                                                                       selected = c(1990, 1995,
                                                                                    2000, 2005, 2010)
                                                    )
                                                  ),
                                                  mainPanel(
                                                    plotOutput("redraftProb")
                                                  )
                                                )
                                       ),
                                    
                                      tabPanel("Change in Draft Stock",
                                               
                                               titlePanel("How Does Draft Stock Change when a Player is Redrafted?"),
                                               
                                               br(),
                                               br(),
                                               
                                               sidebarLayout(
                                                 sidebarPanel(
                                                   
                                                   checkboxGroupInput("stock_year",
                                                                      "Start of Five Year Period:",
                                                                      unique(redraft_table$start_year), 
                                                                      selected = c(1990, 1995,
                                                                                   2000, 2005, 2010)
                                                   )
                                                 ),
                                                 mainPanel(
                                                   plotOutput("redraftStock")
                                                 )
                                               )
                                        
                                      )
                                    )
                           ), 
 
 # Navigating the Minors Tab -----------------------
                           
                           tabPanel("Navigating the Minors", align = "center",
                                    
                                    titlePanel("Path to the Show"),
                                    
                                    br(),
                                    br(),
                                    
                                    sidebarLayout(
                                      sidebarPanel(
                                        
                                        numericInput("peakroundMin", "Minimum Round:", value = 1, min = 1, max = 40),
                                        numericInput("peakroundMax", "Maximum Round:", value = 20, min = 1, max = 40),
                                        numericInput("peakyearMin", "Minimum Year:", value = 1990, min = 1965, max = 2010),
                                        numericInput("peakyearMax", "Maximum Year:", value = 2010, min = 1965, max = 2010)
                                      ),
                                      
                                      mainPanel(
                                        plotOutput("careerPeak")
                                      )
                                    )
                           
                           ),
                                    
                                    
                                    

 # Major League Success Tab -------------------------
 
                           tabPanel("Major League Success", align = "center")
                           
                                       
                                       
))

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  library(ggplot2)
  library(tidyverse)
  library(ggthemes)
  library(directlabels)
  
  
# Introduction
  
  output$intro<-renderUI({
    
    ex_1 <- p("The Major League Baseball draft is an annual event in which the 30 MLB teams 
              take turns selecting top high school and collegiate prospects. Once drafted, a 
              player will choose to sign or, if they are a high school senior or college
              player with remaining eligibility, not sign and potentially get drafted again
              down the road.")
    
    ex_2 <- p("For top high school players especially, the magnitude of this decision is
              immense. While certain young players will sign for small contracts, eager to
              begin their professional career, others will forgo multi-million dollar signing
              bonuses and attend college. For some, the decision pays off handsomely and the
              prospect signs for more money as a polished college star. For others, they get
              injured or fail to perform in college and never play professional baseball.
              Money is obviously not the only factor in a high school prospect's decision -
              the team, prospect's health, and family influences among other things can play a
              significant role. However, given the amount of money and the opportunity many
              high school players opt to delay, it begs the question of how the decision plays
              out for these athletes.")
    
    
    
    ex_3 <- p("Do high school players who sign out of high school have a better chance of
              making it to the Major Leagues relative to their peers who attend college? What
              is the relationship between a player's decision of whether to sign and the
              longevity and production of their professional baseball career? Of drafted high
              school players that make it to the big leagues, do players that choose to attend
              college develop and experience more fruitful careers than players that sign
              immediately out of high school? The Major League Baseball draft is full of
              fascinating topics to analyze, and in this project I will use data to quantify
              high school players' career outcomes in relation to this decision.")
    
    HTML(paste(ex_1, br(),ex_2, br(), ex_3))
    
  })
    
# Getting Re-drafted 
  
# Re-draft probability graph
  
  redraft_subset <- reactive({redraft_table%>% filter(start_year %in%input$redraft_year)})
  
  output$redraftProb <- renderPlot({
      
    redraft_subset() %>%
      ggplot(aes(round, percent_redrafted)) +
      geom_point(na.rm = TRUE) +
      geom_line(aes(group = start_year, color = start_year), na.rm = TRUE) +
      labs(
        x = "Round Range Drafted out of High School",
        y = "Percent of Picks Re-drafted",
        color = "Time Frame",
        title = "High School Re-draft Probability",
        subtitle = "Organized by initial round range and time period"
      ) +
      theme_light() +
      theme(text = element_text(size=20)) +
      scale_color_viridis_c() +
      scale_x_continuous(breaks = c(5, 10, 20, 30, 40, 50))
  })
    
# Re-draft stock graph
  
  stock_subset <- reactive({redraft_table%>% filter(start_year %in%input$stock_year)})  
  
  output$redraftStock <- renderPlot({
      
    redraft_table %>%
      ggplot(aes(round, mean_round_diff)) +
      geom_point(na.rm = TRUE) +
      geom_line(aes(group = start_year, color = start_year), na.rm = TRUE) +
      labs(
        x = "Round Range Drafted out of High School",
        y = "Average Improvement Re-draft Round",
        color = "Time Frame",
        title = "Average Change in Round of Re-drafted Unsigned High School Picks",
        subtitle = "Organized by initial round range and time period"
      ) +
      theme_light() +
      theme(text = element_text(size=20)) +
      scale_color_viridis_c() +
      scale_x_continuous(breaks = c(5, 10, 20, 30, 40, 50)) +
      scale_y_continuous(breaks = c(-10, -5, 0, 10, 20)) +
      geom_hline(color = "red", linetype = "dashed", yintercept = 0)
    })
  
# Career peak distribution
  
  
  peak_subset <- reactive({master %>%
      filter(source == "H" & draft_numb == 1) %>%
      filter(draft_round >= input$peakroundMin & draft_round <= input$peakroundMax 
             & year >= input$peakyearMin & year <= input$peakyearMax) %>%
      mutate(high_level = ifelse(high_level %in% c("-", "JrCollege", "NAIA", "NCAA"),
                                 "<=College",
                                 high_level)) %>%
      group_by(signed, high_level) %>%
      summarize(n = n()) %>%
      group_by(signed) %>%
      mutate(y = n/sum(n)) %>%
      mutate(high_level = factor(high_level,levels = c("<=College",
                                                       "Indy",
                                                       "Rk",
                                                       "A-",
                                                       "A",
                                                       "A+",
                                                       "AA",
                                                       "AAA",
                                                       "MLB")))
                                
  })
  
  output$careerPeak <- renderPlot({
    
    peak_subset() %>%
      ggplot() +
      geom_col(aes(x = high_level, y = y, fill = signed), position = "dodge") +
      scale_y_continuous(labels = scales::percent_format()) +
      theme_light() +
      theme(text = element_text(size=20)) +
      labs(
        x = "Peak Baseball Level Reached",
        y = "Percentage",
        fill = "Signed?",
        title = "Distribution of Peak Baseball Career Level of High School Picks",
        subtitle = "Grouped by whether or not the player signed out of high school"
      )
    
  })
  

        
    
}


# Run the application
shinyApp(ui = ui, server = server)