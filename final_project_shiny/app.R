
library(shiny)
library(graphics)
library(shinythemes)
library(tidyverse)
library(gt)

# NOTE: All work and comments can be fouund in draft_exploration.Rmd

master <- read_csv("csv/master_final.csv")
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
                                                ),
                                               
                                               br(),
                                               br(),
                                               
                                               htmlOutput("redraftprobExp")
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
                                    
                                    tabsetPanel(
                                      
                                      tabPanel("Career Peak",
                                    
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
                                    
                                      tabPanel("Age of MLB Debut Season",
                                        
                                        titlePanel("Do Re-drafted Players Reach the Majors Faster?"),
                                        
                                        br(),
                                        br(),
                                        
                                        plotOutput("debutAge")
                                        
                                        
                                      )
                                    )
                           
                           ),
                                    
                                    

 # Major League Success Tab -------------------------
 
                           tabPanel("Major League Success", align = "center",
                                    
                                    
                                    tabsetPanel(
                                      
                                      tabPanel("MLB Longevity",
                                               
                                               titlePanel("How Long Do High School Picks Stay in the Show?"),
                                               
                                               br(),
                                               br(),
                                               
                                               sidebarLayout(
                                                 sidebarPanel(
                                                   
                                                   checkboxGroupInput("length_position",
                                                                      "Position:",
                                                                      c("C", "1B", "2B", "3B", "SS", "OF", "P"), 
                                                                      selected = c("C", "1B", "2B", "3B", "SS", "OF", "P")
                                                   )
                                                 ),
                                               
                                                 
                                                 mainPanel(
                                                   plotOutput("careerLength")
                                                 )
                                               ),
                                               
                                               br(),
                                               br(),
                                               
                                               br()
                                               
                                      ),
                                      
                                      tabPanel("MLB Production",
                                               
                                               titlePanel("How do High School Picks Perform in the Big Leagues?"),
                                               
                                               br(),
                                               br(),
                                               
                                               sidebarLayout(
                                                 sidebarPanel(
                                                   
                                                   checkboxGroupInput("war_position",
                                                                      "Position:",
                                                                      c("C", "1B", "2B", "3B", "SS", "OF", "P"), 
                                                                      selected = c("C", "1B", "2B", "3B", "SS", "OF", "P")
                                                   ),
                                                   
                                                   br(),
                                                   
                                                   h2("Range of Years in Which Career Ended"),
                                                   
                                                   numericInput("waryearMin", "Minimum Year:", value = 1990, min = 1965, max = 2017),
                                                   numericInput("waryearMax", "Maximum Year:", value = 2017, min = 1965, max = 2017)
                                                 ),
                                                 
                                                 mainPanel(
                                                   gt_output("careerWar")
                                                 )
                                               )
                                               
                                      )
                                    )
                           ),
 
 # State by State Tab --------------------
 
                            tabPanel("State by State", align = "center",
                                     
                                     tabsetPanel(
                                       
                                       tabPanel("US Map of Draft Picks",
                                                
                                
                                      
                                                titlePanel("Which States Produce the Most Productive High School Draft Picks?"),
                                     
                                                 br(),
                                                 br(),
                                                 
                                                 tags$img(src = "us_draft_map.png", width = "1000px", height = "617px")
                                                     
                                        ),
                                       
                                       tabPanel("State Production of Draft Picks",
                                                
                                                titlePanel("Which States Have Had the Most High School Players Go Professional?"),
                                                
                                                br(),
                                                br(),
                                                
                                                tags$img(src = "draft_picks_state.png", width = "1000px", height = "617px")
                                       )
                                     )
                            )
                                                
                                                
                                     
                            
                           
                                       
                                       
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
              Money is not the only factor in a high school prospect's decision -
              the team, the prospect's health, and family influences among other things can play a
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
  
  redraft_subset <- reactive({redraft_table%>% filter(start_year %in% input$redraft_year)})
  
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
  
  output$redraftprobExp <- renderUI({
    
    ex_1 <- p("One factor high school draftees should consider when deciding 
              whether to sign is whether they will get another chance to 
              play professional baseball. For most prospects, turning down pro 
              ball for a four-year college means they will not be draft 
              eligible until after their junior year. In this time, they run
              the risk of injury, lack of production in college, and other
              factors that may prevent them from being drafted again.")
    
    ex_2 <- p("This graph illustrates the probability of being redrafted 
             given a player is drafed in a certain range of rounds
             (1-5, 5-10, 10-20,...) out of high school and opts to turn
             it down. The data is sorted by five year windows in which the 
             player graduated high school in order to track how patterns
             in redraft probability have changed over time.")
    
    ex_3 <- p("With all five time frames selected, we see the most recent
             period starting in 2010 closely resembles the 1990 and 1995
             data for picks within the top 20 rounds. However, from the 20th
             round onward, the slope of the 2010 redraft probability curve is
             less steep than any other time period. Late-round high school
             picks in the 2010 period have the highest chance of being redrafted
             relative to earlier periods. There are various possible explanations
             for this trend, one being that teams are simply better at identifying
             high school prospects than in years previous, perhaps through new
             technologies. This would increase the likelihood these players continue
             to succeed in college and get redrafted. Another possibility is teams
             may be using more late-round picks on top high school prospects who
             are clearly going to college, whether not they truly mean to sign
             them. These would be the elite high school players that get drafted
             in the 35th round, turn down the draft, and are clearly going to
             be redrafted later on.")
    
    HTML(paste(ex_1, br(),ex_2, br(), ex_3))
    
  })
    
# Re-draft stock graph
  
  stock_subset <- reactive({redraft_table%>% filter(start_year %in% input$stock_year)})  
  
  output$redraftStock <- renderPlot({
      
    stock_subset() %>%
      ggplot(aes(round, mean_round_diff)) +
      geom_point(na.rm = TRUE) +
      geom_line(aes(group = start_year, color = start_year), na.rm = TRUE) +
      labs(
        x = "Round Range Drafted out of High School",
        y = "Average Improvement in Re-draft Round",
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
  
  output$debutAge <- renderPlot({
    debut <- master %>%
      filter(source == "H" & draft_numb == 1 & high_level == "MLB") %>%
      group_by(signed, debut) %>%
      summarize(n = n()) %>%
      group_by(signed) %>%
      mutate(y = n/sum(n))
    
    debut %>%
      ggplot(aes(x = debut, y = y, color = signed)) +
      geom_point() +
      geom_line() +
      scale_y_continuous(labels = scales::percent_format()) +
      scale_x_continuous(breaks = c(18:30)) +
      theme_light() +
      theme(text = element_text(size=20)) +
      labs(
        x = "Age During Debut Season",
        y = "Percentage",
        color = "Signed?",
        title = "Distribution of Debut Season Age of High School Picks",
        subtitle = "Grouped by whether or not the player signed out of high school"
      )
  })
  
# career length subset and graph
  
  length_subset <- reactive({master %>%
      filter(source == "H" & draft_numb == 1 & high_level == "MLB") %>%
      filter(position %in% input$length_position) %>%
      filter(mlb_end <= 2017)
  })
  
  output$careerLength <- renderPlot({
    length_subset() %>%
    ggplot(aes(signed, mlb_length, fill = signed)) +
      geom_boxplot() +
      coord_flip() +
      theme_light() +
      theme(text = element_text(size=20)) +
      scale_y_continuous(breaks = c(0, 5, 10, 15, 20)) +
      labs(
        x = "Years Played in MLB",
        y = "Signed out of High School?",
        title = "MLB Career Length Summary of High School Picks to Reach MLB",
        subtitle = "Grouped by whether player signed out of high school"
      )
  })
  
  
  war_subset <- reactive({master %>%
      filter(source == "H" & draft_numb == 1 & high_level == "MLB") %>%
      filter(position %in% input$war_position) %>%
      filter(mlb_end >= input$waryearMin & mlb_end <= input$waryearMax) %>%
      mutate(war_annual = war / mlb_length) %>%
      group_by(signed) %>%
      summarize(q1 = quantile(war, 0.25),
                median_war = median(war),
                mean_war = mean(war),
                q3 = quantile(war, 0.75))
    
  })
  
  output$careerWar <- render_gt({
    war_subset() %>%
      gt() %>%
      tab_header(
        title = "Career Wins Above Replacement of High School Picks to Reach MLB"
      ) %>%
      tab_spanner(
        columns = vars(signed, q1, median_war, mean_war, q3),
        label = "Grouped by whether player signed out of high school"
      ) %>%
      cols_label(
        signed = "Signed?",
        q1 = "25th Percentile",
        median_war = "Median",
        mean_war = "Mean",
        q3 = "75th Percentile"
      ) %>%
      fmt_number(
        columns = vars(q1, median_war, mean_war, q3),
        decimals = 2
      )
  })
        
    
}


# Run the application
shinyApp(ui = ui, server = server)