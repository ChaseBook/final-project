
library(shiny)
library(shinythemes)
library(tidyverse)
library(gt)


ui <- fluidPage(theme = shinytheme("superhero"),tabsetPanel(
  
  # first tab 
  
  tabPanel("Introduction",
           
           #descriptive title to engage viewers       
           titlePanel("High School Players and The Major League Baseball Draft"),
           
           #explaining project, created in the server. 
           
           htmlOutput("intro")
)))

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  library(ggplot2)
  library(tidyverse)
  library(ggthemes)
  library(directlabels)
  
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
}


# Run the application
shinyApp(ui = ui, server = server)