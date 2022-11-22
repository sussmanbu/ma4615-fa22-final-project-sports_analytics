#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
suppressPackageStartupMessages(library(here))
suppressPackageStartupMessages(library(modelr))

load(here::here("dataset-ignore", "clean_nba_data.RData"))
data <-data %>% mutate(PRA=pts+trb+ast) %>%mutate(usg_pct=usg_pct/100)
all_data <-data %>% group_by(player)  %>% filter(mean(minutes) >25,minutes >15) %>% mutate(change_net = pts - mean(pts),pts_direction  = 
                                                                                             sign(change_net),change_net_PRA=PRA - mean(PRA),PRA_direction=sign(change_net_PRA)) %>%mutate(pts_streak=ave(pts_direction, cumsum(pts_direction==-1), FUN = seq_along) - 1,PRA_streak=ave(PRA_direction, cumsum(PRA_direction==-1), FUN = seq_along) - 1)
all_data <-all_data %>%mutate(pts_direction=ifelse(pts_direction==-1,0,1))
players <- all_data %>% pull(player) %>% unique() %>% sort()



# Define UI for application that plots a player's likelihood of scoring above their average
ui <- fluidPage(

    # Application title
    titlePanel("Analyzing Player Performance"),
    selectInput(inputId = "player", label = "player", choices = players),
    ##selectInput(inputId = "game_lag", label = "game_lag", choices = lags),
    plotOutput("linePlot")
        )

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  mod1 <-reactive({
    all_data <-all_data %>% filter(player == input$player)
    mod1 <-all_data %>%ungroup() %>% glm(formula=factor(pts_direction)~ts_pct +off_rtg+usg_pct+usg_pct:ts_pct,family=binomial())
  })
  
  grid <- reactive({
    all_data <-all_data %>% filter(player == input$player)
    grid <- data_grid(ungroup(all_data), ts_pct= seq_range(ts_pct,100),off_rtg=seq_range(off_rtg,3),usg_pct= seq_range(usg_pct,3)) %>% add_predictions(mod1(), type = "response" )
  })
  
  output$linePlot <- renderPlot({
      all_data <-all_data %>% filter(player == input$player) %>%select(usg_pct,off_rtg,ts_pct) %>% group_by(player)
      ggplot(all_data) +geom_line(aes(x=ts_pct,y=pred, color = factor(usg_pct), linetype = factor(off_rtg)),data=grid())
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
