

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

streak <-all_data$pts_streak
indices<-which(diff(sign(diff(streak)))==-2)+1
local_max <-slice(all_data,indices)
local_max<-local_max %>%group_by(player) %>% select(pts_streak,change_net,change_net_PRA,PRA_streak) %>%filter(pts_streak >0,PRA_streak >0)


player_data <-all_data %>% group_by(player) %>% mutate(a=10*lag(pts_direction)+pts_direction) %>% mutate(a=str_c(lag(pts_direction),pts_direction,sep = ',')) %>% count(a) %>% group_by(a)


# Define UI for application that plots a player's likelihood of scoring above their average
ui <- fluidPage(

    # Application title
    titlePanel("Analyzing Player Performance"),
    selectInput(inputId = "player", label = "player", choices = players),
    ##selectInput(inputId = "game_lag", label = "game_lag", choices = lags),
    plotOutput("linePlot"),
    plotOutput('spikePlot'),
    verbatimTextOutput("summary"),
    verbatimTextOutput('proportion')
        )

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  mod1 <-reactive({
    all_data <-all_data %>% filter(player == input$player)
    mod1 <-all_data %>%ungroup() %>% glm(formula=factor(pts_direction)~off_rtg+usg_pct+usg_pct:off_rtg,family=binomial())
  })
  
  grid <- reactive({
    all_data <-all_data %>% filter(player == input$player)
    grid <- data_grid(ungroup(all_data),off_rtg=seq_range(off_rtg,100),usg_pct= seq_range(usg_pct,5)) %>% add_predictions(mod1(), type = "response" )
  })
  
  output$linePlot <- renderPlot({
      all_data <-all_data %>% filter(player == input$player) %>%select(usg_pct,off_rtg) %>% group_by(player)
      ggplot(all_data) +geom_line(aes(x=off_rtg,y=pred, color = factor(usg_pct)),data=grid())
    })
  
  output$spikePlot <-renderPlot({
    scoring <-all_data %>% filter(player == input$player,season == '2022') %>%select(change_net,game_date) %>% group_by(player)
    ggplot(scoring, aes(x=as.Date(as.factor(game_date)), y=change_net, group=1)) +
      geom_ribbon(aes(ymin=pmin(change_net,0), ymax=0), fill="red", col="red", alpha=0.5) +
      geom_ribbon(aes(ymin=0, ymax=pmax(change_net,0)), fill="green", col="green", alpha=0.5) +
      geom_point()+
      geom_line(aes(y=0)) + ggtitle('Change') +
      ylab('Points Deviated from Average') +
      xlab('Date')
    
  })
  
  output$summary <-renderPrint({
    local_max <-local_max %>% filter(player == input$player)
    pts <- all_data%>% filter(player == input$player) %>% select(pts) %>%summarize(pts=mean(pts))
    summary(local_max)
    pts
  })
  output$proportion <-renderPrint({
    player_data <-player_data %>% filter(player == input$player)
    player_data
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
