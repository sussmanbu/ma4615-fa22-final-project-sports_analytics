#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(here))
suppressPackageStartupMessages(library(runner))
suppressPackageStartupMessages(library(modelr))
suppressPackageStartupMessages(library(rsconnect))


load("clean_nba_data_app.RData")
load("clean_nba_data22_app.RData")
nba_players <- all_data %>% pull(player) %>% unique() %>% sort()
load("clean_mlb_data_app.RData")
mlb_players <- all_data_mlb %>% pull(player) %>% unique() %>% sort()
league <-c('MLB','NBA')

get_leagues <- function(league){
  if (league=='NBA'){
    return(nba_players)
  }
  else{
    return(mlb_players)
  }
}

ui <- fluidPage(
  titlePanel("Analyzing Player Performance"),
  selectInput(inputId = "league", label = "league", choices = league),
  uiOutput('pick_league'),
  plotOutput('spikePlot'),
  plotOutput('linePlot'),
  verbatimTextOutput('summary')
)

server <- function(input, output) {
  output$pick_league <- renderUI({
    league <- input$league
    players <- get_leagues(league)
    tagList(
      selectInput(inputId = 'playerID',label='choose player',players)
    )
  })
  
  output$spikePlot <-renderPlot({
    if(input$league =='MLB'){
      scoring <-all_data_mlb %>% ungroup() %>% filter(player == input$playerID) %>%select(change_net,game_date) 
      ggplot(scoring, aes(x=as.Date(as.factor(game_date)), y=change_net, group=1)) +
      geom_ribbon(aes(ymin=pmin(change_net,0), ymax=0), fill="red", col="red", alpha=0.5) +
      geom_ribbon(aes(ymin=0, ymax=pmax(change_net,0)), fill="green", col="green", alpha=0.5) +
      geom_point()+
      geom_line(aes(y=0)) + ggtitle('Change') +
      ylab('Points Deviated from Average') +
      xlab('Date')
      
      }
    else{
      scoring <-data22 %>% ungroup() %>% filter(player == input$playerID) %>%select(change_net,game_date) 
      ggplot(scoring, aes(x=as.Date(as.factor(game_date)), y=change_net, group=1)) +
      geom_ribbon(aes(ymin=pmin(change_net,0), ymax=0), fill="red", col="red", alpha=0.5) +
      geom_ribbon(aes(ymin=0, ymax=pmax(change_net,0)), fill="green", col="green", alpha=0.5) +
      geom_point()+
      geom_line(aes(y=0)) + ggtitle('Change') +
      ylab('Points Deviated from Average') +
      xlab('Date')
    }
  })
  
  mod1 <-reactive({
    all_data <-all_data %>% filter(player == input$playerID)
    mod1 <-all_data %>%ungroup() %>% glm(formula=factor(pts_direction)~off_rtg+usg_pct+usg_pct:off_rtg,family=binomial())
  })
  mod2 <-reactive({
    all_data <-all_data_mlb %>% filter(player == input$playerID)
    mod2 <-all_data_mlb %>% ungroup() %>% glm(formula=factor(h_direction)~pa+rbi+home_runs+so,family=binomial())
  })
  
  grid <- reactive({
    all_data <-all_data %>% filter(player == input$playerID)
    grid <- data_grid(ungroup(all_data),off_rtg=seq_range(off_rtg,100),usg_pct= seq_range(usg_pct,5)) %>% add_predictions(mod1(), type = "response")
  })
  
  output$linePlot <- renderPlot({
    if(input$league =='NBA'){
      all_data <-all_data %>% filter(player == input$playerID) %>%select(usg_pct,off_rtg)
      ggplot(all_data) +geom_line(aes(x=off_rtg,y=pred, color = factor(usg_pct)),data=grid())
    }
    else{
      all_data_mlb <-all_data_mlb %>% filter(player == input$playerID) %>%mutate(h_direction=ifelse(h_direction==-1,0,1)) %>% add_predictions(mod2(), type = "response" ) %>%select(h,pa,rbi,home_runs,so,pred)
      ggplot(all_data_mlb) +geom_point(aes(x=h,y=pred,color=pa),position='jitter')
    }
  })
  
  output$summary <-renderPrint({
    if(input$league =='MLB'){
      all_data_mlb<- all_data_mlb %>% ungroup() %>% filter(player == input$playerID) %>% select(h_streak)
      summary(all_data_mlb)
      
    }
    else{
      all_data <- all_data %>% ungroup() %>% filter(player == input$playerID) %>% select(pts_streak)
      summary(all_data)
    }
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
