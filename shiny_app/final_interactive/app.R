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
suppressPackageStartupMessages(library(gridExtra))

load(here::here("shiny_app/final_interactive/segmented_model/MLB", "bb_predicted.RData"))
load(here::here("shiny_app/final_interactive/segmented_model/MLB", "so_predicted.RData"))
load(here::here("shiny_app/final_interactive/segmented_model/MLB", "hr_predicted.RData"))
load(here::here("shiny_app/final_interactive/segmented_model/MLB", "hit_predicted.RData"))
load(here::here("shiny_app/final_interactive/segmented_model/MLB", "batting_real.RData"))

load(here::here("shiny_app/final_interactive/segmented_model/MLB", "bb_paces.RData"))
load(here::here("shiny_app/final_interactive/segmented_model/MLB", "so_paces.RData"))
load(here::here("shiny_app/final_interactive/segmented_model/MLB", "hr_paces.RData"))
load(here::here("shiny_app/final_interactive/segmented_model/MLB", "hit_paces.RData"))

load(here::here("shiny_app/final_interactive/segmented_model/NBA", "pts_predicted.RData"))
load(here::here("shiny_app/final_interactive/segmented_model/NBA", "ast_predicted.RData"))
load(here::here("shiny_app/final_interactive/segmented_model/NBA", "rb_predicted.RData"))
load(here::here("shiny_app/final_interactive/segmented_model/NBA", "scoring_real.RData"))

load(here::here("shiny_app/final_interactive/segmented_model/NBA", "pts_paces.RData"))
load(here::here("shiny_app/final_interactive/segmented_model/NBA", "ast_paces.RData"))
load(here::here("shiny_app/final_interactive/segmented_model/NBA", "rb_paces.RData"))

load(here::here("shiny_app/final_interactive", "clean_nba_data_app.RData"))
load(here::here("shiny_app/final_interactive", "clean_nba_data22_app.RData"))
load(here::here("shiny_app/final_interactive", "clean_mlb_data_app.RData"))

nba_players <- all_data %>% pull(player) %>% unique() %>% sort()
mlb_players <- batting_dataframe %>% pull(player) %>% unique() %>% sort()
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
  plotOutput('segmentedPlot'),
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
  
  output$segmentedPlot <-renderPlot({
    if(input$league =='MLB'){
      real <- batting_dataframe %>% filter(player == input$playerID)
      bb <- bb_dataframe %>% filter(player == input$playerID)
      so <- so_dataframe %>% filter(player == input$playerID)
      hr <- hr_dataframe %>% filter(player == input$playerID)
      hit <- hit_dataframe %>% filter(player == input$playerID)
      
      bb_p <- bb_paces %>% filter(name == input$playerID)
      so_p <- so_paces %>% filter(name == input$playerID)
      hr_p <- hr_paces %>% filter(name == input$playerID)
      hit_p <- hit_paces %>% filter(name == input$playerID)
      
      p1 <- ggplot() + 
        geom_line(data = hr, aes(x = games, y = stat_cum)) + 
        geom_point(data = real, aes(x=games, y=hr_cum), color="gold4", alpha =0.2, size=1) +
        geom_vline(data = hr_p, aes(xintercept = start), linetype = "dotted") +
        labs(x="Game", y="Cumulative Home Runs", title="Home Run Trajectory")
      
      p2 <- ggplot() +
        geom_line(data = hit, aes(x = games, y = stat_cum)) + 
        geom_point(data = real, aes(x=games, y=hit_cum), color="blue", alpha =0.2, size=1) +
        geom_vline(data = hit_p, aes(xintercept = start), linetype = "dotted") +
        labs(x="Game", y="Cumulative Hits", title="Hits Trajectory")
        
      p3 <- ggplot() +
        geom_line(data = so, aes(x = games, y = stat_cum)) + 
        geom_point(data = real, aes(x=games, y=so_cum), color="red", alpha =0.2, size=1) +
        geom_vline(data = so_p, aes(xintercept = start), linetype = "dotted") +
        labs(x="Game", y="Cumulative Strike Outs", title="Strike Out Trajectory")
      
      p4 <- ggplot() +
        geom_line(data = bb, aes(x = games, y = stat_cum)) + 
        geom_point(data = real, aes(x=games, y=bb_cum), color="green", alpha =0.2, size=1) +
        geom_vline(data = bb_p, aes(xintercept = start), linetype = "dotted") +
        labs(x="Game", y="Cumulative Walkss", title="Walks Trajectory")
      
      grid.arrange(p1, p2, p3, p4, ncol=2)
    }
    else{
      real <- scoring_dataframe %>% filter(player == input$playerID)
      pts <- pts_dataframe %>% filter(player == input$playerID)
      ast <- ast_dataframe %>% filter(player == input$playerID)
      rb <- rb_dataframe %>% filter(player == input$playerID)
      
      pts_p <- pts_paces %>% filter(name == input$playerID)
      ast_p <- ast_paces %>% filter(name == input$playerID)
      rb_p <- rb_paces %>% filter(name == input$playerID)
      
      p1 <- ggplot() + 
        geom_line(data = pts, aes(x = games, y = stat_cum)) + 
        geom_point(data = real, aes(x=games, y=pts_cum), color="green", alpha =0.2, size=1) +
        geom_vline(data = pts_p, aes(xintercept = start), linetype = "dotted") +
        labs(x="Game", y="Cumulative Points", title="Points Trajectory")
      
      p2 <- ggplot() +
        geom_line(data = ast, aes(x = games, y = stat_cum)) + 
        geom_point(data = real, aes(x=games, y=ast_cum), color="blue", alpha =0.2, size=1) +
        geom_vline(data = ast_p, aes(xintercept = start), linetype = "dotted") +
        labs(x="Game", y="Cumulative Assists", title="Assists Trajectory")
      
      p3 <- ggplot() +
        geom_line(data = rb, aes(x = games, y = stat_cum)) + 
        geom_point(data = real, aes(x=games, y=rb_cum), color="red", alpha =0.2, size=1) +
        geom_vline(data = rb_p, aes(xintercept = start), linetype = "dotted") +
        labs(x="Game", y="Cumulative Rebounds", title="Rebounds Trajectory")
      
      grid.arrange(p1, p2, p3, ncol=2)
    }
  })
  
  
  output$spikePlot <-renderPlot({
    if(input$league =='MLB'){
      scoring <-all_data_mlb %>% ungroup() %>% filter(player == input$playerID) %>%dplyr::select(change_net,game_date) 
      ggplot(scoring, aes(x=as.Date(as.factor(game_date)), y=change_net, group=1)) +
      geom_ribbon(aes(ymin=pmin(change_net,0), ymax=0), fill="red", col="red", alpha=0.5) +
      geom_ribbon(aes(ymin=0, ymax=pmax(change_net,0)), fill="green", col="green", alpha=0.5) +
      geom_point()+
      geom_line(aes(y=0)) + ggtitle('Change') +
      ylab('Points Deviated from Average') +
      xlab('Date')
      
      }
    else{
      scoring <-data22 %>% ungroup() %>% filter(player == input$playerID) %>%dplyr::select(change_net,game_date) 
      ggplot(scoring, aes(x=as.Date(as.factor(game_date)), y=change_net, group=1)) +
      geom_ribbon(aes(ymin=pmin(change_net,0), ymax=0), fill="red", col="red", alpha=0.5) +
      geom_ribbon(aes(ymin=0, ymax=pmax(change_net,0)), fill="green", col="green", alpha=0.5) +
      geom_point()+
      geom_line(aes(y=0)) + ggtitle('Change') +
      ylab('Hits Per Game Deviation From Average') +
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
      all_data <-all_data %>% filter(player == input$playerID) %>%dplyr::select(usg_pct,off_rtg)
      ggplot(all_data) +geom_line(aes(x=off_rtg,y=pred, color = factor(usg_pct)),data=grid())
    }
    else{
      all_data_mlb <-all_data_mlb %>% filter(player == input$playerID) %>%mutate(h_direction=ifelse(h_direction==-1,0,1)) %>% add_predictions(mod2(), type = "response" ) %>%dplyr::select(h,pa,rbi,home_runs,so,pred)
      ggplot(all_data_mlb) +geom_point(aes(x=h,y=pred,color=pa),position='jitter')
    }
  })
  
  output$summary <-renderPrint({
    if(input$league =='MLB'){
      all_data_mlb<- all_data_mlb %>% ungroup() %>% filter(player == input$playerID) %>% dplyr::select(h_streak)
      summary(all_data_mlb)
      
    }
    else{
      all_data <- all_data %>% ungroup() %>% filter(player == input$playerID) %>% dplyr::select(pts_streak)
      summary(all_data)
    }
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
