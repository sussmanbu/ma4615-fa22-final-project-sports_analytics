library(tidyverse)
library(here)
data <-read_csv(here::here("dataset-ignore/raw_nba_data.csv"),show_col_types = FALSE)
data <-data %>%  filter(!row_number() %in% c(110510	))


data <-data %>% mutate(PRA=pts+trb+ast) %>%mutate(usg_pct=usg_pct/100)
all_data <-data  %>% group_by(player)  %>% filter(mean(minutes) >25,minutes >15) %>% mutate(change_net = pts - mean(pts),pts_direction  = 
                                                                                              sign(change_net),change_net_PRA=PRA-mean(PRA),PRA_direction=sign(change_net_PRA)) %>% distinct(player,game_id,.keep_all = TRUE) %>%mutate(pts_streak=ifelse(pts_direction >0,streak_run(pts_direction),0),PRA_streak=ifelse(PRA_direction >0,streak_run(PRA_direction),0))  %>% ungroup() %>% mutate(Team_def_rtg=mean(Opponent_Score/Opponent_pace *100), Opponent_def_rtg=mean(Team_Score/Team_pace *100))
save(all_data, file = here::here("dataset/clean_nba_data_app.RData"))

data22 <-all_data %>% filter(season=='2022')
streak <-data22$pts_streak
indices<-which(diff(sign(diff(streak)))==-2)+1
local_max_nba <-slice(all_data,indices)
all_streaks_2022 <- local_max_nba %>% group_by(player) %>% summarize(max_streak=max(pts_streak)) %>% arrange(desc(max_streak))

save(all_streaks_2022,file = here::here("dataset/clean_nba_streaks_data_app.RData"))
save(data22,file = here::here("dataset/clean_nba_data22_app.RData"))


load(here::here("dataset", "MLB_bat_data_2021.RData"))
all_data_mlb <-MLB_bat_data_2021 %>% group_by(player)  %>% mutate(change_net = h - mean(h),h_direction  = sign(change_net)) %>% distinct(player,game_id,.keep_all = TRUE) %>%mutate(h_streak=ifelse(h_direction >0,streak_run(h_direction),0)) %>% ungroup()

mlb_streak <-all_data_mlb$h_streak
indices<-which(diff(sign(diff(mlb_streak)))==-2)+1
local_max_mlb <-slice(all_data_mlb,indices)
all_streaks_mlb_2022 <- local_max_mlb %>% group_by(player) %>% summarize(max_streak=max(mean(h_streak))) %>% arrange(desc(max_streak))

save(all_streaks_mlb_2022,file = here::here("dataset/clean_mlb_streaks_data_app.RData"))
save(all_data_mlb,file = here::here("dataset/clean_mlb_data_app.RData"))

