##### New Idea (9/3)
#### What if I built a table to shows the efficiency of running a play after another?
################################################################################
rm(list = ls())
library(tidyverse)
library(rvest)
library(ggpattern)
library(shadowtext)
library(vistime)
library(ggthemes)
library(nflverse)
library(showtext)
library(sysfonts)
library(ggtext)
library(ggimage)
library(gt)
library(gtExtras)
################################################################################
play_by_play <- load_pbp(seasons = 2025) %>% 
  clean_pbp() %>% 
  select(game_id,week,season,season_type,posteam,drive,play_id,qtr,down,ydstogo,
         goal_to_go,wp,play_type,play_type_nfl,desc,epa,success,yards_gained,
         pass,rush,pass_attempt,incomplete_pass,qb_scramble,rusher_player_id,
         penalty,penalty_team,penalty_yards,touchdown,field_goal_attempt,
         field_goal_result,extra_point_result,two_point_conv_result,
         safety,sack,interception,fumble,fumble_lost,fourth_down_failed,
         punt_attempt,punt_blocked)

pbp <- play_by_play %>% 
  merge(teams_colors_logos %>% select(team_abbr,team_color, team_color2,team_color3),
        by.x = 'posteam',by.y = 'team_abbr')

## Getting plays that should count towards play sequencing
plays <- pbp %>% 
  filter( #game_id == '2024_01_BAL_KC' &
    !(play_type %in% c('kickoff','no_play','field_goal','extra_point','punt','qb_kneel','qb_spike')) & 
      !is.na(posteam)
  ) %>%
  group_by(season,game_id,posteam) %>%
  mutate(drive_no = dense_rank(drive)) %>% 
  group_by(season,game_id,posteam,drive_no) %>% 
  mutate(play_no = dense_rank(play_id)) %>% 
  ungroup() 

## Now, we have to build out the data for the chart
chart_data <- plays %>% 
  ################################################################################
mutate(
  playType = case_when(
    rush == 1 ~ 'Run',
    play_type %in% c('punt','field_goal') ~ 'ST',
    pass_attempt == 1 | qb_scramble == 1  ~ 'Pass',
    TRUE ~ 'No Play' ),
  logo = paste0('~/Extracurricular/NFL Data/Logos/',posteam,'.png')
) 
################################################################################
seq_epa <- chart_data %>%
  ################################################################################
filter(pass == 1 | rush == 1) %>%
  select(game_id,week,season_type,posteam,qtr,down,drive_no, play_no, 
         play_type,playType,desc, epa, success, wp) %>% 
  arrange(game_id,posteam,drive_no,play_no) %>% 
  group_by(posteam,drive_no) %>% 
  mutate(t_last_play = lag(playType),
         t_next_play = lead(playType),
         t_last_down = lag(down),
         t_next_down = lead(down),
         seq_as_start = paste(playType,t_next_play,sep = '-'),
         seq_as_end = lag(seq_as_start)
  )
################################################################################

s <- c('Pass-Pass','Run-Run','Pass-Run','Run-Pass')
all_seq <- c()
for (i in 1:4) {
  seq_data <- s[i]
  
  # First play in sequence
  Seq_1 <- seq_epa %>% 
    filter((seq_as_start == seq_data | seq_as_end == seq_data)) %>% 
    mutate(seq_group = seq_data)
  
  all_seq <- bind_rows(all_seq,Seq_1)
}

all_seq %>% write_csv('Extracurricular/NFL Data/Play_Sequence/All Seq.csv')
