# This notebook contains the code for cricWAR estimates, 
# such as runs above average (RAA), 
# value over replacement player (VORP), wins above replacement (WAR). 
# In addition we provide the code for uncertainty estimation of the metrics mentioned above.
# Results are shown with some plots and tables. 


# Libraries -------------------------------------------------
#devtools::install_github("ropenscilabs/cricketdata")
library(tidyverse)
library(cricketdata)
library(ggplot2)
library(ggthemes)
library(ggridges)

# Reading Cricsheet Data ------------------------


bbb_ipl_df <- fetch_cricsheet("bbb", "male", "ipl")
match_ipl_df <- fetch_cricsheet("match", "male", "ipl")
player_cricsheet_df <- fetch_cricsheet("player", "male", "ipl")

# Meta data for IPL players 
# taken from player_meta df from cricketdata R package and pre-processed to resolve naming clashes
ipl_player_regseason_meta_df = read.csv("ipl_player_regseason_meta.csv")




# Data Pre-Processing ---------------------------------


# Filtering different seasons data for evaluating expected runs (ER)
bbb.ER.df = bbb_ipl_df %>%
  mutate(ball_in_over_avgcal = ifelse(ball_in_over == 0, 1, ball_in_over)) %>%
  filter(season %in% c("2015", "2016", "2017", "2018", "2019", "2021", "2022"))


# Player meta data processed
players_meta_df = ipl_player_regseason_meta_df %>%
  dplyr::select(player, season, batting_style, bowling_style, playing_role)

players_meta_df = players_meta_df %>%
  mutate(bowling_style = tolower(bowling_style)) %>%
  mutate(bowling_hand = case_when(
    grepl("right", bowling_style ) ~ "Right",
    grepl("leg", bowling_style ) ~ "Right",
    grepl("left", bowling_style ) ~ "Left",
    TRUE ~ as.character(bowling_style)
  ),
  bowling_speed = case_when(
    grepl("medium" , bowling_style) | grepl("fast" , bowling_style) ~ "pace",
    grepl("leg" , bowling_style) | grepl("off" , bowling_style) | grepl("slow" , bowling_style) | grepl("spin" , bowling_style)  ~ "spin",
  )
  )


# Expected Runs (ER) calculations -------------------------

ER.over.df = bbb.ER.df %>%
  group_by(over,  wickets_lost_yet) %>%
  #summarize(Count = n(), Q1= summary(runs_off_bat)[2], Q2= summary(runs_off_bat)[3], Avg.R = mean(runs_off_bat), Q3 = summary(runs_off_bat)[5], variation = sd(runs_off_bat), .groups = "drop")
  summarize(Count = n(),  ER.over = mean(runs_off_bat),  .groups = "drop")


Weighted.Mean.over.R = sum(ER.over.df $Count * ER.over.df $ER.over)/sum(ER.over.df $Count)
Weighted.Mean.over.R

# Leverage Index
ER.over.df  = ER.over.df   %>%
  mutate(Leverage.over = ER.over/Weighted.Mean.over.R)



# cricWAR function evaluates RAA, VORP, and WAR -------------------------
# INPUTS : bbb_all_df = ball-by-ball data, match_all_df = match meta data, ER.over.df = expected runs, 
#          players_meta_df = player meta data such as bowling/batting hand, bowling pace,
#          seasons_to_consider = seasons for which cricWAR results are needed, 
#          Batter.No = no. of league level batters related to value over replacement, Bowler.No = no. of league level bowlers,
#          RPW = runs per win estimate
#                      
# OUTPUT : a df containing RAA, VORP, WAR for players by season, team along w some other stats

get_cricWAR = function(bbb_all_df, match_all_df, ER.over.df, players_meta_df, seasons_to_consider, Batter.No, Bowler.No, RPW){
  
  output_df = tibble()
  
  
  for (season_current in seasons_to_consider) {
    
    # Filtering the regular season data (w/o playoffs) for which results will be reported
    bbb.df = bbb_all_df %>%
      filter(season == season_current, innings  %in% c(1,2)) %>%   #  first two innings only
      inner_join(match_all_df %>%
                   filter(season == season_current, !is.na(match_number)) %>%  # regular season games only
                   dplyr::select(match_id,season ) %>%
                   mutate(match_id = as.integer(match_id)), by = c("match_id", "season"),
                   ball_in_over_avgcal = ifelse(ball_in_over == 0, 1, ball_in_over))
    
    
    
    
    
    # Joining season ball-by-ball, player and expected runs data
    
    bbb.df = bbb.df %>%
      inner_join(players_meta_df  %>% filter(season == season_current)  %>% dplyr::select(player, batting_style, playing_role), by = c("striker" = "player" ) ) %>%
      inner_join(players_meta_df  %>% filter(season == season_current) %>% dplyr::select(player, bowling_style, bowling_hand, bowling_speed), by = c("bowler" = "player" ) ) %>%
      inner_join(ER.over.df, by = c("over", "wickets_lost_yet")) %>%
      mutate(batting_hand = ifelse(grepl("Right", batting_style ), "Right", "Left"),
             platoon = ifelse( bowling_hand == batting_hand, "No", "Yes"),
             wides = ifelse(is.na(wides), 0 , wides), 
             noballs = ifelse(is.na(noballs), 0 , noballs),
             Delta_Batting = ifelse(wides == 0 , runs_off_bat - ER.over, 0),  #batting run value
             Delta_Bowling = ER.over - (runs_off_bat + wides + noballs),  #bowling run value
             Delta_Batting.Lev = ifelse( Leverage.over <= 1e-5, ifelse(abs(Delta_Batting) <= 1e-5,  Delta_Batting/ (Leverage.over + 1e-5) , Delta_Batting ), Delta_Batting/(Leverage.over) ), # leveraged run value and avoiding dividing by 0 
             Delta_Bowling.Lev = ifelse( Leverage.over <= 1e-5, ifelse(abs(Delta_Bowling) <= 1e-5,  Delta_Bowling/ (Leverage.over + 1e-5) , Delta_Bowling ), Delta_Bowling/(Leverage.over) ) ) # leveraged run value and avoiding dividing by 0 
    
    
    # REGRESSION for adjusting run values
    
    bbb.reg.df =    bbb.df %>%
      dplyr::select( Delta_Batting, Delta_Bowling,
                     Delta_Batting.Lev, Delta_Bowling.Lev,
                     platoon, bowling_speed,
                     innings, venue ) %>%
      mutate(innings = as.factor(innings))
    
    # Batting run value adjustment
    delta.bat.lm = lm (Delta_Batting ~   platoon + bowling_speed + innings + venue , data = bbb.reg.df )
    
    # Bowling run value adjustment
    delta.bowl.lm = lm (Delta_Bowling ~ platoon + bowling_speed + innings + venue , data = bbb.reg.df )
    
    # Leveraged Batting run value adjustment
    delta.bat.lev.lm = lm (Delta_Batting.Lev ~ platoon + bowling_speed + innings + venue   , data = bbb.reg.df )
    
    # Leveraged Bowling run value adjustment
    delta.bowl.lev.lm = lm (Delta_Bowling.Lev ~platoon + bowling_speed + innings + venue , data = bbb.reg.df )
    
    
    bbb.df = bbb.df %>%
      mutate( Delta_Batting_Adjusted = delta.bat.lm$residuals,
              Delta_Bowling_Adjusted = delta.bowl.lm$residuals,
              Delta_Batting_Lev_Adjusted = delta.bat.lev.lm$residuals,
              Delta_Bowling_Lev_Adjusted = delta.bowl.lev.lm$residuals)
    
    
    # RAA, VORP and WAR Calculations 
    
    
    # All players RAA evaluation
    Total.RAA.df = bbb.df %>%
      group_by(striker) %>%  # batters
      summarise(Team = unique(batting_team),   Count_Bat = n() - sum(wides != 0) , Runs_Bat = sum(runs_off_bat), SR = Runs_Bat*100/Count_Bat, 
                Bound_scored_perc = ifelse(is.na(Count_Bat), 0, sum(runs_off_bat %in% c(4,6))/Count_Bat), 
                RunValue_Bat = sum(Delta_Batting), RunValue_Lev_Bat = sum(Delta_Batting.Lev),
                RAA_Bat = sum(Delta_Batting_Adjusted), RAA_Lev_Bat = sum(Delta_Batting_Lev_Adjusted)) %>%
      full_join(bbb.df %>%
                  mutate(wides = ifelse(is.na(wides), 0 , wides), 
                         noballs = ifelse(is.na(noballs), 0 , noballs)) %>%
                  group_by(bowler) %>%   #bowlers
                  summarise(Team = unique(bowling_team), Count_Bowl = n() - sum(wides != 0) - sum(noballs != 0) , R.Given = sum(runs_off_bat + wides + noballs),
                            Eco = R.Given*6/ Count_Bowl, 
                            #Bound_conceded_perc = ifelse(is.na(Count_Bowl), 0, sum(runs_off_bat %in% c(4,6))/Count_Bowl),
                            RunValue_Bowl = sum(Delta_Bowling),  RunValue_Lev_Bowl = sum(Delta_Bowling.Lev),
                            RAA_Bowl = sum(Delta_Bowling_Adjusted), RAA_Lev_Bowl = sum(Delta_Bowling_Lev_Adjusted)),
                by = c("striker" = "bowler")) %>%
      mutate(RAA_Bat = ifelse(is.na(RAA_Bat), 0, RAA_Bat),
             RAA_Bowl = ifelse(is.na(RAA_Bowl), 0, RAA_Bowl),
             RAA_Total = RAA_Bat + RAA_Bowl,
             RAA_Lev_Bat = ifelse(is.na(RAA_Lev_Bat), 0, RAA_Lev_Bat),
             RAA_Lev_Bowl = ifelse(is.na(RAA_Lev_Bowl), 0, RAA_Lev_Bowl),
             RAA_Total_Lev = RAA_Lev_Bat  + RAA_Lev_Bowl) %>%
      rename(Player = striker) %>%
      mutate(Team  = ifelse( is.na(Team.x), Team.y, Team.x)) 
    
    Total.RAA.df = Total.RAA.df %>%
      dplyr::select(-Team.x, -Team.y) 
    
    Total.RAA.df = Total.RAA.df %>%
      dplyr::select(Player, Team, names(Total.RAA.df)[2:18])
    
    
    
    # Selecting the no. of professional players and the remaining to be used as replacement players
    
    Teams = length(unique(Total.RAA.df$Team))
    #Batter.No = top numbers of league level batters
    #Bowler.No = top numbers of league level bowlers
    
    # Finding the lower limit of balls batted and balls bowled by league-level players
    Bat.limit = sort(Total.RAA.df$Count_Bat, decreasing = TRUE)[(Teams*Batter.No)]
    Bowler.limit = sort(Total.RAA.df$Count_Bowl, decreasing = TRUE)[(Teams*Bowler.No)]
    
    
    # Top League (Professional) Players 
    Professional.RAA.df = Total.RAA.df %>%
      left_join(bbb.df %>%
                  group_by(striker) %>%
                  summarise(Game_Count_Bat = length(unique(match_id))) %>%
                  mutate(Status = "Batter"), by = c("Player" = "striker")) %>%
      left_join(bbb.df %>%
                  group_by(bowler) %>%
                  summarise(Game_Count_Bowl = length(unique(match_id))) %>%
                  mutate(Status = "Bowler"), by = c("Player" = "bowler")) %>%
      filter( (Count_Bat >= Bat.limit ) | (Count_Bowl >= Bowler.limit)  ) %>%
      mutate(Player_type = "League")
    
    
    # Replacement level player average RAA calculation
    
    Replacement.RAA.df = Total.RAA.df %>%
      anti_join(Total.RAA.df %>%
                  left_join(bbb.df %>%
                              group_by(striker) %>%
                              summarise(Game_Count_Bat = length(unique(match_id))) %>%
                              mutate(Status = "Batter"), by = c("Player" = "striker")) %>%
                  left_join(bbb.df %>%
                              group_by(bowler) %>%
                              summarise(Game_Count_Bowl = length(unique(match_id))) %>%
                              mutate(Status = "Bowler"), by = c("Player" = "bowler")) %>%
                  filter( (Count_Bat >= Bat.limit ) | (Count_Bowl >= Bowler.limit)  ), by = c("Player")) %>%
      mutate(Player_type = "Replacement")
    
    
    # Labeling league and replacement level players
    player_type = Professional.RAA.df %>%
      dplyr::select(Player, Player_type) %>%
      rbind(Replacement.RAA.df %>%
              dplyr::select(Player, Player_type)) %>%
      arrange(desc(Player))
    
    Replacement.RAA.df = Replacement.RAA.df %>%
      summarise(Avg.RAA = sum(RAA_Total, na.rm = TRUE)/(sum(Count_Bowl, na.rm = TRUE)+sum(Count_Bat, na.rm = TRUE)),
                Avg.RAA_Lev = sum(RAA_Total_Lev, na.rm = TRUE)/(sum(Count_Bowl, na.rm = TRUE)+sum(Count_Bat, na.rm = TRUE)))
    
    # Value over Replacement (VORP) calculation
    
    Professional.RAA.df = Professional.RAA.df %>%
      mutate(Balls.Played = ifelse(is.na(Count_Bat), 0, Count_Bat) + ifelse(is.na(Count_Bowl), 0, Count_Bowl)) %>%
      mutate(Replacement_Player = "No", VOP_RAA = RAA_Total - Balls.Played * Replacement.RAA.df$Avg.RAA, 
             VOP_RAA_Leg = RAA_Total_Lev - Balls.Played * Replacement.RAA.df$Avg.RAA_Lev) 
    
    Total.RAA.df = Total.RAA.df %>%
      mutate(Balls.Played = ifelse(is.na(Count_Bat), 0, Count_Bat) + ifelse(is.na(Count_Bowl), 0, Count_Bowl)) %>%
      mutate(RAA_replacement = Balls.Played * Replacement.RAA.df$Avg.RAA,
             VOP_RAA = RAA_Total - Balls.Played * Replacement.RAA.df$Avg.RAA, 
             VOP_RAA_Leg = RAA_Total_Lev - Balls.Played * Replacement.RAA.df$Avg.RAA_Lev)

    
    
    # Wins above replacement (WAR) calculation
    #RPW = runs per win 
    
     Total.RAA.df = Total.RAA.df %>%
      left_join(player_type, by = "Player") %>%
      mutate(cricWAR = VOP_RAA_Leg/RPW, 
             season = season_current)
    
    output_df = output_df %>%rbind(Total.RAA.df)
  }
  
  return( output_df)  
} 


# cricWAR OUTPUT & RESULTS ----------------------------------
seasons_to_consider = c("2015", "2016", "2017", "2018", "2019", "2021", "2022")
Batter.No =  8 # no. of league level batters related to value over replacement
Bowler.No = 8  # no. of league level bowlers related to value over replacement
RPW = 84.5  #from Runs Per Win model

# Calling get_cricWAR to get the cricWAR estimates
cricwar_ouput = get_cricWAR(bbb_ipl_df, match_ipl_df, ER.over.df, players_meta_df, seasons_to_consider, 8,8,84.5)

# Viewing the 2019 season cricWAR results
cricwar_ouput %>%
  dplyr::select(Player, season, everything()) %>%
  filter(season == "2019") %>%
  view()

cricwar_ouput %>%
  dplyr::select(Player, season, everything()) %>%
  filter(season == "2019") %>%
  dplyr::select(Player, Team, season,  RAA_Lev_Bat) %>%
  arrange(desc(RAA_Lev_Bat)) %>%
  head() 

cricwar_ouput %>%
  dplyr::select(Player, season, everything()) %>%
  filter(season == "2019") %>%
  dplyr::select(Player, Team, season, RAA_Lev_Bowl) %>%
  arrange(desc(RAA_Lev_Bowl)) %>%
  head() 

# RAA Vs Playing Time Plot (2019 season)

cricwar_ouput %>%
  dplyr::select(Player, season, everything()) %>%
  filter(season == "2019") %>%
  ggplot(aes(x = Balls.Played, y = RAA_Total_Lev, color = Player_type )) +
  geom_point(size = 2.5) +
  scale_color_manual(values=c("#013369", "#D50A0A")) +
  scale_y_continuous(breaks = c(-100, -50, 0, 50 ,100), limits = c(-100, 125)) +
  geom_point(aes(x = Balls.Played, y = RAA_replacement), color= "black", alpha= 0.25, size  = 2 ) +
  geom_abline(intercept = 0, slope = 0, color="black", linetype="solid", size=0.75) +
  geom_text(aes(x=323, y = 116.77, label ="JJ Bumrah"),
            color = "#013369",  fontface = "bold", size = 3.5, nudge_y = 5) +
  geom_segment(aes(x = 323, y = -61.06, xend = 323, yend = 116.77)) + 
  geom_text(aes(x=426, y = 63.79, label ="HH Pandya"),
            color = "#013369",  fontface = "bold", size = 3.5, nudge_y = 5) +
  geom_segment(aes(x = 426, y = -80.5, xend = 426, yend = 63.79)) +
  geom_text(aes(x=242, y = -75.81, label ="AT Rayudu"),
            color = "#013369",  fontface = "bold", size = 3.5, nudge_y = - 5) +
  geom_segment(aes(x = 242, y = -45.75, xend = 242, yend = -75.81)) +
  # theme_538() +
  theme_fivethirtyeight() +
  theme(legend.position = c(0.25, 0.75), legend.title=element_blank())+
  labs( x = "Playing Time (balls faced as a batter plus balls bowled as a bowler)", y = " cricWAR runs above average",
        title = "RAA Vs Playing Time", subtitle = "IPL 2019 regular season ")

# RAA Vs Strike Rate %

cricwar_ouput %>%
  group_by(season) %>%
  filter(Count_Bat >= 60) %>%
  summarise(cor(RAA_Lev_Bat/Count_Bat,SR ))

cricwar_ouput %>%
  filter(Count_Bat >= 60) %>%
  ggplot(aes(x = RAA_Lev_Bat, y = SR, size = Count_Bat)) +
  geom_point(alpha= 0.5) +
  geom_smooth(method = lm) +
  #scale_y_continuous(labels = scales::percent) +
  labs(title = "Strike Rate Vs Batting RAA")


# Auto-correlation between consecutive seasons

cricwar_ouput %>%
  filter(season == "2015") %>%
  inner_join(cricwar_ouput %>%
               filter(season == "2016"), by = c("Player")) %>%
  summarise(WAR = cor(cricWAR.x, cricWAR.y), 
            Count = n()
  )

#  UNCERTAINTY IN RAA & WAR ------------------------------------------


# uncertainty WAR function for uncertainty estimation in RAA, VORP, and WAR -------------------------
# INPUTS : bbb_all_df = ball-by-ball data, match_all_df = match meta data, ER.over.df = expected runs, 
#          players_meta_df = player meta data such as bowling/batting hand, bowling pace,
#          season_to_consider = a single season for which cricWAR uncertainty estimates are needed,
#          n = no. of seasons to be simulated,
#          Batter.No = no. of league level batters related to value over replacement, Bowler.No = no. of league level bowlers,
#          RPW = runs per win estimate
#                      
# OUTPUT : a list containing dfs containing RAA, VORP, WAR estimates for each player

uncertainty_WAR = function(bbb_all_df, match_all_df, ER.over.df, players_meta_df, season_to_consider,n, Batter.no, Bowler.no, RPW) {
  
  
  
  # Filtering the regular season data (w/o playoffs) for which results will be reported
  bbb.df = bbb_all_df %>%
    filter(season == season_to_consider, innings  %in% c(1,2)) %>%   # filtering season to be considered and first two innings only
    inner_join(match_all_df %>%
                 filter(season == season_to_consider, !is.na(match_number)) %>%  # regular season games only
                 dplyr::select(match_id,season ) %>%
                 mutate(match_id = as.integer(match_id)), by = c("match_id", "season"),
               ball_in_over_avgcal = ifelse(ball_in_over == 0, 1, ball_in_over))
  
  
  
  
  for (i in 1:n) { 
    
    # simulating a season w replacement
    bbb.df.resample = bbb.df %>%
      #group_by(striker) %>%
      sample_frac(size = 1, replace = TRUE)
    
    # Joining result season bbb and player data
    
    bbb.df.resample = bbb.df.resample %>%
      inner_join(players_meta_df  %>% filter(season == season_to_consider)  %>% dplyr::select(player, batting_style, playing_role), by = c("striker" = "player" ) ) %>%
      inner_join(players_meta_df  %>% filter(season == season_to_consider) %>% dplyr::select(player, bowling_style, bowling_hand, bowling_speed), by = c("bowler" = "player" ) ) %>%
      inner_join(ER.over.df, by = c("over", "wickets_lost_yet")) %>%
      mutate(batting_hand = ifelse(grepl("Right", batting_style ), "Right", "Left"),
             platoon = ifelse( bowling_hand == batting_hand, "No", "Yes"),
             wides = ifelse(is.na(wides), 0 , wides), 
             noballs = ifelse(is.na(noballs), 0 , noballs),
             Delta_Batting = ifelse(wides == 0 , runs_off_bat - ER.over, 0),
             Delta_Bowling = ER.over - (runs_off_bat + wides + noballs),
             Delta_Batting.Lev = ifelse( Leverage.over <= 1e-5, ifelse(abs(Delta_Batting) <= 1e-5,  Delta_Batting/ (Leverage.over + 1e-5) , Delta_Batting ), Delta_Batting/(Leverage.over) )   ,
             Delta_Bowling.Lev = ifelse( Leverage.over <= 1e-5, ifelse(abs(Delta_Bowling) <= 1e-5,  Delta_Bowling/ (Leverage.over + 1e-5) , Delta_Bowling ), Delta_Bowling/(Leverage.over) ) )
    
    
    # REGRESSION for adjusting run values
    
    bbb.reg.df =    bbb.df.resample %>%
      dplyr::select( Delta_Batting, Delta_Bowling,
                     Delta_Batting.Lev, Delta_Bowling.Lev,
                     platoon, bowling_speed,
                     innings, venue ) %>%
      mutate(innings = as.factor(innings))
    
    
    delta.bat.lm = lm (Delta_Batting ~   platoon + bowling_speed + innings + venue , data = bbb.reg.df )
    
    
    delta.bowl.lm = lm (Delta_Bowling ~ platoon + bowling_speed + innings + venue , data = bbb.reg.df )
    
    delta.bat.lev.lm = lm (Delta_Batting.Lev ~ platoon + bowling_speed + innings + venue   , data = bbb.reg.df )
    
    delta.bowl.lev.lm = lm (Delta_Bowling.Lev ~platoon + bowling_speed + innings + venue , data = bbb.reg.df )
    
    
    bbb.df.resample = bbb.df.resample %>%
      mutate( Delta_Batting_Adjusted = delta.bat.lm$residuals,
              Delta_Bowling_Adjusted = delta.bowl.lm$residuals,
              Delta_Batting_Lev_Adjusted = delta.bat.lev.lm$residuals,
              Delta_Bowling_Lev_Adjusted = delta.bowl.lev.lm$residuals)  
    
    
    # RAA evaluation
    Total.RAA.resample = bbb.df.resample %>%
      group_by(striker) %>%
      summarise(Team = unique(batting_team),   Count_Bat = n() - sum(wides != 0) , Runs_Bat = sum(runs_off_bat), SR = Runs_Bat*100/Count_Bat, 
                RunValue_Bat = sum(Delta_Batting), RunValue_Lev_Bat = sum(Delta_Batting.Lev),
                RAA_Bat = sum(Delta_Batting_Adjusted), RAA_Lev_Bat = sum(Delta_Batting_Lev_Adjusted)) %>%
      full_join(bbb.df.resample %>%
                  mutate(wides = ifelse(is.na(wides), 0 , wides), 
                         noballs = ifelse(is.na(noballs), 0 , noballs)) %>%
                  group_by(bowler) %>%
                  summarise(Team = unique(bowling_team), Count_Bowl = n() - sum(wides != 0) - sum(noballs != 0) , R.Given = sum(runs_off_bat + wides + noballs),
                            Eco = R.Given*6/ Count_Bowl, 
                            RunValue_Bowl = sum(Delta_Bowling),  RunValue_Lev_Bowl = sum(Delta_Bowling.Lev),
                            RAA_Bowl = sum(Delta_Bowling_Adjusted), RAA_Lev_Bowl = sum(Delta_Bowling_Lev_Adjusted)),
                by = c("striker" = "bowler")) %>%
      mutate(RAA_Bat = ifelse(is.na(RAA_Bat), 0, RAA_Bat),
             RAA_Bowl = ifelse(is.na(RAA_Bowl), 0, RAA_Bowl),
             RAA_Total = RAA_Bat + RAA_Bowl,
             RAA_Lev_Bat = ifelse(is.na(RAA_Lev_Bat), 0, RAA_Lev_Bat),
             RAA_Lev_Bowl = ifelse(is.na(RAA_Lev_Bowl), 0, RAA_Lev_Bowl),
             RAA_Total_Lev = RAA_Lev_Bat  + RAA_Lev_Bowl) %>%
      rename(Player = striker) %>%
      mutate(Team  = ifelse( is.na(Team.x), Team.y, Team.x))
    
    Total.RAA.resample = Total.RAA.resample %>%
      dplyr::select(-Team.x, -Team.y) 
    
    
    Teams = length(unique(Total.RAA.resample$Team))
    #Batter.No = top numbers of league level batters
    #Bowler.No = top numbers of league level bowlers
    
    # Finding the lower limit of balls batted and balls bowled by league-level players
    Bat.limit = sort(Total.RAA.resample$Count_Bat, decreasing = TRUE)[(Teams*Batter.No)]
    Bowler.limit = sort(Total.RAA.resample$Count_Bowl, decreasing = TRUE)[(Teams*Bowler.No)]
    
    
    # League level players in the season
    Professional.RAA.resample = Total.RAA.resample %>%
      left_join(bbb.df.resample %>%
                  group_by(striker) %>%
                  summarise(Game_Count_Bat = length(unique(match_id))) %>%
                  mutate(Status = "Batter"), by = c("Player" = "striker")) %>%
      left_join(bbb.df.resample %>%
                  group_by(bowler) %>%
                  summarise(Game_Count_Bowl = length(unique(match_id))) %>%
                  mutate(Status = "Bowler"), by = c("Player" = "bowler")) %>%
      filter( (Count_Bat >= Bat.limit ) | (Count_Bowl >= Bowler.limit)  ) 
    
    # Replacement level players in the season
    Replacement.RAA.resample = Total.RAA.resample %>%
      anti_join(Total.RAA.resample %>%
                  left_join(bbb.df.resample %>%
                              group_by(striker) %>%
                              summarise(Game_Count_Bat = length(unique(match_id))) %>%
                              mutate(Status = "Batter"), by = c("Player" = "striker")) %>%
                  left_join(bbb.df.resample %>%
                              group_by(bowler) %>%
                              summarise(Game_Count_Bowl = length(unique(match_id))) %>%
                              mutate(Status = "Bowler"), by = c("Player" = "bowler")) %>%
                  filter( (Count_Bat >= Bat.limit ) | (Count_Bowl >= Bowler.limit)  ), by = c("Player")) %>%
      #view()
      summarise(Avg.RAA = sum(RAA_Total, na.rm = TRUE)/(sum(Count_Bowl, na.rm = TRUE)+sum(Count_Bat, na.rm = TRUE)),
                Avg.RAA_Lev = sum(RAA_Total_Lev, na.rm = TRUE)/(sum(Count_Bowl, na.rm = TRUE)+sum(Count_Bat, na.rm = TRUE)))
    
    Total.RAA.resample = Total.RAA.resample %>%
      mutate(Balls.Played = ifelse(is.na(Count_Bat), 0, Count_Bat) + ifelse(is.na(Count_Bowl), 0, Count_Bowl)) %>%
      mutate(VOP_RAA = RAA_Total - Balls.Played * Replacement.RAA.resample$Avg.RAA, 
             VOP_RAA_Leg = RAA_Total_Lev - Balls.Played * Replacement.RAA.resample$Avg.RAA_Lev,
             cricWAR = VOP_RAA_Leg/RPW )
    
    if (i == 1){
      RAA = Total.RAA.resample %>%
        dplyr::select(Player)
      
      VORP = Total.RAA.resample %>%
        dplyr::select(Player)
      
      WAR = Total.RAA.resample %>%
        dplyr::select(Player)
    }
    
    
    #RAA[ , ncol(RAA) + 1] <- Total.RAA.resample$RAA_Total_Lev  # Append new column
    RAA = RAA %>%
      left_join(Total.RAA.resample %>%
                  dplyr::select(Player, RAA_Total_Lev ), by = c("Player"))
    colnames(RAA)[ncol(RAA)] <- paste0("RAA", i)  # Rename column name
    
    #VORP[ , ncol(VORP) + 1] <- Total.RAA.resample$VOP_RAA_Leg  # Append new column
    VORP = VORP %>%
      left_join(Total.RAA.resample %>%
                  dplyr::select(Player, VOP_RAA_Leg ), by = c("Player"))
    colnames(VORP)[ncol(VORP)] <- paste0("VORP", i)  # Rename column name
    
    #WAR[ , ncol(WAR) + 1] <- Total.RAA.resample$cricWAR  # Append new column
    WAR = WAR %>%
      left_join(Total.RAA.resample %>%
                  dplyr::select(Player, cricWAR), by = c("Player"))
    colnames(WAR)[ncol(WAR)] <- paste0("WAR", i)  # Rename column name
    
    print(paste("Iteration", i))
  }
  
  
  RAA = RAA %>%
    pivot_longer(
      cols = starts_with("RAA"),
      names_to = "Iteration",
      names_prefix = "RAA",
      values_to = "RAA",
      values_drop_na = TRUE
    )
  
  VORP = VORP %>%
    pivot_longer(
      cols = starts_with("VORP"),
      names_to = "Iteration",
      names_prefix = "VORP",
      values_to = "VORP",
      values_drop_na = TRUE
    )
  
  WAR = WAR %>%
    pivot_longer(
      cols = starts_with("WAR"),
      names_to = "Iteration",
      names_prefix = "WAR",
      values_to = "WAR",
      values_drop_na = TRUE
    )
  
  result = list(RAA = RAA, VORP = VORP, WAR = WAR)
  return(result)
  
}


# UNCERTAINTY OUTPUT & RESULTS ------------------------------------------ 
season_to_consider = c("2019")
Batter.No =  8
Bowler.No = 8
RPW = 84.5
n = 10

Result = uncertainty_WAR(bbb_ipl_df, match_ipl_df, ER.over.df, players_meta_df, season_to_consider,n, Batter.No, Bowler.No, RPW)

RAA_uncertainty = Result$RAA
VORP_uncertainty = Result$VORP
WAR_uncertainty = Result$WAR


#theme
theme_538 <- function(base_size = 12, base_family = "Chivo") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      # drop minor gridlines and axis-ticks
      panel.grid.minor = element_blank(),
      axis.ticks = element_blank(),
      # change font elements/family
      text = element_text(family = "Chivo", size = base_size),
      axis.text = element_text(face = "bold", color = "grey", size = base_size),
      axis.title = element_text(face = "bold", size = rel(1.33)),
      axis.title.x = element_text(margin = margin(0.5, 0, 0, 0, unit = "cm")),
      axis.title.y = element_text(margin = margin(0, 0.5, 0, 0, unit = "cm"), angle =90),
      plot.title = element_text(face = "bold", size = rel(1.67), hjust = 0),
      plot.title.position = "plot",
      plot.subtitle = element_text(size = 16, margin = margin(0.2, 0, 1, 0, unit = "cm"), hjust = 0),
      plot.caption = element_text(size = 10, margin = margin(1, 0, 0, 0, unit = "cm"), hjust = 1),
      # change plot colors for the background/panel elements
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.major =  element_line(color = "#d0d0d0"),
      panel.border = element_blank(),
      # shrinks margin and simplify the strip/facet elements
      plot.margin = margin(0.5, 1, 0.5, 1, unit = "cm"),
      strip.background = element_blank(),
      strip.text = element_text(size = rel(1.33), face = "bold")
    )
}

# Density Plots for RAA uncertainty
RAA_uncertainty %>%
  filter(Player %in% c("RR Pant", "PA Patel", "DA Warner", "JM Bairstow", "CH Gayle" )) %>%
  ggplot(aes(x = RAA, color = Player)) +
  geom_density(alpha=0.4) +
  #theme_538() +
  theme_fivethirtyeight() +
  # theme(legend.position="bottom", legend.direction="horizontal",axis.text = element_text(face="bold") ) +
  labs(title = "Runs Above Average Uncertainty", subtitle = "Batters from IPL 2019", 
       x = "RAA")


# WAR Uncertainty Density plot
WAR_uncertainty %>%
  filter(Player %in% c("RR Pant", "PA Patel", "DA Warner", "JM Bairstow", "CH Gayle" )) %>%
  ggplot(aes(x = WAR, y = Player)) +
  geom_density_ridges(alpha=0.65, scale = 1, fill ="#013369") +
  #theme_538() +
  theme_fivethirtyeight() +
  theme(legend.position="bottom", legend.direction="horizontal",axis.text = element_text(face="bold") ) +
  labs(title = "Batters", 
       #subtitle = "IPL 2019", 
       x = "WAR", y = "")


# 95% confidence interval width for uncertainty estiamtes

WAR_uncertainty %>%
  filter(Player %in% c("AD Russell", "JC Archer", "JJ Bumrah", "HH Pandya",  "SP Narine", "CH Gayle" , "DA Warner",
                       "Rashid Khan", "RR Pant", "R Ashwin")) %>%
  group_by(Player) %>%
  summarise(q2.5 = unname(quantile(WAR,na.rm = T,probs = c(0.025))),
            q25 = unname(quantile(WAR,na.rm = T,probs = c(0.25))),
            q50 = unname(quantile(WAR,na.rm = T,probs = c(0.5))),
            q75 = unname(quantile(WAR,na.rm = T,probs = c(0.75))),
            q97.5 = unname(quantile(WAR,na.rm = T,probs = c(0.975)))) %>%
  mutate(conf.interval.width = (q97.5 - q2.5)/2) %>%
  arrange(desc(conf.interval.width)) 




