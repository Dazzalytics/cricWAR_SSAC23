#devtools::install_github("ropenscilabs/cricketdata")
library(cricketdata)
library(ggplot2)
library(tidyverse)

# This notebook evaluated the Runs per Win for cricWAR framework. We present two
# approaches (1) Run Differential model (2) Pythagorean expectation win formula




# Reading data -----------------

match_df <- fetch_cricsheet("match", "male", "ipl")

bbb_ipl_df <- fetch_cricsheet("bbb", "male", "ipl")



# RUNS PER WIN using RUN DIFFERENTIAL MODEL-------------------------

# evaluating innings totals using ball-by-ball data
innings_total = bbb_ipl_df %>%
  filter(season %in% c("2015", "2016", "2017", "2018", "2019", "2021", "2022")) %>%
  group_by(match_id, innings) %>%
  summarise(total_score = sum(runs_off_bat + extras), .groups = "drop" ) %>%
  filter(innings %in% c(1,2))

# changing shape of innings total df
innings_total = innings_total %>%
  pivot_wider(
    names_from  = "innings",
    values_from = c("total_score")
  ) %>% 
  rename(innings1_total = "1", innings2_total = "2")


# merging innings total df with match df
match_innings_df = innings_total %>%
  inner_join(match_df %>% mutate(match_id = as.integer(match_id)), by = "match_id")


# Processing for regular season games with a result and figuring out which team won
match_innings_regseason_df = match_innings_df %>%
  mutate(batting_first = case_when(
    toss_winner == team1 & toss_decision == "field" ~ team2,
    toss_winner == team1 & toss_decision == "bat" ~ team1,
    toss_winner == team2 & toss_decision == "field" ~ team1,
    toss_winner == team2 & toss_decision == "bat" ~ team2),
    batting_second = ifelse(batting_first == team1, team2, team1)) %>%
  dplyr::select(-gender, - player_of_match, -match_referee, -umpire1, -umpire2,
                -reserve_umpire, -tv_umpire) %>%
  filter(is.na(outcome)) %>%    # removing the tied/no result games
  filter(!is.na(match_number)) %>%  # selecting regular season games only
  mutate(bat_first_winner = ifelse(batting_first == winner, 1 , 0),
         bat_second_winner = ifelse(bat_first_winner == 0, 1, 0))

# Evaluating total runs scored and conceded for score differential along with wins and win %
Win.percent.regseason = match_innings_regseason_df %>%
  ungroup() %>%
  group_by(batting_first, season) %>%
  #group_by(batting_first) %>%
  summarise(Count1 = n(), Runs.Scored1 = sum(innings1_total), Runs.Conceded1 = sum(innings2_total), Wins.1 = sum(bat_first_winner),
            RS1.sd = sd(innings1_total), RC1.sd = sd(innings2_total) )%>%
  rename(Team = batting_first) %>%
  inner_join(match_innings_regseason_df %>%
               group_by(batting_second, season) %>%
               #group_by(batting_second) %>%
               summarise(Count2 = n(), Runs.Scored2 = sum(innings2_total), Runs.Conceded2 = sum(innings1_total), Wins.2 = sum(bat_second_winner),
                         RS2.sd = sd(innings2_total), RC2.sd = sd(innings1_total))%>%
               rename(Team = batting_second), 
             by = c("Team", "season")) %>%
  #by = c("Team")) %>%
  mutate(Total.RS = Runs.Scored1 + Runs.Scored2, Total.RC = Runs.Conceded1 + Runs.Conceded2, TotalRS.sd = sqrt(RS1.sd^2 + RS2.sd^2), TotalRC.sd = sqrt(RC1.sd^2 + RC2.sd^2),
         Ratio = Total.RC/Total.RS,
         Total.Wins = Wins.1 + Wins.2, Count = Count1 + Count2, Win.Percent = Total.Wins*100/Count  ) %>%
  ungroup()

Win.percent.regseason = Win.percent.regseason %>%
  mutate(Score.Diff = Total.RS - Total.RC)

# Fitting a linear regression for estimating runs per win
score.diff.win.model = lm(Total.Wins ~ Score.Diff , data = Win.percent.regseason )
summary(score.diff.win.model)
plot(score.diff.win.model, which = 1)

RPW.score.diff.model = 1/score.diff.win.model$coefficients[2]
# Runs per win from score differential model
RPW.score.diff.model


# RUNS PER WIN using LOG ODSS from PYHTOGOREAN EXPECTED WIN FORMULA ---------------------------

# evaluating totals using ball-by-ball data
innings_total = bbb_ipl_df %>%
  filter(season %in% c("2015", "2016", "2017", "2018", "2019", "2021", "2022")) %>%
  group_by(match_id, innings) %>%
  summarise(total_score = sum(runs_off_bat + extras), .groups = "drop" ) %>%
  filter(innings %in% c(1,2))

# changing shape of innings total df
innings_total = innings_total %>%
  pivot_wider(
    names_from  = "innings",
    values_from = c("total_score")
  ) %>% 
  rename(innings1_total = "1", innings2_total = "2")


# merging innings total + poer play df w match df
match_innings_df = innings_total %>%
  inner_join(match_df %>% mutate(match_id = as.integer(match_id)), by = "match_id")



match_innings_df = match_innings_df %>%
  mutate(batting_first = case_when(
    toss_winner == team1 & toss_decision == "field" ~ team2,
    toss_winner == team1 & toss_decision == "bat" ~ team1,
    toss_winner == team2 & toss_decision == "field" ~ team1,
    toss_winner == team2 & toss_decision == "bat" ~ team2),
    batting_second = ifelse(batting_first == team1, team2, team1)) %>%
  dplyr::select(-gender, - player_of_match, -match_referee, -umpire1, -umpire2,
                -reserve_umpire, -tv_umpire) %>%
  filter(is.na(outcome)) %>%    # removing the tied/no result games
  mutate(bat_first_winner = ifelse(batting_first == winner, 1 , 0),
         bat_second_winner = ifelse(bat_first_winner == 0, 1, 0))


Win.percent = match_innings_df %>%
  group_by(batting_first, season) %>%
  #group_by(batting_first) %>%
  summarise(Count1 = n(), Runs.Scored1 = sum(innings1_total), Runs.Conceded1 = sum(innings2_total), Wins.1 = sum(bat_first_winner),
            RS1.sd = sd(innings1_total), RC1.sd = sd(innings2_total) )%>%
  rename(Team = batting_first) %>%
  inner_join(match_innings_df %>%
               group_by(batting_second, season) %>%
               #group_by(batting_second) %>%
               summarise(Count2 = n(), Runs.Scored2 = sum(innings2_total), Runs.Conceded2 = sum(innings1_total), Wins.2 = sum(bat_second_winner),
                         RS2.sd = sd(innings2_total), RC2.sd = sd(innings1_total))%>%
               rename(Team = batting_second), 
             by = c("Team", "season")) %>%
  #by = c("Team")) %>%
  mutate(Total.RS = Runs.Scored1 + Runs.Scored2, Total.RC = Runs.Conceded1 + Runs.Conceded2, TotalRS.sd = sqrt(RS1.sd^2 + RS2.sd^2), TotalRC.sd = sqrt(RC1.sd^2 + RC2.sd^2),
         Ratio = Total.RC/Total.RS,
         Total.Wins = Wins.1 + Wins.2, Count = Count1 + Count2, Win.Percent = Total.Wins*100/Count  )




Win.total.model = lm(Log.Odds ~ log(1/Ratio) - 1 , data = Win.percent %>%
                       mutate(Log.Odds = log(Win.Percent*0.01/(1 - 0.01*Win.Percent))) )


summary(Win.total.model)

Avg.Runs_df = Win.percent %>%
  ungroup() %>%
  summarise(Avg.RS = sum(Count * Total.RS)/sum(Count), Avg.RC = sum(Count * Total.RC)/sum(Count) )

N = 14 # no. of games in a season
p = Win.total.model$coefficients
Avg.RS = Avg.Runs_df$Avg.RS 

RPW.winpercent.model = Avg.RS/(N*p) * (1+1)^2/1
RPW.winpercent.model


# Extra calculations

# Win.percent = Win.percent %>%
#   ungroup() %>%
#   mutate(Pyth.Win.Percent = 100/(1 + (Total.RC/Total.RS )^8.1857))
# 
# 
# Win.percent %>%
#   ggplot(aes(y = Win.Percent, x = Pyth.Win.Percent)) +
#   geom_point() + 
#   geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95)
# 
# cor(Win.percent$Win.Percent, Win.percent$Pyth.Win.Percent)  
# summary(lm(Win.Percent ~ Pyth.Win.Percent - 1, data  = Win.percent))
# 
# # Mean Error in Win % and Games for Total Run Model
# Win.percent %>%
#   ungroup() %>%
#   mutate(Pyth.Win.Percent = 100/(1 + (Total.RC/Total.RS )^8.1857)) %>%
#   summarise(Win.err = sum(Count*(abs(Pyth.Win.Percent - Win.Percent)))/sum(Count) , Game.err = sum(Count*abs(Pyth.Win.Percent - Win.Percent)*Count/100)/sum(Count))
# 
# 
# # Game Err Vs Offensive Consistency 
# 
# Win.percent %>%
#   mutate(Game.Err = (Pyth.Win.Percent - Win.Percent)*Count/100) %>%
#   ggplot(aes(x = TotalRS.sd, y = Win.Percent)) + 
#   geom_point() +
#   geom_smooth(method="lm", se=TRUE, fullrange=FALSE, level=0.95)
# 
# cor(Win.percent$Game.Err)
# Win.percent %>%
#   mutate(Log.Odds = log(Win.Percent*0.01/(1 - 0.01*Win.Percent))) 
# 
# Total.Win.CV.Err = function(df) {
#   df = df  %>%
#     mutate(Log.Odds = log(Win.Percent*0.01/(1 - 0.01*Win.Percent)))
#   
#   n = length(df$Log.Odds)
#   
#   Cv.Err = numeric(n)
#   
#   for (i in 1:n) {
#     Win.model = lm(Log.Odds ~ log(Ratio) - 1 , data = df[-i,])
#     Cv.Err[i] = 100/(1+(df$Ratio[i])^(-Win.model$coefficients)) - df$Win.Percent[i]
#   }
#   
#   return(Cv.Err)
# }
# 
# Cv.err = Total.Win.CV.Err(Win.percent)
# 
# Win.percent = Win.percent %>%
#   ungroup() %>%
#   mutate(CV.Err = Cv.err ) 
# 
# 
# Win.percent %>%
#   summarise(Mean.CV.Err = sum(Count * abs(CV.Err))/sum(Count), Mean.CV.Game.Err = sum(Count*abs(CV.Err)*Count/100)/sum(Count) )
# 
# 
# Win.percent %>%
#   ggplot(aes(x = Ratio, y = Win.Percent)) +
#   geom_point(color = "blue", aes(size = Count1))+
#   geom_point(data = data.frame(R = R, W = 100/(1 + R^8.18)), aes(x = R, y= W), color = "red")





