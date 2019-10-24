# initialize
library("dplyr")
df_initial = read.csv("Champions.csv")
df_initial = tbl_df(df_initial)

# question 1.1
win_team = filter(df_initial, HomeGoal > AwayGoal)
head(winTeam)
name_define = filter(df_initial, HomeTeam == "Barcelona" | HomeTeam == "Real Madrid")
head(name_define)

# question 1.2
home_team = select(df_initial, starts_with("Home"))
head(home_team)
table_team = select(df_initial, contains("Team"), contains("Goal"), contains("Corner"))
head(table_team)

# question 1.3
reorder_team = arrange(table_team, desc(HomeGoal))
head(reorder_team)

# question 1.4
group_hometeam = group_by(df_initial, HomeTeam)
summary_team = summarise(group_hometeam, HomeGoal = mean(HomeGoal), 
                         HomePossession = mean(HomePossession), 
                         HomeYellow = mean(HomeYellow))
head(summary_team)

# question 1.5
score_team = mutate(df_initial, score = ifelse(HomeGoal > AwayGoal,
                                               paste(HomeGoal, AwayGoal, sep = ":"),
                                               paste(AwayGoal, HomeGoal, sep = ":")))
group_score = arrange(summarise(group_by(score_team, score), total_score = n()), desc(total_score))
head(group_score)

# question 2.1
