# first class homework
fib = c(1,2)
i = 2
sum = 0
while (fib[i] < 4e6){
  if (fib[i] %% 2 == 0){
    sum = sum + fib[i]
  }
  fib[i+1] = fib[i] +fib[i-1]
  i = i + 1
}
sum

list = c("a", "b", "c", "d", "e")
list1 = paste0(rep(list,5), rep(list, each = 5))
list1


# question 1 initialize
df_initial = read.csv("/Users/ningzesun/Documents/class/kaggle/Homework/Champions.csv")
library(dplyr)
df_initial = tbl_df(df_initial)

# question 1.1
win_team = filter(df_initial, HomeGoal > AwayGoal)
head(win_team)
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

# question 2 initialize
library(ggplot2)
data(cars)

# question 2.1
plot(cars$speed, cars$dist)

#question 2.2
theme_update(plot.title = element_text(hjust = 0.5))
ggplot(data = cars, aes(x = speed, y = dist)) + 
       geom_point(color = "red", pch = 17) + xlab("Speed (mpg)") + 
       ylab("Stopping Distance (ft)") + ggtitle("Speed VS Distance")

