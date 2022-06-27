# Section 1 ------------ Loading Packages and Dataframe building
#Loading in R Packages for Baseball Analysis
library(Lahman)
library(ggplot2)
library(dplyr)
library(knitr)
library(Hmisc)

#Loading in the salaries of each team for the 2013 Season
S <- Salaries %>% select(yearID,teamID,lgID,playerID,salary) %>%
  filter(yearID==2013) %>%
  group_by(teamID) %>%
  mutate(Payroll = sum(salary))

#Loading in the general statistics of each team for the 2013 Season
T <- Teams %>% select(yearID,lgID,teamID,W,L,R,RA) %>%
  filter(yearID==2013) %>%
  group_by(teamID)

#Loading in individual batting statistics for the 2013 Season
B <- Batting %>% select(playerID,yearID,teamID,AB,R,H,X2B,X3B,HR,BB,IBB,HBP,SF)%>%
  filter(yearID==2013) %>%
  filter(AB >= 120) %>%
  mutate(wOBA = (((0.69*BB) + (0.719*HBP) + (0.87*H) + (1.217*X2B) + (1.529*X3B) + (1.94*HR)) / 
                (AB+BB+IBB+SF+HBP)))%>%
  select(playerID,yearID,teamID,wOBA)

#Loading in individual pitching statistics for the 2013 Season
P <- Pitching %>% select(playerID,yearID,teamID,HR,BB,HBP,SO,IPouts) %>%
  filter(yearID==2013)%>%
  filter(IPouts >= 60) %>%
  mutate(IP = IPouts/3) %>%
  mutate(FIP = ((HR*13 + 3*(BB+HBP) - 2*SO) / 
                  IP + 3.2)) %>%
  select(playerID,yearID,teamID,FIP,IP)

#Merge Salaries, Teams, Batting, and Pitching, Dataframes by Teamid, Yearid, and Leagueid to create 
#One Dataframe for analysis
df <- left_join(S, T, by = c("teamID", "yearID", "lgID"))
df <- left_join(df,B, by = c("playerID", "teamID", "yearID"), na_matches="never")
df <- left_join(df,P,by = c("playerID", "teamID", "yearID"), na_matches="never")

df <- df %>%
  group_by(teamID) %>%
  mutate(TEAMFIP =mean(FIP, na.rm=TRUE))

df <- df %>%
  group_by(teamID) %>%
  mutate(TEAMwOBA =mean(wOBA, na.rm=TRUE))

#Removing duplicate observations for one observation per team
df2 <- subset(df, !duplicated(df$teamID))

#Selecting need columns for analysis
df2 <- df2 %>% select(teamID,lgID,Payroll,W,L,R,RA,TEAMwOBA,TEAMFIP)
head(df2, n=30)

#Section 2--------- Building Graphs
# Creating graph to explore possible relationship of FIP and wOBA
phi <- ggplot(df2, aes(x=TEAMFIP, y=TEAMwOBA, size=Payroll, color=lgID)) + geom_point() 
phi + 
  labs(
  title = "2013 MLB Team Average wBOA vs. FIP",
  x = "Team: Fielding Independent Pitching",
  y = "Team: Weighted On-Base Average"
) + facet_grid() + theme_gray()

# Creating graph to explore possible relationship of Wins and FIP
tau <- ggplot(df2, aes(x=W, y=TEAMFIP, size=Payroll, color=lgID)) + geom_point() 
tau + 
  labs(
    title = "2013 MLB Team Wins vs. FIP",
    x = "Team Wins",
    y = "Team: Fielding Independent Pitching"
  ) + facet_grid() + theme_gray()

# Creating graph to explore possible relationship of Wins and FIP
nu <- ggplot(df2, aes(x=W, y=TEAMwOBA, size=Payroll, color=lgID)) + geom_point() 
nu + 
  labs(
    title = "2013 MLB Team Wins vs. wOBA",
    x = "Team Wins",
    y = "Team: Weighted On-Base Average"
  ) + facet_grid() + theme_gray()

#Section 3 ------------

# Creating Predictive Model of teams that did make the playoffs verses those who are
#predicted to make the playoffs
Data <- df2 %>% 
  mutate(EWP=round(R/(R+RA), 4), EWins=EWP*(W+L), WinDiff=round(EWins-W)) 


#Now we can Create a plot to show Wins vs. Expected Wins
theta <- ggplot(Data, aes(x=W, y=EWins, color=Payroll)) + geom_point() + stat_smooth(method = 'lm')
theta + scale_color_gradient(low="red", high="green") + 
  labs(title = "2013 MLB Season Team Wins vs. Expected Wins",
       x = "Actual Wins by Team",
       y = "Expected Wins by Team") +
  guides(fill = guide_legend(title = "Team Payroll"))

Wins <- lm(W~EWins+TEAMwOBA+TEAMFIP+Payroll, data=Data)  
Wins
summary(Wins)
