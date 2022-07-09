# Section 1 ------------ Loading Packages and Dataframe building
#Loading in R Packages for Baseball Analysis
library(Lahman)
library(ggplot2)
library(dplyr)
library(knitr)
library(Hmisc)
library(car)
library(MVN)
library(lmtest)
library(formattable)
library(stargazer)
library(MVN)
library(broom)

head(Lahman::Salaries)

#Loading in the salaries of each team for the 2013 Season
S <- Lahman::Salaries %>% select(yearID,teamID,lgID,playerID,salary) %>%
  filter(yearID==2013) %>%
  group_by(teamID) %>%
  mutate(Payroll = sum(salary)) %>%
  mutate(Payroll = Payroll/1000000)

#Loading in the general statistics of each team for the 2013 Season
T <- Lahman::Teams %>% select(yearID,lgID,teamID,W,L,R,RA) %>%
  filter(yearID==2013) %>%
  group_by(teamID)

#Loading in individual batting statistics for the 2013 Season
B <- Lahman::Batting %>% select(playerID,yearID,teamID,AB,R,H,X2B,X3B,HR,BB,IBB,HBP,SF)%>%
  filter(yearID==2013) %>%
  filter(AB >= 120) %>%
  mutate(wOBA = (((0.69*BB) + (0.719*HBP) + (0.87*H) + (1.217*X2B) + (1.529*X3B) + (1.94*HR)) / 
                (AB+BB+IBB+SF+HBP)))%>%
  select(playerID,yearID,teamID,wOBA)

#Loading in individual pitching statistics for the 2013 Season
P <- Lahman::Pitching %>% select(playerID,yearID,teamID,HR,BB,HBP,SO,IPouts) %>%
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

# Creating Predictive Model of teams that did make the playoffs verses those who are
#predicted to make the playoffs
Data <- df2%>%
  mutate(EWP = ((R/RA)^1.6)/((R/RA)^1.6+1), EWins = EWP*162, AWP = W/162, WinPctDiff = AWP-EWP)

#Confirming that an exponent of 1.6 is yields the lowest Mean Average Deviation of Actual Win Percentage vs. Expected Win Percentage
mean(abs(Data$WinPctDiff))

DataNeat <- Data

DataNeat$Payroll <- currency(DataNeat$Payroll, digits = 0L)

DataNeat$TEAMwOBA <- percent(DataNeat$TEAMwOBA, digits = 2L)

DataNeat$EWP <- percent(DataNeat$EWP, digits = 2L)

# Proposed Linear Model
Wins <- lm(EWins~TEAMwOBA+TEAMFIP+Payroll, data=Data) 
Wins
summary(Wins)

#Section 2--------- Building Graphs and Testing Assumption 1: Linear Relationship 
# Assumption 1: There is a linear relationship between the predictors (x) and the outcome (y)

# Creating graph to explore possible relationship of FIP and wOBA
phi <- ggplot(df2, aes(x=TEAMFIP, y=TEAMwOBA, size=Payroll, color=lgID)) + geom_point() 
phi + 
  labs(
  title = "2013 MLB Team Average wBOA vs. FIP",
  x = "Team: Fielding Independent Pitching",
  y = "Team: Weighted On-Base Average"
) + facet_grid() + theme_gray()

# Creating graph to explore possible relationship of Wins and FIP
tau <- ggplot(Data, aes(x=EWins, y=TEAMFIP, size=Payroll, color=lgID)) + geom_point() 
tau + 
  labs(
    title = "2013 MLB Team Wins vs. FIP",
    x = "Team Expected Wins",
    y = "Team: Fielding Independent Pitching"
  ) + facet_grid() + theme_gray()

# Creating graph to explore possible relationship of Wins and wOBA
nu <- ggplot(Data, aes(x=EWins, y=TEAMwOBA, size=Payroll, color=lgID)) + geom_point() 
nu + 
  labs(
    title = "2013 MLB Team Wins vs. wOBA",
    x = "Team Expected Wins",
    y = "Team: Weighted On-Base Average"
  ) + facet_grid() + theme_gray()

#Now we can Create a plot to show Wins vs. Expected Wins
theta <- ggplot(Data, aes(x=W, y=EWins, color=Payroll)) + geom_point() + stat_smooth(method = 'lm')
theta + scale_color_gradient(low="red", high="green") + 
  labs(title = "2013 MLB Season Team Wins vs. Expected Wins",
       x = "Actual Wins by Team",
       y = "Expected Wins by Team") +
  guides(fill = guide_legend(title = "Team Payroll"))

alpha <- ggplot(Data, aes(x=EWins, y=Payroll, color=lgID)) + geom_point() 
alpha + 
  labs(
    title = "2013 MLB Team Wins vs. Payroll",
    x = "Team Expected Wins",
    y = "Team Payroll"
  ) + facet_grid() + theme_gray()

# Assumption 2: No Multicollinearity
VIF <- vif(Wins)
VIF

#create horizontal bar chart to display each VIF value
barplot(VIF, main = "VIF Values", horiz = TRUE, col = "steelblue",  
        names.arg=c( "TEAMwOBA", "TEAMFIP", "Payroll"), xlim=c(0,6))
#add vertical line at 5
abline(v = 1, lwd = 3, lty = 2)
abline(v = 5, lwd = 3, lty = 2)

# Assumption 3: Multivariate Normality
DataQQ <- lapply(Data, as.numeric)
DataQQ <- as.data.frame(DataQQ)
DataQQ <- DataQQ %>%
  select(EWins, TEAMwOBA, TEAMFIP, Payroll)

mvn(DataQQ, mvnTest = "mardia", multivariatePlot = "qq")

# Assumption 4: Independence
durbinWatsonTest(Wins)

# Assumption 5: Homoskedasticity
BPT <- lm(EWins~TEAMwOBA+TEAMFIP+Payroll, data=Data)  
summary(BPT)

plot(fitted(BPT), resid(BPT), xlab="Fitted Values", ylab="Residuals")
abline(0,0)

bptest(BPT)

