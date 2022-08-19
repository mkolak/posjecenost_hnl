matches <- read.csv2("hnl_matches.csv")
attach(matches)
str(matches)

# Test s obzirom na pobjede 

trend <- c()
for(teamname in names(table(home_team))){
  tim_tekme <- matches[home_team == teamname | away_team == teamname,]
  last_home <- 0
  last_won <- FALSE
  for(j in 1:nrow(tim_tekme)){
    if(tim_tekme[j,"home_team"] == teamname){
      if(last_won & last_home != 0){
        trend <- c(trend, tim_tekme[j,"attendance"] > last_home)
      }
      last_won <- tim_tekme[j,"home_goals"] > tim_tekme[j,"away_goals"] 
      last_home <- tim_tekme[j,"attendance"]
    } else {
      last_won <- tim_tekme[j,"home_goals"] < tim_tekme[j,"away_goals"]
    }
  }
}

table(trend)
barplot(table(trend), col = c("pink","lightblue"), names = c("Pad", "Rast"))
binom.test(sum(trend), length(trend), p = 0.5, a = "g")

# Test s obzirom na poraze

trend_negative <- c()
for(teamname in names(table(home_team))){
  tim_tekme <- matches[home_team == teamname | away_team == teamname,]
  last_home <- 0
  last_won <- FALSE
  for(j in 1:nrow(tim_tekme)){
    if(tim_tekme[j,"home_team"] == teamname){
      if(last_won & last_home != 0){
        trend_negative <- c(trend_negative, tim_tekme[j,"attendance"] > last_home)
      }
      last_won <- tim_tekme[j,"home_goals"] < tim_tekme[j,"away_goals"] 
      last_home <- tim_tekme[j,"attendance"]
    } else {
      last_won <- tim_tekme[j,"home_goals"] > tim_tekme[j,"away_goals"]
    }
  }
}

table(trend_negative)
barplot(table(trend_negative), col = c("pink","lightblue"), names = c("Pad", "Rast"))
binom.test(length(trend_negative) - sum(trend_negative), length(trend_negative), p = 0.5, a = "g")

png(filename="trend_trend_neg.png", width=1100, height = 550)
par(mfrow = c(1,2))
barplot(table(trend_negative), col = c("lightblue","pink"), names = c("Pad", "Rast"))
barplot(table(trend), col = c("lightblue","pink"), names = c("Pad", "Rast"))
dev.off()

# Gledanost s obzirom na konkurentnost 

under_elo <- c()
over_elo <- c()
team_elo <- c()
under_elo_all <- c()
over_elo_all <- c()
for (teamname in rownames(table(home_team))) {
  tim_elo <- c(home_elo[home_team == teamname], away_elo[away_team == teamname])
  median_elo <- median(tim_elo)
  team_elo <- c(team_elo, median_elo)
  under_elo <- c(under_elo, mean(attendance[home_team == teamname & home_elo < median_elo & attendance != 0]))
  over_elo <- c(over_elo, mean(attendance[home_team == teamname & home_elo > median_elo & attendance != 0]))  
  under_elo_all <- c(under_elo_all, attendance[home_team == teamname & home_elo < median_elo & attendance != 0])
  over_elo_all <- c(over_elo_all, attendance[home_team == teamname & home_elo > median_elo & attendance != 0])
}
mat <- rbind(under_elo, over_elo)
colnames(mat) <- rownames(table(home_team))
png(filename="median_elo_teams.png", width=800, height = 450)
barplot(mat, beside = T, names = c("CIB","DIN","HRD","HAJ","GOR","INT","IST","LOK","ZAG","OS","RI","RUD","SIB","SLB","SPL","VAR"), legend.text = c("Elo manji od medijana","Elo veći od medijana"), col = c("lightblue","pink"))
dev.off()
t.test(over_elo_all, under_elo_all, a = "g")

