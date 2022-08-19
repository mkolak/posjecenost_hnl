matches <- read.csv2("hnl_matches.csv")
attach(matches)
str(matches)

## Prosječna posjećenost domaćih utakmica po klubu ######################
avgs <- c()
for (teamname in rownames(table(home_team))) {
  avgs <- c(avgs,mean(matches[home_team == teamname & attendance!= 0, "attendance"]))
}
rownames(table(home_team))
avgs
png(filename="simple_graphic.png", width=850, height = 450)
barplot(avgs, names = c("CIB","DIN","HRD","HAJ","GOR","INT","IST","LOK","ZAG","OS","RI","RUD","SIB","SLB","SPL","VAR"),
        col = c("cyan","blue","black","white","red","yellow","green","blue","red","white", "white","blue3","orange","blue","red","blue"))
dev.off()

rbind(rownames(table(home_team)), avgs)
mean(attendance[attendance != 0])
fivenum(attendance[attendance != 0])
median(attendance[attendance != 0])

#########################################################################
png(filename="boxplot_attendance.png", width=300, height = 400)
boxplot(attendance[attendance != 0], main="Broj gledatelja po utakmici")
dev.off()
mean(attendance[home_team == "Hajduk" & attendance != 0])
fivenum(attendance[home_team == "Hajduk" & attendance != 0])
length(attendance[home_team == "Hajduk" & attendance != 0])
quantile(attendance[home_team == "Hajduk" & attendance != 0], 0.4)
#########################################################################

## Proporcija posjećenosti domaćih utakmica Dinama i Hajduka. ###########

hajdin_uk <- sum(attendance[(home_team == "Hajduk" | home_team == "Dinamo Zagreb") & attendance != 0])
uk <- sum(attendance[attendance != 0])
binom.test(hajdin_uk, uk, p = 0.5, a = "g")
hajdin_uk/uk

## Proporcija posjećenosti domaćih utakmica Dinama, Hajduka, Osijeka i Rijeke. ###########

haj_os_ri_din <- sum(attendance[(home_team %in% c("Hajduk","Dinamo Zagreb", "Osijek", "Rijeka") & attendance != 0)])
haj_os_ri_din
haj_os_ri_din/uk
binom.test(haj_os_ri_din, uk, p = 0.75, a = "g")

# Utakmice u kojima gostuju hajduk i dinamo naspram ostalih utakmica

hajduk_gosti <- attendance[away_team == "Hajduk" & attendance != 0]
ostali_gosti <- attendance[home_team != "Hajduk" & away_team != "Hajduk" & attendance != 0]
t.test(hajduk_gosti, ostali_gosti, a = "g")

dinamo_gosti <- attendance[away_team == "Dinamo Zagreb" & attendance != 0]
ostali_gosti_din <- attendance[home_team != "Dinamo Zagreb" & away_team != "Dinamo Zagreb" & attendance != 0]
t.test(dinamo_gosti, ostali_gosti_din, a = "g")

osijek_gosti <- attendance[away_team == "Osijek" & attendance != 0]
ostali_gosti_os <- attendance[home_team != "Osijek" & away_team != "Osijek" & attendance != 0]
t.test(osijek_gosti, ostali_gosti_os, a="g")
 
rijeka_gosti <- attendance[away_team == "Rijeka" & attendance != 0]
ostali_gosti_ri <- attendance[home_team != "Rijeka" & away_team != "Rijeka" & attendance !=0]
t.test(rijeka_gosti, ostali_gosti_ri, a = "g")

dragovoljac_gosti <- attendance[away_team == "Dragovoljac" & attendance != 0]
ostali_gosti_dragov <- attendance[home_team != "Dragovoljac" & away_team != "Dragovoljac" & attendance != 0]
t.test(dragovoljac_gosti, ostali_gosti_dragov, a = "l")

# Posjecenost s obzirom na međusobne utakmice

parovi <- matches[,c("home_team","away_team")]
broj_utakmica <- table(parovi)

pair_matrix <- matrix(list(), nrow=16, ncol=16)
rownames(pair_matrix) <- rownames(broj_utakmica)
colnames(pair_matrix) <- colnames(broj_utakmica)
mean_matrix <- matrix(double(), nrow=16, ncol=16)
rownames(mean_matrix) <- rownames(broj_utakmica)
colnames(mean_matrix) <- colnames(broj_utakmica)


parovi_timova2 <- data.frame(matrix(ncol=3,nrow=0))
colnames(parovi_timova2) <- c("Tim1","Tim2","Prosjecna posjecenost")

for (team1 in colnames(broj_utakmica)) {
  for (team2 in rownames(broj_utakmica)){
    if(broj_utakmica[team1,team2] > 0)
      pair_matrix[[team1, team2]] <- c(attendance[home_team == team1 & away_team == team2 & attendance != 0],attendance[home_team == team2 & away_team == team1  & attendance != 0])
  }  
}


for (team1 in colnames(broj_utakmica)) {
  for (team2 in colnames(broj_utakmica)){
    if(broj_utakmica[team1,team2] > 0 & nrow(parovi_timova2[parovi_timova2$Tim1==team2 & parovi_timova2$Tim2 == team1,]) == 0){
      if((team1 == "Sibenik" & team2 == "Varazdin") | (team1 == "Varazdin" & team2 == "Sibenik"))
        next
      parovi_timova2[nrow(parovi_timova2)+1,] <- c(team1, team2, mean(pair_matrix[[team1, team2]]))
    }
  }
}

parovi_order2 <- parovi_timova2[order(as.numeric(parovi_timova2$`Prosjecna posjecenost`)),]
png(filename="pairs_avg.png", width=1100, height = 550)
par(mfrow = c(1,2))
barplot(as.numeric(parovi_order2[,3])[1:5], names = c("HRD - LOK", "INT - RUD", "HRD - GOR", "HRD - šIB", "LOK - RUD"), col = "pink")
barplot(as.numeric(parovi_order2[,3])[(nrow(parovi_order2) - 4):nrow(parovi_order2)], names = c("HAJ - LOK", "DIN - RI", "HAJ - OS", "HAJ - RI", "DIN - HAJ"), col = "lightblue")
dev.off()

# Dinamo Hajduk 0.6-kvantil
hajdin <- attendance[((home_team == "Dinamo Zagreb" & away_team == "Hajduk") | (home_team == "Hajduk" & away_team == "Dinamo Zagreb")) & attendance != 0]
min(hajdin)
quantile(attendance, 0.6)
quantile(attendance, 0.7)
sum(attendance < 1967)
binom.test(sum(attendance < 1967), length(attendance < 1967), p = 0.6, a = "g")

# Dinamo Hajduk 0.94-kvantil
hajdin <- matches[((home_team == "Dinamo Zagreb" & away_team == "Hajduk") | (home_team == "Hajduk" & away_team == "Dinamo Zagreb")) & season != "2019/2020" & attendance != 0, ]
min(hajdin)
quantile(attendance, 0.94)
sum(attendance < 9974)

binom.test(sum(attendance < 9974), length(attendance < 9974), p = 0.94, a = "g")
