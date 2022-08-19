matches <- read.csv2("hnl_matches.csv")
attach(matches)
str(matches)

## Posjećenost s obzirom na godišnje doba odigravanja utakmice

zima <- attendance[(ISOdate(0,month,day) >= ISOdate(0,12,21) | ISOdate(0,month,day) < ISOdate(0,3,21)) & attendance != 0]
proljece <- attendance[(ISOdate(0,month,day) >= ISOdate(0,3,21) & ISOdate(0,month, day) < ISOdate(0,6,21)) & attendance != 0]
ljeto <- attendance[(ISOdate(0,month,day) >= ISOdate(0,6,21) & ISOdate(0,month, day) < ISOdate(0,9,23)) & attendance != 0]
jesen <- attendance[(ISOdate(0,month,day) >= ISOdate(0,9,23) & ISOdate(0,month, day) < ISOdate(0,12,21)) & attendance != 0]

kategorije <-  c(0,500, 1500, 2500, 5000, 10000, 35000)
zima_cut <- cut(zima, breaks = kategorije)
proljece_cut <- cut(proljece, breaks = kategorije)
ljeto_cut <- cut(ljeto, breaks = kategorije)
jesen_cut <- cut(jesen, breaks = kategorije)
cols <- c("<500","500-1500","1500-2500","2500-5000","5000-10000",">10000")
sva_doba <- rbind(table(zima_cut), table(proljece_cut), table(ljeto_cut), table(jesen_cut))
rownames(sva_doba) <- c("Zima", "Proljece", "Ljeto", "Jesen")
colnames(sva_doba) <- cols
sva_doba

alt <- cbind(table(zima_cut),table(proljece_cut),table(ljeto_cut),table(jesen_cut))
rownames(alt) <- cols
colnames(alt) <- c("Zima", "Proljece", "Ljeto", "Jesen")
alt
alt2 <- cbind(prop.table(table(zima_cut)),prop.table(table(proljece_cut)),prop.table(table(ljeto_cut)),prop.table(table(jesen_cut)))
rownames(alt2) <- cols
colnames(alt2) <- c("Zima", "Proljece", "Ljeto", "Jesen")
alt2
png(filename="freq_prop_doba.png", width=750, height = 400)
par(mfrow = c(1,2))
barplot(alt,beside = T, legend.text = cols, args.legend = list(x = "topright", inset = c(-0.06), cex = 0.7))
barplot(alt2,beside = T, legend.text = cols, args.legend = list(x = "topright", inset = c(-0.06), cex = 0.7), ylim = c(0,0.35))
dev.off()
barplot(sve, beside = T, legend.text = rownames(sva_doba))

chisq.test(sva_doba)

## Očekivanje s obzirom na vrijeme odigravanja

zima <- attendance[(month > 11 | month < 3) & attendance != 0]
ljeto <- attendance[(month > 3 & month < 12) & attendance != 0]
t.test(zima, ljeto, a = "l")

# Testiranje s obzirom na dan u tjednu 

weekday %in% c("Sun", "Sat")

radni_dan <- attendance[!(weekday %in% c("Sun", "Sat")) & attendance != 0]
vikend <- attendance[weekday %in% c("Sun", "Sat") & attendance != 0]

t.test(radni_dan, vikend, a = "l")

att_day_count <- c()
att_days_avg <- c()
att_days_med <- c()
dani <- c("Pon","Uto","Sri","Čet","Pet","Sub","Ned")
days <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
for(dan in days){
  att_days_avg <- c(att_days_avg, mean(attendance[weekday==dan & attendance != 0]))
  att_days_med <- c(att_days_med, median(attendance[weekday==dan & attendance != 0]))
  att_day_count <- c(att_day_count, length(attendance[weekday==dan]))
}

att_d <- rbind(att_days_avg, att_days_med)
colnames(att_d) <- days
barplot(att_day_count)
att_day_count
png(filename="dan_u_tjednu.png", width=750, height = 400)
barplot(att_d, beside = T, legend.text = c("Mean","Median"), args.legend = list(x = "top"), ylim = c(0,5000), names = dani)
dev.off()

wed_g <- matches[weekday=="Wed",]
nrow(wed_g[wed_g$attendance != 0,])


# Posjecenost srijedom

t.test(attendance[attendance != 0 & weekday=="Wed"], attendance[attendance != 0 & weekday=="Sat"], a = "g")


