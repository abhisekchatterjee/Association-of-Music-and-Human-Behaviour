rm(list = ls())
library(readxl)
data <- read_excel("C:/Users/Abhishek/Desktop/Project Questionnaire (Responses).xlsx")
#install.packages("rpartScore")
#install.packages("hot.deck")
library(rpartScore)
library(rpart)
library(rpart.plot)
library(hot.deck)
#Checking Missing Values
n = dim(data)[1]
mo = numeric(n)
for (i in 1:n) {
  mo[i] = sum(is.na(data[i,]))
}
mo1 = numeric(44)
for (i in 1:44) {
  mo1[i] = sum(is.na(data[,i]))
}

data = data.frame(data)

#Checking the Missing value distribution
library(mice)
md.pattern(data)
#Removing the missing observations at first
#Two genres that people listened to the most have a lot of missing values
mo = numeric(n)
for (i in 1:n) {
  mo[i] = sum(is.na(data[i,-c(18:44)]))
}
data$mo = mo
#Observation 5,92,99,113,119,141,147 should be removed
data = data[-c(5,92,99,113,119,141,147),]

#Doing imputation
set.seed(123)
data2 = hot.deck(data[,c(19:27,31:44)], m = 1,method = "p.draw", impContinuous = "HD")
data[,19:27] = data2$data[[1]][,1:9]
data[,31:44] = data2$data[[1]][,10:23]
n = dim(data)[1]
mo = numeric(n)
# for (i in 1:n) {
#   mo[i] = sum(is.na(data[i,c(6:17,19:27,31:44)]))
# }
for (i in 1:n) {
  mo[i] = sum(is.na(data[i,-c(18:44)]))
}
data$mo = mo
mo1 = numeric(44)
for (i in 1:44) {
  mo1[i] = sum(is.na(data[,i]))
}

data = data[which(data$mo ==0),]

#Calculating PHQ9 Scores
data$PHQSC = numeric(nrow(data))
for (i in 19:27) {
data[,i] = factor(data[,i],
                  levels = c("Not at all" , "Several days", "More than half the days",
                             "Nearly every day"),
                  labels = c(0, 1, 2, 3))
data[,i] = as.numeric(as.character(data[,i]))
data$PHQSC = data$PHQSC + data[,i]
}

#Calculating PSS Scores
#Fixing the scores of the positive items
for (i in c(34,35,36,37,39,40,43)) {
  data[,i] = 4 - data[,i]
}

data$PSS = numeric(nrow(data))
for (i in 31:44) {
  data[,i] = as.numeric(data[,i])
  data$PSS = data$PSS + data[,i]
}

shapiro.test(data$PSS)
shapiro.test(data$PHQSC)
#Some information
summary(data$PHQSC)
sum(data$PHQSC>4)
sum(data$PHQSC>9)
sum(data$PHQSC>14)
sum(data$PHQSC>19)

summary(data$PSS)
sum(data$PSS<27)
sum(data$PSS>39)

#Checking the average Depression score of different time groups
data$time = data$On.an.average..how.many.minutes.per.day..have.you.listened.to.music.in.the.last.month.
unique(data[,5])
dataT1 = data[which(data$time=="0-30 Minutes/Day"),]
dataT2 = data[which(data$time=="30-60 Minutes/Day"),]
dataT3 = data[which(data$time=="More than 60 Minutes/Day"),]
summary(dataT1$PHQSC)
summary(dataT2$PHQSC)
summary(dataT3$PHQSC)

#Checking the average Perceived Stress score of different time groups
summary(dataT1$PSS)
summary(dataT2$PSS)
summary(dataT3$PSS)

#Categorizing the Scores
data$PSSC = ifelse(data$PSS<27,"Mild to Moderate P-Stress",
                   ifelse(data$PSS <40,"High P-Stress","Severe P-Stress"))
data$PHQSCC = ifelse(data$PHQSC<5,"No Depression",
                     ifelse(data$PHQSC <10,"Mild Depression",
                            ifelse(data$PHQSC <15,"Moderate Depression",
                                   ifelse(data$PHQSC <20,"High Depression","Severe Depression"))))

#Converting the covid variable to an ordinal one for model building
data$covid = data$How.difficult.have.the.second.wave.of.pandemic.made.it.for.you.to.do.your.work...take.care.of.things.at.home.or.get.along.with.other.people.
data$covidord = ifelse(data$covid == "Not difficult at all",0,
                       ifelse(data$covid == "Somewhat difficult",1,
                              ifelse(data$covid == "Very difficult",2,3)))

#Fitting Classification Tree
ct = rpart(PSSC~Classical+Bollywood+Country+Electronic.Dance.Music+Pop+Hip.hop+Jazz+Rock+Metal+Blues+Instrumental.Film.or.TV.Series.Scores+Bengali.Music+covidord, data = data, method = 'class')
rpart.plot(ct)
ctd = rpart(data$PHQSCC~Classical+Bollywood+Country+Electronic.Dance.Music+Pop+Hip.hop+Jazz+Rock+Metal+Blues+Instrumental.Film.or.TV.Series.Scores+Bengali.Music+covidord, data = data, method = 'class')
rpart.plot(ctd)

#Cross Validation 
printcp(ct)
printcp(ctd)
plotcp(ct)
plotcp(ctd)

#Pruning the trees
ctsf = prune(ct, cp=   ct$cptable[which.min(ct$cptable[,"xerror"]),"CP"])
rpart.plot(ctsf)
ctdf = prune(ctd, cp=   ctd$cptable[which.min(ctd$cptable[,"xerror"]),"CP"])
rpart.plot(ctdf)
printcp(ctsf)
printcp(ctdf)
summary(ctsf)
summary(ctdf)
#We can see some association of PHQ9 and PSS Scores with the genres Classical and Country
#We perform further non-parametric test
#Fist we split the data into two groups based on classical music
uclsc = data[which(data$Classical >6),]
lclsc = data[which(data$Classical <7),]
summary(uclsc$PSS)
summary(lclsc$PSS)
wilcox.test(uclsc$PSS,lclsc$PSS, alternative = "two.sided")

#If we separately split the data in three groups based on classical music and country music
uclsc = data[which(data$Classical >6),]
mclsc = data[which(data$Classical <7 & data$Classical >3),]
lclsc = data[which(data$Classical <4),]

uctr = data[which(data$Country >6),]
mctr = data[which(data$Country <7 & data$Country >3),]
lctr = data[which(data$Country <4),]

data$ClassicalC = ifelse(data$Classical>6,"Very often listened",
                         ifelse(data$Classical>3,"Sometimes listened","Rarely listened"))

data$CountryC = ifelse(data$Country>6,"Very often listened",
                         ifelse(data$Country>3,"Sometimes listened","Rarely listened"))

#Running tests on PSS Scores
#For Classical Music Categories
anova(aov(data$PSS~data$ClassicalC))
t.test(mclsc$PSS,uclsc$PSS, alternative = "greater")
t.test(lclsc$PSS,mclsc$PSS, alternative = "greater")

#For country music categories
anova(aov(data$PSS~data$CountryC))
t.test(mctr$PSS,uctr$PSS, alternative = "greater")
t.test(lctr$PSS,mctr$PSS, alternative = "greater")

#For time spent listening to music categories
anova(aov(data$PSS~data$time))

#For the severity of 2nd wave of pandemic categories
anova(aov(data$PSS~data$covid))
t.test(data[which(data$covidord == 3),]$PSS,data[which(data$covidord == 2),]$PSS, alternative = "greater")
t.test(data[which(data$covidord == 2),]$PSS,data[which(data$covidord == 1),]$PSS, alternative = "greater")
t.test(data[which(data$covidord == 1),]$PSS,data[which(data$covidord == 0),]$PSS, alternative = "greater")

#We now look at the odds ratios, by creating contingency tables
table(data$ClassicalC, data$PSSC)
table(data$CountryC, data$PSSC)
table(data$time, data$PSSC)
table(data$covid, data$PSSC)

#Now we do the tests to the PHQ9 Variable
kruskal.test(list(lctr$PHQSC, mctr$PHQSC, uctr$PHQSC))
wilcox.test(mctr$PHQSC,uctr$PHQSC, alternative = "greater")
wilcox.test(lctr$PHQSC,mctr$PHQSC, alternative = "greater")

#Further we check importance of Country, Blues and EDM for Depression scores
ublues = data[which(data$Blues >6),]
mblues = data[which(data$Blues <7 & data$Blues >3),]
lblues = data[which(data$Blues <4),]

uedm = data[which(data$Electronic.Dance.Music >6),]
medm = data[which(data$Electronic.Dance.Music <7 & data$Electronic.Dance.Music >3),]
ledm = data[which(data$Electronic.Dance.Music <4),]

#Performing Non-parametric test
kruskal.test(list(lblues$PHQSC, mblues$PHQSC, ublues$PHQSC))
kruskal.test(list(ledm$PHQSC, medm$PHQSC, uedm$PHQSC))
kruskal.test(list(dataT1$PHQSC, dataT2$PHQSC, dataT3$PHQSC))
kruskal.test(list(data[which(data$covidord == 3),]$PHQSC,
                  data[which(data$covidord == 2),]$PHQSC,
                  data[which(data$covidord == 1),]$PHQSC,
                  data[which(data$covidord == 0),]$PHQSC))
wilcox.test(data[which(data$covidord == 3),]$PHQSC,data[which(data$covidord == 2),]$PHQSC, alternative = "greater")
wilcox.test(data[which(data$covidord == 2),]$PHQSC,data[which(data$covidord == 1),]$PHQSC, alternative = "greater")
wilcox.test(data[which(data$covidord == 1),]$PHQSC,data[which(data$covidord == 0),]$PHQSC, alternative = "greater")

#Contingency tables for odds ratios
table(data$CountryC, data$PHQSCC)
table(data$time, data$PHQSCC)
table(data$covid, data$PHQSCC)

#Regression
LM_PSS = lm(PSS~Classical+Bollywood+Country+Electronic.Dance.Music+Pop+Hip.hop+Jazz+Rock+Metal+Blues+Instrumental.Film.or.TV.Series.Scores+Bengali.Music+covidord, data = data)
summary(LM_PSS)
LM_PHQ = lm(PHQSC~Classical+Bollywood+Country+Electronic.Dance.Music+Pop+Hip.hop+Jazz+Rock+Metal+Blues+Instrumental.Film.or.TV.Series.Scores+Bengali.Music+covidord, data = data)
summary(LM_PHQ)
