library(data.table)
library(fitdistrplus)
library(xtable)

plus <- exp(1) #for SND



## 3 spp
df3 <- fread("results/str_coex/FG-metrics_coexistence-complete.txt")
df3 <- df3[which(df3$scenario == 5),] #full network

#distribution
df3_complete <- df3[complete.cases(df3$SND),]
df3_complete <- df3_complete[complete.cases(df3_complete$SFD),]

descdist(df3_complete$SND + plus)
descdist(df3_complete$SFD)

#model
snd3 <- glm(SND + plus ~ dominance * skewness * kurtosis * PNR, data = df3, family = "Gamma"(link = "log"))
sfd3 <- glm(SFD ~ dominance * skewness * kurtosis * PNR, data = df3, family = "Gamma"(link = "log"))

#summaries
summary(snd3)
summary(sfd3)
#xtable(snd3)
#xtable(sfd3)



## 4 spp
df4 <- fread("results/str_coex/FG-metrics_coexistence-complete-4spp.txt")
df4 <- df4[which(df4$scenario == 5),] #full network

#distribution
df4_complete <- df4[complete.cases(df4$SND),]
df4_complete <- df4_complete[complete.cases(df4_complete$SFD),]

descdist(df4_complete$SND + plus)
descdist(df4_complete$SFD)

#model
snd4 <- glm(SND + plus ~ dominance * skewness * kurtosis * PNR, data = df4, family = "Gamma"(link = "log"))
sfd4 <- glm(SFD ~ dominance * skewness * kurtosis * PNR, data = df4, family = "Gamma"(link = "log"))

#summaries
summary(snd4)
summary(sfd4)
#xtable(snd4)
#xtable(sfd4)