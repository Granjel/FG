library(data.table)
library(fitdistrplus)
library(xtable)


#3 spp
df <- fread("results/str_coex/FG-metrics_coexistence.txt")
df <- df[which(df$scenario == 5),]


descdist(df$SND + exp(1))
descdist(df$SFD + exp(1))


snd3 <- glm(SND + exp(1) ~ dominance * skewness * kurtosis, data = df, family = "Gamma"(link = "log"))
#SND = 0.4436 + 0.2112 * S + 0.0477 * K - 0.0153 * S * K

sfd3 <- glm(SFD + exp(1) ~ dominance * skewness * kurtosis, data = df, family = "Gamma"(link = "log"))
#SFD = 0.4436 + 0.2112 * S + 0.0477 * K - 0.0153 * S * K

summary(snd3)
summary(sfd3)
xtable(snd3)
xtable(sfd3)



#4 spp
df <- fread("results/str_coex/FG-metrics_coexistence-4spp.txt")
df <- df[which(df$scenario == 5),]

summary(glm(SND ~ dominance * skewness * kurtosis, data = df))
#SND = 0.4436 + 0.2112 * S + 0.0477 * K - 0.0153 * S * K

summary(glm(SFD ~ dominance * skewness * kurtosis, data = df))
#SFD = 0.4436 + 0.2112 * S + 0.0477 * K - 0.0153 * S * K

print(xtable(summary(glm(SFD ~ dominance * skewness * kurtosis, data = df))))









