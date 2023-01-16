library(tidyverse)
library(ggplot2)
library(ggridges)
library(ggpubr)

spp <- read.table("results/str_coex/FG-structural_coexistence-complete.txt", header = TRUE, sep = "\t")
spp$scenario <- as.factor(spp$scenario)

spp_real <- read.table("results/str_coex/FG-structural_coexistence.txt", header = TRUE, sep = "\t")
spp_real$scenario <- as.factor(spp_real$scenario)


#define se function
se <- function(x) sqrt(var(x, na.rm = TRUE) / length(x))

#SND
chg_SND <- c(
  sum(abs(spp$SND[which(spp$scenario == 2)] - spp$SND[which(spp$scenario == 1)]), na.rm = TRUE) /
    (max(abs(spp$SND[which(spp$scenario == 2)]), na.rm = TRUE) * length(spp$SND[which(spp$scenario == 1)])) * 100,
  sum(abs(spp$SND[which(spp$scenario == 3)] - spp$SND[which(spp$scenario == 1)]), na.rm = TRUE) /
    (max(abs(spp$SND[which(spp$scenario == 3)]), na.rm = TRUE) * length(spp$SND[which(spp$scenario == 1)])) * 100,
  sum(abs(spp$SND[which(spp$scenario == 4)] - spp$SND[which(spp$scenario == 1)]), na.rm = TRUE) /
    (max(abs(spp$SND[which(spp$scenario == 4)]), na.rm = TRUE) * length(spp$SND[which(spp$scenario == 1)])) * 100,
  sum(abs(spp$SND[which(spp$scenario == 5)] - spp$SND[which(spp$scenario == 1)]), na.rm = TRUE) /
    (max(abs(spp$SND[which(spp$scenario == 5)]), na.rm = TRUE) * length(spp$SND[which(spp$scenario == 1)])) * 100
)

se_SND <- c(
  se(abs(spp$SND[which(spp$scenario == 2)] - spp$SND[which(spp$scenario == 1)])) /
    max(abs(spp$SND[which(spp$scenario == 2)]), na.rm = TRUE) * 100,
  se(abs(spp$SND[which(spp$scenario == 3)] - spp$SND[which(spp$scenario == 1)])) /
    max(abs(spp$SND[which(spp$scenario == 3)]), na.rm = TRUE) * 100,
  se(abs(spp$SND[which(spp$scenario == 4)] - spp$SND[which(spp$scenario == 1)])) /
    max(abs(spp$SND[which(spp$scenario == 4)]), na.rm = TRUE) * 100,
  se(abs(spp$SND[which(spp$scenario == 5)] - spp$SND[which(spp$scenario == 1)])) /
    max(abs(spp$SND[which(spp$scenario == 5)]), na.rm = TRUE) * 100
)

perc_SND <- data.frame(layer = as.factor(c("2", "3", "4", "5")), change = chg_SND, error = se_SND)
perc_SND[2, 2] <- 0
perc_SND[2, 3] <- NA

textsize <- 7

perc1 <- 
  ggplot(data = perc_SND, aes(x = change, y = layer)) +
  geom_errorbar(aes(xmin = change - error, xmax = change + error), width = 0.5) +
  geom_col(fill = "white", alpha = 1, width = 1, show.legend = FALSE) +
  geom_col(aes(fill = layer), alpha = 0.75, width = 1, show.legend = FALSE) +
  scale_y_discrete(limits = rev) +
  scale_fill_manual(values = c("gray20", "gray40", "gray60", "gray80"), guide = FALSE) +
  scale_x_continuous(breaks = seq(0, 1.5, 0.5), limits = c(0, 1.75), expand = c(0.01, 0.01)) +
  labs(x = expression(paste("Change from ", alpha, " (%)", sep = "")),
       y = "Scenario", title = "Structural fitness differences (SND)") +
  #geom_text(aes(label = "alpha + beta[alpha]"), x = perc_SND$change[1] / 2, y = 4, parse = T, colour = "white", size = textsize) +
  #geom_text(aes(label = "alpha + gamma"), x = 0.2, y = 3, parse = T, colour = "black", size = 6) +
  #geom_text(aes(label = "alpha + beta[alpha] + gamma"), x = perc_SND$change[3] / 2, y = 2, parse = T, colour = "black", size = textsize) +
  #geom_text(aes(label = "alpha + beta[alpha]"), x = perc_SND$change[4] / 2, y = 1.2, parse = T, colour = "black", size = textsize) +
  #geom_text(aes(label = "+ gamma + beta[gamma]"), x = perc_SND$change[4] / 2, y = 0.8, parse = T, colour = "black", size = textsize) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.title.x = element_text(color = "white", size = 10),
        axis.text.y = element_text(size = 12),
        title = element_text(size = 8)); perc1


#SFD
chg_SFD <- c(
  sum(abs(spp$SFD[which(spp$scenario == 2)] - spp$SFD[which(spp$scenario == 1)]), na.rm = TRUE) /
    (max(abs(spp$SFD[which(spp$scenario == 2)]), na.rm = TRUE) * length(spp$SFD[which(spp$scenario == 1)])) * 100,
  sum(abs(spp$SFD[which(spp$scenario == 3)] - spp$SFD[which(spp$scenario == 1)]), na.rm = TRUE) /
    (max(abs(spp$SFD[which(spp$scenario == 3)]), na.rm = TRUE) * length(spp$SFD[which(spp$scenario == 1)])) * 100,
  sum(abs(spp$SFD[which(spp$scenario == 4)] - spp$SFD[which(spp$scenario == 1)]), na.rm = TRUE) /
    (max(abs(spp$SFD[which(spp$scenario == 4)]), na.rm = TRUE) * length(spp$SFD[which(spp$scenario == 1)])) * 100,
  sum(abs(spp$SFD[which(spp$scenario == 5)] - spp$SFD[which(spp$scenario == 1)]), na.rm = TRUE) /
    (max(abs(spp$SFD[which(spp$scenario == 5)]), na.rm = TRUE) * length(spp$SFD[which(spp$scenario == 1)])) * 100
)

se_SFD <- c(
  se(abs(spp$SFD[which(spp$scenario == 2)] - spp$SFD[which(spp$scenario == 1)])) /
    max(abs(spp$SFD[which(spp$scenario == 2)]), na.rm = TRUE) * 100,
  se(abs(spp$SFD[which(spp$scenario == 3)] - spp$SFD[which(spp$scenario == 1)])) /
    max(abs(spp$SFD[which(spp$scenario == 3)]), na.rm = TRUE) * 100,
  se(abs(spp$SFD[which(spp$scenario == 4)] - spp$SFD[which(spp$scenario == 1)])) /
    max(abs(spp$SFD[which(spp$scenario == 4)]), na.rm = TRUE) * 100,
  se(abs(spp$SFD[which(spp$scenario == 5)] - spp$SFD[which(spp$scenario == 1)])) /
    max(abs(spp$SFD[which(spp$scenario == 5)]), na.rm = TRUE) * 100
)

perc_SFD <- data.frame(layer = as.factor(c("2", "3", "4", "5")), change = chg_SFD, error = se_SFD)

perc2 <- 
  ggplot(data = perc_SFD, aes(x = change, y = layer)) +
  geom_errorbar(aes(xmin = change - error, xmax = change + error), width = 0.5) +
  geom_col(fill = "white", alpha = 1, width = 1, show.legend = FALSE) +
  geom_col(aes(fill = layer), alpha = 0.75, width = 1, show.legend = FALSE) +
  scale_y_discrete(limits = rev) +
  scale_fill_manual(values = c("gray20", "gray40", "gray60", "gray80"), guide = FALSE) +
  scale_x_continuous(breaks = seq(0, 1.5, 0.5), limits = c(0, 1.5), expand = c(0.01, 0.01)) +
  labs(x = "Change from scenario 1 (%)",
       y = "Scenario", title = "Structural fitness differences (SFD)") +
  #geom_text(aes(label = round(change, 2)), hjust = c(-1.11, -0.8, -1.1, -1.3), color = c("black", "black", "black", "black")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.text.y = element_text(size = 12),
        title = element_text(size = 8)); perc2


perc_plot <- ggarrange(perc1, perc2, nrow = 2, align = "hv", labels = "auto", font.label = list(size = 12))

ggsave(perc_plot, filename = "figures/percentages.png", device = "png",
       width = 5, height = 4, dpi = 320)

