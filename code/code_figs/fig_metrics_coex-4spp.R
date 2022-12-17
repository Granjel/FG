library(tidyverse)
library(ggplot2)
library(ggpubr)
library(MASS)
library(beepr)
library(data.table)
library(readr)

#df <- read.table("results/multi_metrics3.txt", sep = "\t", header = TRUE)
df <- fread("results/str_coex/FG-metrics_coexistence-4spp.txt")
#df <- df[which(df$scenario == 5),] all scenarios combined


df$dominance <- log(df$dominance)

df$dominance <- as.factor(round(df$dominance, 1))
df$skewness <- as.factor(round(df$skewness, 1))
df$kurtosis <- as.factor(round(df$kurtosis, 1))

sz <- 16
csz <- 0.7

df1 <- 
  df %>%
  group_by(dominance, skewness) %>%
  summarise_at(vars(c("SND", "SFD")), list(mean))

fig3a1 <- 
  ggplot(data = df1, aes(x = as.numeric(as.character(skewness)), y = as.numeric(as.character(dominance)))) +
  geom_tile(aes(fill = log(SND + 0.1))) +
  labs(x = "Skewness", y = "Diagonal dominance (ln)", title = "",
       fill = "Structural niche differences (SND; ln)") +
  scale_x_continuous(limits = c(-3, 3), expand = c(0.01, 0.01)) +
  scale_y_continuous(limits = c(-8, 4), expand = c(0.01, 0.01)) +
  scale_fill_viridis_c(option = "A", direction = -1, values = c(-0.1, 1.15)) +
  geom_hline(yintercept = 0, color = "gray60", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "gray60", linetype = "dashed") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = sz, face = "bold"),
        legend.text = element_text(size = sz * csz),
        axis.text = element_text(size = sz * csz),
        axis.title.x = element_text(colour = "black", size = sz),
        axis.title.y = element_text(colour = "black", size = sz),
        legend.position = "top"); fig3a1

fig3b1 <- 
  ggplot(data = df1, aes(x = as.numeric(as.character(skewness)), y = as.numeric(as.character(dominance)))) +
  geom_tile(aes(fill = SFD)) +
  labs(x = "Skewness", y = "Diagonal dominance (ln)", title = "",
       fill = "Structural fitness differences (SFD)") +
  scale_x_continuous(limits = c(-3, 3), expand = c(0.01, 0.01)) +
  scale_y_continuous(limits = c(-8, 4), expand = c(0.01, 0.01)) +
  scale_fill_viridis_c(direction = -1, values = c(-0.1, 1.15)) +
  geom_hline(yintercept = 0, color = "gray60", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "gray60", linetype = "dashed") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = sz, face = "bold"),
        legend.text = element_text(size = sz * csz),
        axis.text = element_text(size = sz * csz),
        axis.title.x = element_text(colour = "black", size = sz),
        axis.title.y = element_text(colour = "black", size = sz),
        legend.position = "top"); fig3b1


df2 <- 
  df %>%
  group_by(dominance, kurtosis) %>%
  summarise_at(vars(c("SND", "SFD")), list(mean))

fig3a2 <- 
  ggplot(data = df2, aes(x = as.numeric(as.character(dominance)), y = as.numeric(as.character(kurtosis)))) +
  geom_tile(aes(fill = log(SND + 0.1))) +
  labs(x = "Diagonal dominance (ln)", y = "Kurtosis", title = "",
       fill = "Structural niche differences (SND; ln)") +
  scale_y_continuous(expand = c(0.01, 0.01)) +
  scale_x_continuous(limits = c(-8, 4), expand = c(0.01, 0.01)) +
  scale_fill_viridis_c(option = "A", direction = -1, values = c(-0.1, 1.15)) +
  geom_hline(yintercept = 0, color = "gray60", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "gray60", linetype = "dashed") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = sz, face = "bold"),
        legend.text = element_text(size = sz * csz),
        axis.text = element_text(size = sz * csz),
        axis.title.x = element_text(colour = "black", size = sz),
        axis.title.y = element_text(colour = "black", size = sz),
        legend.position = "top"); fig3a2

fig3b2 <- 
  ggplot(data = df2, aes(x = as.numeric(as.character(dominance)), y = as.numeric(as.character(kurtosis)))) +
  geom_tile(aes(fill = SFD)) +
  labs(x = "Diagonal dominance (ln)", y = "Kurtosis", title = "",
       fill = "Structural fitness differences (SFD)") +
  scale_y_continuous(expand = c(0.01, 0.01)) +
  scale_x_continuous(limits = c(-8, 4), expand = c(0.01, 0.01)) +
  scale_fill_viridis_c(direction = -1, values = c(-0.1, 1.15)) +
  geom_hline(yintercept = 0, color = "gray60", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "gray60", linetype = "dashed") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = sz, face = "bold"),
        legend.text = element_text(size = sz * csz),
        axis.text = element_text(size = sz * csz),
        axis.title.x = element_text(colour = "black", size = sz),
        axis.title.y = element_text(colour = "black", size = sz),
        legend.position = "top"); fig3b2


df3 <- 
  df %>%
  group_by(skewness, kurtosis) %>%
  summarise_at(vars(c("SND", "SFD")), list(mean))

fig3a3 <- 
  ggplot(data = df3, aes(x = as.numeric(as.character(skewness)), y = as.numeric(as.character(kurtosis)))) +
  geom_tile(aes(fill = log(SND + 0.1))) +
  labs(x = "Skewness", y = "Kurtosis", title = "",
       fill = "Structural niche differences (SND; ln)") +
  scale_y_continuous(expand = c(0.01, 0.01)) +
  scale_x_continuous(limits = c(-3, 3), expand = c(0.01, 0.01)) +
  scale_fill_viridis_c(option = "A", direction = -1, values = c(-0.1, 1.15)) +
  geom_hline(yintercept = 0, color = "gray60", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "gray60", linetype = "dashed") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = sz, face = "bold"),
        legend.text = element_text(size = sz * csz),
        axis.text = element_text(size = sz * csz),
        axis.title.x = element_text(colour = "black", size = sz),
        axis.title.y = element_text(colour = "black", size = sz),
        legend.position = "top"); fig3a3

fig3b3 <- 
  ggplot(data = df3, aes(x = as.numeric(as.character(skewness)), y = as.numeric(as.character(kurtosis)))) +
  geom_tile(aes(fill = SFD)) +
  labs(x = "Skewness", y = "Kurtosis", title = "",
       fill = "Structural fitness differences (SFD)") +
  scale_y_continuous(expand = c(0.01, 0.01)) +
  scale_x_continuous(limits = c(-3, 3), expand = c(0.01, 0.01)) +
  scale_fill_viridis_c(direction = -1, values = c(-0.1, 1.15)) +
  geom_hline(yintercept = 0, color = "gray60", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "gray60", linetype = "dashed") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = sz, face = "bold"),
        legend.text = element_text(size = sz * csz),
        axis.text = element_text(size = sz * csz),
        axis.title.x = element_text(colour = "black", size = sz),
        axis.title.y = element_text(colour = "black", size = sz),
        legend.position = "top"); fig3b3


##POINTS

##SND
#fig3a1 <- ggplot(data = df, aes(x = skewness, y = dominance)) +
#  geom_point(aes(colour = log(SND + 0.1)), size = sz, alpha = al) +
#  scale_x_continuous(expand = c(0.01, 0.01)) +
#  scale_y_continuous(limits = c(-8, 4), expand = c(0.01, 0.01)) +
#  scale_colour_gradientn(colours = c("orange", "black")) +
#  geom_hline(yintercept = 0, color = "gray60", linetype = "dashed") +
#  geom_vline(xintercept = 0, color = "gray60", linetype = "dashed") +
#  labs(x = "Skewness\n", y = "Diagonal dominance (log)", colour = "SND\n(log)") +
#  theme_bw() +
#  theme(panel.grid = element_blank(), legend.position = "top")
#
#fig3a2 <- ggplot(data = df, aes(x = dominance, y = kurtosis)) +
#  geom_point(aes(colour = log(SND + 0.1)), size = sz, alpha = al) +
#  scale_x_continuous(limits = c(-8, 4), expand = c(0.01, 0.01)) +
#  scale_y_continuous(expand = c(0.01, 0.01)) +
#  scale_colour_gradientn(colours = c("orange", "black")) +
#  geom_hline(yintercept = 0, color = "gray60", linetype = "dashed") +
#  geom_vline(xintercept = 0, color = "gray60", linetype = "dashed") +
#  labs(x = "Diagonal dominance (log)\n", y = "Kurtosis", colour = "SND\n(log)") +
#  theme_bw() +
#  theme(panel.grid = element_blank(), legend.position = "top")
#
#fig3a3 <- ggplot(data = df, aes(x = skewness, y = kurtosis)) +
#  geom_point(aes(colour = log(SND + 0.1)), size = sz, alpha = al) +
#  scale_x_continuous(expand = c(0.01, 0.01)) +
#  scale_y_continuous(expand = c(0.01, 0.01)) +
#  scale_colour_gradientn(colours = c("orange", "black")) +
#  geom_hline(yintercept = 0, color = "gray60", linetype = "dashed") +
#  geom_vline(xintercept = 0, color = "gray60", linetype = "dashed") +
#  labs(x = "Skewness\n", y = "Kurtosis", colour = "SND\n(log)") +
#  theme_bw() +
#  theme(panel.grid = element_blank(), legend.position = "top")
#
#
#
##SFD
#fig3b1 <- ggplot(data = df, aes(x = skewness, y = dominance)) +
#  geom_point(aes(colour = SFD), size = sz, alpha = al) +
#  scale_x_continuous(expand = c(0.01, 0.01)) +
#  scale_y_continuous(limits = c(-8, 4), expand = c(0.01, 0.01)) +
#  scale_colour_viridis_c(direction = -1) +
#  geom_hline(yintercept = 0, color = "gray60", linetype = "dashed") +
#  geom_vline(xintercept = 0, color = "gray60", linetype = "dashed") +
#  labs(x = "Skewness\n", y = "Diagonal dominance (log)", colour = "SFD\n") +
#  theme_bw() +
#  theme(panel.grid = element_blank(), legend.position = "top")
#
#fig3b2 <- ggplot(data = df, aes(x = dominance, y = kurtosis)) +
#  geom_point(aes(colour = SFD), size = sz, alpha = al) +
#  scale_x_continuous(limits = c(-8, 4), expand = c(0.01, 0.01)) +
#  scale_y_continuous(expand = c(0.01, 0.01)) +
#  scale_colour_viridis_c(direction = -1) +
#  geom_hline(yintercept = 0, color = "gray60", linetype = "dashed") +
#  geom_vline(xintercept = 0, color = "gray60", linetype = "dashed") +
#  labs(x = "Diagonal dominance (log)\n", y = "Kurtosis", colour = "SFD\n") +
#  theme_bw() +
#  theme(panel.grid = element_blank(), legend.position = "top")
#
#fig3b3 <- ggplot(data = df, aes(x = skewness, y = kurtosis)) +
#  geom_point(aes(colour = SFD), size = sz, alpha = al) +
#  scale_x_continuous(expand = c(0.01, 0.01)) +
#  scale_y_continuous(expand = c(0.01, 0.01)) +
#  scale_colour_viridis_c(direction = -1) +
#  geom_hline(yintercept = 0, color = "gray60", linetype = "dashed") +
#  geom_vline(xintercept = 0, color = "gray60", linetype = "dashed") +
#  labs(x = "Skewness\n", y = "Kurtosis", colour = "SFD\n") +
#  theme_bw() +
#  theme(panel.grid = element_blank(), legend.position = "top")

#fig3b1 <- ggplot(data = df, aes(x = cv, y = dominance)) +
#  geom_point(aes(colour = SFD), size = sz, alpha = al) +
#  scale_x_continuous(limits = c(-15, 15), expand = c(0.01, 0.01)) +
#  scale_y_continuous(limits = c(-8, 4), expand = c(0.01, 0.01)) +
#  scale_colour_viridis_c(direction = -1) +
#  geom_hline(yintercept = 0, color = "gray60", linetype = "dashed") +
#  geom_vline(xintercept = 0, color = "gray60", linetype = "dashed") +
#  labs(x = "Coefficient of variation (CV)", y = "Diagonal dominance (log scale)") +
#  theme_bw() +
#  theme(panel.grid = element_blank()); fig3b1
#
#fig3b2 <- ggplot(data = df, aes(x = cv, y = SKR)) +
#  geom_point(aes(colour = SFD), size = sz, alpha = al) +
#  scale_x_continuous(limits = c(-15, 15), expand = c(0.01, 0.01)) +
#  scale_y_continuous(limits = c(-10, 10), expand = c(0.01, 0.01)) +
#  scale_colour_viridis_c(direction = -1) +
#  geom_hline(yintercept = 0, color = "gray60", linetype = "dashed") +
#  geom_vline(xintercept = 0, color = "gray60", linetype = "dashed") +
#  labs(x = "Coefficient of variation (CV)", y = "SKR") +
#  theme_bw() +
#  theme(panel.grid = element_blank()); fig3b2
#
#fig3b3 <- ggplot(data = df, aes(x = dominance, y = SKR)) +
#  geom_point(aes(colour = SFD), size = sz, alpha = al) +
#  scale_x_continuous(limits = c(-8, 4), expand = c(0.01, 0.01)) +
#  scale_y_continuous(limits = c(-4, 4), expand = c(0.01, 0.01)) +
#  scale_colour_viridis_c(direction = -1) +
#  geom_hline(yintercept = 0, color = "gray60", linetype = "dashed") +
#  geom_vline(xintercept = 0, color = "gray60", linetype = "dashed") +
#  labs(x = "Diagonal dominance (log scale)", y = "SKR") +
#  theme_bw() +
#  theme(panel.grid = element_blank()); fig3b3
#
#fig3b3s <- ggplot(data = df, aes(x = dominance, y = skewness)) +
#  geom_point(aes(colour = SFD), size = sz, alpha = al) +
#  scale_x_continuous(limits = c(-8, 4), expand = c(0.01, 0.01)) +
#  #scale_y_continuous(limits = c(-4, 4), expand = c(0.01, 0.01)) +
#  scale_colour_viridis_c(direction = -1) +
#  geom_hline(yintercept = 0, color = "gray60", linetype = "dashed") +
#  geom_vline(xintercept = 0, color = "gray60", linetype = "dashed") +
#  labs(x = "Diagonal dominance (log scale)", y = "Skewness") +
#  theme_bw() +
#  theme(panel.grid = element_blank()); fig3b3s
#
#fig3b3k <- ggplot(data = df, aes(x = dominance, y = kurtosis)) +
#  geom_point(aes(colour = SFD), size = sz, alpha = al) +
#  scale_x_continuous(limits = c(-8, 4), expand = c(0.01, 0.01)) +
#  #scale_y_continuous(limits = c(-4, 4), expand = c(0.01, 0.01)) +
#  scale_colour_viridis_c(direction = -1) +
#  geom_hline(yintercept = 0, color = "gray60", linetype = "dashed") +
#  geom_vline(xintercept = 0, color = "gray60", linetype = "dashed") +
#  labs(x = "Diagonal dominance (log scale)", y = "Kurtosis") +
#  theme_bw() +
#  theme(panel.grid = element_blank()); fig3b3k
#
#fig3b3 <- ggplot(data = df, aes(x = skewness, y = kurtosis)) +
#  geom_point(aes(colour = SFD), size = sz, alpha = al) +
#  #scale_x_continuous(limits = c(-8, 4), expand = c(0.01, 0.01)) +
#  #scale_y_continuous(limits = c(-4, 4), expand = c(0.01, 0.01)) +
#  scale_colour_viridis_c(direction = -1) +
#  geom_hline(yintercept = 0, color = "gray60", linetype = "dashed") +
#  geom_vline(xintercept = 0, color = "gray60", linetype = "dashed") +
#  labs(x = "Skewness", y = "Kurtosis") +
#  theme_bw() +
#  theme(panel.grid = element_blank()); fig3b3

#ggplot(data = df, aes(x = kurtosis, y = SND)) +
#  geom_smooth(color = "black", method = "gam", alpha = 0.45) +
#  theme_bw() + theme(panel.grid = element_blank())


fig3a <- ggarrange(fig3a3, fig3a1, fig3a2,
                   nrow = 1, ncol = 3,
                   common.legend = TRUE,
                   legend = "bottom", labels = c("a", "b", "c"), vjust = 1.5)
fig3b <- ggarrange(fig3b3, fig3b1, fig3b2,
                   nrow = 1, ncol = 3,
                   common.legend = TRUE,
                   legend = "bottom", labels = c("d", "e", "f"), vjust = 1.5)

fig4 <- ggarrange(fig3a, NULL, fig3b,
                  heights = c(1, 0.075, 1),
                  align = "v",
                  nrow = 3)

ggsave(fig4, filename = "figures/metrics_coexb-4spp.png", device = "png",
       width = 10.5, height = 9, dpi = 320)
