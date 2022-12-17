#Nico Gross's data
raw <- read.table("data/FG_raw_cover.txt", sep = "\t", header = TRUE)

library(dplyr)

summary_df <- raw %>% 
  group_by(Spcode) %>% 
  summarise(Cover = sum(Cover),
            Count = n())

summary_df$Cover <- (summary_df$Cover / sum(summary_df$Cover)) * 100
summary_df$Count <- (summary_df$Count / sum(summary_df$Count)) * 100
summary_df$Cover <- round(summary_df$Cover, 1)
summary_df$Count <- round(summary_df$Count, 1)

library(ggplot2)

ggplot(summary_df, aes(x = reorder(Spcode, Cover), y = Cover)) + 
  geom_col() +
  labs(y = "Species cover (% of the total)",
       x = "Plant species code") +
  coord_flip()

