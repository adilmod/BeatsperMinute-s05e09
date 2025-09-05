library(tidyverse)
library(Hmisc)
library(skimr)



# Put the data in long format (all vars except BeatsPerMinute)
train_long <- train %>%
  pivot_longer(
    cols = -BeatsPerMinute,
    names_to = "Variable",
    values_to = "Value"
  )

# Scatter plots: BeatsPerMinute vs each variable
ggplot(train_long, aes(x = Value, y = BeatsPerMinute)) +
  geom_point(aes(color = Variable), alpha = 0.3, size = 0.7) +
  facet_wrap(~ Variable, scales = "free_x") +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal() +
  labs(
    title = "Scatterplots: Each Variable vs. BeatsPerMinute",
    x = "Variable Value",
    y = "Beats Per Minute"
  )

glimpse(train_long)


# Run correlations
cor_results <- rcorr(as.matrix(train))

r_vec <- cor_results$r["BeatsPerMinute", ]
p_vec <- cor_results$P["BeatsPerMinute", ]

# 3) Build a tidy table (drop self-correlation)
bpm_corr <- tibble(
  Variable     = names(r_vec),
  Correlation  = as.numeric(r_vec),
  Pvalue       = as.numeric(p_vec)
) %>%
  filter(Variable != "BeatsPerMinute") %>%
  mutate(
    Pvalue_fmt = ifelse(Pvalue < 0.001, "<0.001", sprintf("%.3f", Pvalue)),
    # optional: add significance stars
    Sig = case_when(
      Pvalue < 0.001 ~ "***",
      Pvalue < 0.01  ~ "**",
      Pvalue < 0.05  ~ "*",
      TRUE           ~ ""
    )
  ) %>%
  arrange(Pvalue)  # or arrange(desc(abs(Correlation))) if you prefer by magnitude

bpm_corr


df <- subset(train, select = c(MoodScore, BeatsPerMinute))
df <- df[complete.cases(df), ]

ggplot2::ggplot(df, ggplot2::aes(x = MoodScore, y = BeatsPerMinute)) +
  ggplot2::geom_point(color = "red", alpha = 0.3, size = 0.7) +
  ggplot2::geom_smooth(method = "lm", se = FALSE, color = "blue", lwd = 1) +
  ggplot2::theme_minimal() +
  ggplot2::labs(title = "MoodScore vs BeatsPerMinute",
                x = "MoodScore",
                y = "BeatsPerMinute")
