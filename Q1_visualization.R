library(dplyr)
library(ggplot2)

# summarize the mean.payoff and mean.RT and mean performance of each Subject in the last 60% rounds
schema_df <- read.csv("./allresult_processed.csv")
high_payoff_df  <- read.csv("./allresult_processed.csv")

calc_metrics <- function(df, policy_name) {
  df %>%
    group_by(Subject) %>%
    filter(Round > max(Round) * 0.4) %>%
    summarise(
      mean.payoff = mean(payoff, na.rm = TRUE),
      mean.RT     = mean(RT_1 + RT_2 + RT_3 + RT_4, na.rm = TRUE),
      mean.performance = mean(performance, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(Policy = policy_name)
}

schema_metrics <- calc_metrics(schema_df, "schema_based")
high_payoff_metrics  <- calc_metrics(high_payoff_df,  "high_payoff")

all_metrics <- bind_rows(schema_metrics, high_payoff_metrics)

# draw the box plot of mean.payoff and mean.RT

# mean.payoff
p_payoff <- ggplot(all_metrics, aes(x = Policy, y = mean.payoff, fill = Policy)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.2, size = 1.5, alpha = 0.5, color = "black") +
  scale_fill_manual(values = c("schema_based" = "lightblue","high_payoff" = "tomato")) +
  labs(
    title = "Mean Payoff (Last 60% of Rounds)",
    x = "Decision Policy",
    y = "Mean Payoff"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title     = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none"
  ) +
  stat_compare_means(comparisons = list(c('schema_based','high_payoff')), 
                     method = "t.test", 
                     label = "p.signif",exact = FALSE,tip.length = 0.01,
                     bracket.size = 0.6, size = 5)


png("Q1_payoff.png", width = 1500, height = 1500, res = 300)
print(p_payoff)
dev.off()

# mean.RT
p_rt <- ggplot(all_metrics, aes(x = Policy, y = mean.RT, fill = Policy)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.2, size = 1.5, alpha = 0.5, color = "black") +
  scale_fill_manual(values = c("high_payoff" = "tomato","schema based" = "lightblue")) +
  # scale_y_continuous(
  #   limits = c(min(all_metrics$mean.RT),mean(all_metrics$mean.RT)*1.6)
  # ) +
  labs(
    title = "Mean RT (Last 60% of Rounds)",
    x = "Decision Policy",
    y = "Mean RT (s)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title     = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none"
  ) +
  stat_compare_means(comparisons = list(c('high_payoff','schema_based')), 
                     method = "wilcox.test", 
                     label = "p.signif",exact = FALSE,tip.length = 0.01,
                     bracket.size = 0.6, size = 5)

png("Q1_rt.png", width = 1500, height = 1500, res = 300)
print(p_rt)
dev.off()

# mean.performance
p_performance <- ggplot(all_metrics, aes(x = Policy, y = mean.performance, fill = Policy)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.2, size = 1.5, alpha = 0.5, color = "black") +
  scale_fill_manual(values = c("high_payoff" = "tomato","schema based" = "lightblue")) +
  # scale_y_continuous(
  #   limits = c(min(all_metrics$mean.performance),mean(all_metrics$mean.performance)*1.6)
  # ) +
  labs(
    title = "Mean performance (Last 60% of Rounds)",
    x = "Decision Policy",
    y = "Mean performance"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title     = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none"
  ) +
  stat_compare_means(comparisons = list(c('high_payoff','schema_based')), 
                     method = "wilcox.test", 
                     label = "p.signif",exact = FALSE,tip.length = 0.01,
                     bracket.size = 0.6, size = 5)

png("Q1_performance.png", width = 1500, height = 1500, res = 300)
print(p_performance)
dev.off()

#do test
library(car)
library(ggpubr)
#performance
shapiro.test(all_metrics$mean.performance[all_metrics$Policy=="schema_based"]) #p-value = 0.005436
shapiro.test(all_metrics$mean.performance[all_metrics$Policy=="high_payoff"])#p-value = 0.004817
leveneTest(mean.performance ~ Policy, data = all_metrics) #p=4.461e-05
wilcox.test(mean.performance ~ Policy, data = all_metrics) #p-value < 2.2e-16

#payoff
shapiro.test(all_metrics$mean.payoff[all_metrics$Policy=="schema_based"]) #p-value = 0.1694
shapiro.test(all_metrics$mean.payoff[all_metrics$Policy=="high_payoff"]) #p-value = 0.09339
leveneTest(mean.payoff ~ Policy, data = all_metrics) #p=0.00330,do welch t test
t.test(mean.payoff ~ Policy, data = all_metrics) #p-value = 0.006141

#Reaction Time
shapiro.test(all_metrics$mean.RT[all_metrics$Policy=="schema_based"]) #p=0.123
shapiro.test(all_metrics$mean.RT[all_metrics$Policy=="high_payoff"])#p=3.811e-09
leveneTest(mean.RT ~ Policy, data = all_metrics)#0.02798
wilcox.test(mean.RT ~ Policy, data = all_metrics)#p-value < 2.2e-16

