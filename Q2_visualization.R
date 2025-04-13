library(dplyr)
library(ggplot2)

# summarize the mean.payoff and mean.RT and mean performance of each Subject in the last 60% rounds
value_df <- read.csv("./allresult_processed.csv")
wsls_df  <- read.csv("./allresult_processed.csv")

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

value_metrics <- calc_metrics(value_df, "value_based")
wsls_metrics  <- calc_metrics(wsls_df,  "wsls")

all_metrics <- bind_rows(value_metrics, wsls_metrics)

# draw the box plot of mean.payoff and mean.RT 

# mean.payoff
p_payoff <- ggplot(all_metrics, aes(x = Policy, y = mean.payoff, fill = Policy)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.2, size = 1.5, alpha = 0.5, color = "black") +
  scale_fill_manual(values = c("value_based" = "lightblue", "wsls" = "tomato")) +
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
  stat_compare_means(comparisons = list(c('wsls','value_based')), 
                     method = "t.test", 
                     label = "p.signif",exact = FALSE,tip.length = 0.01,
                     bracket.size = 0.6, size = 5)


png("Q2_payoff.png", width = 1500, height = 1500, res = 300)
print(p_payoff)
dev.off()

#AC barplot
value_data = value_df %>%
  mutate(Policy = "Value-based")
wsls_data = wsls_df %>%
  mutate(Policy = "WSLS")
combined_data <- bind_rows(value_data, wsls_data)
ac_distribution <- combined_data %>%
  group_by(Policy, AC) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Policy) %>%
  mutate(prop = count / sum(count)) %>%
  ungroup()


ac_distribution$AC <- factor(ac_distribution$AC, levels = c(0, 0.5, 1))

# draw histogram
p_AC <- ggplot(ac_distribution, aes(x = AC, y = prop, fill = Policy)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  labs(title = "AC Distribution by Policy",
       x = "AC",
       y = "Proportion") +
  scale_fill_manual(values = c("Value-based" = "steelblue", "WSLS" = "tomato")) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

png("Q2_AC.png", width = 1500, height = 1500, res = 300)
print(p_AC)
dev.off()


# mean.RT
p_rt <- ggplot(all_metrics, aes(x = Policy, y = mean.RT, fill = Policy)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.2, size = 1.5, alpha = 0.5, color = "black") +
  scale_fill_manual(values = c("value based" = "lightblue", "wsls" = "tomato")) +
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
  stat_compare_means(comparisons = list(c('wsls','value_based')), 
                     method = "t.test", 
                     label = "p.signif",exact = FALSE,tip.length = 0.01,
                     bracket.size = 0.6, size = 5)

png("Q2_rt.png", width = 1500, height = 1500, res = 300)
print(p_rt)
dev.off()

# mean.performance
p_performance <- ggplot(all_metrics, aes(x = Policy, y = mean.performance, fill = Policy)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.2, size = 1.5, alpha = 0.5, color = "black") +
  scale_fill_manual(values = c("value based" = "lightblue", "wsls" = "tomato")) +
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
  stat_compare_means(comparisons = list(c('wsls','value_based')), 
                     method = "wilcox.test", 
                     label = "p.signif",exact = FALSE,tip.length = 0.01,
                     bracket.size = 0.6, size = 5)

png("Q2_performance.png", width = 1500, height = 1500, res = 300)
print(p_performance)
dev.off()

#do test
library(car)
library(ggpubr)
#performance
shapiro.test(all_metrics$mean.performance[all_metrics$Policy=="value_based"]) #p-value = 0.005436
shapiro.test(all_metrics$mean.performance[all_metrics$Policy=="wsls"])#p-value = 0.001646
leveneTest(mean.performance ~ Policy, data = all_metrics) #p=0.001662
wilcox.test(mean.performance ~ Policy, data = all_metrics) #p-value = 0.006166

#payoff
shapiro.test(all_metrics$mean.payoff[all_metrics$Policy=="value_based"]) #p-value = 0.1694
shapiro.test(all_metrics$mean.payoff[all_metrics$Policy=="wsls"]) #p-value = 0.9678
leveneTest(mean.payoff ~ Policy, data = all_metrics) #p=0.98,do t test
t.test(mean.payoff ~ Policy, data = all_metrics) #p-value = 0.002

#Reaction Time
shapiro.test(all_metrics$mean.RT[all_metrics$Policy=="value_based"]) #p=0.123
shapiro.test(all_metrics$mean.RT[all_metrics$Policy=="wsls"])#p=0.003
leveneTest(mean.RT ~ Policy, data = all_metrics)#0.6548
t.test(mean.RT ~ Policy, data = all_metrics)#p-value 2.188e-08
