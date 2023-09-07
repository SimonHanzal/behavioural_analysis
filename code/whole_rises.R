library(tidyverse)
library(lmerTest)
library(lme4)
library(ez)

task_related <- read_csv("taskrelated_blocks_long.csv") %>%
    mutate(age_group = ifelse(participant <= 10600, 0, 1))

inter_trial <- read_csv("intertrial_blocks_long.csv") %>%
    filter(participant != 10007) %>%
    mutate(age_group = ifelse(participant <= 10600, 0, 1))

task_related_summary <- task_related %>%
    group_by(block) %>%
    summarise(summary = mean(signal))

inter_trial_summary <- inter_trial %>%
    group_by(block) %>%
    summarise(summary = mean(signal))

model <- lm(signal ~ block*age_group, task_related)
model2 <- lm(signal ~ block, task_related)
anova(model, model2)
summary(model)

model <- lm(signal ~ block*age_group, inter_trial)
model2 <- lm(signal ~ block, inter_trial)
anova(model, model2)
summary(model)


model <- lmer(signal ~ block + (1|participant), task_related)
summary(model)

model <- lmer(signal ~ block + (1|participant), inter_trial)
summary(model)

ggplot(data = task_related, aes(x = block, y = signal)) +
    geom_violin(aes(fill = as.factor(block)), alpha = 0.8) +
    geom_boxplot(aes(fill = as.factor(block)), alpha = 0.8) +
    geom_smooth(method = "lm", colour = "black", size = 3) +
    labs(title = "Task-related lower beta") +
    theme_minimal() +
    theme(legend.position="none") +
    scale_x_continuous(breaks = round(seq(min(task_related$block), max(task_related$block), by = 1),1))

    

ggplot(data = inter_trial, aes(x = block, y = signal)) +
    geom_violin(aes(fill = as.factor(block)), alpha = 0.8) +
    geom_boxplot(aes(fill = as.factor(block)), alpha = 0.8) +
    geom_smooth(method = "lm", colour = "black", size = 3) +
    labs(title = "Inter-trial parietal alpha") +
    theme_minimal() +
    theme(legend.position="none") +
    scale_x_continuous(breaks = round(seq(min(task_related$block), max(task_related$block), by = 1),1))

trial_type <- read_csv("trial_type.csv") %>%
  mutate(age_group = ifelse(participant <= 10600, 0, 1))
  

model <- ezANOVA(
  trial_type,
  dv = cz_power,
  wid = .(participant),
  between = .(trial, age_group),
  type = 2,
  detailed = TRUE
)
model

ggplot(data = trial_type, aes(x = trial, y = cz_power, fill = as.character(age_group))) +
  geom_violin(alpha = 0.8) +
  geom_boxplot(alpha = 0.8) +
  labs(title = "Inter-trial parietal alpha") +
  theme_minimal() +
  theme(legend.position="none")

age_difference <- read_csv("difference_group.csv") %>%
  mutate(age_group = ifelse(participant <= 10600, 0, 1))

age_just_alpha_young <- age_difference %>%
  filter(X3 == "alpha", age_group == 0)

age_just_alpha_older <- age_difference %>%
  filter(X3 == "alpha", age_group == 1)

model <- t.test(age_just_alpha_older$power, age_just_alpha_young$power)
model

young <- age_difference %>%
  filter(age_group == 0) %>%
  mutate(power_z = scale(power, center = TRUE, scale = TRUE)) %>%
  filter(X3 == "alpha") %>%
  summarise(mean = mean(power_z), sd = sd(power_z))

older <- age_difference %>%
  filter(age_group == 1) %>%
  mutate(power_z = scale(power, center = TRUE, scale = TRUE)) %>%
  filter(X3 == "alpha") %>%
  summarise(mean = mean(power_z), sd = sd(power_z))

age_difference_summary <- age_difference %>%
  group_by(age_group) %>%
  summarise(mean = mean(power), sd = sd(power))

age_difference_z <- age_difference %>%
  group_by(age_group) %>%
  summarise(mean = mean(power_z), sd = sd(power_z))
