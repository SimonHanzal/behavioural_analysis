# Simon Hanzal (c)

# Setup----

# Modify path as needed depending on where the files are stored in your device
path <- r"(C:\Users\hanza\OneDrive\Dokumenty\R)"
#path <- r"(C:\Users\simonha\OneDrive - University of Glasgow\research\data\exp1_data\compilation)"
setwd(path)
library(tidyverse)
library(readxl)
library(broom)
library(viridis)
library(ez)
library(lme4)
library(lmerTest)
library(emmeans)
library(ggpubr)


# Load----

#Loads in all behavioural performance
trial_level_raw <- read_excel("behavioural.xlsx")
excluded_motivation = c(10502, 15003, 15004, 15006, 15008, 15502, 15508)

# Cleaning----
# Main cleaning
trial_data_m <- trial_level_raw %>%
    filter(!is.na(participant)) %>%
    # exclusions
    filter(!participant %in% excluded_motivation) %>%
    filter(block == 9.0) %>%
    mutate(age_group = ifelse(participant >= 10600, "older", "young")) %>%
    group_by(block, age_group) %>%
    mutate(mean_rt = mean(Stimulus.RT), sd_rt = sd(Stimulus.RT)) %>%
    ungroup() %>%
    filter(Stimulus.RT >= mean_rt-2*sd_rt, Stimulus.RT <= mean_rt+2*sd_rt)
# Keeping only correct go trials for further RT analysis
rt_data_m <- trial_data_m %>%
    filter(condition == "Go") %>%
    filter(Stimulus.ACC == 1)
# Whilst keeping all for error analysis
nogo_data_m <- trial_data_m %>%
    filter(condition == "NoGo")
go_data_m <- trial_data_m %>%
    filter(condition == "Go")

## RT----

condition_motivation <- demographic_data %>%
    select(ID, motivation)

rt_natural_m <- rt_data_m %>%
  group_by(block, participant) %>%
  summarise(rt = mean(Stimulus.RT), sd = sd(Stimulus.RT),
            min = min(Stimulus.RT), max = max(Stimulus.RT), trials = n()) %>%
  mutate(age_group = ifelse(participant >= 10600, "older", "young"),
         ID=participant) %>%
  inner_join(condition_motivation, by="ID") %>%
  select(-ID)

## Error

condition_motivation <- demographic_data %>%
    select(ID, motivation)

err_m <- nogo_data_m %>%
  filter(block == 9) %>%
  group_by(participant) %>%
  count(Stimulus.ACC) %>%
  mutate(block_total = sum(n)) %>%
  filter(Stimulus.ACC == 1) %>%
  mutate(err = (1 - (n/block_total) ) * 100) %>%
  mutate(ID = participant) %>%
  inner_join(condition_motivation, by = "ID") %>%
  mutate(age_group = ifelse(participant >= 10600, "older", "young"))

err_m_summary <- err_m %>%
  group_by(motivation, age_group) %>%
    summarise(mean = mean(err), sd = sd(err),
            min = min(err), max = max(err), trials = n())

## Further----

# Main cleaning
trial_level_raw_motivation <- trial_level_raw %>%
    filter(participant < 16000) %>% # removing piloting participants
    filter(!is.na(participant)) %>%
    filter(!participant %in% excluded_motivation) %>% # removing excluded participants
    filter(block == 9.0) %>% # Specifying the motivational block (block) 9
    mutate(age_group = ifelse(participant >= 10600, "older", "young")) %>% # making age groups
    group_by(block, age_group) %>%
    mutate(mean_rt = mean(Stimulus.RT), sd_rt = sd(Stimulus.RT)) %>%
    ungroup() %>%
    filter(Stimulus.RT >= mean_rt-2*sd_rt, Stimulus.RT <= mean_rt+2*sd_rt) # Trimming away trials outside of normal response rates

# Keeping only correct go trials for further RT analysis
rt_motivation <- trial_level_raw_motivation %>%
    filter(condition == "Go") %>%
    filter(Stimulus.ACC == 1)
# Whilst keeping all for error analysis
nogo_motivation <- trial_level_raw_motivation %>%
    filter(condition == "NoGo")
go_motivation <- trial_level_raw_motivation %>%
    filter(condition == "Go")


# Description----

# log_transform necessary
hist(rt_motivation$Stimulus.RT)
hist(log(rt_motivation$Stimulus.RT))

# Importing motivational condition information
condition_motivation <- demographic_data %>%
    select(ID, motivation)

rt_motivation <- rt_motivation %>%
  group_by(block, participant, age_group) %>%
  summarise(rt = mean(Stimulus.RT), sd = sd(Stimulus.RT),
            min = min(Stimulus.RT), max = max(Stimulus.RT), trials = n(), ID = participant) %>%
  inner_join(condition_motivation, by="ID") %>%
  select(-ID)

## Error----

condition_motivation <- demographic_data %>%
    select(ID, motivation)

commission_error_motivation <- nogo_motivation %>%
  filter(block == 9) %>%
  group_by(participant, age_group) %>%
  count(Stimulus.ACC) %>%
  mutate(block_total = sum(n)) %>%
  filter(Stimulus.ACC == 1) %>%
  mutate(err = (1 - (n/block_total) ) * 100) %>%
  mutate(ID = participant) %>%
  inner_join(condition_motivation, by = "ID")

commission_error_motivation_summary <- commission_error_motivation %>%
  group_by(motivation, age_group) %>%
  summarise(mean = mean(err), sd = sd(err),
            min = min(err), max = max(err), trials = n())

# calculate accuracy score more cleverly
omission_error_motivation <- go_motivation %>%
  group_by(participant, block) %>%
  count(Stimulus.ACC) %>%
  mutate(block_total = sum(n)) %>%
  filter(Stimulus.ACC == 1) %>%
  mutate(err = (1 - (n/block_total) ) * 100)

omission_error_motivation_summary <- omission_error_motivation %>%
  mutate(age_group = ifelse(participant >= 10600, "older", "young")) %>%
  group_by(block, age_group) %>%
    summarise(mean = mean(err), sd = sd(err),
            min = min(err), max = max(err), trials = n())

# Tests----

## Motivation vs normal----

model <- lm(log(rt) ~ age_group*motivation, rt_motivation)
summary(model)

model <- lm(err ~ age_group*motivation, commission_error_motivation)
summary(model)

emmeans(model, pairwise ~ age_group)

## RT----

rt_m_reduced <- rt_motivation %>%
    mutate(rt_9 = log(rt)) %>%
    select(participant, rt_9, motivation, age_group)

rt_reduced <- rt %>%
    filter(block == 8) %>%
    mutate(rt_8 = rt)

rt_all <- rt_reduced %>%
    inner_join(rt_m_reduced, by="participant") %>%
    select(participant, rt_8, rt_9, motivation, age_group) %>%
    filter(! participant %in% c("15003", "15006", "15008", "15502", "15503"))
    #filter(motivation == "high", age_group == "young")

rt_all_summary <- rt_all %>%
    group_by(motivation, age_group) %>%
    summarise(mean_8 = mean(rt_8), mean_9 = mean(rt_9), rt_change = mean_9 - mean_8)

ggplot(rt_all_summary, aes(x = age_group, y = rt_change, fill = motivation)) +
    geom_col(position = "dodge")


## Error----


err_m_reduced <- err_m %>%
    mutate(err_9 = err) %>%
    select(participant, err_9, motivation, age_group)

err_reduced <- err %>%
    filter(block == 8) %>%
    mutate(err_8 = err)

err_all <- err_reduced %>%
    inner_join(err_m_reduced, by="participant") %>%
    select(participant, err_8, err_9, motivation, age_group) %>%
    filter(! participant %in% c("15003", "15006", "15008", "15502", "15503"))

err_all_summary <- err_all %>%
    group_by(motivation, age_group) %>%
    summarise(mean_8 = mean(err_8), mean_9 = mean(err_9), err_change = mean_9 - mean_8)

selection <- err_all %>%
    filter(motivation == "high", age_group == "young")


ggplot(err_all_summary, aes(x = age_group, y = err_change, fill = motivation)) +
    geom_col(position = "dodge")

# Further testing----


model <- lm(rt_9 ~ motivation*age_group, rt_m_reduced)
summary(model)

model <- lm(err_9 ~ motivation*age_group, err_m_reduced)
summary(model)



commission_error_motivation_reduced <- commission_error_motivation %>%
    mutate(err_9 = err) %>%
    select(participant, err_9, motivation, age_group)

err_reduced <- err %>%
    filter(block == 8) %>%
    mutate(err_8 = err)

err_all <- err_reduced %>%
    inner_join(commission_error_motivation_reduced, by="participant") %>%
    select(participant, err_8, err_9, motivation, age_group) %>%
    filter(! participant %in% c("15003", "15006", "15008", "15502", "15503"))

err_all_summary <- err_all %>%
    group_by(motivation, age_group) %>%
    summarise(mean_8 = mean(err_8), mean_9 = mean(err_9), err_change = mean_9 - mean_8)

selection <- err_all %>%
    filter(motivation == "high", age_group == "young")

model <- t.test(selection$err_8, selection$err_9, paired=TRUE, alternative = "greater")
model

selection <- err_all %>%
    filter(motivation == "high")

model <- t.test(selection$err_8, selection$err_9, paired=TRUE, alternative = "two.sided")
model

selection <- err_all %>%
    filter(motivation == "low", age_group == "young")

model <- t.test(selection$err_8, selection$err_9, paired=TRUE, alternative = "two.sided")
model


ggplot(err_all_summary, aes(x = age_group, y = err_change, fill = motivation)) +
    geom_col(position = "dodge")


#Not needed to check all of this
selection <- err_all %>%
    mutate(motivation = ifelse(motivation == "high", 1, 0), age_group = ifelse(age_group == "young",0,1), err = err_9 - err_8)

model <- lm(err ~ motivation*age_group, selection)
summary(model)

# Testing akin ie----
err_all_long <- err_all %>%
    pivot_longer(3:4, names_to="block_section", values_to="err_level")

## Actual ERR----

err_all_long_test <- err_all_long %>%
    mutate(participant = as.character(participant),
           block_section = as.character(block_section),
           age_group = as.character(age_group),
           motivation = as.character(motivation))
model <- ezANOVA(
    err_all_long_test,
    dv = err_level,
    wid = .(participant),
    within = .(block_section),
    between = .(age_group, motivation),
    type = 2,
    detailed = TRUE,
    return_aov = TRUE
)
test <- as.data.frame(model$ANOVA)
options(scipen = 999)
test
summary(model$aov)
library(nlme)

## Actual RT----
rt_all_long <- rt_all %>%
    pivot_longer(2:3, names_to="block", values_to="rt_level")

rt_all_long_test <- rt_all_long %>%
    mutate(participant = as.character(participant),
           block = as.character(block),
           age_group = as.character(age_group),
           motivation = as.character(motivation))

model <- ezANOVA(
    rt_all_long_test,
    dv = rt_level,
    wid = .(participant),
    within = .(block),
    between = .(age_group, motivation),
    type = 2,
    detailed = TRUE,
    return_aov = TRUE
)
test <- as.data.frame(model$ANOVA)
options(scipen = 999)
test
summary(model$aov)
library(nlme)




# Plotting----

 path = r"(C:\Users\hanza\OneDrive\Dokumenty\R\plots)"
#path <- r"(C:\Users\simonha\OneDrive - University of Glasgow\research\data\exp1_data\compilation\newer_plots)"

plot_rt_m_young <- ggplot(filter(rt_motivation, age_group=="young"), aes(x = motivation, y = as.numeric(rt), fill = motivation)) +
  geom_violin(alpha = 0.4, adjust  = 0.8) +
  geom_boxplot(alpha = 0.2, varwidth = TRUE, width = 0.45, outlier.shape = NA, coef = 0) +
  ggtitle("Young - Motivation change") +
  xlab("Motivation") +
  ylab("Reaction Time (ms)") +
  theme_minimal() +
  scale_color_viridis(discrete = TRUE, option = "D") +
  # scale_y_continuous(limits = c(250, 1000)) +
  scale_fill_viridis_d(limits=rev, option = "D", begin = 0.4, end = 0.8) +
  theme(legend.position = 0, text=element_text(size=18,  family="Times New Roman"), axis.text=element_text(size=16))

plot_rt_m_older <- ggplot(filter(rt_motivation, age_group=="older"), aes(x = motivation, y = as.numeric(rt), fill = motivation)) +
  geom_violin(alpha = 0.4, adjust  = 0.8) +
  geom_boxplot(alpha = 0.2, varwidth = TRUE, width = 0.45, outlier.shape = NA, coef = 0) +
  ggtitle("Older - Motivation change") +
  xlab("Motivation") +
  ylab("Reaction Time (ms)") +
  theme_minimal() +
  scale_color_viridis(discrete = TRUE, option = "D") +
  # scale_y_continuous(limits = c(250, 1000)) +
  scale_fill_viridis_d(limits=rev, option = "D", begin = 0.4, end = 0.8) +
  theme(legend.position = 0, text=element_text(size=18,  family="Times New Roman"), axis.text=element_text(size=16))

plot_rt_m_young
plot_rt_m_older
ggsave(plot_rt_m_young, filename = 'rt_young_m.png', path = path, dpi = 1200, type = 'cairo', width = 10, height = 4.5, units = 'in', bg = 'white')
ggsave(plot_rt_m_older, filename = 'rt_older_m.png', path = path, dpi = 1200, type = 'cairo', width = 10, height = 4.5, units = 'in', bg = 'white')

plot_nogo_m_young <- ggplot(filter(commission_error_motivation, participant <= 10600), aes(x = as.factor(motivation), y = as.numeric(err), fill = as.factor(motivation))) +
  geom_violin(alpha = 0.4, adjust  = 0.6) +
  stat_summary(fun.y="median") +
  ggtitle("Young - Commission Error") +
  xlab("Block") +
  ylab("Commission Error") +
  theme_minimal() +
  scale_color_viridis(discrete = TRUE, option = "D") +
  scale_fill_viridis_d(limits=rev, option = "D", begin = 0.4, end = 0.8) +
  scale_y_continuous(limits = c(0, 100)) +
  theme(legend.position = 0, text=element_text(size=18,  family="Times New Roman"), axis.text=element_text(size=16))

plot_nogo_m_older <- ggplot(filter(commission_error_motivation, participant >= 10600), aes(x = as.factor(motivation), y = as.numeric(err), fill = as.factor(motivation))) +
  geom_violin(alpha = 0.4, adjust  = 3) +
  stat_summary(fun.y="median") +
  ggtitle("Older - Commission Error") +
  xlab("Block") +
  ylab("Commission Error") +
  theme_minimal() +
  scale_color_viridis(discrete = TRUE, option = "D") +
  scale_fill_viridis_d(limits=rev, option = "D", begin = 0.4, end = 0.8) +
  scale_y_continuous(limits = c(0, 100)) +
  theme(legend.position = 0, text=element_text(size=18,  family="Times New Roman"), axis.text=element_text(size=16))

plot_nogo_m_young
plot_nogo_m_older

ggsave(plot_nogo_young, filename = 'nogo_young.png', path = path, dpi = 1200, type = 'cairo', width = 10, height = 4.5, units = 'in', bg = 'white')
ggsave(plot_nogo_older, filename = 'nogo_older.png', path = path, dpi = 1200, type = 'cairo', width = 10, height = 4.5, units = 'in', bg = 'white')

model <- lm(log(rt) ~ age_group*motivation, rt_natural_m)
summary(model)

model <- lm(err ~ age_group*motivation, err_m)
summary(model)

emmeans(model, pairwise ~ age_group)

rt_m_reduced <- rt_natural_m %>%
    mutate(rt_9 = log(rt)) %>%
    select(participant, rt_9, motivation, age_group)

rt_reduced <- rt %>%
    filter(block == 8) %>%
    mutate(rt_8 = rt)

rt_all <- rt_reduced %>%
    inner_join(rt_m_reduced, by="participant") %>%
    select(participant, rt_8, rt_9, motivation, age_group) %>%
    filter(! participant %in% c("15003", "15006", "15008", "15502", "15503"))
    #filter(motivation == "high", age_group == "young")

rt_all_summary <- rt_all %>%
    group_by(motivation, age_group) %>%
    summarise(mean_8 = mean(rt_8), mean_9 = mean(rt_9), rt_change = mean_9 - mean_8)

ggplot(rt_all_summary, aes(x = age_group, y = rt_change, fill = motivation)) +
    geom_col(position = "dodge")

