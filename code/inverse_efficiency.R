# Simon Hanzal (c)

# Setup----

path <- r"(..\ie_compilation)"
path <- r"(C:\Users\hanza\OneDrive\Dokumenty\R)"
 path <- r"(C:\Users\simonha\OneDrive - University of Glasgow\research\data\exp1_data\compilation)"
setwd(here())
library(tidyverse)
library(here)
library(readxl)
library(broom)
library(lme4)
library(psych)
library(ez)

# General----
 
err_ie <- err %>%
    select(participant, block, err)

rt_ie <- rt_natural %>%
    select(participant, block, rt, sd)

ie <- inner_join(err_ie, rt_ie, by=c("participant", "block")) %>%
    ungroup() %>%
    mutate(ie = rt/(100-err)) %>%
    mutate(ie_z =  (rt-mean(rt))/sd(rt) / (100-err-mean(err)) ) %>%
    mutate(age_group = ifelse(participant >= 10600, "older", "young"))

ie_natural_test <-  inner_join(err_ie, rt_natural, by=c("participant", "block")) %>%
    ungroup() %>%
    mutate(ie = rt/(100-err)) %>%
    mutate(ie_z =  (rt-mean(rt))/sd(rt) / (100-err-mean(err)) ) %>%
    mutate(age_group = ifelse(participant >= 10600, "older", "young"))

ie_18 <- ie %>%
    filter(block == 1 | block == 8) %>%
    mutate(block = ifelse(block == 1, 0, 1))


ie_summary <- ie %>%
    group_by(block, age_group) %>%
    summarise(mean_ie = mean(ie))
    #filter(block < 8)

ie_test <- ie %>%
    group_by(participant, age_group) %>%
    summarise(mean_ie = mean(ie)) %>%
    mutate(age_group = ifelse(age_group == "young",0,1))

ie_18 <- ie_18 %>%
    mutate(participant = as.character(participant),
           block = as.character(block),
           age_group = as.character(age_group))
model <- ezANOVA(
	ie_18,
	dv = ie,
	wid = .(participant),
	within = .(block),
    between = .(age_group),
	type = 2,
	detailed = TRUE
)
model

model <- t.test(filter(ie_test, age_group == 0) %>% pull(mean_ie), filter(ie_test, age_group == 1) %>% pull(mean_ie), within = FALSE, anternative = "two.sided")
model
tidy(model)

model <- lm(age_group ~ mean_ie, ie_test)
summary(model)

ie_simple <- ie_summary %>%
    pivot_wider(names_from = "age_group", values_from = "mean_ie") %>%
    mutate(difference = older-young) %>%
    select(block, difference)


model <- lm(block ~ difference, ie_simple)
summary(model)
#plot(model)

model <- lm(mean_ie ~ block*age_group, ie_summary)
summary(model)
#plot(model)


plot_ie <- ggplot(ie, aes(x = as.factor(block), y = as.numeric(ie), fill = as.factor(age_group))) +
  # geom_violin(alpha = 0.4, adjust  = 0.8) +
  # geom_point(aes(colour = as.factor(participant)), shape = 4, alpha = 0.5) +
  # geom_line(aes(colour = as.factor(participant)), group= 1, alpha = 0.5) +
  geom_boxplot(alpha = 0.2, varwidth = TRUE, width = 0.45, outlier.shape = NA, coef = 0) +
  #ggtitle("Inverse Efficiency") +
  xlab("Block") +
  ylab("Inverse Efficiency") +
  scale_color_viridis(discrete = TRUE, option = "D") +
  #scale_y_continuous(limits = c(-0.03, 0.03)) +
  scale_fill_viridis_d(limits=rev, option = "D", begin = 0.4, end = 0.8) +
  labs(fill = "age", colour = 0) +
  theme_minimal()

plot_ie

ie_sum <- ie %>%
     mutate(age_group = ifelse(participant > 10600, "Older", "Young")) %>%
     group_by(block, age_group) %>%
     summarise(mean = mean(ie), sd = sd(ie))

ie_natural_sum <- ie_natural_test %>%
     mutate(age_group = ifelse(participant > 10600, "Older", "Young")) %>%
     group_by(block, age_group) %>%
     summarise(mean = mean(ie), sd = sd(ie))

plot_ie_2 <- ggplot(ie_natural_sum, aes(x = as.factor(block), y = as.numeric(mean), color = age_group, ymin = as.numeric(mean)-as.numeric(sd), ymax = as.numeric(mean)+as.numeric(sd))) +
  # geom_violin(alpha = 0.4, adjust  = 0.8) +
  # geom_point(aes(colour = as.factor(participant)), shape = 4, alpha = 0.5) +
  # geom_line(aes(colour = as.factor(participant)), group= 1, alpha = 0.5) +
  geom_point(position = position_dodge(width = 0.90), size=4.5, alpha = 1) +
  geom_errorbar(position = position_dodge(width = 0.90), size=1, alpha = 0.7) +
  #ggtitle("Inverse Efficiency") +
  xlab("Block") +
  ylab("Inverse Efficiency") +
  scale_color_viridis(discrete = TRUE, option = "D") +
  #scale_y_continuous(limits = c(-0.03, 0.03)) +
  scale_color_viridis_d(limits=rev, option = "D", begin = 0.4, end = 0.8) +
  ylim(0,8) +
  labs(color = "Age") +
  theme_minimal()

plot_ie_2


err_sum <- err %>%
     mutate(age_group = ifelse(participant > 10600, "Older", "Young")) %>%
     group_by(block, age_group) %>%
     summarise(mean = mean(err), sd = sd(err))

plot_err_2 <- ggplot(err_sum, aes(x = as.factor(block), y = as.numeric(mean), color = age_group, ymin = as.numeric(mean)-as.numeric(sd), ymax = as.numeric(mean)+as.numeric(sd))) +
  # geom_violin(alpha = 0.4, adjust  = 0.8) +
  # geom_point(aes(colour = as.factor(participant)), shape = 4, alpha = 0.5) +
  # geom_line(aes(colour = as.factor(participant)), group= 1, alpha = 0.5) +
  geom_point(position = position_dodge(width = 0.90), size=4.5, alpha = 1) +
  geom_errorbar(position = position_dodge(width = 0.90), size=1, alpha = 0.7) +
  #ggtitle("Inverse Efficiency") +
  xlab("Block") +
  ylab("Commission Error (%)") +
  scale_color_viridis(discrete = TRUE, option = "D") +
  #scale_y_continuous(limits = c(-0.03, 0.03)) +
  scale_color_viridis_d(limits=rev, option = "D", begin = 0.4, end = 0.8) +
  ylim(-5,100) +
  labs(color = "Age") +
  theme_minimal()

plot_err_2

rt_sum <- rt_natural %>%
     mutate(age_group = ifelse(participant > 10600, "Older", "Young")) %>%
     group_by(block, age_group) %>%
     summarise(mean = mean(rt), sd = sd(rt))

plot_rt_2 <- ggplot(rt_sum, aes(x = as.factor(block), y = as.numeric(mean), color = age_group, ymin = as.numeric(mean)-as.numeric(sd), ymax = as.numeric(mean)+as.numeric(sd))) +
  # geom_violin(alpha = 0.4, adjust  = 0.8) +
  # geom_point(aes(colour = as.factor(participant)), shape = 4, alpha = 0.5) +
  # geom_line(aes(colour = as.factor(participant)), group= 1, alpha = 0.5) +
  geom_point(position = position_dodge(width = 0.90), size=4.5, alpha = 1) +
  geom_errorbar(position = position_dodge(width = 0.90), size=1, alpha = 0.7) +
  #ggtitle("Inverse Efficiency") +
  xlab("Block") +
  ylab("Reaction Time (ms)") +
  scale_color_viridis(discrete = TRUE, option = "D") +
  #scale_y_continuous(limits = c(-0.03, 0.03)) +
  scale_color_viridis_d(limits=rev, option = "D", begin = 0.4, end = 0.8) +
  ylim(0,750) +
  labs(color = "Age") +
  theme_minimal()


plot <- ggarrange(plot_err_2, plot_rt_2, plot_ie_2,
                    ncol = 1, nrow = 3)
plot
ggsave(plot, filename = 'triple_plot.png', path = path, dpi = 1200, type = 'cairo', width = 10, height = 10, units = 'in', bg = 'white')

ggsave(plot_ie_2, filename = 'single_plot.png', path = path, dpi = 1200, type = 'cairo', width = 10, height = 10, units = 'in', bg = 'white')



model <- lm(ie ~ block*age_group, ie_18)
summary(model)


model <- lmer(ie ~ block*age_group * (1|participant), ie)
summary(model)

# Macroscopic----

#Online SART
online_sart <- read_csv(here("data","ie_compilation","quantile_behaviour.csv"))
#Comparison
offline_comparison <- read_excel(here("data","ie_compilation","offline_scores.xlsx"))
online_comparison <- read_excel(here("data","ie_compilation","online_scores.xlsx"))
#EEG
eeg_error <- read_csv(here("data","ie_compilation","err.csv"))
eeg_rt <- read_csv(here("data","ie_compilation","rt_natural.csv"))
#Motivation
motivation_error <- read_csv(here("data","ie_compilation","err_m.csv"))
motivation_rt <- read_csv(here("data","ie_compilation","rt_natural_m.csv"))

# Clean----

online_sart_ie <- online_sart %>%
    select(id, quantile, mean_rt_stats, sd_rt, no_go_accuracy, age) %>%
    filter(quantile == 1 | quantile == 4) %>%
    mutate(ie = log(mean_rt_stats) / no_go_accuracy, block = quantile, condition = "online_brief", age = ifelse(age < 55, "young", "older"), fatigued = ifelse(block == 1, "no", "yes")) %>%
    select(id, condition, fatigued, ie, age)

comparison_ie <- full_join(offline_comparison, online_comparison) %>%
    select(1,2,7,8,10,11) %>%
    pivot_longer(3:4, names_to = "block", values_to = "error") %>%
    mutate(block = recode(block, `first block commission errors` = "first", `last block commission errors` = "last")) %>%
    pivot_longer(3:4, names_to = "block_temp", values_to = "rt") %>%
    mutate(block_temp = recode(block_temp, `first block mean rt` = "first", `last block mean rt` = "last")) %>%
    filter(block == block_temp) %>%
    select(-block_temp) %>%
    mutate(ie = log(rt)/((100-error)/100), id = participant, fatigued = recode(block, `first` = "no", `last` = "yes"), age = "young") %>%
    select(id, condition, fatigued, ie, age)

eeg_ie <- eeg_error %>%
    select(participant, block, err) %>%
    inner_join(eeg_rt, by = c("participant", "block")) %>%
    select(participant, block, err, rt, age_group) %>%
    filter(block == 1 | block == 8) %>%
    mutate(ie = log(rt)/((100-err)/100), id = participant, condition = "eeg_long", age = age_group, fatigued = ifelse(block == 1, "no", "yes")) %>%
    select(id, condition, fatigued, ie, age)

motivation_ie <- motivation_error %>%
    select(participant, err) %>%
    inner_join(motivation_rt, by = "participant") %>%
    select(participant, block, err, rt, age_group, motivation) %>%
    mutate(ie = log(rt)/((100-err)/100), id = participant, age = age_group, condition = ifelse(motivation == "low", "low_motivation", "high_motivation"), fatigued = "yes") %>%
    select(id, condition, fatigued, ie, age)
#online_sart_ie, 
ie <- list(comparison_ie, eeg_ie, motivation_ie) %>% reduce(full_join)

## Describe----

ie_summary <- ie %>%
    na.omit() %>%
    group_by(condition, fatigued, age) %>%
    summarise(mean_ie = mean(ie), sd_ie = sd(ie))

ie_summary_condition <- ie %>%
    na.omit() %>%
    group_by(condition) %>%
    summarise(mean_ie = mean(ie), sd_ie = sd(ie))

ie_summary_age <- ie %>%
    na.omit() %>%
    group_by(age) %>%
    summarise(mean_ie = mean(ie), sd_ie = sd(ie))

ie_summary_time <- ie %>%
    na.omit() %>%
    group_by(fatigued) %>%
    summarise(mean_ie = mean(ie), sd_ie = sd(ie))

t.test(ie$ie ~ ie$fatigued)

ie_summary_person <- ie %>%
    na.omit() %>%
    group_by(id) %>%
    summarise(mean_ie = mean(ie), sd_ie = sd(ie))

ie_summary_all <- ie %>%
    na.omit() %>%
    summarise(mean_ie = mean(ie), sd_ie = sd(ie))

## ToT----

online_sart_ie_time <- online_sart %>%
    select(id, quantile, mean_rt_stats, sd_rt, no_go_accuracy, age) %>%
    mutate(ie = log(mean_rt_stats) / no_go_accuracy, block = quantile, condition = "online_brief", age = ifelse(age < 55, "young", "older"), time = (quantile - 1) * 2.5) %>%
    select(id, condition, time, ie, age)

comparison_ie_time <- full_join(offline_comparison, online_comparison) %>%
    select(1,2,7,8,10,11) %>%
    pivot_longer(3:4, names_to = "block", values_to = "error") %>%
    mutate(block = recode(block, `first block commission errors` = "first", `last block commission errors` = "last")) %>%
    pivot_longer(3:4, names_to = "block_temp", values_to = "rt") %>%
    mutate(block_temp = recode(block_temp, `first block mean rt` = "first", `last block mean rt` = "last")) %>%
    filter(block == block_temp) %>%
    select(-block_temp) %>%
    mutate(ie = log(rt)/((100-error)/100), id = participant, time = recode(block, `first` = 0, `last` = 35), age = "young") %>%
    select(id, condition, time, ie, age)

eeg_ie_time <- eeg_error %>%
    select(participant, block, err) %>%
    inner_join(eeg_rt, by = c("participant", "block")) %>%
    select(participant, block, err, rt, age_group) %>%
    mutate(ie = log(rt)/((100-err)/100), id = participant, condition = "eeg_long", age = age_group, time = (block - 1) * 5) %>%
    select(id, condition, time, ie, age)

motivation_ie_time <- motivation_error %>%
    select(participant, err) %>%
    inner_join(motivation_rt, by = "participant") %>%
    select(participant, block, err, rt, age_group, motivation) %>%
    mutate(ie = log(rt)/((100-err)/100), id = participant, age = age_group, condition = ifelse(motivation == "low", "low_motivation", "high_motivation"), time = 45) %>%
    select(id, condition, time, ie, age)

ie_time <- list(online_sart_ie_time, comparison_ie_time, eeg_ie_time, motivation_ie_time) %>% reduce(full_join)

ie_time_summary <- ie_time %>%
    na.omit() %>%
    group_by(time) %>%
    summarise(mean_ie = mean(ie), sd_ie = sd(ie))

# Hierarchical----

err_ie <- err %>%
    select(participant, block, err)

rt_ie <- rt_natural %>%
    select(participant, block, rt, sd)

ie <- inner_join(err_ie, rt_ie, by=c("participant", "block")) %>%
    ungroup() %>%
    mutate(ie = rt/(100-err)) %>%
    mutate(ie_z =  (rt-mean(rt))/sd(rt) / (100-err-mean(err)) ) %>%
    mutate(age_group = ifelse(participant >= 10600, "older", "young"))

ie_natural_test <-  inner_join(err_ie, rt_natural, by=c("participant", "block")) %>%
    ungroup() %>%
    mutate(ie = rt/(100-err)) %>%
    mutate(ie_z =  (rt-mean(rt))/sd(rt) / (100-err-mean(err)) ) %>%
    mutate(age_group = ifelse(participant >= 10600, "older", "young"))

ie_18 <- ie %>%
    filter(block == 1 | block == 8) %>%
    mutate(block = ifelse(block == 1, 0, 1))


ie_summary <- ie %>%
    group_by(block, age_group) %>%
    summarise(mean_ie = mean(ie))
    #filter(block < 8)

ie_test <- ie %>%
    group_by(participant, age_group) %>%
    summarise(mean_ie = mean(ie)) %>%
    mutate(age_group = ifelse(age_group == "young",0,1))

ie_18 <- ie_18 %>%
    mutate(participant = as.character(participant),
           block = as.character(block),
           age_group = as.character(age_group))
model <- ezANOVA(
	ie_18,
	dv = ie,
	wid = .(participant),
	within = .(block),
    between = .(age_group),
	type = 2,
	detailed = TRUE
)
model

model <- t.test(filter(ie_test, age_group == 0) %>% pull(mean_ie), filter(ie_test, age_group == 1) %>% pull(mean_ie), within = FALSE, anternative = "two.sided")
model
tidy(model)

model <- lm(age_group ~ mean_ie, ie_test)
summary(model)

ie_simple <- ie_summary %>%
    pivot_wider(names_from = "age_group", values_from = "mean_ie") %>%
    mutate(difference = older-young) %>%
    select(block, difference)

#plot(ie_simple)

model <- lm(block ~ difference, ie_simple)
summary(model)
#plot(model)

model <- lm(mean_ie ~ block*age_group, ie_summary)
summary(model)
#plot(model)


plot_ie <- ggplot(ie, aes(x = as.factor(block), y = as.numeric(ie), fill = as.factor(age_group))) +
  # geom_violin(alpha = 0.4, adjust  = 0.8) +
  # geom_point(aes(colour = as.factor(participant)), shape = 4, alpha = 0.5) +
  # geom_line(aes(colour = as.factor(participant)), group= 1, alpha = 0.5) +
  geom_boxplot(alpha = 0.2, varwidth = TRUE, width = 0.45, outlier.shape = NA, coef = 0) +
  #ggtitle("Inverse Efficiency") +
  xlab("Block") +
  ylab("Inverse Efficiency") +
  scale_color_viridis(discrete = TRUE, option = "D") +
  #scale_y_continuous(limits = c(-0.03, 0.03)) +
  scale_fill_viridis_d(limits=rev, option = "D", begin = 0.4, end = 0.8) +
  labs(fill = "age", colour = 0) +
  theme_minimal()

plot_ie

ie_sum <- ie %>%
     mutate(age_group = ifelse(participant > 10600, "Older", "Young")) %>%
     group_by(block, age_group) %>%
     summarise(mean = mean(ie), sd = sd(ie))

ie_natural_sum <- ie_natural_test %>%
     mutate(age_group = ifelse(participant > 10600, "Older", "Young")) %>%
     group_by(block, age_group) %>%
     summarise(mean = mean(ie), sd = sd(ie))

plot_ie_2 <- ggplot(ie_natural_sum, aes(x = as.factor(block), y = as.numeric(mean), color = age_group, ymin = as.numeric(mean)-as.numeric(sd), ymax = as.numeric(mean)+as.numeric(sd))) +
  # geom_violin(alpha = 0.4, adjust  = 0.8) +
  # geom_point(aes(colour = as.factor(participant)), shape = 4, alpha = 0.5) +
  # geom_line(aes(colour = as.factor(participant)), group= 1, alpha = 0.5) +
  geom_point(position = position_dodge(width = 0.90), size=4.5, alpha = 1) +
  geom_errorbar(position = position_dodge(width = 0.90), size=1, alpha = 0.7) +
  #ggtitle("Inverse Efficiency") +
  xlab("Block") +
  ylab("Inverse Efficiency") +
  scale_color_viridis(discrete = TRUE, option = "D") +
  #scale_y_continuous(limits = c(-0.03, 0.03)) +
  scale_color_viridis_d(limits=rev, option = "D", begin = 0.4, end = 0.8) +
  ylim(0,8) +
  labs(color = "Age") +
  theme_minimal()

plot_ie_2


err_sum <- err %>%
     mutate(age_group = ifelse(participant > 10600, "Older", "Young")) %>%
     group_by(block, age_group) %>%
     summarise(mean = mean(err), sd = sd(err))

plot_err_2 <- ggplot(err_sum, aes(x = as.factor(block), y = as.numeric(mean), color = age_group, ymin = as.numeric(mean)-as.numeric(sd), ymax = as.numeric(mean)+as.numeric(sd))) +
  # geom_violin(alpha = 0.4, adjust  = 0.8) +
  # geom_point(aes(colour = as.factor(participant)), shape = 4, alpha = 0.5) +
  # geom_line(aes(colour = as.factor(participant)), group= 1, alpha = 0.5) +
  geom_point(position = position_dodge(width = 0.90), size=4.5, alpha = 1) +
  geom_errorbar(position = position_dodge(width = 0.90), size=1, alpha = 0.7) +
  #ggtitle("Inverse Efficiency") +
  xlab("Block") +
  ylab("Commission Error (%)") +
  scale_color_viridis(discrete = TRUE, option = "D") +
  #scale_y_continuous(limits = c(-0.03, 0.03)) +
  scale_color_viridis_d(limits=rev, option = "D", begin = 0.4, end = 0.8) +
  ylim(-5,100) +
  labs(color = "Age") +
  theme_minimal()

plot_err_2

rt_sum <- rt_natural %>%
     mutate(age_group = ifelse(participant > 10600, "Older", "Young")) %>%
     group_by(block, age_group) %>%
     summarise(mean = mean(rt), sd = sd(rt))

plot_rt_2 <- ggplot(rt_sum, aes(x = as.factor(block), y = as.numeric(mean), color = age_group, ymin = as.numeric(mean)-as.numeric(sd), ymax = as.numeric(mean)+as.numeric(sd))) +
  # geom_violin(alpha = 0.4, adjust  = 0.8) +
  # geom_point(aes(colour = as.factor(participant)), shape = 4, alpha = 0.5) +
  # geom_line(aes(colour = as.factor(participant)), group= 1, alpha = 0.5) +
  geom_point(position = position_dodge(width = 0.90), size=4.5, alpha = 1) +
  geom_errorbar(position = position_dodge(width = 0.90), size=1, alpha = 0.7) +
  #ggtitle("Inverse Efficiency") +
  xlab("Block") +
  ylab("Reaction Time (ms)") +
  scale_color_viridis(discrete = TRUE, option = "D") +
  #scale_y_continuous(limits = c(-0.03, 0.03)) +
  scale_color_viridis_d(limits=rev, option = "D", begin = 0.4, end = 0.8) +
  ylim(0,750) +
  labs(color = "Age") +
  theme_minimal()


plot <- ggarrange(plot_err_2, plot_rt_2, plot_ie_2,
                    ncol = 1, nrow = 3)
plot
ggsave(plot, filename = 'triple_plot.png', path = path, dpi = 1200, type = 'cairo', width = 10, height = 10, units = 'in', bg = 'white')

ggsave(plot_ie_2, filename = 'single_plot.png', path = path, dpi = 1200, type = 'cairo', width = 10, height = 10, units = 'in', bg = 'white')



model <- lm(ie ~ block*age_group, ie_18)
summary(model)


model <- lmer(ie ~ block*age_group * (1|participant), ie)
summary(model)


# Interaction 89S----

## Inverse Efficiency
compare_motivation <- rt_motivation %>%
    dplyr::select(rt, participant) %>%
    inner_join(commission_error_motivation, by = c("participant","age_group")) %>%
    mutate(ie = log(rt)/((100-err)/100)) %>%
    mutate(ie_z =  (rt-mean(rt))/sd(rt) / (100-err-mean(err)))

compare_motivation_ie_summary <- compare_motivation %>%
    group_by(motivation, age_group) %>%
    summarise(mean = mean(ie), sd = sd(ie),
            min = min(ie), max = max(ie), trials = n())


compare_motivation_err_summary <- compare_motivation %>%
    group_by(motivation, age_group) %>%
    summarise(mean = mean(err), sd = sd(err),
            min = min(err), max = max(err), trials = n())

compare_motivation_rt_summary <- compare_motivation %>%
    group_by(motivation, age_group) %>%
    summarise(mean = mean(rt), sd = sd(rt),
            min = min(rt), max = max(rt), trials = n())

model <- lm(ie ~ age_group*motivation, compare_motivation)
summary(model)

# IE makes most sense since it neutralises age effects
m_em <- emmeans(model, c( "age_group", "motivation"))
test(m_em)

contrast(m_em, 'tukey') %>%
  broom::tidy() %>%
  head(6)

m_em

m_em <- as.data.frame(m_em)

m_em %>%
  ggplot(aes(motivation, emmean, ymin=lower.CL, ymax=upper.CL, color=age_group, group=age_group)) +
  geom_pointrange() +
  geom_line() +
  ylab("IE")

# ERROR COMPARISON

##ERR
model <- lm(err ~ age_group*motivation, compare_motivation)
summary(model)

# IE makes most sense since it neutralises age effects
m_em <- emmeans(model, c( "age_group", "motivation"))
test(m_em)

contrast(m_em, 'tukey') %>%
  broom::tidy() %>%
  head(7)

m_em

m_em <- as.data.frame(m_em)

m_em %>%
  ggplot(aes(motivation, emmean, ymin=lower.CL, ymax=upper.CL, color=age_group, group=age_group)) +
  geom_pointrange() +
  geom_line() +
  ylab("Error")


ie_m_reduced <- ie_m_reduced %>%
    mutate(age_group = ifelse(participant > 10600, 1, 0))

ie_m_summary <- ie_m_reduced %>%
    group_by(age_group, motivation) %>%
    summarise(mean = mean(ie_9))

model <- lm(ie_9 ~ motivation*age_group, ie_m_reduced)
summary(model)

ie_m_reduced <- ie_m_reduced %>%
    mutate(participant = as.character(participant),
           motivation = ifelse(motivation == "low",1,0),
           motivation = as.character(motivation),
           age_group = as.character(age_group))

model <- ezANOVA(
	ie_m_reduced,
	dv = ie_9,
	wid = .(participant),
    between = .(age_group, motivation),
	type = 2,
	detailed = TRUE
)
model


# Various----

ie_m_reduced <- compare_motivation %>%
    mutate(ie_9 = ie) %>%
    select(participant, ie_9, motivation)

ie_reduced <- ie %>%
    filter(block == 8) %>%
    mutate(ie_8 = ie)

ie_all <- ie_reduced %>%
    inner_join(ie_m_reduced, by=c("participant", "age_group")) %>%
    select(participant, ie_8, ie_9, motivation, age_group) %>%
    filter(! participant %in% c("15003", "15006", "15008", "15502", "15503"))

ie_all_summary <- ie_all %>%
    group_by(motivation, age_group) %>%
    summarise(mean_8 = mean(ie_8), mean_9 = mean(ie_9), ie_change = mean_9 - mean_8)

selection <- ie_all %>%
    filter(motivation == "high", age_group == "young")

model <- t.test(selection$ie_8, selection$ie_9, paired=TRUE, alternative = "greater")
model

selection <- ie_all %>%
    filter(motivation == "low", age_group == "young")

model <- t.test(selection$ie_8, selection$ie_9, paired=TRUE, alternative = "two.sided")
model

#Not needed to check all of this
selection <- ie_all %>%
    mutate(motivation = ifelse(motivation == "high", 1, 0), age_group = ifelse(age_group == "young",0,1), ie = ie_9 - ie_8)

ie_all <- ie_all %>%
    mutate(ie = ie_9 - ie_8)

ggplot(ie_all, aes(x = age_group, y = ie_9, fill = motivation)) +
    geom_col(position = "dodge")
ggplot(ie_all, aes(x = age_group, y = ie_8, fill = motivation)) +
    geom_col(position = "dodge")

model <- lm(ie ~ motivation*age_group, selection)
summary(model)


#Not needed to check all of this
selection <- err_all %>%
    mutate(motivation = ifelse(motivation == "high", 1, 0), age_group = ifelse(age_group == "young",0,1), err_change = err_9 - err_8)

er_all <- err_all %>%
    mutate(err_change = err_9 - err_8)

ggplot(err_all_summary, aes(x = age_group, y = err_change, fill = motivation)) +
    geom_col(position = "dodge")

model <- lm(err_change ~ motivation*age_group, selection)
summary(model)


selection <- ie_all %>%
    mutate(motivation = ifelse(motivation == "high", 1, 0), age_group = ifelse(age_group == "young",0,1), ie = ie_9 - ie_8)


selection_summary <- selection %>%
    group_by(motivation, age_group) %>%
    summarise(mean = mean(ie))

selection_plot <- selection %>%
    pivot_longer(`ie_8`:`ie_9`, names_to="time", values_to="ie_value", names_repair = "minimal") %>%
    mutate(reduction = ifelse(age_group == 1 & motivation == 0, "older_unmotivated",
                       ifelse(age_group == 1 & motivation == 1, "older_motivated",
                       ifelse(age_group == 0 & motivation == 0, "younger_unmotivated","younger_motivated"))))

ggplot(selection_plot, aes(x = time, y = ie_value, fill = reduction)) +
    geom_col(position = "dodge")

ie_all_long <- ie_all %>%
    pivot_longer(2:3, names_to="block", values_to="ie_level")

# Actual----

ie_all_long_test <- ie_all_long %>%
    mutate(participant = as.character(participant),
           block = as.character(block),
           age_group = as.character(age_group),
           motivation = as.character(motivation))
model <- ezANOVA(
    ie_all_long_test,
	dv = ie_level,
	wid = .(participant),
	within = .(block),
    between = .(age_group, motivation),
	type = 2,
	detailed = TRUE,
    return_aov = TRUE
)
test <- as.data.frame(model$ANOVA)
options(scipen = 999)
summary(model$aov)
library(nlme)
##ERR
model <- lm(ie_level ~ age_group*block, ie_all_long_test)
summary(model)


m_em <- emmeans(model, c( "age_group", "block"))
test(m_em)

contrast(m_em, 'tukey') %>%
  broom::tidy() %>%
  head(7)

m_em # This is for interpreting the output

m_em <- as.data.frame(m_em)

m_em %>%
  ggplot(aes(motivation, emmean, ymin=lower.CL, ymax=upper.CL, color=age_group, group=age_group)) +
  geom_pointrange() +
  geom_line() +
  ylab("Inverse Efficiency")

ie_m_summary <- ie_m_reduced %>%
    mutate(age_group = ifelse(participant > 10600, 1, 0)) %>%
    group_by(age_group, motivation) %>%
    summarise(mean = mean(ie_9))

model <- lm(ie_9 ~ motivation*age_group, ie_m_reduced)
summary(model)

ie_m_reduced <- ie_m_reduced %>%
    mutate(participant = as.character(participant),
           motivation = ifelse(motivation == "low",1,0),
           motivation = as.character(motivation),
           age_group = as.character(age_group))

plot_ie_m_young <- ggplot(filter(ie_m_reduced, participant <= 10600), aes(x = as.factor(motivation), y = as.numeric(ie_9), fill = as.factor(motivation))) +
  geom_violin(alpha = 0.4, adjust  = 0.6) +
  stat_summary(fun.y="median") +
  ggtitle("Young - IE") +
  xlab("Block") +
  ylab("IE") +
  theme_minimal() +
  scale_color_viridis(discrete = TRUE, option = "D") +
  scale_fill_viridis_d(limits=rev, option = "D", begin = 0.4, end = 0.8) +
  scale_y_continuous(limits = c(0, 10)) +
  theme(legend.position = 0, text=element_text(size=18,  family="Times New Roman"), axis.text=element_text(size=16))

plot_ie_m_older <- ggplot(filter(ie_m_reduced, participant >= 10600), aes(x = as.factor(motivation), y = as.numeric(ie_9), fill = as.factor(motivation))) +
  geom_violin(alpha = 0.4, adjust  = 3) +
  stat_summary(fun.y="median") +
  ggtitle("Older - IE") +
  xlab("Block") +
  ylab("IE") +
  theme_minimal() +
  scale_color_viridis(discrete = TRUE, option = "D") +
  scale_fill_viridis_d(limits=rev, option = "D", begin = 0.4, end = 0.8) +
  scale_y_continuous(limits = c(0, 10)) +
  theme(legend.position = 0, text=element_text(size=18,  family="Times New Roman"), axis.text=element_text(size=16))

plot_ie_m_young
plot_ie_m_older

ggsave(plot_ie_young, filename = 'ie_m_young.png', path = path, dpi = 1200, type = 'cairo', width = 10, height = 4.5, units = 'in', bg = 'white')
ggsave(plot_ie_older, filename = 'ie_m_older.png', path = path, dpi = 1200, type = 'cairo', width = 10, height = 4.5, units = 'in', bg = 'white')

## Inverse Efficiency
compare_m <- rt_natural_m %>%
    dplyr::select(rt, participant) %>%
    inner_join(err_m, by = "participant") %>%
    mutate(ie = log(rt)/((100-err)/100)) %>%
    mutate(ie_z =  (rt-mean(rt))/sd(rt) / (100-err-mean(err)))

compare_m_summary <- compare_m %>%
    group_by(motivation, age_group) %>%
    summarise(mean = mean(ie), sd = sd(ie),
            min = min(ie), max = max(ie), trials = n())

compare_m_ie <- compare_m %>%
    group_by(motivation, age_group) %>%
    summarise(mean = mean(ie), sd = sd(ie),
            min = min(ie), max = max(ie), trials = n())

compare_m_err <- compare_m %>%
    group_by(motivation, age_group) %>%
    summarise(mean = mean(err), sd = sd(err),
            min = min(err), max = max(err), trials = n())

compare_m_rt <- compare_m %>%
    group_by(motivation, age_group) %>%
    summarise(mean = mean(rt), sd = sd(rt),
            min = min(rt), max = max(rt), trials = n())

model <- lm(ie ~ age_group*motivation, compare_m)
summary(model)

# IE makes most sense since it neutralises age effects
m_em <- emmeans(model, c( "age_group", "motivation"))
test(m_em)

contrast(m_em, 'tukey') %>%
  broom::tidy() %>%
  head(6)

m_em

m_em <- as.data.frame(m_em)

m_em %>%
  ggplot(aes(motivation, emmean, ymin=lower.CL, ymax=upper.CL, color=age_group, group=age_group)) +
  geom_pointrange() +
  geom_line() +
  ylab("IE")

##ERR
model <- lm(err ~ age_group*motivation, compare_m)
summary(model)

# IE makes most sense since it neutralises age effects
m_em <- emmeans(model, c( "age_group", "motivation"))
test(m_em)

contrast(m_em, 'tukey') %>%
  broom::tidy() %>%
  head(7)

m_em

m_em <- as.data.frame(m_em)

m_em %>%
  ggplot(aes(motivation, emmean, ymin=lower.CL, ymax=upper.CL, color=age_group, group=age_group)) +
  geom_pointrange() +
  geom_line() +
  ylab("Error")



ie_m_reduced <- compare_m %>%
    mutate(ie_9 = ie) %>%
    select(participant, ie_9, motivation)

ie_reduced <- ie %>%
    filter(block == 8) %>%
    mutate(ie_8 = ie)

ie_all <- ie_reduced %>%
    inner_join(ie_m_reduced, by="participant") %>%
    select(participant, ie_8, ie_9, motivation, age_group) %>%
    filter(! participant %in% c("15003", "15006", "15008", "15502", "15503"))

ie_all_summary <- ie_all %>%
    group_by(motivation, age_group) %>%
    summarise(mean_8 = mean(ie_8), mean_9 = mean(ie_9), ie_change = mean_9 - mean_8)

selection <- ie_all %>%
    filter(motivation == "high", age_group == "young")

model <- t.test(selection$ie_8, selection$ie_9, paired=TRUE, alternative = "greater")
model

selection <- ie_all %>%
    filter(motivation == "low", age_group == "young")

model <- t.test(selection$ie_8, selection$ie_9, paired=TRUE, alternative = "two.sided")
model

#Not needed to check all of this
selection <- ie_all %>%
    mutate(motivation = ifelse(motivation == "high", 1, 0), age_group = ifelse(age_group == "young",0,1), ie = ie_9 - ie_8)

ie_all <- ie_all %>%
    mutate(ie = ie_9 - ie_8)

ggplot(ie_all, aes(x = age_group, y = ie_9, fill = motivation)) +
    geom_col(position = "dodge")
ggplot(ie_all, aes(x = age_group, y = ie_8, fill = motivation)) +
    geom_col(position = "dodge")

model <- lm(ie ~ motivation*age_group, selection)
summary(model)


#Not needed to check all of this
selection <- err_all %>%
    mutate(motivation = ifelse(motivation == "high", 1, 0), age_group = ifelse(age_group == "young",0,1), err_change = err_9 - err_8)

er_all <- err_all %>%
    mutate(err_change = err_9 - err_8)

ggplot(err_all_summary, aes(x = age_group, y = err_change, fill = motivation)) +
    geom_col(position = "dodge")


model <- lm(ie ~ motivation*age_group, selection)
summary(model)

selection <- ie_all %>%
    mutate(motivation = ifelse(motivation == "high", 1, 0), age_group = ifelse(age_group == "young",0,1), ie = ie_9 - ie_8)


selection_summary <- selection %>%
    group_by(motivation, age_group) %>%
    summarise(mean = mean(ie))

selection_plot <- selection %>%
    pivot_longer(`ie_8`:`ie_9`, names_to="time", values_to="ie_value", names_repair = "minimal") %>%
    mutate(reduction = ifelse(age_group == 1 & motivation == 0, "older_unmotivated",
                       ifelse(age_group == 1 & motivation == 1, "older_motivated",
                       ifelse(age_group == 0 & motivation == 0, "younger_unmotivated","younger_motivated"))))

ggplot(selection_plot, aes(x = time, y = ie_value, fill = reduction)) +
    geom_col(position = "dodge")

ie_all_long <- ie_all %>%
    pivot_longer(2:3, names_to="block", values_to="ie_level")

ie_all_long <- ie_all_long %>%
    mutate(participant = as.character(participant),
           block = as.character(block),
           age_group = as.character(age_group))
model <- ezANOVA(
	ie_all_long,
	dv = ie_level,
	wid = .(participant),
	within = .(block),
    between = .(age_group),
	type = 2,
	detailed = TRUE
)
model

##ERR
model <- lm(ie_level ~ age_group*motivation, ie_all_long)
summary(model)

# IE makes most sense since it neutralises age effects
m_em <- emmeans(model, c( "age_group", "motivation"))
test(m_em)

contrast(m_em, 'tukey') %>%
  broom::tidy() %>%
  head(7)

m_em

m_em <- as.data.frame(m_em)

m_em %>%
  ggplot(aes(motivation, emmean, ymin=lower.CL, ymax=upper.CL, color=age_group, group=age_group)) +
  geom_pointrange() +
  geom_line() +
  ylab("Error")

