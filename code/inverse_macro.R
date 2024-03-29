library(here)
library(tidyverse)
library(readxl)
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

