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
# Main Data Loading
trial_data <- read_excel("behavioural.xlsx")

# Parameters
excluded = c(10004, 10006, 10502, 15004, 15508, 15509)

# Cleaning----

## Participant level----
trial_data <- trial_data %>%
    filter(participant < 16000) %>%
    filter(!is.na(participant)) %>%
    # exclusions
    filter(!participant %in% excluded) %>%
    filter(block >= 1.0 & block <= 8.0) %>%
    mutate(age_group = ifelse(participant >= 10600, "older", "young")) %>%
    group_by(participant) %>%
    mutate(observation = 1:n()+1) %>%
    ungroup()
# Keeping only correct go trials for further RT analysis
rt_data <- trial_data %>%
    filter(condition == "Go") %>%
    filter(Stimulus.ACC == 1)
# Whilst keeping all for error analysis
nogo_data <- trial_data %>%
    filter(condition == "NoGo")
go_data <- trial_data %>%
    filter(condition == "Go")

## Trial level----
behavioural_rejections <- rt_data %>%
  group_by(block, age_group) %>%
  mutate(mean_rt = mean(Stimulus.RT), sd_rt = sd(Stimulus.RT)) %>%
  ungroup() %>%
  select(participant, block, stimulus, age_group, observation, Stimulus.RT, mean_rt, sd_rt) %>%
  filter(Stimulus.RT <= mean_rt-2*sd_rt | Stimulus.RT >= mean_rt+2*sd_rt)

behavioural_rejections_specific <- behavioural_rejections %>%
    filter(!participant %in% excluded) %>%
    filter(observation < 91 | observation > 625)

write.csv(behavioural_rejections_specific, "behavioural_rejections.csv")
  
behavioural_rejections_intersect <- behavioural_rejections %>%
    select(participant, observation)

behavioural_rt <- rt_data %>%
    anti_join(behavioural_rejections_intersect)


## Neural to behavioural----

neural_rejections <- read_excel("data_removal_long_f.xlsx")

neural_rejections_intersect <- neural_rejections %>%
    select(participant, observation)

neural_rt <- behavioural_rt %>%
    inner_join(neural_rejections_intersect)

neural_nogo <- nogo_data %>%
    inner_join(neural_rejections_intersect)

# Description----


skew(neural_rt$Stimulus.RT)

hist(neural_rt$Stimulus.RT)

skew(log(neural_rt$Stimulus.RT))

hist(log(neural_rt$Stimulus.RT))
# log_transform necessary

## RT----

rt <- neural_rt %>%
  group_by(block, participant) %>%
  summarise(rt = mean(log(Stimulus.RT)), sd = sd(log(Stimulus.RT)),
            min = min(log(Stimulus.RT)), max = max(log(Stimulus.RT)), trials = n())

rt_natural <- neural_rt %>%
  group_by(block, participant) %>%
  summarise(rt = mean(Stimulus.RT), sd = sd(Stimulus.RT),
            min = min(Stimulus.RT), max = max(Stimulus.RT), trials = n()) %>%
  mutate(age_group = ifelse(participant >= 10600, "older", "young"))

nogo_rt <- nogo_data %>%
    filter(Stimulus.RT > 150) %>%
    group_by(block, participant) %>%
    summarise(rt = mean(Stimulus.RT), sd = sd(Stimulus.RT),
              min = min(Stimulus.RT), max = max(Stimulus.RT), trials = n()) %>%
    mutate(age_group = ifelse(participant >= 10600, "older", "young"))

nogo_rt_anova_avg <- nogo_data %>%
    filter(Stimulus.RT > 150) %>%
    mutate(age_group = ifelse(participant >= 10600, "older", "young")) %>%
    mutate(age_group = recode(age_group, "young" = 0, "older" = 1)) %>%
    group_by(participant, age_group) %>%
    summarise(mean_rt = mean(Stimulus.RT)) %>%
    mutate(rt_overall = log(mean_rt)) %>%
    mutate(type = "incorrect")

nogo_rt_anova_number <- nogo_data %>%
    filter(Stimulus.RT > 150) %>%
    mutate(age_group = ifelse(participant >= 10600, "older", "young")) %>%
    mutate(age_group = recode(age_group, "young" = 0, "older" = 1)) %>%
    group_by(participant, age_group) %>%
    summarise(n = n()) %>%
    ungroup() %>%
#    group_by(age_group) %>%
    summarise(mean = mean(n), sd = sd(n))

# calculate change score
rt_change <-  rt %>%
  filter(block == 1 | block == 8) %>%
  select(participant, block, rt) %>%
  pivot_wider(names_from = block, values_from = rt) %>%
  mutate(rt_change = `8` - `1`) %>%
  select(participant, rt_change)

rt_overall <- rt %>%
  ungroup() %>%
  group_by(participant) %>%
  summarise(rt_overall = mean(rt))

rt_anova <- rt %>%
      mutate(age_group = ifelse(participant >= 10600, "older", "young")) %>%
    filter(block == 1 | block == 8) %>%
    mutate(age_group = recode(age_group, "young" = 0, "older" = 1)) %>%
    mutate(block = recode(block, "1" = 0, "8" = 1))

rt_summary <- rt_natural %>%
    group_by(age_group) %>%
    summarise(mean = mean(rt), sd = sd(rt), min = min(rt), max = max(rt))

nogo_rt_anova <- nogo_rt %>%
    mutate(age_group = ifelse(participant >= 10600, "older", "young")) %>%
    filter(block == 1 | block == 8) %>%
    mutate(age_group = recode(age_group, "young" = 0, "older" = 1)) %>%
    mutate(block = recode(block, "1" = 0, "8" = 1)) %>%
    # remove incomplete participants
    filter(participant != 10008 & participant != 10506 & participant != 15507) %>%
    mutate(log_rt = log(rt))

## Error----

# calculate accuracy score more cleverly
err <- nogo_data %>%
  group_by(participant, block) %>%
  count(Stimulus.ACC) %>%
  mutate(block_total = sum(n)) %>%
  filter(Stimulus.ACC == 1) %>%
  mutate(err = (1 - (n/block_total) ) * 100)

err_summary <- err %>%
  mutate(age_group = ifelse(participant >= 10600, "older", "young")) %>%
  group_by(block, age_group) %>%
    summarise(mean = mean(err), sd = sd(err),
            min = min(err), max = max(err), trials = n())

err_change <- err %>%
  filter(block == 1 | block == 8) %>%
  select(participant, block, err) %>%
  pivot_wider(names_from = block, values_from = err) %>%
  mutate(err_change = `8` - `1`) %>%
  select(participant, err_change)

err_overall <- err %>%
  ungroup() %>%
  group_by(participant) %>%
  summarise(err_overall = mean(err))

err_anova <- err %>%
      mutate(age_group = ifelse(participant >= 10600, "older", "young")) %>%
    filter(block == 1 | block == 8) %>%
    mutate(age_group = recode(age_group, "young" = 0, "older" = 1)) %>%
    mutate(block = recode(block, "1" = 0, "8" = 1))

err_summary <- err %>%
    mutate(age_group = ifelse(participant >= 10600, "older", "young")) %>%
    group_by(age_group) %>%
    summarise(mean = mean(err), sd = sd(err), min = min(err), max = max(err))


## Omission error----

# calculate accuracy score more cleverly
go_err <- go_data %>%
  group_by(participant, block) %>%
  count(Stimulus.ACC) %>%
  mutate(block_total = sum(n)) %>%
  filter(Stimulus.ACC == 1) %>%
  mutate(err = (1 - (n/block_total) ) * 100)

go_err_summary <- go_err %>%
  mutate(age_group = ifelse(participant >= 10600, "older", "young")) %>%
  group_by(block, age_group) %>%
    summarise(mean = mean(err), sd = sd(err),
            min = min(err), max = max(err), trials = n())

go_err_change <- go_err %>%
  filter(block == 1 | block == 8) %>%
  select(participant, block, err) %>%
  pivot_wider(names_from = block, values_from = err) %>%
  mutate(err_change = `8` - `1`) %>%
  select(participant, err_change)

go_err_overall <- go_err %>%
  ungroup() %>%
  group_by(participant) %>%
  summarise(err_overall = mean(err))

go_rr_anova <- go_err %>%
      mutate(age_group = ifelse(participant >= 10600, "older", "young")) %>%
    filter(block == 1 | block == 8) %>%
    mutate(age_group = recode(age_group, "young" = 0, "older" = 1)) %>%
    mutate(block = recode(block, "1" = 0, "8" = 1))

go_err_all_summary <- go_err %>%
  mutate(age_group = ifelse(participant >= 10600, "older", "young")) %>%
  group_by(age_group) %>%
    summarise(mean = mean(err), sd = sd(err),
            min = min(err), max = max(err), trials = n())


## Questionnaire merge----

# assemble

# has age, vas_change, err_change, rt_change, mean_rt, mean_err and space for alpha_change
vas_change <- read_csv("subjective_difference.csv") %>%
  mutate(participant = ID, vas_change = subjective_difference) %>%
  select(participant, vas_change)

all_data <- inner_join(
  inner_join(
  rt_change, rt_overall, by="participant"
  ),
  inner_join(
  err_change, err_overall, by="participant"
  ), by="participant"
) %>%
  inner_join(vas_change, by="participant") %>%
  mutate(age_group = ifelse(participant >= 10600, 1, 0))

neural_data <- read_xlsx("neural_summary.xlsx") %>%
    inner_join(all_data, by="participant")

neural_data <- neural_data %>%
    mutate(block_change_beta = last_beta-first_beta, block_change_alpha = last_alpha - first_alpha)
# Look for brain and behaviour link

wan_addition <- wan_anova %>%
    pivot_wider(names_from ="timepoint", values_from = "wan") %>%
    mutate(wan_change = `1` - `0`, participant = ID) %>%
    select(participant, wan_change)
neural_data <- neural_data %>%
    mutate(participant = as.character(participant)) %>%
    inner_join(wan_addition, "participant")


neural_data_long_alpha <- neural_data %>%
    pivot_longer(first_alpha:last_alpha, "block","value")

neural_data_long_beta <- neural_data %>%
    pivot_longer(first_beta:last_beta, "block","value")


# Models----

## Tot----
rt_test <- rt %>%
  filter(block == 1 | block == 8) %>%
  select(participant, block, rt) %>%
  pivot_wider(names_from = block, values_from = rt)

rt_test <- t.test(rt_test$`1`, rt_test$`8`, paired = TRUE)
tidy(rt_test)
### error change----
err_test <- err %>%
  filter(block == 1 | block == 8) %>%
  select(participant, block, err) %>%
  pivot_wider(names_from = block, values_from = err)

err_test <- t.test(err_test$`1`, err_test$`8`, paired = TRUE)
tidy(err_test)

model <- ezANOVA(
	neural_data_long_beta
	, dv = value
	, wid = participant
	, within = block
	, between = age_group
)
model

model <- ezANOVA(
	neural_data_long_alpha
	, dv = value
	, wid = participant
	, within = block
	, between = age_group

)
model

model <- t.test(filter(all_data, age_group == 0) %>% pull(err_overall), filter(all_data, age_group == 1) %>% pull(err_overall), within = FALSE, anternative = "two.sided")
model
tidy(model)

model <- lm(age_group ~ err_overall, all_data)
summary(model)

model <- t.test(filter(all_data, age_group == 0) %>% pull(rt_overall), filter(all_data, age_group == 1) %>% pull(rt_overall), within = FALSE, anternative = "two.sided")
model
tidy(model)

model <- lm(age_group ~ rt_overall, all_data)
summary(model)

err_anova <- err_anova %>%
    mutate(participant = as.character(participant),
           block = as.character(block),
           age_group = as.character(age_group))
model <- ezANOVA(
	err_anova,
	dv = err,
	wid = .(participant),
	within = .(block),
    between = .(age_group),
	type = 2,
	detailed = TRUE
)
model

err <- err %>%
    mutate(age_group = ifelse(participant > 15000, 1, 0))

model <- lmer(err ~ block*age_group + (1|participant), data = err)

summary(model)
### RT change----
rt_anova <- rt_anova %>%
    mutate(participant = as.character(participant),
           block = as.character(block),
           age_group = as.character(age_group))
model <- ezANOVA(
	rt_anova,
	dv = rt,
	wid = .(participant),
	within = .(block),
    between = .(age_group),
	type = 2,
	detailed = TRUE
)
model

rt <- rt %>%
    mutate(age_group = ifelse(participant > 15000, 1, 0))

model <- lmer(rt ~ block*age_group + (1|participant), data = rt)

summary(model)

### SD change----

model <- ezANOVA(
    rt_anova,
    dv = sd,
    wid = .(participant),
    within = .(block),
    between = .(age_group),
    type = 2,
    detailed = TRUE
)
model

rt <- rt %>%
    mutate(age_group = ifelse(participant > 15000, 1, 0))

model <- lmer(sd ~ block*age_group + (1|participant), data = rt)

summary(model)

### Trial-difference----
rt_overall <- rt_overall %>%
    mutate(participant = as.numeric(participant)) %>%
    mutate(type = "correct")
rt_type_anova <- full_join(nogo_rt_anova_avg, rt_overall) %>%
    select(-mean_rt) %>%
    mutate(age_group = ifelse(participant > 15000, 1, 0))

model <- ezANOVA(
    rt_type_anova,
    dv = rt_overall,
    wid = .(participant),
    between = .(type, age_group),
    type = 2,
    detailed = TRUE
)
model

rt_type_anova_summary <- rt_type_anova %>%
    group_by(type, age_group) %>%
    summarise(mean = 10 * mean(rt_overall ** 2), sd = 10 * sd(rt_overall ** 2))

## Covid----

covid_info <- demographic_data %>%
    mutate(participant = ID) %>%
    select(participant, recent_covid)

covid_test_rt <- inner_join(covid_info, rt_overall, by="participant")

wilcox.test(rt_overall ~ recent_covid, covid_test_rt)

covid_test_err <- inner_join(covid_info, err_overall, by="participant")

wilcox.test(err_overall ~ recent_covid, covid_test_err)


covid_test_vas <- inner_join(covid_info, vas_change, by="participant")

wilcox.test(vas_change ~ recent_covid, covid_test_vas)


baseline_state_covid <- demographic_data %>%
    select(ID, recent_covid, `pre-vas`, `pre-wan`)

wilcox.test(`pre-wan` ~ recent_covid, baseline_state_covid)

baseline_state_sleep <- demographic_data %>%
    select(ID, sleep, `dif-vas`, `dif-wan`, `pre-vas`)

model <- lm(`dif-vas` ~ sleep, baseline_state_sleep)
summary(model)

model <- lm(`pre-vas` ~ sleep, baseline_state_sleep)
summary(model)

baseline_state_caffeine <- demographic_data %>%
    select(ID, caffeine, `dif-vas`, `dif-wan`, `pre-vas`)

model <- lm(`dif-vas` ~ caffeine, baseline_state_caffeine)
summary(model)

model <- lm(`pre-vas` ~ caffeine, baseline_state_caffeine)
summary(model)

