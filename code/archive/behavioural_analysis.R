# Setup

# Pathsetting and package loading
# path <- r"(C:\Users\hanza\OneDrive\Dokumenty\R)"
path <- r"(C:\Users\simonha\OneDrive - University of Glasgow\research\data\exp1_data\compilation)"
library(tidyverse)
library(readxl)
library(broom)
library(viridis)
library(ez)
library(lme4)
library(lmerTest)
library(emmeans)

# # Main Data Loading
trial_data <- read_excel("behavioural.xlsx")
excluded = c(
   10004, 10006, 10502, 15004, 15508, 15509 
)

# Removals

## Motivation

# Main cleaning
trial_data_m <- trial_data %>%
    filter(participant < 16000) %>%
    filter(!is.na(participant)) %>%
    # exclusions
    filter(!participant %in% excluded) %>%
    filter(block == 9.0) %>%
    mutate(age_group = ifelse(participant >= 10600, "older", "young"))
# Keeping only correct go trials for further RT analysis
rt_data_m <- trial_data_m %>%
    filter(condition == "Go") %>%
    filter(Stimulus.ACC == 1)
# Whilst keeping all for error analysis
nogo_data_m <- trial_data_m %>%
    filter(condition == "NoGo")


## Participant level
# Main cleaning
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

## Trial level

### Behavioural to Neural

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

### Neural to Behavioural
neural_rejections <- read_excel("data_removal_long_f.xlsx")

neural_rejections_intersect <- neural_rejections %>%
    select(participant, observation)

neural_rt <- behavioural_rt %>%
    inner_join(neural_rejections_intersect)

neural_nogo <- nogo_data %>%
    inner_join(neural_rejections_intersect)

# Description

## Assumption checks

hist(log(neural_rt$Stimulus.RT))
# log_transform necessary


## Summaries

### Motivation RT
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


### RT 
rt <- neural_rt %>%
  group_by(block, participant) %>%
  summarise(rt = mean(log(Stimulus.RT)), sd = sd(log(Stimulus.RT)),
            min = min(log(Stimulus.RT)), max = max(log(Stimulus.RT)), trials = n())

rt_natural <- neural_rt %>%
  group_by(block, participant) %>%
  summarise(rt = mean(Stimulus.RT), sd = sd(Stimulus.RT),
            min = min(Stimulus.RT), max = max(Stimulus.RT), trials = n()) %>%
  mutate(age_group = ifelse(participant >= 10600, "older", "young"))
  

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

### Motivation Error

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
### Error

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

## Assembly

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

wan_addition <- demo_clean_wan %>%
    pivot_wider(names_from ="timepoint", values_from = "wan") %>%
    mutate(wan_change = `1` - `0`, participant = ID) %>%
    select(participant, wan_change)
neural_data <- neural_data %>%
    inner_join(wan_addition, "participant")


neural_data_long_alpha <- neural_data %>%
    pivot_longer(first_alpha:last_alpha, "block","value")

neural_data_long_beta <- neural_data %>%
    pivot_longer(first_beta:last_beta, "block","value")

vas_anova <- demo_clean_lm %>%
    mutate(age_group = ifelse(ID >= 10600, 1, 0))

wan_anova <- demo_clean_wan %>%
    mutate(age_group = ifelse(ID >= 10600, 1, 0))
```

# Modelling

## Confirmation t-test for changes

# rt change
rt_test <- rt %>%
  filter(block == 1 | block == 8) %>%
  select(participant, block, rt) %>%
  pivot_wider(names_from = block, values_from = rt)

rt_test <- t.test(rt_test$`1`, rt_test$`8`, paired = TRUE)
tidy(rt_test)
# error change
err_test <- err %>%
  filter(block == 1 | block == 8) %>%
  select(participant, block, err) %>%
  pivot_wider(names_from = block, values_from = err)

err_test <- t.test(err_test$`1`, err_test$`8`, paired = TRUE)
tidy(err_test)


## Pre-registered models

### Simple

#### 1c Neural change

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

#### 3a Age effect

model <- t.test(all_data$age_group, all_data$err_overall, within = FALSE, anternative = "greater")
tidy(model)


# err_anova <- err_anova %>%
#     filter(participant > 10001)


#### 4a Error
model <- ezANOVA(
	err_anova,
	dv = err,
	wid = participant,
	within = as.factor(block),
    between = as.factor(age_group)
)
model


#### 4b RTs
model <- ezANOVA(
	rt_anova,
	dv = err,
	wid = participant,
	within = as.factor(block),
    between = as.factor(age_group)
)
model

#### 4c VAS

model <- ezANOVA(
	rt_anova,
	dv = err,
	wid = participant,
	within = as.factor(block),
    between = as.factor(age_group)
)
model


#### 4d WAN


model <- ezANOVA(
	rt_anova,
	dv = err,
	wid = participant,
	within = as.factor(block),
    between = as.factor(age_group)
)
model


#### 4E Neural

model <- ezANOVA(
	alpha,
	dv = err,
	wid = participant,
	within = as.factor(block),
    between = as.factor(age_group)
)
model



model <- ezANOVA(
	beta,
	dv = err,
	wid = participant,
	within = as.factor(block),
    between = as.factor(age_group)
)
model


### Motivation


model <- lm(log(rt) ~ age_group*motivation, rt_natural_m)
summary(model)

model <- lm(err ~ age_group*motivation, err_m)
summary(model)

emmeans(model, pairwise ~ age_group)

## Inverse Efficiency
compare_m <- rt_natural_m %>%
    dplyr::select(rt, participant) %>%
    inner_join(err_m, by = "participant") %>%
    mutate(ie = rt/(100-err)) %>%
    mutate(ie_z =  (rt-mean(rt))/sd(rt) / (100-err-mean(err)))

compare_m_summary <- compare_m %>%
    group_by(motivation, age_group) %>%
    summarise(mean = mean(ie), sd = sd(ie),
            min = min(ie), max = max(ie), trials = n())

model <- lm(ie ~ age_group*motivation, compare_m)
summary(model)




### Complex reconstructions

model <- lmer(err ~ block * age_group + (1 | participant), err_anova)
summary(model)

model <- lmer(rt ~ block * age_group + (1 | participant), rt_anova)
summary(model)

model <- lm(rt_overall ~ err_overall*age_group, all_data)
summary(model)

comparison <- lm(vas ~ timepoint * age_group, vas_anova)
summary(model)

model <- lmer(vas ~ timepoint * age_group + (1 | ID), vas_anova)
summary(model)
#anova(model,comparison)

model <- lmer(wan ~ timepoint * age_group + (1 | ID), wan_anova)
summary(model)


### Confirmations



model <- t.test(as.numeric(neural_data_long_beta$value), neural_data_long_beta$time, paired = TRUE)
tidy(model)

model <- lm(time ~ value + (1 | participant), neural_data_long_beta)
summary(model)
```


## Hierarchical exploration

### Inverse Efficiency

err_ie <- err %>%
    select(participant, block, err)

rt_ie <- rt_natural %>%
    select(participant, block, rt, sd)

ie <- inner_join(err_ie, rt_ie, by=c("participant", "block")) %>%
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

ie_simple <- ie_summary %>%
    pivot_wider(names_from = "age_group", values_from = "mean_ie") %>%
    mutate(difference = older-young) %>%
    select(block, difference)

plot(ie_simple)

model <- lm(block ~ difference, ie_simple)
summary(model)
plot(model)

model <- lm(mean_ie ~ block*age_group, ie_summary)
summary(model)
plot(model)


plot_ie <- ggplot(ie, aes(x = as.factor(block), y = as.numeric(ie), fill = as.factor(age_group))) +
  # geom_violin(alpha = 0.4, adjust  = 0.8) +
  # geom_point(aes(colour = as.factor(participant)), shape = 4, alpha = 0.5) +
  # geom_line(aes(colour = as.factor(participant)), group= 1, alpha = 0.5) +
  geom_boxplot(alpha = 0.2, varwidth = TRUE, width = 0.45, outlier.shape = NA, coef = 0) +
  ggtitle("All - Inverse Efficiency") +
  xlab("Block") +
  ylab("Inverse Efficiency") +
  scale_color_viridis(discrete = TRUE, option = "D") +
  #scale_y_continuous(limits = c(-0.03, 0.03)) +
  scale_fill_viridis_d(limits=rev, option = "D", begin = 0.4, end = 0.8) +
  labs(fill = "age", colour = 0) +
  theme_minimal()

plot_ie

model <- lm(ie ~ block*age_group, ie_18)
summary(model)


model <- lmer(ie ~ block*age_group * (1|participant), ie)
summary(model)


### Sleep


sleep <- demographic_data %>%
    mutate(vas_change = `dif-vas`, wan_change = `dif-wan`, age_group = ifelse(ID > 10600, 1, 0), vas = `pre-vas`) %>%
    select(sleep,vas_change,wan_change,age_group, vas)

model <- lm(vas ~ sleep*age_group, sleep)
summary(model)



### Error

# error

# Think about the interactions
model <- lm(err_overall ~ age_group + vas_change + rt_overall + rt_change + err_change,
all_data)
summary(error_model)

### Error change


model <- lm(err_change ~ age_group + vas_change + rt_overall + rt_change + err_overall,
all_data)
summary(model)

model <- lm(err_change ~ block_change_beta*age_group, neural_data)
summary(model)

model <- lm(err_change ~ block_change_alpha, neural_data)
summary(model)

model <- lm(vas_change ~ block_change_alpha*age_group, neural_data)
summary(model)

model <- lm(wan_change ~ block_change_beta*age_group, neural_data)
summary(model)

model <- lm(value ~ block, neural_data_long_alpha)
summary(model)

model <- lm(value ~ block, neural_data_long_beta)
summary(model)

model <- lm(block_change_alpha ~ block_change_beta, neural_data_long_beta)
summary(model)



## Neural links

### Indeces



neural_data_bin <- neural_data %>%
    mutate(beta_change = ifelse(block_change_beta > median(block_change_beta), 1, 0)) %>%
    mutate(alpha_change = ifelse(block_change_alpha > median(block_change_alpha), 1, 0))

neural_data_long_beta <- neural_data_long_beta %>%
    mutate(age_group = ifelse(age_group == 0, "young", "older"))
    # filter(participant > 10001)

neural_data_long_alpha <- neural_data_long_alpha %>%
    mutate(age_group = ifelse(age_group == 0, "young", "older"))
    # filter(participant > 10001)


### Modelling

model <- lm(err_change ~ block_change_alpha, neural_data)
summary(model)
#!!
model <- lm(block_change_alpha ~ block_change_beta * vas_change * age_group, neural_data)
summary(model)
#!!!!


model <- lm(wan_change ~ alpha_change, neural_data_bin)
summary(model)

control  <- lm(wan_change ~ err_change, neural_data_bin)
summary(control)
anova(model, control)


model <- lm(err_change ~ block_change_beta * block_change_alpha * age_group, neural_data)
summary(model)

model <- lm(err_change ~ block_change_beta * vas_change, neural_data)
summary(model)




model <- lm(block_change_alpha ~ err_change, filter(neural_data, participant != 10007))
summary(model)

basic <- t.test(to_dB(value$age_group), neural_data$block_change_alpha)
tidy(basic)

basic <- t.test(neural_data$age_group, neural_data$block_change_beta)
tidy(basic)


### Depictions

```{r}
hist(neural_data$err_change)

hist(neural_data$block_change_alpha)

plot_brain_behaviour <- ggplot(neural_data, aes(block_change_alpha, err_change)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE)

plot_brain_behaviour

plot_trade_off <- ggplot(all_data, aes(rt_change, err_change)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE)

plot_trade_off 


# Plotting

```{r}
# path = r"(C:\Users\hanza\OneDrive\Dokumenty\R\plots)"
path <- r"(C:\Users\simonha\OneDrive - University of Glasgow\research\data\exp1_data\compilation\newer_plots)"
```

## RT Motivation

```{r}
plot_rt_m_young <- ggplot(filter(rt_natural_m, age_group=="young"), aes(x = motivation, y = as.numeric(rt), fill = motivation)) +
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

plot_rt_m_older <- ggplot(filter(rt_natural_m, age_group=="older"), aes(x = motivation, y = as.numeric(rt), fill = motivation)) +
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

rt_natural_m %>%
    group_by(age_group, motivation) %>%
    summarise(mean = mean(rt), sd = sd(rt))

plot_rt_m_young
plot_rt_m_older
ggsave(plot_rt_m_young, filename = 'rt_young_m.png', path = path, dpi = 1200, type = 'cairo', width = 10, height = 4.5, units = 'in', bg = 'white')
ggsave(plot_rt_m_older, filename = 'rt_older_m.png', path = path, dpi = 1200, type = 'cairo', width = 10, height = 4.5, units = 'in', bg = 'white')
```

## Err Motivation

```{r}
plot_nogo_m_young <- ggplot(filter(err_m, participant <= 10600), aes(x = as.factor(motivation), y = as.numeric(err), fill = as.factor(motivation))) +
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

plot_nogo_m_older <- ggplot(filter(err_m, participant >= 10600), aes(x = as.factor(motivation), y = as.numeric(err), fill = as.factor(motivation))) +
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
```

## RT

```{r}
plot_rt_young <- ggplot(filter(rt_natural, age_group=="young"), aes(x = as.factor(block), y = as.numeric(rt), fill = as.factor(block))) +
  geom_violin(alpha = 0.4, adjust  = 0.8) +
  geom_point(aes(colour = as.factor(participant)),shape = 4, alpha = 0.5) +
  geom_line(aes(colour = as.factor(participant)), group= 1, alpha = 0.5) +
  geom_boxplot(alpha = 0.2, varwidth = TRUE, width = 0.45, outlier.shape = NA, coef = 0) +
  ggtitle("Young - Reaction Time change") +
  xlab("Block") +
  ylab("Reaction Time (ms)") +
  theme_minimal() +
  scale_color_viridis(discrete = TRUE, option = "D") +
  scale_y_continuous(limits = c(250, 1000)) +
  scale_fill_viridis_d(limits=rev, option = "D", begin = 0.4, end = 0.8) +
  theme(legend.position = 0, text=element_text(size=18,  family="Times New Roman"), axis.text=element_text(size=16))

plot_rt_older <- ggplot(filter(rt_natural, age_group=="older"), aes(x = as.factor(block), y = as.numeric(rt), fill = as.factor(block))) +
  geom_violin(alpha = 0.4, adjust  = 0.8) +
  geom_point(aes(colour = as.factor(participant)),shape = 4, alpha = 0.5) +
  geom_line(aes(colour = as.factor(participant)), group= 1, alpha = 0.5) +
  geom_boxplot(alpha = 0.2, varwidth = TRUE, width = 0.45, outlier.shape = NA, coef = 0) +
  ggtitle("Older - Reaction Time change") +
  xlab("Block") +
  ylab("Reaction Time (ms)") +
  theme_minimal() +
  scale_color_viridis(discrete = TRUE, option = "D") +
  scale_y_continuous(limits = c(250, 1000)) +
  scale_fill_viridis_d(limits=rev, option = "D", begin = 0.4, end = 0.8) +
  theme(legend.position = 0, text=element_text(size=18,  family="Times New Roman"), axis.text=element_text(size=16))

plot_rt_young
plot_rt_older
ggsave(plot_rt_young, filename = 'rt_young.png', path = path, dpi = 1200, type = 'cairo', width = 10, height = 4.5, units = 'in', bg = 'white')
ggsave(plot_rt_older, filename = 'rt_older.png', path = path, dpi = 1200, type = 'cairo', width = 10, height = 4.5, units = 'in', bg = 'white')
```

## Error

```{r}
plot_nogo_young <- ggplot(filter(err, participant <= 10600), aes(x = as.factor(block), y = as.numeric(err), fill = as.factor(block))) +
  geom_violin(alpha = 0.4, adjust  = 0.6) +
  geom_point(aes(colour = as.factor(participant)),shape = 4, alpha = 0.5) +
  geom_line(aes(colour = as.factor(participant)), group= 1, alpha = 0.5) +
  stat_summary(fun.y="median") +
  ggtitle("Young - Commission Error") +
  xlab("Block") +
  ylab("Commission Error") +
  theme_minimal() +
  scale_color_viridis(discrete = TRUE, option = "D") +
  scale_fill_viridis_d(limits=rev, option = "D", begin = 0.4, end = 0.8) +
  scale_y_continuous(limits = c(0, 100)) +
  theme(legend.position = 0, text=element_text(size=18,  family="Times New Roman"), axis.text=element_text(size=16))

plot_nogo_older <- ggplot(filter(err, participant >= 10600), aes(x = as.factor(block), y = as.numeric(err), fill = as.factor(block))) +
  geom_violin(alpha = 0.4, adjust  = 3) +
  geom_point(aes(colour = as.factor(participant)),shape = 4, alpha = 0.5) +
  geom_line(aes(colour = as.factor(participant)), group= 1, alpha = 0.5) +
  stat_summary(fun.y="median") +
  ggtitle("Older - Commission Error") +
  xlab("Block") +
  ylab("Commission Error") +
  theme_minimal() +
  scale_color_viridis(discrete = TRUE, option = "D") +
  scale_fill_viridis_d(limits=rev, option = "D", begin = 0.4, end = 0.8) +
  scale_y_continuous(limits = c(0, 100)) +
  theme(legend.position = 0, text=element_text(size=18,  family="Times New Roman"), axis.text=element_text(size=16))

plot_nogo_young
plot_nogo_older

ggsave(plot_nogo_young, filename = 'nogo_young.png', path = path, dpi = 1200, type = 'cairo', width = 10, height = 4.5, units = 'in', bg = 'white')
ggsave(plot_nogo_older, filename = 'nogo_older.png', path = path, dpi = 1200, type = 'cairo', width = 10, height = 4.5, units = 'in', bg = 'white')

```

## Alpha

```{r}
plot_alpha_young <- ggplot(filter(neural_data_long_alpha, participant <= 10600), aes(x = as.factor(block), y = as.numeric(value), fill = as.factor(block))) +
  geom_violin(alpha = 0.4, adjust  = 0.8) +
  geom_point(aes(colour = as.factor(participant)),shape = 4, alpha = 0.5) +
  geom_line(aes(colour = as.factor(participant)), group= 1, alpha = 0.5) +
  geom_boxplot(alpha = 0.2, varwidth = TRUE, width = 0.45, outlier.shape = NA, coef = 0) +
  ggtitle("Young - Alpha change") +
  xlab("Time") +
  ylab("Total Score") +
  theme_minimal() +
  scale_color_viridis(discrete = TRUE, option = "C") +
  scale_fill_viridis_d(option = "C", begin = 0, end = 1) +
  #scale_x_discrete(labels=c("1" = "First Block", "0" = "Last block"), limits=rev) +
  ylim(-20,20) +
  theme(legend.position = 0, text=element_text(size=18,  family="Times New Roman"), axis.text=element_text(size=16))

plot_alpha_older <- ggplot(filter(neural_data_long_alpha, participant >= 10600), aes(x = as.factor(block), y = as.numeric(value), fill = as.factor(block))) +
  geom_violin(alpha = 0.4, adjust  = 0.8) +
  geom_point(aes(colour = as.factor(participant)),shape = 4, alpha = 0.5) +
  geom_line(aes(colour = as.factor(participant)), group= 1, alpha = 0.5) +
  geom_boxplot(alpha = 0.2, varwidth = TRUE, width = 0.45, outlier.shape = NA, coef = 0) +
  ggtitle("Older - Alpha change") +
  xlab("Time") +
  ylab("Total Score") +
  theme_minimal() +
  scale_color_viridis(discrete = TRUE, option = "C") +
  scale_fill_viridis_d(option = "C", begin = 0, end = 1) +
  #scale_x_discrete(labels=c("1" = "First Block", "0" = "Last block"), limits=rev) +
  ylim(-20,20) +
  theme(legend.position = 0, text=element_text(size=18,  family="Times New Roman"), axis.text=element_text(size=16))


plot_alpha_young
plot_alpha_older
ggsave(plot_alpha_young, filename = 'alpha_young.png', path = path, dpi = 1200, type = 'cairo', width = 10, height = 4.5, units = 'in', bg = 'white')
ggsave(plot_alpha_older, filename = 'alpha_older.png', path = path, dpi = 1200, type = 'cairo', width = 10, height = 4.5, units = 'in', bg = 'white')

```

## Beta

```{r}
plot_beta_young <- ggplot(filter(neural_data_long_beta, participant <= 10600 & participant != 10007), aes(x = as.factor(block), y = as.numeric(value), fill = as.factor(block))) +
  geom_violin(alpha = 0.4, adjust  = 0.8) +
  geom_point(aes(colour = as.factor(participant)),shape = 4, alpha = 0.5) +
  geom_line(aes(colour = as.factor(participant)), group= 1, alpha = 0.5) +
  geom_boxplot(alpha = 0.2, varwidth = TRUE, width = 0.45, outlier.shape = NA, coef = 0) +
  ggtitle("Young - Beta change") +
  xlab("Time") +
  ylab("Total Score") +
  theme_minimal() +
  scale_color_viridis(discrete = TRUE, option = "E") +
  scale_fill_viridis_d(option = "E", begin = 0, end = 1) +
  #scale_x_discrete(labels=c("1" = "First Block", "0" = "Last block"), limits=rev) +
  ylim(-1,6) +
  theme(legend.position = 0, text=element_text(size=18,  family="Times New Roman"), axis.text=element_text(size=16))

plot_beta_older <- ggplot(filter(neural_data_long_beta, participant >= 10600), aes(x = as.factor(block), y = as.numeric(value), fill = as.factor(block))) +
  geom_violin(alpha = 0.4, adjust  = 0.8) +
  geom_point(aes(colour = as.factor(participant)),shape = 4, alpha = 0.5) +
  geom_line(aes(colour = as.factor(participant)), group= 1, alpha = 0.5) +
  geom_boxplot(alpha = 0.2, varwidth = TRUE, width = 0.45, outlier.shape = NA, coef = 0) +
  ggtitle("Older - Beta change") +
  xlab("Time") +
  ylab("Total Score") +
  theme_minimal() +
  scale_color_viridis(discrete = TRUE, option = "E") +
  scale_fill_viridis_d(option = "E", begin = 0, end = 1) +
  #scale_x_discrete(labels=c("1" = "First Block", "0" = "Last block"), limits=rev) +
  ylim(-1,6) +
  theme(legend.position = 0, text=element_text(size=18,  family="Times New Roman"), axis.text=element_text(size=16))

neural_data_long_beta %>%
    group_by(block, age_group) %>%
    summarise(mean = mean(value))

plot_beta_young
plot_beta_older
ggsave(plot_beta_young, filename = 'newer_plots/beta_young.png', path = path, dpi = 1200, type = 'cairo', width = 10, height = 4.5, units = 'in', bg = 'white')
ggsave(plot_beta_older, filename = 'newer_plots/beta_older.png', path = path, dpi = 1200, type = 'cairo', width = 10, height = 4.5, units = 'in', bg = 'white')

```

## VSS

### RT

```{r}
rt_plots <- rt_natural %>%
    group_by(age_group, block) %>%
    summarise(rt = mean(rt))

err_plots <- err %>%
    mutate(age_group = ifelse(participant > 10600, "older", "young")) %>%
    group_by(age_group, block) %>%
    summarise(err = mean(err))

ie_plots <- ie %>%
    mutate(age_group = ifelse(participant > 10600, "Older", "Young")) %>%
    group_by(age_group, block) %>%
    summarise(ie = mean(ie))

plot_rt <- ggplot(rt_plots, aes(x = as.factor(block), y = as.numeric(rt), color = as.factor(age_group), group=age_group, col=age_group)) +
  geom_point(size = 5, alpha = 0.9) +
  geom_path(aes(col = age_group, group = age_group), size = 3, alpha = 0.9) +
  ggtitle("A") +
  xlab("Block") +
  ylab("") +
  theme_minimal() +
  scale_color_viridis(discrete = TRUE, option = "B", begin = 0.3, end = 0.9,direction=-1) +
  scale_y_continuous(limits = c(400, 600)) +
  scale_fill_viridis_d(limits=rev, option = "B", begin = 0.3, end = 0.9,direction=-1) +
  theme(legend.position = 0, text=element_text(size=28, family="Tahoma", colour="black"), axis.text=element_text(size=28, colour="black"), plot.background = element_rect(fill = "white"),panel.border = element_blank())

plot_nogo <- ggplot(err_plots, aes(x = as.factor(block), y = as.numeric(err), color = as.factor(age_group), group=age_group, col=age_group)) +
  geom_point(size = 5, alpha = 0.9) +
  geom_path(aes(col = age_group, group = age_group), size = 3, alpha = 0.9) +
  ggtitle("B") +
  xlab("Block") +
  ylab("") +
  theme_minimal() +
  scale_color_viridis(discrete = TRUE, option = "B", begin = 0.3, end = 0.9,direction=-1) +
  scale_y_continuous(limits = c(0, 30)) +
  scale_fill_viridis_d(limits=rev, option = "B", begin = 0.3, end = 0.9,direction=-1) +
  theme(legend.position = 0, text=element_text(size=28, family="Tahoma", colour="black"), axis.text=element_text(size=28, colour="black"), plot.background = element_rect(fill = "white"),panel.border = element_blank())


plot_ie <- ggplot(ie_plots, aes(x = as.factor(block), y = as.numeric(ie), color = as.factor(age_group), group=age_group, col=age_group)) +
  geom_point(size = 5, alpha = 0.9) +
  geom_path(aes(col = age_group, group = age_group), size = 3, alpha = 0.9) +
  ggtitle("C") +
  xlab("Block") +
  ylab("") +
  theme_minimal() +
  scale_color_viridis(discrete = TRUE, option = "B", begin = 0.3, end = 0.9,direction=-1) +
  scale_y_continuous(limits = c(5, 7)) +
  scale_fill_viridis_d(limits=rev, option = "B", begin = 0.3, end = 0.9,direction=-1) +
  labs(color = "Age group") +
  theme(text=element_text(size=28, family="Tahoma", colour="black"), axis.text=element_text(size=28, colour="black"), plot.background = element_rect(fill = "white"),panel.border = element_blank())
```