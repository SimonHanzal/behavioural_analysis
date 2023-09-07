
# Confirming regressions----

model <- lmer(err ~ block * age_group + (1 | participant), err_anova)
summary(model)

model <- lmer(rt ~ block * age_group + (1 | participant), rt_anova)
summary(model)

model <- lm(rt_overall ~ err_overall*age_group, all_data)
summary(model)

vas_change <- vas_change %>%
    mutate(age_group = ifelse(participant > 10600,1,0))

model <- lm(vas_change ~ age_group, vas_change)
summary(model)

wan_addition <- wan_addition %>%
    mutate(age_group = ifelse(participant > 10600,1,0))

model <- lm(wan_change ~ age_group, wan_addition)
summary(model)

comparison <- lm(wan ~ timepoint * age_group, wan_anova)
summary(comparison)

model <- lmer(vas ~ timepoint * age_group + (1 | ID), vas_anova)
summary(model)
#anova(model,comparison)

model <- lmer(wan ~ timepoint * age_group + (1 | ID), wan_anova)
summary(model)

model <- lmer(wan ~ timepoint * age_group + (1 | ID), wan_anova)
summary(model)

# Sleep----

sleep <- demographic_data %>%
    mutate(vas_change = `dif-vas`, wan_change = `dif-wan`, age_group = ifelse(ID > 10600, 1, 0), vas = `pre-vas`) %>%
    select(sleep,vas_change,wan_change,age_group, vas)

model <- lm(vas ~ sleep*age_group, sleep)
summary(model)

# Error----

# Think about the interactions
model <- lm(err_overall ~ age_group + vas_change + rt_overall + rt_change + err_change,
all_data)
summary(model)

# Error change----

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

# Neural links----

neural_data_bin <- neural_data %>%
    mutate(beta_change = ifelse(block_change_beta > median(block_change_beta), 1, 0)) %>%
    mutate(alpha_change = ifelse(block_change_alpha > median(block_change_alpha), 1, 0))

neural_data_long_beta <- neural_data_long_beta %>%
    mutate(age_group = ifelse(age_group == 0, "young", "older"))
    # filter(participant > 10001)

neural_data_long_alpha <- neural_data_long_alpha %>%
    mutate(age_group = ifelse(age_group == 0, "young", "older"))
    # filter(participant > 10001)
model <- lm(err_change ~ block_change_alpha, neural_data)
summary(model)
#!!
model <- lm(block_change_alpha ~ block_change_beta * vas_change * age_group, neural_data)
summary(model)
#!!!!
model <- lm(block_change_alpha ~ block_change_beta, neural_data)
summary(model)

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


model  <- lm(wan_change ~ vas_change, neural_data_bin)
summary(model)
