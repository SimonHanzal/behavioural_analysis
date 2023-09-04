library(tidyverse)
library(here)

# Since----

## Column----

# tt = Trial_Type
tt_switching <- trial_data %>%
    select(participant, block, duration, Stimulus.ACC, Stimulus.RT, age_group,
           trig, observation) %>%
    mutate(trial = as.numeric(observation))



tt_switching_since <- tt_switching %>%
    arrange(participant, trial) %>%
    group_by(participant, temp = rev(cumsum(rev(Stimulus.ACC==0)))) %>%
    mutate(since = ifelse(Stimulus.ACC==0, n()-1, NA)) %>%
    ungroup() %>%
    select(-temp)


no = 1
repeat {
    no = no + 1  
    tt_switching_since$since = ifelse(!is.na(tt_switching_since$since), tt_switching_since$since, lead(tt_switching_since$since)-1)
    if(no > (441)) {
        break
    }
}

tt_switching_since <- tt_switching_since %>%
    mutate(since = ifelse(since < 0, 0, since))

## Summaries----

tt_switching_since_summary <- tt_switching_since %>%
    group_by(since, age_group) %>%
    summarise(mean = mean(Stimulus.RT)) %>%
    filter(since <= 50)

tt_switching_since_summary_statistic <- tt_switching_since %>%
    group_by(since, age_group, participant) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    group_by(since) %>%
    summarise(mean = mean(n), sd = sd(n))
    

ggplot(tt_switching_since_summary) +
    geom_line(aes(x = since, y = mean, color = as.character(age_group)))

tt_switching_since_anova <- tt_switching_since %>%
    group_by(since, age_group, participant) %>%
    summarise(mean = mean(Stimulus.RT)) %>%
    filter(since <= 1) %>%
    mutate(participant = as.factor(participant))

model <- ezANOVA(
    tt_switching_since_anova,
    dv = mean,
    wid = .(participant),
    between = .(age_group, since),
    type = 2,
    detailed = TRUE
)
model

## Conclusion: Older age group seems to have a very high "0" bin, surpassed only after a lot of trials, indicative of
# Potentially some post-error slowing. This can be followed up.

# Since mistake----

tt_switching_mistake <- tt_switching_since %>%  
    mutate(first_mistake = ifelse(since == 0, 1, 0))

tt_switching_mistake[1,18] <- 0


tt_switching_mistake <- tt_switching_mistake %>%
    mutate(since1 = ifelse(since > -1, since, 
                           since+1000),
           since2 = ifelse(since > -1, since, 
                           abs(since)),
           since3 = ifelse(since > -1, 0, 
                           1),
           since4 = cumsum(since3)) %>%
    select(-since1, -since2, -since3) %>%
    mutate(since = ifelse(since > -1, since, since4)) %>%
    select(-since4) %>%
    mutate()

## Summaries----

tt_switching_mistake_summary <- tt_switching_mistake %>%
    group_by(since, age_group) %>%
    summarise(mean = mean(Stimulus.RT)) %>%
    filter(since <= 50)

ggplot(tt_switching_mistake_summary) +
    geom_line(aes(x = since, y = mean, color = as.character(age_group)))



tt_switching_mistake_anova <- tt_switching_mistake %>%
    group_by(since, age_group, participant) %>%
    summarise(mean = mean(Stimulus.RT)) %>%
    filter(since <= 1) %>%
    mutate(participant = as.factor(participant))

model <- ezANOVA(
    tt_switching_mistake_anova,
    dv = mean,
    wid = .(participant),
    between = .(age_group, since),
    type = 2,
    detailed = TRUE
)
model


## Poster

tt_switching_mistake_summary <- tt_switching_switch %>%
    group_by(after, age_group, participant) %>%
    summarise(mean = mean(Stimulus.RT)) %>%
    filter(after <= 50) 

rt_natural_unite <- rt_natural %>%
    group_by(participant)%>%
    summarise(mean_overall = mean(rt), participant = as.character(participant))

tt_compare <- tt_switching_mistake_summary %>%
    mutate(participant = as.character(participant)) %>%
    inner_join(rt_natural_unite, by = "participant") %>%
    mutate(difference = mean / mean_overall) %>%
    group_by(after, age_group) %>%
    summarise(mean = mean(difference)) %>%
    filter(after <= 10) 


threes_statistic <- tt_switching_switch %>%
    group_by(since, age_group, participant) %>%
    summarise(mean = mean(Stimulus.RT))
#
threes_reaction <- threes_statistic %>%
    mutate(participant = as.character(participant)) %>%
    inner_join(rt_natural_unite, by = "participant") %>%
    mutate(difference = log(mean) / log(mean_overall)) %>%
    #mutate(difference = log(mean)) %>%
    group_by(since, age_group, participant) %>%
    na.omit(Stimulus.RT) %>%
    summarise(mean = mean(as.numeric(difference))) %>%
    filter(since <= 1) %>%
    mutate(since = ifelse(since <= 0, 0, 1)) %>%
    mutate(participant = as.character(participant), since = as.character(since),
           age_group = as.character(age_group))

ggplot(threes_reaction) +
    geom_violin(aes(x = as.character(since), y = mean, color = as.character(age_group)))


model <- ezANOVA(
    threes_reaction
    , dv = mean
    , wid = participant
    , within = since
    , between = age_group
)
model
#
library(ez)

threes_statistic <- tt_switching_switch %>%
    group_by(after, age_group, participant) %>%
    filter((trig == 3 || trig == 6) & Stimulus.ACC == 0) %>%
    summarise(mean = mean(Stimulus.RT)) %>%
    mutate(participant = as.character(participant), after = as.character(after),
           age_group = as.character(age_group))
n = 4
threes_reaction <- threes_statistic %>%
    filter(participant != 15003) %>%
    filter(participant != 15501) %>%
    filter(as.numeric(after) <= 1) %>%
    inner_join(rt_natural_unite, by = "participant") %>%
    mutate(difference = log(mean) / log(mean_overall)) %>%
    mutate(after = ifelse(after <= 1, 0, 1)) %>%
    #mutate(difference = mean) %>%
    group_by(after, age_group, participant) %>%
    na.omit(Stimulus.RT) %>%
    summarise(mean = mean(as.numeric(difference))) %>%
    #filter(after == 5) %>%
    mutate(participant = as.character(participant), after = as.character(after),
           age_group = as.character(age_group))

ggplot(threes_reaction) +
    geom_violin(aes(x = as.character(after), y = mean, color = as.character(age_group)))


model <- ezANOVA(
    threes_reaction
    , dv = mean
    , wid = participant
    , within = after
    , between = age_group
)
model

>>>>>>> 92466e491738dd7abcd78f2806b63e9bc5f58e53
# Stimulus occurence----

tt_switching_switch <- tt_switching_since %>%
    arrange(participant, trial) %>%
    group_by(participant, temp = rev(cumsum(rev(trig=="3" | trig=="6")))) %>%
    mutate(after = ifelse(trig=="3" | trig=="6", n()-1, NA)) %>%
    ungroup() %>%
    select(-temp)

no = 1
repeat {
    no = no + 1  
    tt_switching_switch$after = ifelse(!is.na(tt_switching_switch$after),
                                       tt_switching_switch$after, lead(tt_switching_switch$after)-1)
    if(no > 468) {
        break
    }
}

tt_switching_switch <- tt_switching_switch %>%
    group_by(participant) %>%
    mutate(since1 = ifelse(after > -1, after, 
                           after+1000),
           since2 = ifelse(after > -1, after, 
                           abs(after)),
           since3 = ifelse(after > -1, 0, 
                           1),
           since4 = cumsum(since3)) %>%
    select(-since1, -since2, -since3) %>%
    mutate(after = ifelse(after > -1, after, since4)) %>%
    select(-since4)

## Summaries----

threes_statistic <- tt_switching_switch %>%
    group_by(after) %>%
    summarise(n())

threes_reaction <- tt_switching_switch %>%
    group_by(after, age_group) %>%
    na.omit(Stimulus.RT) %>%
    summarise(mean = mean(as.numeric(Stimulus.RT)))

ggplot(threes_reaction) +
    geom_line(aes(x = after, y = mean, color = as.character(age_group)))

tt_switching_threes_anova <- tt_switching_switch %>%
    group_by(after, age_group, participant) %>%
    summarise(mean = mean(Stimulus.RT)) %>%
    filter(after <= 1) %>%
    mutate(participant = as.factor(participant))

model <- ezANOVA(
    tt_switching_threes_anova,
    dv = mean,
    wid = .(participant),
    between = .(age_group, after),
    type = 2,
    detailed = TRUE
)
model
## This looks a little suspicious

#

#endurance_statistic <- tt_switching_switch %>%
#    group_by(since) %>%
#    summarise(n())
#
#endurance_reaction <- tt_switching_switch %>%
#    group_by(since) %>%
#    na.omit(Stimulus.RT) %>%
#    summarise(mean = mean(as.numeric(Stimulus.RT))) %>%
#    slice(1:468)

## Proceedings----

# Identify where the amount of post-trials drops too much to be viable for investigation
# Get the trial indeces for trials after a long switch versus a short switch.
# This is to be done in tt_switching_switch

### Pushing forward----
tt_switching_switch_clean <- tt_switching_switch %>%
    filter(removed_list == 0, removed_length == 0, removed_omission == 0, Stimulus.ACC == 1) %>%
    filter(trig %in% c(3,6))

# No need to group by age, results were similar

tt_switching_switch_clean_summary <- tt_switching_switch_clean %>%
    group_by(after) %>%
    summarise(n = n())

bin_size_mean <- tt_switching_switch_clean_summary %>%
    ungroup() %>%
    summarise(mean = mean(n)) %>%
    pluck(1)

tt_passing_trials <- tt_switching_switch_clean_summary %>%
    filter(n > bin_size_mean)

## Answer: Get omission trials with switch either after a nogo (after = 0)
# Or one go in between (after = 1)
# Or five go in between (after = 5)

### Trial indeces----

# Very few between participants. Best to analyse the switch.

tt_5 <- tt_switching_switch_clean %>%
    filter(after == 5)

# This is more promising

tt_0 <- tt_switching_switch_clean %>%
    filter(after == 0) %>%
    select(participant, trial) %>%
    pivot_wider(names_from=participant, values_from=trial) %>%
    pivot_longer(1:40, names_to = "participant", values_to = "trials") %>%
    separate(trials, into = as.character(c(1:27))) %>%
    select(-2) %>%
    unite(trial, as.character(c(2:27)), sep = " ")




tt_1 <- tt_switching_switch_clean %>%
    filter(after == 1) %>%
    select(participant, trial) %>%
    pivot_wider(names_from=participant, values_from=trial) %>%
    pivot_longer(1:40, names_to = "participant", values_to = "trials") %>%
    separate(trials, into = as.character(c(1:27))) %>%
    select(-2) %>%
    unite(trial, as.character(c(2:27)), sep = " ")


write.csv2(tt_0, filename=here("data", "processed", "tt_0.csv"))

## Just switch 1----

rt_natural_tt <- rt_natural %>%
    group_by(age_group) %>%
    summarise(mean_overall = mean(rt)) %>%
    mutate(age_group = as.character(age_group))

tt_switching_switch_ttest <-tt_switching_switch %>%
    filter(since == 1) %>%
    filter(trig != 3) %>%
    filter(trig != 6) %>%
    filter(Stimulus.ACC == 1) %>%
    group_by(participant, age_group) %>%
    summarise(mean = mean(Stimulus.RT)) 

%>%
    mutate(age_group = as.character(age_group)) %>%
    inner_join(rt_natural_tt, by = "age_group") %>%
    mutate(difference = mean-mean_overall)

young <- tt_switching_switch_ttest %>%
    filter(age_group == 0)

older <- tt_switching_switch_ttest %>%
    filter(age_group == 1)

model <- t.test(young$difference, older$difference)
model
