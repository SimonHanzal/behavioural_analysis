# Simon Hanzal (c)

# House management----
library(tidyverse)
library(readxl)
library(ez)
library(Cairo)


splinter <- function(dataset, var, sub_part) {
    splintered <- filter(dataset, {{ var }} == sub_part)
}



signal <- read_excel("data/290423_anova.xlsx")

signal_uncorrected <- splinter(signal, type, "uncorrected")

signal_corrected <- splinter(signal, type, "corrected")

signal_motivation <- read_excel("data/290423_anova_motivation.xlsx")

signal_uncorrected_motivation <- signal_motivation %>%
    filter(type == "uncorrected")

signal_corrected_motivation <- signal_motivation %>%
    filter(type == "corrected")

# Main----

## Uncorrected----

signal_uncorrected <- signal_uncorrected %>%
    mutate(participant = as.character(participant),
           time = as.character(time),
           age_group = as.character(age_group))

model <- ezANOVA(
    signal_uncorrected,
    dv = signal,
    wid = .(participant),
    within = .(time),
    between = .(age_group),
    type = 2,
    detailed = TRUE
)
model

## Corrected----

signal_corrected <- signal_corrected %>%
    mutate(participant = as.character(participant),
           time = as.character(time),
           age_group = as.character(age_group))

model <- ezANOVA(
    signal_corrected,
    dv = signal,
    wid = .(participant),
    within = .(time),
    between = .(age_group),
    type = 2,
    detailed = TRUE
)
model

df <- signal_corrected %>%
    mutate(time = recode(time, `pre` = "first", `post` = "last")) %>%
    rename(`Age group` = age_group, Block = time) %>%
    group_by(Block, `Age group`) %>%
    summarise(`signal (db)` = mean(signal), sd = as.numeric(sd(abs(signal))))

p <- ggplot(df, aes(x = Block, y = `signal (db)`, fill = `Age group`)) +
    geom_col(position = "dodge") +
    scale_fill_viridis_d(option = "B", begin = 0.25, end = 0.9, direction = -1) +
    theme_minimal() +
    theme(axis.text.x = element_text(colour = "#7F7F7F", size = 28),
          axis.text.y = element_text(colour = "#7F7F7F", size = 28),
          axis.title.x = element_text(colour = "#7F7F7F", size = 38),
          axis.title.y = element_text(colour = "#7F7F7F", size = 38),
          legend.text = element_text(colour = "#7F7F7F", size = 20),
          legend.title = element_text(colour = "#7F7F7F", size = 28))
p
png(filename="figures/corrected_anova.png", width = 600, height = 600, units = "px", pointsize = 12, bg = "#262626", type="cairo")
print(p)
dev.off()

# Brain-Subjective----

## Uncorrected----
signal_uncorrected_1 <- signal_uncorrected %>%
    filter(time == "post") %>%
    rename(vas_post = vas, wan_post = wan, signal_post = signal) %>%
    select(-time)

signal_uncorrected_2 <- signal_uncorrected %>%
    filter(time == "pre") %>%
    rename(vas_pre = vas, wan_pre = wan, signal_pre = signal) %>%
    select(-time)

signal_uncorrected_3 <- signal_uncorrected_1 %>%
    inner_join(signal_uncorrected_2, by = c("participant", "age_group", "type")) %>%
    mutate(signal = signal_post - signal_pre, vas = vas_post - vas_pre, wan = wan_post - wan_pre)


model <- lm(vas ~ signal, signal_uncorrected_3)
summary(model)

model <- lm(wan ~ signal, signal_uncorrected_3)
summary(model)
## Corrected----

signal_corrected_1 <- signal_corrected %>%
    filter(time == "post") %>%
    rename(vas_post = vas, wan_post = wan, signal_post = signal) %>%
    select(-time)

signal_corrected_2 <- signal_corrected %>%
    filter(time == "pre") %>%
    rename(vas_pre = vas, wan_pre = wan, signal_pre = signal) %>%
    select(-time)

signal_corrected_3 <- signal_corrected_1 %>%
    inner_join(signal_corrected_2, by = c("participant", "age_group", "type")) %>%
    mutate(signal = signal_post - signal_pre, vas = vas_post - vas_pre, wan = wan_post - wan_pre)


model <- lm(vas ~ signal, signal_corrected_3)
summary(model)

model <- lm(wan ~ signal, signal_corrected_3)
summary(model)

#    geom_errorbar(aes(ymin = as.numeric(`signal (db)`)-sd, ymax = as.numeric(`signal (db)`)+sd), position = "dodge", size = 0.3)

# Motivation----

## Uncorrected----

signal_uncorrected <- signal_uncorrected_motivation %>%
    mutate(participant = as.character(participant),
           time = as.character(time),
           age_group = as.character(age_group))

model <- ezANOVA(
    signal_uncorrected,
    dv = signal,
    wid = .(participant),
    within = .(time),
    between = .(age_group),
    type = 2,
    detailed = TRUE
)
model

df <- signal_uncorrected %>%
    mutate(time = recode(time, `pre` = "first", `post` = "last")) %>%
    rename(`Age group` = age_group, Block = time) %>%
    group_by(Block, `Age group`) %>%
    summarise(`signal` = mean(signal), sd = as.numeric(sd(abs(signal))))

p <- ggplot(df, aes(x = Block, y = `signal`, fill = `Age group`)) +
    geom_col(position = "dodge") +
    scale_fill_viridis_d(option = "B", begin = 0.25, end = 0.9, direction = -1, guide="none") +
    theme_minimal() +
    theme(axis.text.x = element_text(colour = "#7F7F7F", size = 28),
          axis.text.y = element_text(colour = "#7F7F7F", size = 28),
          axis.title.x = element_text(colour = "#7F7F7F", size = 38),
          axis.title.y = element_text(colour = "#7F7F7F", size = 38),
          legend.text = element_text(colour = "#7F7F7F", size = 20),
          legend.title = element_text(colour = "#7F7F7F", size = 28))
p
png(filename="figures/uncorrected_anova.png", width = 600, height = 600, units = "px", pointsize = 12, bg = "#262626", type="cairo")
print(p)
dev.off()

## Corrected----

signal_corrected <- signal_corrected_motivation %>%
    mutate(participant = as.character(participant),
           time = as.character(time),
           age_group = as.character(age_group))

model <- ezANOVA(
    signal_corrected,
    dv = signal,
    wid = .(participant),
    within = .(time),
    between = .(age_group),
    type = 2,
    detailed = TRUE
)
model

## All motivated----

signal_uncorrected_motivation <- signal_motivation %>%
    filter(type == "uncorrected")

signal_corrected_motivation <- signal_motivation %>%
    filter(type == "corrected")

## corrected----

signal_corrected <- signal_corrected_motivation %>%
    mutate(participant = as.character(participant),
           time = as.character(time),
           motivation = as.character(motivation),
           age_group = as.character(age_group))

model <- ezANOVA(
    signal_corrected,
    dv = signal,
    wid = .(participant),
    within = .(time),
    between = .(age_group, motivation),
    type = 2,
    detailed = TRUE
)
model

df <- signal_corrected %>%
    mutate(time = recode(time, `post` = "main", `motivate` = "motivation")) %>%
    rename(`Age group` = age_group, Block = time, Motivation = motivation) %>%
    group_by(`Age group`, Block, Motivation) %>%
    summarise(`signal (db)` = mean(signal))

p <- ggplot(df, aes(x = Block, y = `signal (db)`, fill = `Age group`, alpha = Motivation, colour = Motivation)) +
    geom_col(position = "dodge") +
    scale_fill_viridis_d(option = "B", begin = 0.25, end = 0.9, direction = -1) +
    scale_colour_viridis_d(option = "B", begin = 0.25, end = 0.9) +
#    scale_alpha_discrete(guide = "none")+
    theme_minimal() +
    theme(axis.text.x = element_text(colour = "#7F7F7F", size = 28),
          axis.text.y = element_text(colour = "#7F7F7F", size = 28),
          axis.title.x = element_text(colour = "#7F7F7F", size = 38),
          axis.title.y = element_text(colour = "#7F7F7F", size = 38),
          legend.text = element_text(colour = "#7F7F7F", size = 20),
          legend.title = element_text(colour = "#7F7F7F", size = 28))
p
png(filename="figures/corrected_moti_anova.png", width = 600, height = 600, units = "px", pointsize = 12, bg = "#262626", type="cairo")
print(p)
dev.off()


## Uncorrected----

signal_uncorrected <- signal_uncorrected_motivation %>%
    mutate(participant = as.character(participant),
           time = as.character(time),
           motivation = as.character(motivation),
           age_group = as.character(age_group))

model <- ezANOVA(
    signal_uncorrected,
    dv = signal,
    wid = .(participant),
    within = .(time),
    between = .(age_group, motivation),
    type = 2,
    detailed = TRUE
)
model

df <- signal_uncorrected %>%
    mutate(time = recode(time, `post` = "main", `motivate` = "motivation")) %>%
    rename(`Age group` = age_group, Block = time, Motivation = motivation) %>%
    group_by(`Age group`, Block, Motivation) %>%
    summarise(`signal` = mean(signal)) #%>%
   # mutate(Motivation = ifelse(Motivation = "demotivated", 0.3,0.7))

p <- ggplot(df, aes(x = Block, y = `signal`, fill = `Age group`, alpha = Motivation, colour = Motivation)) +
    geom_col(position = "dodge") +
    scale_fill_viridis_d(option = "B", begin = 0.25, end = 0.9, direction = -1) +
    scale_colour_viridis_d(option = "B", begin = 0.25, end = 0.9) +
    #    scale_alpha_discrete(guide = "none")+
    theme_minimal() +
    theme(axis.text.x = element_text(colour = "#7F7F7F", size = 28),
          axis.text.y = element_text(colour = "#7F7F7F", size = 28),
          axis.title.x = element_text(colour = "#7F7F7F", size = 38),
          axis.title.y = element_text(colour = "#7F7F7F", size = 38),
          legend.text = element_text(colour = "#7F7F7F", size = 20),
          legend.title = element_text(colour = "#7F7F7F", size = 28))
p
png(filename="figures/uncorrected_moti_anova.png", width = 600, height = 600, units = "px", pointsize = 12, bg = "#262626", type="cairo")
print(p)
dev.off()



