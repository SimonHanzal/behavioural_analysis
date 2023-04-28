# Simon Hanzal (c)

# Setup----

# Modify path as needed depending on where the files are stored in your device
#path <- r"(C:\Users\hanza\OneDrive\Dokumenty\R)"
path <- r"(C:\Users\simonha\OneDrive - University of Glasgow\research\data\exp1_data\compilation)"
setwd(path)
library(tidyverse)
library(viridis)
library(readxl)
library(broom)
library(lme4)
library(psych)


# Load----
demographic_data <- read_excel("demographic.xlsx")
excluded = c(
    10004, 10006, 10502, 15004, 15508, 15509
)

alpha_change <- read_excel("alpha_change.xlsx")


# Cleaning----

demo_clean <- demographic_data %>%
    filter(!is.na(ID)) %>%
    # exclusions
    filter(!ID %in% excluded) %>%
    mutate(age_group = ifelse(age <= 50, "young", "older"))


# Summaries----
demo_clean %>%
  count(handedness, gender)


# Age

age_information <- demo_clean %>%
 #   group_by(age_group) %>%
    summarise(mean = mean(`percent kept`), sd = sd(`percent kept`), median = median(`percent kept`), min = min(`percent kept`), max = max(`percent kept`), number = n())

# Moca
summary(lm(moca ~ age_group, demo_clean))


# Vas----
prevas_information <- demo_clean %>%
    group_by(age_group) %>%
    summarise(mean = mean(`pre-vas`), sd = sd(`pre-vas`), median = median(`pre-vas`), min = min(`pre-vas`), max = max(`pre-vas`), number = n())
prevas_information

postvas_information <- demo_clean %>%
    group_by(age_group) %>%
    summarise(mean = mean(`post-vas`), sd = sd(`post-vas`), median = median(`pre-vas`), min = min(`post-vas`), max = max(`post-vas`), number = n())
postvas_information

vaschange_information <- demo_clean %>%
    group_by(age_group) %>%
    summarise(mean = mean(`dif-vas`), sd = sd(`dif-vas`), median = median(`dif-vas`), min = min(`dif-vas`), max = max(`dif-vas`), number = n())
vaschange_information

wanchange_information <- demo_clean %>%
    group_by(age_group) %>%
    summarise(mean = mean(`dif-wan`), sd = sd(`dif-wan`), median = median(`dif-wan`), min = min(`dif-wan`), max = max(`dif-wan`), number = n())
wanchange_information

pre_wan_alpha <-  demo_clean %>%
    select(pre_wan_1:pre_wan_4) %>%
    psych::alpha()
pre_wan_alpha

post_wan_alpha <-  demo_clean %>%
    select(post_wan_1:post_wan_4) %>%
    psych::alpha()
post_wan_alpha

pre_vas_alpha <-  demo_clean %>%
    select(pre_vas_1:pre_vas_13) %>%
    psych::alpha()
pre_vas_alpha

post_vas_alpha <-  demo_clean %>%
    select(post_vas_1:post_vas_13) %>%
    psych::alpha()
post_vas_alpha
        

post_wan_alpha

# Testing----

main_test <- t.test(demo_clean$`pre-vas`, demo_clean$`post-vas`, paired = TRUE)
tidy(main_test)

vas_anova <- demo_clean %>%
    pivot_longer(c(`pre-vas`, `post-vas`), names_to = "timepoint", values_to = "vas") %>%
    mutate(timepoint = recode(timepoint, "pre-vas" = 0, "post-vas" = 1), age_group = ifelse(ID < 10600, 0, 1)) %>%
    select(ID, timepoint, vas, age_group) %>%
    mutate(ID = as.character(ID),
       timepoint = as.character(timepoint),
       age_group = as.character(age_group))

wan_anova <- demo_clean %>%
    pivot_longer(c(`pre-wan`, `post-wan`), names_to = "timepoint", values_to = "wan") %>%
    mutate(timepoint = recode(timepoint, "pre-wan" = 0, "post-wan" = 1)) %>%
    mutate(wan = wan/4, age_group = ifelse(ID < 10600, 0, 1)) %>%
    select(ID, timepoint, wan, age_group) %>%
    mutate(ID = as.character(ID),
       timepoint = as.character(timepoint),
       age_group = as.character(age_group))

vas_anova_pre <- vas_anova %>%
  filter(timepoint == 0)

main_test <- t.test(filter(vas_anova_pre, age_group == 0)%>%pull(vas),
                    filter(vas_anova_pre, age_group == 1)%>%pull(vas),
                    paired = FALSE)
tidy(main_test)


model <- ezANOVA(
	vas_anova,
	dv = vas,
	wid = .(ID),
	within = .(timepoint),
    between = .(age_group),
	type = 2,
	detailed = TRUE
)
model

model <- ezANOVA(
	wan_anova,
	dv = wan,
	wid = .(ID),
	within = .(timepoint),
    between = .(age_group),
	type = 2,
	detailed = TRUE
)
model


# Export----

demo_export <- demo_clean %>%
    mutate(subjective_difference = `dif-vas`) %>%
    select(ID, subjective_difference)

write.csv(demo_export, "subjective_difference.csv")

