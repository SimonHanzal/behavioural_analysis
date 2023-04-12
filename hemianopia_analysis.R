library(here)
library(tidyverse)
library(readxl)

here()
path <- r"(C:\Users\simonha\OneDrive - University of Glasgow\research\data\exp1_data\compilation)"
setwd(path)
hemianopia_df <- read_excel("optometric.xlsx")

hemianopia_cleaned <- hemianopia_df %>%
    filter(Running == "TrialList", ImageName != "fixation") %>%
    mutate(age_group = ifelse(participant > 10600, "older", "young")) %>%
    group_by(ImageName, age_group) %>%
    count(Record.Acc) %>%
    separate(ImageName,c("horizontal", "vertical"), "y") %>%
    mutate(horizontal = substring(horizontal,2), vertical = substring(vertical, first = 1, last = 1)) %>%
    filter(Record.Acc == 1) %>%
    filter(as.numeric(horizontal)>1 & as.numeric(horizontal)<10) %>%
    filter(as.numeric(vertical)>1 & as.numeric(vertical)<6) %>%
    mutate(n = n/0.21)

ggplot(hemianopia_cleaned, aes(x = as.numeric(horizontal),y = as.numeric(vertical), fill = n)) +
    geom_tile() + labs(title ="Visual Screening results", x = "Horizontal Position", y = "Vertical Position", fill = "Percentage correct") +
    facet_grid(. ~ age_group)



+
    theme(axis.text.x = element_blank(), axis.text.y = element_blank())    
