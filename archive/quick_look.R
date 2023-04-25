library(tidyverse)
library(readxl)
library("viridis") #for a plotting color scheme
library('Cairo') #for anti-aliasing
library("ggpubr")
CairoWin() #initialise
library("car") #for analysis
data <- read_excel("behavioural.xlsx")
demographic <- read_excel("demographic.xlsx") #%>%
  #filter(ID < 15000) %>%
  #filter(ID %in% c(10001,10003,10006,10009,10502,10504,10507,
                            # 10509)) %>%
  #filter(ID %in% c(10000,10002, 10004, 10005, 10007, 10008, 10010, 10501, 10503, 10505, 10506, 10508, 10510)) %>%
  summarise(mean = mean(`dif-vas`), sd = sd(`dif-vas`),
            min = min(`dif-vas`), max = max(`dif-vas`))

summary <- data %>%
 #  filter(participant %in% c(10001,10003,10006,10009,10502,10504,10507,
 #                           10509)) %>%
 # filter(participant %in% c(10000,10002, 10004, 10005, 10007, 10008, 10010, 10501, 10503, 10505, 10506, 10508, 10510)) %>%
  #filter(participant != 15000) %>%
  group_by(participant) %>%
  filter(condition == "NoGo") %>%
  filter(block >= 1.0 & block <= 8.0) %>%
  count(Stimulus.ACC) %>%
  mutate(proportion = round(n/144,4)) %>%
  filter(Stimulus.ACC == 1) %>%
  mutate(proportion = proportion*100) %>%
  select(participant, proportion) # %>%
  #ungroup() %>%
  #summarise(mean = mean(proportion), sd = sd(proportion),
  #          min = min(proportion), max = max(proportion))

summary_2 <- data %>%
  filter(participant < 16000) %>%
  group_by(block, participant) %>%
  filter(condition == "NoGo") %>%
  filter(block >= 1.0) %>%
  count(Stimulus.ACC) %>%
  mutate(proportion = round(n/(0.2),2)) %>%
  # mutate(proportion = round(n/(0.18*21),2)) %>%
  filter(Stimulus.ACC == 1) %>%
  select(block, proportion)

summary_3 <- data %>%
  # filter(participant %in% c(10000,10001,10003,10005,10007,10009,10501,
  #                          10503,10505,10507,10509)) %>%
  #filter(participant %in% c(10002,10004,10006,10008,10010,
  #                          10502,10504,10506,10508,10510)) %>%
  filter(participant < 16000) %>%
  group_by(block) %>%
  filter(block >= 1.0 & block <= 9.0) %>%
  filter(Stimulus.RT >= 150) %>%
  ungroup() %>%
  group_by(block, participant) %>%
  summarise(mean = mean(Stimulus.RT), sd = sd(Stimulus.RT),
            min = min(Stimulus.RT), max = max(Stimulus.RT))

summary_4 <- data %>%
  filter(participant < 15000) %>%
  group_by(participant) %>%
  filter(block >= 1.0 & block <= 8.0) %>%
  filter(Stimulus.RT >= 150) %>%
  ungroup() %>%
  summarise(mean = mean(Stimulus.RT), sd = sd(Stimulus.RT),
            min = min(Stimulus.RT), max = max(Stimulus.RT))

summary_5 <- data %>%
  filter(participant %in% c(10002,10004,10006,10008,10010,
                            10502,10504,10506,10508,10510)) %>%
  filter(participant < 15000) %>%
  group_by(block, participant) %>%
  filter(block >= 1.0) %>%
  filter(Stimulus.RT >= 150) %>%
  summarise(mean = mean(Stimulus.RT)) %>%
  filter(block == 1) %>%
  select(-block)

summary_6 <- data %>%

  filter(participant < 15000) %>%
  group_by(block, participant) %>%
  filter(block >= 1.0) %>%
  filter(Stimulus.RT >= 150) %>%
  summarise(mean = mean(Stimulus.RT)) %>%
 # filter(block == 8) %>%
  select(-block) %>%
  inner_join(y = summary_5, by = "participant") %>%
  mutate(difference = mean.x - mean.y) %>%
  summarise(mean = mean(difference), sd = sd(difference),
            min = min(difference), max = max(difference))

summary_go <- data %>%
  filter(participant < 15000) %>%
  group_by(participant) %>%
  filter(condition == "Go") %>%
  filter(block >= 1.0) %>%
  count(Stimulus.ACC) %>%
  mutate(proportion = round(n/648,4)) %>%
  filter(Stimulus.ACC == 1) %>%
  mutate(proportion = proportion*100) %>%
  select(participant, proportion) %>%
  ungroup() %>%
  summarise(mean = mean(proportion), sd = sd(proportion),
            min = min(proportion), max = max(proportion))

summary_go2 <- data %>%
  filter(participant < 15000) %>%
  group_by(block) %>%
  filter(condition == "Go") %>%
  filter(block >= 1.0) %>%
  count(Stimulus.ACC) %>%
  mutate(proportion = round(n/(0.72*21),2)) %>%
  filter(Stimulus.ACC == 1) %>%
  select(block, proportion)

summary_nogo <- data %>%
  filter(participant < 15000) %>%
  group_by(participant, block) %>%
  filter(condition == "NoGo") %>%
  filter(block >= 1.0) %>%
  count(Stimulus.ACC) %>%
  mutate(proportion = round(n/18,4)) %>%
  filter(Stimulus.ACC == 1) %>%
  mutate(proportion = proportion*100) %>%
  filter(block == 1) %>%
  select(-block) %>%
  select(participant, proportion)

summary_nogo2 <- data %>%
  filter(participant < 15000) %>%
  group_by(participant, block) %>%
  filter(condition == "NoGo") %>%
  filter(block >= 1.0) %>%
  count(Stimulus.ACC) %>%
  mutate(proportion = round(n/18,4)) %>%
  filter(Stimulus.ACC == 1) %>%
  mutate(proportion = proportion*100) %>%
  filter(block == 8) %>%
  select(-block) %>%
  select(participant, proportion) %>%
  inner_join(y = summary_nogo, by = "participant") %>%
  mutate(difference = proportion.x - proportion.y) #%>%
  #ungroup() %>%
 # summarise(mean = mean(difference), sd = sd(difference),
 #           min = min(difference), max = max(difference))

summary_9nogo <- data %>%
 # filter(participant %in% c(10000,10001,10003,10005,10007,10009,10501,
 #                          10503,10505,10507,10509)) %>%
 # filter(participant %in% c(10002,10004,10006,10008,10010,
  #                          10502,10504,10506,10508,10510)) %>%
  group_by(participant) %>%
  filter(condition == "NoGo") %>%
  filter(block == 9) %>%
  count(Stimulus.ACC) %>%
  mutate(proportion = round(n/18,4)) %>%
  filter(Stimulus.ACC == 1) %>%
  mutate(proportion = proportion*100) %>%
  select(participant, proportion) %>%
  ungroup() %>%
  summarise(mean = mean(proportion), sd = sd(proportion),
            min = min(proportion), max = max(proportion))

summary_9rt <- data %>%
  # filter(participant %in% c(10000,10001,10003,10005,10007,10009,10501,
  #                          10503,10505,10507,10509)) %>%
  #filter(participant %in% c(10002,10004,10006,10008,10010,
  #                          10502,10504,10506,10508,10510)) %>%
  group_by(block, participant) %>%
  filter(block >= 1.0) %>%
  filter(Stimulus.RT >= 150) %>%
  summarise(mean = mean(Stimulus.RT)) %>%
  filter(block == 9) %>%
  select(-block) %>%
  summarise(means = mean(mean), sd = sd(mean),
            min = min(mean), max = max(mean)) %>%
  ungroup()

summary_nogo3 <- summary_nogo2 %>%
  select(participant, proportion.x, proportion.y) %>%
  pivot_longer(cols = proportion.x:proportion.y, names_to = "when") %>%
  mutate(when=recode(when,`proportion.x`="first",`proportion.y`="last")) %>%
  ungroup()


summary_nogo4 <- data %>%
  filter(participant < 15000) %>%
  group_by(block, participant) %>%
  filter(condition == "NoGo") %>%
  filter(Stimulus.ACC == 1) %>%
  filter(block >= 1.0 & block <= 8.0) %>%
  count(Stimulus.ACC) %>%
  mutate(proportion = round(n/18,4)) %>%
  select(participant, block, proportion)

plot_nogo <- ggplot(summary_nogo4, aes(x = as.factor(block), y = as.numeric(proportion), fill = as.factor(block))) +
  geom_violin(alpha = 0.4, adjust  = 0.6) +
  geom_point(aes(colour = as.factor(participant)),shape = 4, alpha = 0.5) +
  geom_line(aes(colour = as.factor(participant)), group= 1, alpha = 0.5) +
  stat_summary(fun.y="median") +
  ggtitle("Nogo Accuracy") +
  xlab("Block") +
  ylab("Nogo Accuracy") +
  theme_minimal() +
  scale_color_viridis(discrete = TRUE, option = "D") +
  scale_fill_viridis_d(limits=rev, option = "D", begin = 0.4, end = 0.8) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  theme(legend.position = 0, text=element_text(size=18,  family="Times New Roman"), axis.text=element_text(size=16))

plot_nogo
ggsave(plot_nogo, filename = 'plots/nogo.png', dpi = 1200, type = 'cairo',
       width = 10, height = 4.5, units = 'in')


summary_rt <- data %>%
  # filter(participant %in% c(10000,10001,10003,10005,10007,10009,10501,
  #                          10503,10505,10507,10509)) %>%
  #filter(participant %in% c(10002,10004,10006,10008,10010,
  #                          10502,10504,10506,10508,10510)) %>%
  group_by(block, participant) %>%
  filter(block >= 1.0 & block <= 8.0) %>%
  filter(Stimulus.RT >= 150) %>%
  summarise(mean = mean(Stimulus.RT))

plot_rt <- ggplot(summary_rt, aes(x = as.factor(block), y = as.numeric(mean), fill = as.factor(block))) +
  geom_violin(alpha = 0.4, adjust  = 0.8) +
  geom_point(aes(colour = as.factor(participant)),shape = 4, alpha = 0.5) +
  geom_line(aes(colour = as.factor(participant)), group= 1, alpha = 0.5) +
  geom_boxplot(alpha = 0.2, varwidth = TRUE, width = 0.45, outlier.shape = NA, coef = 0) +
  ggtitle("Reaction Time") +
  xlab("Block") +
  ylab("Reaction Time (ms)") +
  theme_minimal() +
  scale_color_viridis(discrete = TRUE, option = "D") +
  scale_fill_viridis_d(limits=rev, option = "D", begin = 0.4, end = 0.8) +
  theme(legend.position = 0, text=element_text(size=18,  family="Times New Roman"), axis.text=element_text(size=16))

plot_rt
ggsave(plot_rt, filename = 'plots/rt.png', dpi = 1200, type = 'cairo',
       width = 10, height = 4.5, units = 'in')

summary_vas <- read_excel("demographic.xlsx") %>%
  filter(ID < 15000) %>%
  select(ID, `pre-vas`:`post-wan`) %>%
  pivot_longer(cols = `pre-vas`:`post-wan`, names_to = "condition") %>%
  filter(condition == "pre-vas" | condition == "post-vas")


plot_vas <- ggplot(summary_vas, aes(x = as.factor(condition), y = as.numeric(value), fill = as.factor(condition))) +
  geom_violin(alpha = 0.4, adjust  = 0.8) +
  geom_point(aes(colour = as.factor(ID)),shape = 4, alpha = 0.5) +
  geom_line(aes(colour = as.factor(ID)), group= 1, alpha = 0.5) +
  geom_boxplot(alpha = 0.2, varwidth = TRUE, width = 0.45, outlier.shape = NA, coef = 0) +
  ggtitle("Subjective Fatigue change") +
  xlab("Time") +
  ylab("Total Score") +
  theme_minimal() +
  scale_color_viridis(discrete = TRUE, option = "D") +
  scale_fill_viridis_d(option = "D", begin = 0, end = 1) +
  scale_x_discrete(labels=c("pre-vas" = "Pre-task", "post-vas" = "Post-task"), limits=rev) +
  theme(legend.position = 0, text=element_text(size=18,  family="Times New Roman"), axis.text=element_text(size=16))


plot_vas
ggsave(plot_vas, filename = 'plots/vas.png', dpi = 1200, type = 'cairo',
       width = 10, height = 4.5, units = 'in')


summary_wan <- read_excel("demographic.xlsx") %>%
  filter(ID < 15000) %>%
  select(ID, `pre-vas`:`post-wan`) %>%
  pivot_longer(cols = `pre-vas`:`post-wan`, names_to = "condition") %>%
  filter(condition == "pre-wan" | condition == "post-wan")

plot_wan <- ggplot(summary_wan, aes(x = as.factor(condition), y = as.numeric(value), fill = as.factor(condition))) +
  geom_violin(alpha = 0.4) +
  geom_point(aes(colour = as.factor(ID)),shape = 4, alpha = 0.5) +
  geom_line(aes(colour = as.factor(ID)), group= 1, alpha = 0.5) +
  geom_boxplot(alpha = 0.2, varwidth = TRUE, width = 0.45, outlier.shape = NA, coef = 0) +
  ggtitle("Subjective mind-wandering change") +
  xlab("Time") +
  ylab("Total Score") +
  theme_minimal() +
  scale_color_viridis(discrete = TRUE, option = "D") +
  scale_fill_viridis_d(option = "D") +
  scale_x_discrete(labels=c("pre-wan" = "Pre-task", "post-wan" = "Post-task"), limits=rev) +
  theme(legend.position = 0, text=element_text(size=18,  family="Times New Roman"), axis.text=element_text(size=16))

plot_wan
ggsave(plot_wan, filename = 'plots/wan.png', dpi = 1200, type = 'cairo',
       width = 10, height = 4.5, units = 'in')

summary_motivation_nogo <- data %>%
  filter(participant < 15000) %>%
  group_by(block, participant) %>%
  filter(condition == "NoGo") %>%
  filter(Stimulus.ACC == 1) %>%
  filter(block == 9) %>%
  count(Stimulus.ACC) %>%
  mutate(proportion = round(n/18,4)) %>%
  mutate(motivation = ifelse((participant %% 2 == 1 || participant == 10000),"Low", "High"))
  select(motivation, block, proportion)

plot_motivation_nogo <- ggplot(summary_motivation_nogo, aes(x = as.factor(motivation), y = as.numeric(proportion), fill = motivation), size = 0.1) +
  geom_violin(alpha = 0.4, adjust  = 1.3) +
  geom_boxplot(alpha = 0.5, varwidth = TRUE, width = 0.45) +
  ggtitle("Accuracy in Block 9 based on motivation") +
  xlab("Motivational Group") +
  ylab("Nogo Accuracy") +
  scale_color_viridis(discrete = TRUE, option = "D") +
  theme(legend.position = 0) +
  theme_minimal() +
  scale_x_discrete(labels=c("Low" = "Low (n = 11)", "High" = "High (n = 10)"), limits=rev) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  theme(legend.position = "none", text=element_text(size=18,  family="Times New Roman"), axis.text=element_text(size=16))

plot_motivation_nogo
ggsave(plot_motivation_nogo, filename = 'plots/motivation_nogo.png', dpi = 1200, type = 'cairo',
       width = 10, height = 4.5, units = 'in')

summary_motivation_rt <- data %>%
  # filter(participant %in% c(10000,10001,10003,10005,10007,10009,10501,
  #                          10503,10505,10507,10509)) %>%
  #filter(participant %in% c(10002,10004,10006,10008,10010,
  #                          10502,10504,10506,10508,10510)) %>%
  filter(block == 9.0) %>%
  filter(Stimulus.RT >= 150) %>%
  group_by(participant) %>%
  summarise(mean = mean(Stimulus.RT)) %>%
  ungroup() %>%
  mutate(motivation = ifelse((participant %% 2 == 1 | participant == 10000),"Low", "High"))



plot_motivation_rt <- ggplot(summary_motivation_rt, aes(x = as.factor(motivation), y = as.numeric(mean), fill = motivation), size = 0.1) +
  geom_violin(alpha = 0.4, adjust  = 0.6) +
  geom_boxplot(alpha = 0.5, varwidth = TRUE, width = 0.15) +
  ggtitle("Reaction Times in Block 9 based on motivation") +
  xlab("Motivational Group") +
  ylab("Reaction Time (ms)") +
  scale_color_viridis(discrete = TRUE, option = "D") +
  theme(legend.position = 0) +
  theme_minimal() +
  scale_x_discrete(labels=c("Low" = "Low (n = 11)", "High" = "High (n = 10)"), limits=rev) +
  theme(legend.position = "none", text=element_text(size=18,  family="Times New Roman"), axis.text=element_text(size=16))

plot_motivation_rt
ggsave(plot_motivation_rt, filename = 'plots/motivation_rt.png', dpi = 1200, type = 'cairo',
       width = 10, height = 4.5, units = 'in')


summary_covid_nogo <- data %>%
  filter(participant < 15000) %>%
  group_by(block, participant) %>%
  filter(condition == "NoGo") %>%
  filter(Stimulus.ACC == 1) %>%
  filter(block == 9) %>%
  count(Stimulus.ACC) %>%
  mutate(proportion = round(n/18,4)) %>%
  mutate(covid = ifelse(participant %in% c(10001,10003,10006,10009,10502,10504,10507, 10509), "had_covid", "no_covid")) %>%
  select(covid, block, proportion)


plot_covid_nogo <- ggplot(summary_covid_nogo, aes(x = as.factor(covid), y = as.numeric(proportion), fill = covid), size = 0.1) +
  geom_violin(alpha = 0.4, adjust  = 0.5) +
  geom_boxplot(alpha = 0.5, varwidth = TRUE, width = 0.4, outlier.shape = NA) +
  ggtitle("Overall nogo accuracy based on recent Covid") +
  xlab("Covid Group") +
  ylab("Nogo Accuracy") +
  scale_color_viridis(discrete = TRUE, option = "D") +
  theme(legend.position = 0) +
  theme_minimal() +
  scale_fill_viridis_d(limits=rev) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_x_discrete(labels=c("had_covid" = "Recent Covid (n = 8)", "no_covid" = "No Covid (n = 13)"), limits=rev) +
  theme(legend.position = "none", text=element_text(size=18,  family="Times New Roman"), axis.text=element_text(size=19))

plot_covid_nogo
ggsave(plot_covid_nogo, filename = 'plots/covid_nogo.png', dpi = 1200, type = 'cairo',
       width = 10, height = 4.5, units = 'in')

summary_covid_rt <- data %>%
  # filter(participant %in% c(10000,10001,10003,10005,10007,10009,10501,
  #                          10503,10505,10507,10509)) %>%
  #filter(participant %in% c(10002,10004,10006,10008,10010,
  #                          10502,10504,10506,10508,10510)) %>%
  group_by(block, participant) %>%
  filter(block == 1.0 || block == 8.0) %>%
  filter(rt >= 150) %>%
  summarise(mean = mean(rt)) %>%
  mutate(covid = ifelse(participant %in% c(10001,10003,10006,10009,10502,10504,10507, 10509, 15510, 15001), "had_covid", "no_covid")) %>%
  pivot_wider(names_from = "block", values_from = mean) %>%
  mutate(difference = `8` - `1`)


plot_covid_rt <- ggplot(summary_covid_rt, aes(x = as.factor(covid), y = as.numeric(difference), fill = covid), size = 0.1) +
  geom_violin(alpha = 0.4, adjust  = 3) +
  geom_boxplot(alpha = 0.5, varwidth = TRUE, width = 0.4, outlier.shape = NA) +
  ggtitle("Reaction time change between block 8 and 1 based on recent Covid (ms)") +
  xlab("Covid Group") +
  ylab("Reaction time change (ms)") +
  scale_color_viridis(discrete = TRUE, option = "D") +
  theme(legend.position = 0) +
  theme_minimal() +
  scale_fill_viridis_d(limits=rev) +
  scale_x_discrete(labels=c("had_covid" = "Recent Covid (n = 8)", "no_covid" = "No Covid (n = 13)"), limits=rev) +
  theme(legend.position = "none", text=element_text(size=18,  family="Times New Roman"), axis.text=element_text(size=16))

plot_covid_rt
ggsave(plot_covid_rt, filename = 'plots/covid_rt.png', dpi = 1200, type = 'cairo',
       width = 10, height = 4.5, units = 'in')


summary_covid_vas <- read_excel("demographic.xlsx") %>%
  filter(ID < 25000) %>%
  select(ID, `recent_covid`, `dif-vas`)

plot_covid_vas <- ggplot(summary_covid_vas, aes(x = as.factor(recent_covid), y = as.numeric( `dif-vas`), fill = recent_covid), size = 0.1) +
  geom_violin(alpha = 0.4, adjust  = 0.8) +
  geom_boxplot(alpha = 0.5, varwidth = TRUE, width = 0.4) +
  ggtitle("Change in subjective fatigue based on recent Covid") +
  xlab("Covid Group") +
  ylab("Difference in VAS-F Scores before and after task") +
  scale_color_viridis(discrete = TRUE, option = "D") +
  theme(legend.position = 0) +
  theme_minimal() +
  scale_fill_viridis_d() +
  scale_x_discrete(labels=c("yes" = "Recent Covid (n = 10)", "no" = "No Covid (n = 24)")) +
  theme(legend.position = "none", text=element_text(size=18,  family="Times New Roman"), axis.text=element_text(size=16))

plot_covid_vas
ggsave(plot_covid_vas, filename = 'plots/covid_vas.png', dpi = 1200, type = 'cairo',
       width = 10, height = 4.5, units = 'in')

