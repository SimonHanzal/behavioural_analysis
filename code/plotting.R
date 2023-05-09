# Simon Hanzal (c)

# Setup----
# Modify path as needed depending on where the files are stored in your device
#path <- r"(C:\Users\hanza\OneDrive\Dokumenty\R)"
path <- r"(C:\Users\simonha\OneDrive - University of Glasgow\research\data\exp1_data\compilation)"
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
library(Cairo)
CairoWin()

GeomSplitViolin <- ggplot2::ggproto(
    "GeomSplitViolin",
    ggplot2::GeomViolin,
    draw_group = function(self,
                          data,
                          ...,
                          # add the nudge here
                          nudge = 0,
                          draw_quantiles = NULL) {
        data <- transform(data,
                          xminv = x - violinwidth * (x - xmin),
                          xmaxv = x + violinwidth * (xmax - x))
        grp <- data[1, "group"]
        newdata <- plyr::arrange(transform(data,
                                           x = if (grp %% 2 == 1) xminv else xmaxv),
                                 if (grp %% 2 == 1) y else -y)
        newdata <- rbind(newdata[1, ],
                         newdata,
                         newdata[nrow(newdata), ],
                         newdata[1, ])
        newdata[c(1, nrow(newdata)-1, nrow(newdata)), "x"] <- round(newdata[1, "x"])
        
        # now nudge them apart
        newdata$x <- ifelse(newdata$group %% 2 == 1,
                            newdata$x - nudge,
                            newdata$x + nudge)
        
        if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
            
            stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <= 1))
            
            quantiles <- ggplot2:::create_quantile_segment_frame(data,
                                                                 draw_quantiles)
            aesthetics <- data[rep(1, nrow(quantiles)),
                               setdiff(names(data), c("x", "y")),
                               drop = FALSE]
            aesthetics$alpha <- rep(1, nrow(quantiles))
            both <- cbind(quantiles, aesthetics)
            quantile_grob <- ggplot2::GeomPath$draw_panel(both, ...)
            ggplot2:::ggname("geom_split_violin",
                             grid::grobTree(ggplot2::GeomPolygon$draw_panel(newdata, ...),
                                            quantile_grob))
        }
        else {
            ggplot2:::ggname("geom_split_violin",
                             ggplot2::GeomPolygon$draw_panel(newdata, ...))
        }
    }
)

geom_split_violin <- function(mapping = NULL,
                              data = NULL,
                              stat = "ydensity",
                              position = "identity",
                              # nudge param here
                              nudge = 0,
                              ...,
                              draw_quantiles = NULL,
                              trim = TRUE,
                              scale = "area",
                              na.rm = FALSE,
                              show.legend = NA,
                              inherit.aes = TRUE) {
    
    ggplot2::layer(data = data,
                   mapping = mapping,
                   stat = stat,
                   geom = GeomSplitViolin,
                   position = position,
                   show.legend = show.legend,
                   inherit.aes = inherit.aes,
                   params = list(trim = trim,
                                 scale = scale,
                                 # don't forget the nudge
                                 nudge = nudge,
                                 draw_quantiles = draw_quantiles,
                                 na.rm = na.rm,
                                 ...))
}



# path = r"(C:\Users\hanza\OneDrive\Dokumenty\R\plots)"
path <- r"(C:\Users\simonha\OneDrive - University of Glasgow\research\data\exp1_data\compilation\newer_plots)"

# Depictions----

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

# RT----

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
#ggsave(plot_rt_young, filename = 'rt_young.png', path = path, dpi = 1200, type = 'cairo', width = 10, height = 4.5, units = 'in', bg = 'white')
#ggsave(plot_rt_older, filename = 'rt_older.png', path = path, dpi = 1200, type = 'cairo', width = 10, height = 4.5, units = 'in', bg = 'white')

# Error----

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

#ggsave(plot_nogo_young, filename = 'nogo_young.png', path = path, dpi = 1200, type = 'cairo', width = 10, height = 4.5, units = 'in', bg = 'white')
#ggsave(plot_nogo_older, filename = 'nogo_older.png', path = path, dpi = 1200, type = 'cairo', width = 10, height = 4.5, units = 'in', bg = 'white')


# Alpha----

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
#ggsave(plot_alpha_young, filename = 'alpha_young.png', path = path, dpi = 1200, type = 'cairo', width = 10, height = 4.5, units = 'in', bg = 'white')
#ggsave(plot_alpha_older, filename = 'alpha_older.png', path = path, dpi = 1200, type = 'cairo', width = 10, height = 4.5, units = 'in', bg = 'white')

# Beta----

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
#ggsave(plot_beta_young, filename = 'newer_plots/beta_young.png', path = path, dpi = 1200, type = 'cairo', width = 10, height = 4.5, units = 'in', bg = 'white')
#ggsave(plot_beta_older, filename = 'newer_plots/beta_older.png', path = path, dpi = 1200, type = 'cairo', width = 10, height = 4.5, units = 'in', bg = 'white')

# VAS----

plot_vas_young <- ggplot(filter(vas_anova, ID <= 10600), aes(x = as.factor(timepoint), y = as.numeric(vas), fill = as.factor(timepoint))) +
  geom_violin(alpha = 0.4, adjust  = 0.8) +
  geom_point(aes(colour = as.factor(ID)),shape = 4, alpha = 0.5) +
  geom_line(aes(colour = as.factor(ID)), group= 1, alpha = 0.5) +
  geom_boxplot(alpha = 0.2, varwidth = TRUE, width = 0.45, outlier.shape = NA, coef = 0) +
  ggtitle("Young - Subjective Fatigue change") +
  xlab("Time") +
  ylab("Total Score") +
  theme_minimal() +
  scale_color_viridis(discrete = TRUE, option = "D") +
  scale_fill_viridis_d(option = "D", begin = 0, end = 1) +
  scale_x_discrete(labels=c("1" = "Pre-task", "0" = "Post-task"), limits=rev) +
  theme(legend.position = 0, text=element_text(size=18,  family="Times New Roman"), axis.text=element_text(size=16))

plot_vas_older <- ggplot(filter(vas_anova, ID >= 10600), aes(x = as.factor(timepoint), y = as.numeric(vas), fill = as.factor(timepoint))) +
  geom_violin(alpha = 0.4, adjust  = 0.8) +
  geom_point(aes(colour = as.factor(ID)),shape = 4, alpha = 0.5) +
  geom_line(aes(colour = as.factor(ID)), group= 1, alpha = 0.5) +
  geom_boxplot(alpha = 0.2, varwidth = TRUE, width = 0.45, outlier.shape = NA, coef = 0) +
  ggtitle("Older - Subjective Fatigue change") +
  xlab("Time") +
  ylab("Total Score") +
  theme_minimal() +
  scale_color_viridis(discrete = TRUE, option = "D") +
  scale_fill_viridis_d(option = "D", begin = 0, end = 1) +
  scale_x_discrete(labels=c("1" = "Pre-task", "0" = "Post-task"), limits=rev) +
  theme(legend.position = 0, text=element_text(size=18,  family="Times New Roman"), axis.text=element_text(size=16))

p <- ggplot(filter(vas_anova), aes(x = as.factor(timepoint), y = as.numeric(vas), fill = as.factor(age_group))) +
    geom_split_violin(alpha = 0.8, adjust  = 0.8, color="#7F7F7F") +
    #geom_point(aes(colour = as.factor(ID)),shape = 4, alpha = 0.5) +
    geom_line(aes(colour = as.factor(ID)), group= 1, alpha = 0.6) +
    geom_boxplot(alpha = 0.7, varwidth = TRUE, width = 0.45, outlier.shape = NA, coef = 0, color="#7F7F7F") +
    labs(x = "Time", y = "Total score", fill = "Age group") +
    theme_minimal() +
    scale_color_viridis_d(option = "B", guide="none") +
    scale_fill_viridis_d(option = "B", begin = 0.2, end = 0.9, labels=c("0" = "young", "1" = "older")) +
    scale_x_discrete(labels=c("0" = "Pre-task", "1" = "Post-task")) +
    theme(text=element_text(size=18), axis.text=element_text(size=16),
    panel.background = element_rect(fill='transparent', color=NA),
    plot.background = element_rect(fill='transparent', color=NA),
    legend.background = element_rect(fill='transparent', color=NA),
    legend.box.background = element_rect(fill='transparent', color=NA),
    axis.text.x = element_text(colour = "#7F7F7F", size = 48),
    axis.text.y = element_text(colour = "#7F7F7F", size = 48),
    legend.text = element_text(colour = "#7F7F7F", size = 48),
    axis.title.x = element_text(colour = "#7F7F7F", size = 64),
    axis.title.y = element_text(colour = "#7F7F7F", size = 64),
    legend.title = element_text(colour = "#7F7F7F", size = 48),
    legend.position="bottom"
    )

plot_vas_young
plot_vas_older
#path = r"(C:\Users\hanza\OneDrive\Dokumenty\R\plots)"
path <- r"(C:\Users\simonha\OneDrive - University of Glasgow\research\data\exp1_data\compilation)"
#ggsave(plot_vas_young, filename = 'vas_young.png', path = path, dpi = 1200, type = 'cairo', width = 10, height = 4.5, units = 'in', bg = 'white')
#ggsave(plot_vas_older, filename = 'vas_older.png', path = path, dpi = 1200, type = 'cairo', width = 10, height = 4.5, units = 'in', bg = 'white')

# WAN----

plot_wan_young <- ggplot(filter(wan_anova, ID <= 10600), aes(x = as.factor(timepoint), y = as.numeric(wan), fill = as.factor(timepoint))) +
  geom_violin(alpha = 0.4, adjust  = 0.8) +
  geom_point(aes(colour = as.factor(ID)),shape = 4, alpha = 0.5) +
  geom_line(aes(colour = as.factor(ID)), group= 1, alpha = 0.5) +
  geom_boxplot(alpha = 0.2, varwidth = TRUE, width = 0.45, outlier.shape = NA, coef = 0) +
  ggtitle("Young - Mind-wandering change") +
  xlab("Time") +
  ylab("Total Score") +
  theme_minimal() +
  scale_color_viridis(discrete = TRUE, option = "D") +
  scale_fill_viridis_d(option = "D", begin = 0, end = 1) +
  scale_x_discrete(labels=c("0" = "Pre-task", "1" = "Post-task")) +
  theme(legend.position = 0, text=element_text(size=18,  family="Times New Roman"), axis.text=element_text(size=16))

plot_wan_older <- ggplot(filter(wan_anova, ID >= 10600), aes(x = as.factor(timepoint), y = as.numeric(wan), fill = as.factor(timepoint))) +
  geom_violin(alpha = 0.4, adjust  = 0.8) +
  geom_point(aes(colour = as.factor(ID)),shape = 4, alpha = 0.5) +
  geom_line(aes(colour = as.factor(ID)), group= 1, alpha = 0.5) +
  geom_boxplot(alpha = 0.2, varwidth = TRUE, width = 0.45, outlier.shape = NA, coef = 0) +
  ggtitle("Older - Mind-wandering change") +
  xlab("Time") +
  ylab("Total Score") +
  theme_minimal() +
  scale_color_viridis(discrete = TRUE, option = "D") +
  scale_fill_viridis_d(option = "D", begin = 0, end = 1) +
  scale_x_discrete(labels=c("0" = "Pre-task", "1" = "Post-task")) +
  theme(legend.position = 0, text=element_text(size=18,  family="Times New Roman"), axis.text=element_text(size=16))

plot_wan <- ggplot(wan_anova, aes(x = as.factor(timepoint), y = as.numeric(wan), fill = as.factor(age_group))) +
  #geom_violin(alpha = 0.4, adjust  = 0.8, width=1, position = "dodge") +
  #geom_point(aes(colour = as.factor(ID)),shape = 4, alpha = 0.5) +
  #geom_line(aes(colour = as.factor(ID)), group= 1, alpha = 0.5) +
  geom_boxplot(alpha = 0.2, varwidth = TRUE, width = 0.45, width=1, position = "dodge") +
  ggtitle("Older - Mind-wandering change") +
  xlab("Time") +
  ylab("Total Score") +
  theme_minimal() +
  scale_color_viridis(discrete = TRUE, option = "D") +
  scale_fill_viridis_d(option = "D", begin = 0, end = 1) +
  scale_x_discrete(labels=c("0" = "Pre-task", "1" = "Post-task")) +
  theme(legend.position = 0, text=element_text(size=18,  family="Times New Roman"), axis.text=element_text(size=16))

plot_wan
plot_wan_young
plot_wan_older
#path = r"(C:\Users\hanza\OneDrive\Dokumenty\R\plots)"
path <- r"(C:\Users\simonha\OneDrive - University of Glasgow\research\data\exp1_data\compilation)"
ggsave(plot_wan_young, filename = 'wan_young.png', path = path, dpi = 1200, type = 'cairo', width = 10, height = 4.5, units = 'in', bg = 'white')
ggsave(plot_wan_older, filename = 'wan_older.png', path = path, dpi = 1200, type = 'cairo', width = 10, height = 4.5, units = 'in', bg = 'white')

# Links----

alpha_vas <- inner_join(alpha_change, demo_clean, by="ID")

model <- lm(`dif-wan` ~ alpha, alpha_vas)
summary(model)

ggplot(alpha_vas, aes(alpha, `dif-wan`)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE)

vas_change_summary <- vas_change %>%
  mutate(age_group = ifelse(participant > 10600, 1, 0)) %>%
  group_by(age_group) %>%
  summarise(mean = mean(vas_change), sd = sd(vas_change), min = min(vas_change), max = max(vas_change))

wan_addition_summary <- wan_addition %>%
  mutate(age_group = ifelse(participant > 10600, 1, 0)) %>%
  group_by(age_group) %>%
  summarise(mean = mean(wan_change), sd = sd(wan_change), min = min(wan_change), max = max(wan_change))

vas_summary <- vas_anova %>%
  group_by(age_group, timepoint) %>%
  summarise(mean = mean(vas), sd = sd(vas), min = min(vas), max = max(vas))
  
wan_summary <- wan_anova %>%
    group_by(age_group, timepoint) %>%
  summarise(mean = mean(wan), sd = sd(wan), min = min(wan), max = max(wan))

all_subjective_anova <- vas_change %>%
  mutate(participant = as.character(participant)) %>%
  inner_join(wan_addition, by = c("participant"))

model <- lm(vas_change ~ wan_change, all_subjective_anova)
summary(model)

# Motivation----
## RT----

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

## Error----

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

## IE----

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

