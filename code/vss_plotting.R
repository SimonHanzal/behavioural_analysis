path <- r"(C:\Users\simonha\OneDrive - University of Glasgow\research\data\exp1_data\compilation)"
setwd(path)
library(tidyverse)
library(ggpubr)
library(Cairo)
CairoWin()



p <- ggplot(vas_anova, aes(x = as.factor(timepoint), y = as.numeric(vas), fill = as.factor(age_group))) +
    geom_split_violin(alpha = 0.8, adjust  = 0.8, color="#7F7F7F") +
    #geom_point(aes(colour = as.factor(ID)),shape = 4, alpha = 0.5) +
    geom_line(aes(colour = as.factor(ID)), group= 1, alpha = 0.6) +
    geom_boxplot(alpha = 0.7, varwidth = TRUE, width = 0.45, outlier.shape = NA, coef = 0, color="#7F7F7F") +
    labs(x = "Time", y = "Total score", fill = "Age group") +
    theme_minimal() +
    scale_color_viridis_d(option = "B", guide="none") +
    scale_fill_viridis_d(option = "B", begin = 0.25, end = 0.9, labels=c("0" = "young", "1" = "older")) +
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
set.seed(1)
p
png(filename="figures/vas.png", width = 1600, height = 800, units = "px", pointsize = 12, bg = "transparent", type="cairo")
print(p)
dev.off()

p <- ggplot(wan_anova, aes(x = as.factor(timepoint), y = as.numeric(wan), fill = as.factor(age_group))) +
    geom_split_violin(alpha = 0.8, adjust  = 0.8, color="#7F7F7F") +
    #geom_point(aes(colour = as.factor(ID)),shape = 4, alpha = 0.5) +
    geom_line(aes(colour = as.factor(ID)), group= 1, alpha = 0.6) +
    geom_boxplot(alpha = 0.7, varwidth = TRUE, width = 0.45, outlier.shape = NA, coef = 0, color="#7F7F7F") +
    labs(x = "Time", y = "Total score", fill = "Age group") +
    theme_minimal() +
    scale_color_viridis_d(option = "B", guide="none") +
    scale_fill_viridis_d(option = "B", begin = 0.25, end = 0.9, labels=c("0" = "young", "1" = "older")) +
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
set.seed(1)
p
png(filename="figures/wan.png", width = 1600, height = 800, units = "px", pointsize = 12, bg = "transparent", type="cairo")
print(p)
dev.off()

# Behaviour----

## Position----

rt_natural_plot <- rt_natural %>%
    mutate(block = as.character(block)) %>%
    group_by(block, age_group) %>%
    summarise(low = mean(rt), high = mean(rt))

rt_natural_m_plot <- rt_natural_m %>%
    mutate(block = "motivation") %>%
    group_by(block, age_group, motivation) %>%
    summarise(rt = mean(rt)) %>%
    pivot_wider(names_from = motivation, values_from = rt)

rt_plot <- rt_natural_plot %>%
    full_join(rt_natural_m_plot)


p <- ggplot(rt_plot, aes(x = as.factor(block), fill = as.factor(age_group), colour =age_group, shape =age_group, group=age_group)) +
    geom_point(aes(y = as.numeric(low)), shape = "circle", size = 4) +
    geom_line(aes(y = as.numeric(low)), size = 2) +
    geom_line(aes(y = as.numeric(high)), linetype = "dashed", size = 2)
p

plot_rt_young <- ggplot(filter(rt_natural, age_group=="young"), aes(x = as.factor(block), y = as.numeric(rt), fill = as.factor(block))) +
    geom_violin(alpha = 0.4, adjust  = 0.8) +
    geom_point(aes(colour = as.factor(participant)),shape = 4, alpha = 0.5) +
    geom_line(aes(colour = as.factor(participant)), group= 1, alpha = 0.5)