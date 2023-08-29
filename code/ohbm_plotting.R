library(here)
library(tidyverse)
library(ggpubr)
library(Cairo)
CairoWin()
# Functions----
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



# Subjective----

## VAS----

p <- ggplot(vas_anova, aes(x = as.factor(timepoint), y = as.numeric(vas), fill = as.factor(age_group))) +
    geom_split_violin(alpha = 0.8, adjust  = 0.8, color="#7F7F7F") +
    #geom_point(aes(colour = as.factor(ID)),shape = 4, alpha = 0.5) +
    geom_line(aes(colour = as.factor(ID)), group= 1, alpha = 0.6) +
    geom_boxplot(alpha = 0.7, varwidth = TRUE, width = 0.45, outlier.shape = NA, coef = 0, color="#7F7F7F") +
    labs(x = "Time", y = "Total score", fill = "Age group") +
    theme_minimal() +
    scale_color_viridis_d(option = "B", guide="none") +
    scale_fill_viridis_d(option = "B", begin = 0.25, end = 0.9, labels=c("0" = "young", "1" = "older")) +
    scale_x_discrete(labels=c("1" = "Pre-task", "0" = "Post-task"), limits=rev) +
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
png(filename="figures/vas.png", width = 1600, height = 800, units = "px", pointsize = 12, bg = "#262626", type="cairo")
print(p)
dev.off()

## WAN----

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
png(filename="figures/wan.png", width = 1600, height = 800, units = "px", pointsize = 12, bg = "#262626", type="cairo")
print(p)
dev.off()

# Behaviour----

## RT----

rt_natural_plot <- rt_natural %>%
    mutate(block = as.character(block)) %>%
    group_by(block, age_group) %>%
    summarise(low = mean(rt), high = mean(rt))

rt_natural_m_plot <- rt_natural_m %>%
    mutate(block = "M") %>%
    group_by(block, age_group, motivation) %>%
    summarise(rt = mean(rt)) %>%
    pivot_wider(names_from = motivation, values_from = rt)

rt_plot <- rt_natural_plot %>%
    full_join(rt_natural_m_plot)


p <- ggplot(rt_plot, aes(x = as.factor(block), fill = as.factor(age_group), colour =age_group, shape =age_group, group=age_group)) +
    geom_point(aes(y = as.numeric(low)), shape = "circle", size = 4) +
    geom_line(aes(y = as.numeric(low)), size = 2.3) +
    geom_line(aes(y = as.numeric(high)), linetype = "longdash", size = 2.3) +
    scale_colour_viridis_d(option = "B", begin = 0.25, end = 0.9, direction = -1) +
    scale_fill_viridis_d(option = "B", begin = 0.25, end = 0.9, direction = -1) +
    theme_minimal() +
    theme(legend.position = "none",
      axis.text.x = element_text(colour = "black", size = 28),
      axis.text.y = element_text(colour = "black", size = 28),
      axis.title.x = element_text(colour = "black", size = 38),
      axis.title.y = element_text(colour = "black", size = 38)) +
    #geom_vline(xintercept=8.5, color = "black", size=1) +
    labs(x = "Block", y = "RT (ms)", fill)
p
png(filename="figures/rt.png", width = 600, height = 600, units = "px", pointsize = 12, bg = "white", type="cairo")
print(p)
dev.off()

## ERR----

err_plot <- err %>%
    mutate(block = as.character(block), age_group = ifelse(participant > 10600, "older", "young")) %>%
    group_by(block, age_group) %>%
    summarise(low = mean(err), high = mean(err))

err_m_plot <- err_m %>%
    mutate(block = "M") %>%
    group_by(block, age_group, motivation) %>%
    summarise(err = mean(err)) %>%
    pivot_wider(names_from = motivation, values_from = err)

err_plot <- err_plot %>%
    full_join(err_m_plot)


p <- ggplot(err_plot, aes(x = as.factor(block), fill = as.factor(age_group), colour =age_group, shape =age_group, group=age_group)) +
    geom_point(aes(y = as.numeric(low)), shape = "circle", size = 4) +
    geom_line(aes(y = as.numeric(low)), size = 2.3) +
    geom_line(aes(y = as.numeric(high)), linetype = "longdash", size = 2.3) +
    scale_colour_viridis_d(option = "B", begin = 0.25, end = 0.9, direction = -1) +
    scale_fill_viridis_d(option = "B", begin = 0.25, end = 0.9, direction = -1) +
    theme_minimal() +
    theme(legend.position = "none",
          axis.text.x = element_text(colour = "black", size = 28),
          axis.text.y = element_text(colour = "black", size = 28),
          axis.title.x = element_text(colour = "black", size = 38),
          axis.title.y = element_text(colour = "black", size = 38)) +
    #geom_vline(xintercept=8.5, color = "black", size=1) +
    labs(x = "Block", y = "Error (%)", fill)
p
png(filename="figures/err.png", width = 600, height = 600, units = "px", pointsize = 12, bg = "white", type="cairo")
print(p)
dev.off()

## IE----

ie_plot <- ie %>%
    mutate(block = as.character(block), age_group = ifelse(participant > 10600, "older", "young")) %>%
    group_by(block, age_group) %>%
    summarise(low = mean(ie), high = mean(ie))

ie_m_plot <- ie_m_reduced %>%
    mutate(block = "M", age_group = ifelse(participant > 10600, "older", "young")) %>%
    group_by(block, age_group, motivation) %>%
    summarise(ie = mean(ie_9)) %>%
    pivot_wider(names_from = motivation, values_from = ie)

ie_plot <- ie_plot %>%
    full_join(ie_m_plot)


p <- ggplot(ie_plot, aes(x = as.factor(block), fill = as.factor(age_group), colour =age_group, shape =age_group, group=age_group)) +
    geom_point(aes(y = as.numeric(low)), shape = "circle", size = 4) +
    geom_line(aes(y = as.numeric(low)), size = 2.3) +
    geom_line(aes(y = as.numeric(high)), linetype = "longdash", size = 2.3) +
    scale_colour_viridis_d(option = "B", begin = 0.25, end = 0.9, direction = -1) +
    scale_fill_viridis_d(option = "B", begin = 0.25, end = 0.9, direction = -1, guide="none") +
    theme_minimal() +
    theme(axis.text.x = element_text(colour = "black", size = 28),
          axis.text.y = element_text(colour = "black", size = 28),
          axis.title.x = element_text(colour = "black", size = 38),
          axis.title.y = element_text(colour = "black", size = 38),
          legend.text = element_text(colour = "black", size = 20),
          legend.title = element_text(colour = "black", size = 28)) +
    labs(x = "Block", y = "IE", colour = "Age group")
p
png(filename="figures/ie.png", width = 800, height = 600, units = "px", pointsize = 12, bg = "white", type="cairo")
print(p)
dev.off()

