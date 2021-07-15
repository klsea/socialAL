# Figure 1
# 7.15.21 KLS

# load required packages ####
library(here)
library(tidyverse)
library(ggplot2)
library(cowplot)

# load source functions
source(here::here('scr', 'multiplot.R'))

# set hard-coded variables

# load data
p1 <- readRDS(here::here('figs', 'bbg.RDS'))
p2 <- readRDS(here::here('figs', 'per.RDS'))
p3 <- readRDS(here::here('figs', 'bot.RDS'))
p4 <- readRDS(here::here('figs', 'decay.RDS'))

# make figure
multiplot(p1 + theme(legend.position = 'None'), 
          p3, 
          p2 + theme(legend.position = 'None'), 
          p4,
          cols=2)

# cowplot v1
ggdraw() + 
  draw_plot(p1 + theme(legend.position = 'none'), x = 0, y = 0.5, width = .5, height = .5) + 
  draw_plot(p2 + theme(legend.position = 'none'), x = 0.5, y = 0.5, width = .5, height = .5) +
  draw_plot(p3, x = 0, y = 0, width = .75, height = .5) + 
  draw_plot(p4 + theme(legend.position = 'none'), x = 0.75, y = 0, width = .25, height = .5) + 
  draw_plot_label(label = c("A", "B", "C", "D"), size = 15, x = c(0, 0.5, 0, 0.75), y = c(1, 1, 0.5, 0.5))

# cowplot v2
legend <- get_legend(p1 + theme(legend.position = 'top'))
prow <- plot_grid(
  p1 + theme(legend.position = 'none'),
  p2 + theme(legend.position = 'none'),
  p4 + theme(legend.position = 'none'),
  align = 'vh',
  labels = c("A", "B", "C"),
  hjust = -1,
  nrow = 1
)
plot_grid(legend, prow, p3, ncol = 1, rel_heights = c(.1, 1, 1), labels = c('', '', 'D'))

# cowplot v3
prow2 <- ggdraw() + 
  draw_plot(p1 + theme(legend.position = 'none'), x = 0, y = 0, width = .4, height = 1) + 
  draw_plot(p2 + theme(legend.position = 'none'), x = 0.4, y = 0, width = .4, height = 1) +
  draw_plot(p4 + theme(legend.position = 'none'), x = 0.8, y = 0, width = .2, height = 1) +
  draw_plot_label(label = c("A", "B", "C"), size = 15, x = c(0, 0.4, 0.8), y = c(1, 1, 1))
prow2
plot_grid(legend, prow2, p3 + theme(legend.position = 'top'), ncol = 1, rel_heights = c(.1, 1, 1), labels = c('', '', 'D'))

