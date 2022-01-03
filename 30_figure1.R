# Figure 1
# 7.15.21 KLS

# load required packages ####
library(here)
library(tidyverse)
library(ggplot2)
library(cowplot)

# load source functions
#source(here::here('scr', 'multiplot.R'))

# set hard-coded variables

# load data
p1 <- readRDS(here::here('figs', 'bbg.RDS'))
p2 <- readRDS(here::here('figs', 'per.RDS'))
p3 <- readRDS(here::here('figs', 'bot.RDS'))
p4 <- readRDS(here::here('figs', 'decay.RDS'))

# graph constants
lg = 12 # text size
sm = 9
custom_plot = list(theme(
  plot.title = element_text(size = 24),
  axis.title.x = element_text(size = lg), axis.text.x = element_text(size = sm),
  axis.title.y = element_text(size = lg), axis.text.y = element_text(size = sm), 
  legend.title = element_text(size = lg), legend.text = element_text(size = sm))
)

# cowplot v3
legend <- get_legend(p1 + theme(legend.position = 'top') + custom_plot)
prow1 <- ggdraw() + 
  draw_plot(p1 + theme(legend.position = 'none')+ custom_plot, x = 0, y = 0, width = .37, height = 1) + 
  draw_plot(p4 + theme(legend.position = 'none')+ custom_plot, x = 0.375, y = 0, width = .25, height = 1) +
  draw_plot(p2 + theme(legend.position = 'none')+ custom_plot, x = 0.625, y = 0, width = .37, height = 1) +
  draw_plot_label(label = c("A", "C", "D"), size = 15, x = c(0, 0.375, 0.625), y = c(1, 1, 1))
prow1
prow2 <- ggdraw() + 
  draw_plot(p3 + custom_plot + theme(legend.position = 'top', strip.text.x = element_text(size=lg)), x = 0.05, y = 0, width = .9, height = 1) + 
  draw_plot_label(label = c("B"), size = 15, x = 0, y = 1)
prow2

#fig1 <- plot_grid(legend, prow1, p3 + theme(legend.position = 'top') + custom_plot, ncol = 1, rel_heights = c(.2, 1, 1.5), labels = c('', '', 'D'))
fig1 <- plot_grid(legend, prow1, prow2 , ncol = 1, rel_heights = c(.2, 1, 1.5))

fig1

# save
save_plot(here::here('figs', 'fig1.pdf'), fig1, base_height = 7, base_width = 7)



