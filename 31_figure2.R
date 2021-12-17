# Figure 2
# 12.16.21 KLS

# load required packages ####
library(here)
library(tidyverse)
library(ggplot2)
library(cowplot)
library(rsvg)
library(magick)

# load source functions
addline_format <- function(x,...){
  gsub('\\s','\n',x)
}
# set hard-coded variables

# load data
p1 <- readRDS(here::here('figs', 'ifg.RDS'))
p3 <- readRDS(here::here('figs', 'tpj.RDS'))
p5 <- readRDS(here::here('figs', 'vis.RDS'))

# load images
ifg <- image_read(here::here('figs', 'OA_vs_YA_decision_MNI152_2mm copy.png'))
p2 <- ggdraw() + draw_image(ifg, scale = 1.1) 
tpj <- image_read(here::here('figs', 'feedback_rpe_YAvOA_p005k50 copy.png'))
p4 <- ggdraw() + draw_image(tpj, scale = 1.1) 
vis <- image_read(here::here('figs', 'feedback_rpe_OAvYA_p005k50_MNI152_2mm copy.png'))
p6 <- ggdraw() + draw_image(vis, scale = 1.1) 

# graph constants
lg = 12 # text size
sm = 10
custom_plot = list(theme(
  plot.title = element_text(size = 24),
  axis.title.x = element_text(size = lg), axis.text.x = element_text(size = sm),
  axis.title.y = element_text(size = lg), axis.text.y = element_text(size = sm), 
  #legend.title = element_text(size = lg), legend.text = element_text(size = sm))
  legend.position = 'none'))

# create plot
fig2 <- plot_grid(p2, p1 + custom_plot + scale_x_discrete(labels =addline_format(c('Younger Adults', 'Older Adults'))),
                  p4,  p3 + custom_plot + scale_x_discrete(labels =addline_format(c('Younger Adults', 'Older Adults'))), 
                  p6, p5 + custom_plot + scale_x_discrete(labels =addline_format(c('Younger Adults', 'Older Adults'))), 
                  ncol = 2, labels = c('a', '', 'b','', 'c', ''), scale = .9, rel_widths = c(2, 1, 1, 2, 2, 1))
fig2
# save
save_plot(here::here('figs', 'fig2.pdf'), fig2, base_height = 7, base_width = 7)
