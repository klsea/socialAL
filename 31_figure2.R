# Figure 2
# 12.16.21 KLS updated 7.27.22

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
p1 <- readRDS(here::here('figs', 'rAG.RDS'))
p3 <- readRDS(here::here('figs', 'rPHC.RDS'))

# load images
rAG <- image_read(here::here('figs', 'OA_learn_v_non_40-5028_zoom.png'))
p2 <- ggdraw() + draw_image(rAG, scale = 1.1) 
rPHC <- image_read(here::here('figs', 'OA_learn_v_non_24-20-18_zoom.png'))
p4 <- ggdraw() + draw_image(rPHC, scale = 1.1) 

# graph constants
lg = 12 # text size
sm = 10
custom_plot = list(theme(
  plot.title = element_text(size = 24),
  axis.title.x = element_text(size = lg), axis.text.x = element_text(size = sm),
  axis.title.y = element_text(size = lg), axis.text.y = element_text(size = sm),
  legend.position = 'None'
  #legend.title = element_text(size = lg), legend.text = element_text(size = sm))
  ))

# create plot
fig2 <- plot_grid(p2, p1 + custom_plot + scale_x_discrete(labels =addline_format(c('OA Learners', 'OA Non-learners'))),
                  p4,  p3 + custom_plot + scale_x_discrete(labels =addline_format(c('OA Learners', 'OA Non-learners'))), 
                  ncol = 2, labels = c('a', '', 'b',''), scale = 1)
fig2
# save
save_plot(here::here('figs', 'fig2.pdf'), fig2, base_height = 7, base_width = 7)
