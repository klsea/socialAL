# Presentation figs
# 1.6.22 KLS

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
p1 <- readRDS(here::here('figs', 'bot.RDS'))
p2 <- readRDS(here::here('figs', 'decay.RDS'))
p3 <- readRDS(here::here('figs', 'tpj.RDS'))
tpj <- image_read(here::here('figs', 'feedback_rpe_YAvOA_p005k50 copy.png'))
p4 <- ggdraw() + draw_image(tpj, scale = 1.1) 

# graph constants
lg = 18 # text size
sm = 14
custom_plot = list(theme(
  plot.title = element_text(size = 24),
  axis.title.x = element_text(size = lg), axis.text.x = element_text(size = sm),
  axis.title.y = element_text(size = lg), axis.text.y = element_text(size = sm), 
  legend.title = element_text(size = lg), legend.text = element_text(size = sm))
)

# twitter fig 2 ####
p1

# save
ggsave(here::here('figs', 'presentfig1.pdf'), p1, width = 7, height = 4)

# twitter fig 2 ####
p2

# save
ggsave(here::here('figs', 'presentfig2.pdf'), p2, width = 5, height = 5)

# twitter fig 2 ####

fig <- plot_grid(p4,  p3 + custom_plot + scale_x_discrete(labels =addline_format(c('Older Adults', 'Younger Adults'))) + theme(legend.position = 'none'), 
                 ncol = 2, scale = .9, rel_widths = c(2, 1))
fig

# save
ggsave(here::here('figs', 'presentfig3.pdf'), fig, width = 7, height = 4)
