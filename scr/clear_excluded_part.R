library(here)

dt <- read.csv(here('data', 'socialAL_clean_data.csv'))
cut <- read.csv(here('output', 'socialAL_cut.csv'), header =FALSE)
colnames(cut) <- 'id'
cut[nrow(cut) + 1,] = 'sub-2040'

# clean baseline
bl <- read.csv(here('output', 'baseline_model_params.csv'))
new_bl <- bl[!(bl$id %in% cut$id),]
write.csv(new_bl, here('output', 'baseline_model_params.csv'), row.names = FALSE)

# clean single alpha
a1 <- read.csv(here('output', 'single_alpha_model_params.csv'))
new_a1 <- a1[!(a1$id %in% cut$id),]
write.csv(new_a1, here('output', 'single_alpha_model_params.csv'), row.names = FALSE)

# clean two alpha
a2 <- read.csv(here('output', 'two_alpha_model_params.csv'))
new_a2 <- a2[!(a2$id %in% cut$id),]
write.csv(new_a2, here('output', 'two_alpha_model_params.csv'), row.names = FALSE)

# clean single alpha with decay
a1d <- read.csv(here('output', 'single_alpha_with_decay_model_params.csv'))
new_a1d <- a1d[!(a1d$id %in% cut$id),]
write.csv(new_a1d, here('output', 'single_alpha_with_decay_model_params.csv'), row.names = FALSE)
