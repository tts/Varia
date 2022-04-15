library(tidyverse)
library(ggpattern)
library(biscale)
library(cowplot)

# https://twitter.com/jhilden/status/1513882835937026073
# https://slu-opengis.github.io/biscale/articles/biscale.html
data <- bi_class(stl_race_income, x = pctWhite, y = medInc, style = "quantile", dim = 3)

data20 <- data %>% 
  slice_head(n = 20) %>% 
  mutate(bi_class = as.factor(bi_class))

map_biv_patt <- ggplot(data20) +
  geom_sf_pattern(aes(pattern = bi_class, pattern_fill = bi_class)) +
  bi_theme() +
  theme(legend.position = "none")

map_biv_patt_color <- ggplot(small) +
  geom_sf_pattern(aes(pattern = bi_class, pattern_fill = bi_class)) +
  geom_sf(data = small, mapping = aes(fill = bi_class, alpha = 0.1)) +
  bi_scale_fill(pal = "DkBlue", dim = 3) +
  bi_theme() +
  theme(legend.position = "none")

maps <- plot_grid(map_biv_patt, map_biv_patt_color)

maps


