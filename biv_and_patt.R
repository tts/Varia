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

# pattern by class
map_biv_patt1 <- ggplot(data20) +
  geom_sf_pattern(aes(pattern = bi_class)) +
  bi_theme() +
  theme(legend.position = "none")

# pattern and fill by class
map_biv_patt2 <- ggplot(data20) +
  geom_sf_pattern(aes(pattern = bi_class, pattern_fill = bi_class)) +
  bi_theme() +
  theme(legend.position = "none")

# pattern and fill by class plus biscale fill
map_biv_patt2_color <- ggplot(data20) +
  geom_sf_pattern(aes(pattern = bi_class, pattern_fill = bi_class)) +
  geom_sf(mapping = aes(fill = bi_class, alpha = 0.1)) +
  bi_scale_fill(pal = "DkBlue", dim = 3) +
  bi_theme() +
  theme(legend.position = "none")

maps <- plot_grid(map_biv_patt1, map_biv_patt2, map_biv_patt2_color, ncol = 3)

maps

ggsave("maps.png", width = 60, height = 20, dpi = 72, units = "cm", device = "png")

