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
  bi_scale_fill(pal = "DkBlue", dim = 3)  +
  bi_theme() +
  theme(legend.position = "none")

maps <- plot_grid(map_biv_patt1, map_biv_patt2, map_biv_patt2_color, ncol = 3)

maps

ggsave("maps.png", width = 60, height = 20, dpi = 72, units = "cm", device = "png")

# The last map with legends (a rough one)
map_biv_patt2_color <- ggplot(data20) +
  geom_sf_pattern(aes(pattern = bi_class, pattern_fill = bi_class)) +
  geom_sf(mapping = aes(fill = bi_class, alpha = 0.1), show.legend = FALSE) +
  bi_scale_fill(pal = "DkBlue", dim = 3)  +
  theme_void() 

legend <- bi_legend(pal = "DkBlue",
                    dim = 3,
                    xlab = "Higher % White ",
                    ylab = "Higher Income ",
                    size = 8)

comboPlot <- ggdraw() +
  draw_plot(map_biv_patt2_color, 0, 0, 1, 1) +
  draw_plot(legend, 0.75, 0.2, 0.2, 0.2)

comboPlot

ggsave("comboPlot.png", width = 30, height = 30, dpi = 72, units = "cm", device = "png")

