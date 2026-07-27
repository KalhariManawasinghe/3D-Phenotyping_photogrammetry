
library(readr)
library(ggplot2)
library(ggpmisc)
library(ragg)



data <- read_csv("Desktop/UOW/CAF vs Photogrammetry.csv")


data$Genotype <- factor(
  data$Genotype,
  levels = c(
    "CDC Teal",
    "Alsen",
    "Gladius",
    "Chara",
    "Kukri"
  )
)


data$CAF <- as.numeric(data$CAF)
data$CAG <- as.numeric(data$CAG)


data <- na.omit(data)


cols <- c(
  "CDC Teal" = "#7570B3",
  "Alsen" = "#D95F02",
  "Gladius" = "#E7298A",
  "Chara" = "#66A61E",
  "Kukri" = "#A6761D"
)



shapes <- c(
  "CDC Teal" = 1,
  "Alsen" = 17,
  "Gladius" = 15,
  "Chara" = 18,
  "Kukri" = 8
)


model <- lm(CAG ~ CAF, data = data)

summary(model)


CA_plot <-
  
  ggplot(data, aes(x = CAF, y = CAG)) +
  
  ## Genotype points
  geom_point(
    aes(
      colour = Genotype,
      shape = Genotype
    ),
    size = 4,
    stroke = 1
  ) +
  
  ## Regression line
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    colour = "black",
    fill = "grey80",
    linewidth = 1,
    se = TRUE
  ) +
  
  ## R² and P-value
  stat_poly_eq(
    aes(
      label = paste(
        after_stat(rr.label),
        after_stat(p.value.label),
        sep = "~~~~"
      )
    ),
    formula = y ~ x,
    parse = TRUE,
    label.x = "right",
    label.y = "top",
    size = 5
  ) +
  
  scale_colour_manual(values = cols) +
  
  scale_shape_manual(values = shapes) +
  
  coord_fixed(
    xlim = c(1, 10),
    ylim = c(1, 10)
  ) +
  
  scale_x_continuous(
    breaks = 1:10
  ) +
  
  scale_y_continuous(
    breaks = 1:10
  ) +
  
  labs(
    x = "Field canopy architecture score",
    y = "Photogrammetry canopy architecture score",
    colour = "Genotype",
    shape = "Genotype"
  ) +
  
  theme_classic(base_size = 14) +
  
  theme(
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.8
    ),
    legend.position = "right",
    axis.text = element_text(colour = "black")
  )


CA_plot


ggsave(
  "~/Desktop/Figure_CAF_vs_CAG.tiff",
  CA_plot,
  device = ragg::agg_tiff,
  width = 18.2,
  height = 12,
  units = "cm",
  dpi = 600,
  compression = "lzw"
)
