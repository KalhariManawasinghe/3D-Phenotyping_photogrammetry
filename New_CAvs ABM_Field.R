#########################################################
# Packages
#########################################################

library(readr)
library(ggplot2)
library(ggpmisc)
library(ragg)

#########################################################
# Load data
#########################################################

data <- read_csv("Desktop/UOW/CAFvsAGBMF.csv")

#########################################################
# Factor order
#########################################################

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

#########################################################
# Convert variables
#########################################################

data$CAF   <- as.numeric(data$CAF)
data$AGBMF <- as.numeric(data$AGBMF)

#########################################################
# Remove missing values
#########################################################

data <- na.omit(
  data[, c("Genotype","CAF","AGBMF")]
)

#########################################################
# Check data range
#########################################################

summary(data$AGBMF)

#########################################################
# Colours
#########################################################

cols <- c(
  "CDC Teal" = "#7570B3",
  "Alsen"    = "#D95F02",
  "Gladius"  = "#E7298A",
  "Chara"    = "#66A61E",
  "Kukri"    = "#A6761D"
)

#########################################################
# Symbols
#########################################################

shapes <- c(
  "CDC Teal" = 1,
  "Alsen"    = 17,
  "Gladius"  = 15,
  "Chara"    = 18,
  "Kukri"    = 8
)

#########################################################
# Linear regression
#########################################################

model <- lm(AGBMF ~ CAF, data = data)

summary(model)

#########################################################
# Residual normality
#########################################################

shapiro.test(residuals(model))

#########################################################
# Plot
#########################################################

AGBM_plot <- ggplot(
  data,
  aes(x = CAF, y = AGBMF)
) +
  
  geom_point(
    aes(
      colour = Genotype,
      shape = Genotype
    ),
    size = 3.5,
    stroke = 1
  ) +
  
  geom_smooth(
    aes(group = 1),
    method = "lm",
    formula = y ~ x,
    colour = "black",
    fill = "grey80",
    linewidth = 1,
    se = TRUE
  ) +
  
  stat_poly_eq(
    aes(
      label = paste(
        after_stat(rr.label),
        after_stat(p.value.label),
        sep = "~~~~"
      ),
      group = 1
    ),
    formula = y ~ x,
    parse = TRUE,
    label.x = "right",
    label.y ="top",
    size = 5
  ) +
  
  scale_colour_manual(values = cols) +
  
  scale_shape_manual(values = shapes) +
  
  scale_x_continuous(
    limits = c(1,10),
    breaks = 1:10
  ) +
  
  scale_y_continuous(
    breaks = seq(10,30,5),
    expand = c(0,0)
  ) +
  
  coord_cartesian(
    ylim = c(10,30)
  ) +
  
  labs(
    x = "Canopy architecture score (Field)",
    y = expression("Above-ground biomass (" * g ~ plant^{-1} * ")"),
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

#########################################################
# Display
#########################################################

AGBM_plot

#########################################################
# Save figure
#########################################################

ggsave(
  "~/Desktop/Figure_CAF_vs_AGBMF.tiff",
  AGBM_plot,
  device = ragg::agg_tiff,
  width = 16,
  height = 12,
  units = "cm",
  dpi = 600,
  compression = "lzw"
)

