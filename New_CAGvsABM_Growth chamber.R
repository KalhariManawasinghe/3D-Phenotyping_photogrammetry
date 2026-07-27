library(readr)
CAGAGBM <- read_csv("Desktop/Publication/AGBM_photogrammtry.csv")

# Assume your data is in a data frame called '14CAGAGBM'
# ConLAert Genotype to a factor (if it's not already)
CAGAGBM$Genotype <- as.factor(CAGAGBM$Genotype)

# Fit linear regression model
model <- lm(AGBM ~ CAG, data = CAGAGBM)

# Set custom margins (bottom, left, top, right)
par(mar = c(4,5, 5,4))  # You can adjust these numbers if needed

# Fit model
model <- lm(AGBM ~ CAG, data = CAGAGBM)
summary(model)

# Extract coefficients
intercept <- round(coef(model)[1], 3)
slope <- round(coef(model)[2], 3)

# Extract and round R-squared to 2 decimal places
r_squared <- round(summary(model)$r.squared, 2)

# Create generic equation and R² strings
equation_generic <- paste0("y = ",slope, "x" , " + ", intercept)
r2_text <- paste0("R² = ", r_squared)
par(family = "Times")
# Plot data
plot(CAGAGBM$CAG, CAGAGBM$AGBM,
     xlab = "Canopy architecture score (Photogrammetry)",
     ylab = "Above ground biomass (g/plant) ",
     pch = 19,
     cex = 1.5,
     col = "black",
     xlim = c(1, 10),
     ylim = c(1, 27),
     xaxt = "n",
     cex.lab = 1.5,  # Change axis label font size
     cex.axis = 1.3)

axis(side = 1, at = 1:10, cex.axis = 1.3)

# Add regression line
abline(model, col = "black", lwd = 2)
y_base <- max(CAGAGBM$AGBM) * 2

# Add equation and R² with smaller font
text(x = 7, y = max(CAGAGBM$AGBM) * 2, labels = equation_generic, col = "black", cex = 1.5, pos = 4)
text(x = 7, y = max(CAGAGBM$AGBM) * 1.8, labels = r2_text, col = "black", cex = 1.5, pos = 4)

