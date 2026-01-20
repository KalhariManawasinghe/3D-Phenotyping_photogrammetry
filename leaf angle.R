library(readr)
DAPLA <- read_csv("Desktop/Publication/14DAPLA.csv")

# Assume your data is in a data frame called '14DAPLA'
# ConLAert Genotype to a factor (if it's not already)
DAPLA$Genotype <- as.factor(DAPLA$Genotype)

# Fit linear regression model
model <- lm(LA ~ CA, data = DAPLA)

# Set custom margins (bottom, left, top, right)
par(mar = c(4,5, 4,4))  # You can adjust these numbers if needed

# Fit model
model <- lm(LA ~ CA, data = DAPLA)
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
plot(DAPLA$CA, DAPLA$LA,
     xlab = "Canopy architecture score",
     ylab = "Leaf angle (º/7 days after planting) ",
     pch = 19,
     col = "black",
     xlim = c(1, 10),
     ylim = c(10, 100),
     xaxt = "n",
     cex.lab = 1.5,  # Change axis label font size
     cex.axis = 1.5)

axis(side = 1, at = 1:10, cex.axis=1.4)

# Add regression line
abline(model, col = "black", lwd = 2)
y_base <- max(DAPLA$LA) * 2

# Add equation and R² with smaller font
text(x = 6, y = max(DAPLA$LA) * 0.95, labels = equation_generic, col = "black", cex = 1.5, pos = 4)
text(x = 6, y = max(DAPLA$LA) * 0.87, labels = r2_text, col = "black", cex = 1.5, pos = 4)

