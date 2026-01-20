library(readr)
DAPHR <- read_csv("Desktop/Publication/35DAPHR.csv")

# Assume your data is in a data frame called '14DAPHR'
# Convert Genotype to a factor (if it's not already)
DAPHR$Genotype <- as.factor(DAPHR$Genotype)

# Fit linear regression model
model <- lm(HR ~ CA, data = DAPHR)

# Set custom margins (bottom, left, top, right)
par(mar = c(4,5, 4,4))  # You can adjust these numbers if needed

# Fit model
model <- lm(HR ~ CA, data = DAPHR)

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
plot(DAPHR$CA, DAPHR$HR,
     xlab = "Canopy architecture score",
     ylab = "Canopy height:width ratio",
     pch = 19,
     col = "black",
     xlim = c(1, 10),
     ylim = c(1, 3.5),
     xaxt = "n",  
     cex.lab = 1.2,  # Change axis label font size
     cex.axis = 1.2)

axis(side = 1, at = 1:10)

# Add regression line
abline(model, col = "black", lwd = 2)
y_base <- max(DAPHR$HR)*20

# Add equation and R² with smaller font
text(x = 6, y = max(DAPHR$HR) * 0.95, labels = equation_generic, col = "black", cex = 1.2, pos = 4)
text(x = 6, y = max(DAPHR$HR) * 0.89, labels = r2_text, col = "black", cex = 1.2, pos = 4)


