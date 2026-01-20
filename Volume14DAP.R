library(readr)
DPAV <- read_csv("Desktop/Publication/14DAPV.csv")

# Assume your data is in a data frame called '14DPAV'
# Convert Genotype to a factor (if it's not already)
DPAV$Genotype <- as.factor(DPAV$Genotype)

# Fit linear regression model
model <- lm(V ~ CA, data = DPAV)

# Set custom margins (bottom, left, top, right)
par(mar = c(4,5, 4,4))  # You can adjust these numbers if needed

# Fit model
model <- lm(V ~ CA, data = DPAV)

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
plot(DPAV$CA, DPAV$V,
     xlab = "Canopy architecture score",
     ylab = "Canopy volume",
     pch = 19,
     col = "black",
     xlim = c(1, 10),
     ylim = c(70, 300),
     xaxt = "n",,
     cex.lab = 1.2,  # Change axis label font size
     cex.axis = 1.2)

axis(side = 1, at = 1:10)
axis(side = 1, at = 50:300)

# Add regression line
abline(model, col = "black", lwd = 2)
y_base <- max(DPAV$V) * 2

# Add equation and R² with smaller font
text(x = 6, y = max(DPAV$V) * 0.95, labels = equation_generic, col = "black", cex = 1.2, pos = 4)
text(x = 6, y = max(DPAV$V) * 0.86, labels = r2_text, col = "black", cex = 1.2, pos = 4)

