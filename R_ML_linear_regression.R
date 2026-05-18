# =========================================================
# LINEAR REGRESSION IN R USING y = mx + b
# 1. BUILT-IN lm() METHOD
# 2. MANUAL METHOD (FORMULA BASED)
# =========================================================


# =========================================================
# METHOD 1 : USING CSV FILE + BUILT-IN FUNCTION
# =========================================================

# Read dataset from CSV file
data <- read.csv("/home/ds-da-22/anusreekj/data.csv")

# Display dataset
print(data)

# -------------------------------
# FIT LINEAR REGRESSION MODEL
# -------------------------------
# lm() automatically finds best-fit line using least squares method
model <- lm(y ~ x, data = data)

# Show model details (slope, intercept, error, R², etc.)
summary(model)

# -------------------------------
# PLOT ORIGINAL DATA POINTS
# -------------------------------
plot(data$x, data$y,
     main = "Linear Regression (lm method)",
     xlab = "X (Hours)",
     ylab = "Y (Marks)",
     pch = 19,        # solid circle points
     col = "blue")    # blue points

# -------------------------------
# DRAW REGRESSION LINE
# -------------------------------
# This is the best-fit line: y = mx + b (computed internally)
abline(model, col = "red", lwd = 2)

# -------------------------------
# USER INPUT FOR PREDICTION
# -------------------------------
x_new <- as.numeric(readline("Enter hours (lm method): "))

# Predict output using built-in model
y_pred1 <- predict(model, data.frame(x = x_new))

# Display predicted value
cat("Prediction (lm):", round(y_pred1, 2), "\n")

# -------------------------------
# SHOW PREDICTED POINT ON GRAPH
# -------------------------------
points(x_new, y_pred1, col = "green", pch = 19, cex = 1.5)
text(x_new, y_pred1, labels = round(y_pred1, 2), pos = 3, col = "green")


# =========================================================
# METHOD 2 : MANUAL LINEAR REGRESSION USING y = mx + b
# =========================================================

# -------------------------------
# STEP 1: CREATE DATASET MANUALLY
# -------------------------------
data2 <- data.frame(
  x = c(1, 2, 3, 4, 5, 6, 7, 8),
  y = c(12, 18, 25, 32, 45, 52, 65, 70)
)

# Display dataset
print(data2)

# Extract x and y values
x <- data2$x
y <- data2$y

# -------------------------------
# STEP 2: CALCULATE MEANS
# -------------------------------
# mean of x (x̄)
x_bar <- mean(x)

# mean of y (ȳ)
y_bar <- mean(y)

# -------------------------------
# STEP 3: CALCULATE SLOPE (m)
# -------------------------------
# Formula:
# m = Σ(x - x̄)(y - ȳ) / Σ(x - x̄)²
m <- sum((x - x_bar) * (y - y_bar)) / sum((x - x_bar)^2)

# -------------------------------
# STEP 4: CALCULATE INTERCEPT (b)
# -------------------------------
# Formula:
# b = ȳ - m * x̄
b <- y_bar - m * x_bar

# Display results
cat("\nManual Model Coefficients\n")
cat("Slope (m):", m, "\n")
cat("Intercept (b):", b, "\n")

# -------------------------------
# STEP 5: PLOT DATA POINTS
# -------------------------------
plot(x, y,
     main = "Manual Linear Regression (y = mx + b)",
     xlab = "X (Hours)",
     ylab = "Y (Marks)",
     pch = 19,
     col = "blue")

# -------------------------------
# STEP 6: DRAW REGRESSION LINE
# -------------------------------
# Using equation y = mx + b
abline(a = b, b = m, col = "red", lwd = 2)

# -------------------------------
# STEP 7: USER INPUT
# -------------------------------
x_new2 <- as.numeric(readline("Enter hours (manual method): "))

# -------------------------------
# STEP 8: PREDICTION USING FORMULA
# -------------------------------
# y = mx + b
y_pred2 <- m * x_new2 + b

# Display prediction
cat("Prediction (manual):", round(y_pred2, 2), "\n")

# -------------------------------
# STEP 9: SHOW POINT ON GRAPH
# -------------------------------
points(x_new2, y_pred2, col = "green", pch = 19, cex = 1.5)
text(x_new2, y_pred2, labels = round(y_pred2, 2), pos = 3, col = "green")
