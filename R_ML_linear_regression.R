data <- read.csv("home/ds-da-22/anusreekj/data.txl")  
print(data)

# Fit Linear Regression Model
model <- lm(y ~ x, data = data)
summary(model)

# Plot the data points and regression line
plot(data$x, data$y,
     main = "Linear Regression: Y vs X",
     xlab = "X (Hours)",
     ylab = "Y (Marks)",
     pch = 19,          
     col = "blue")

# Add regression line
abline(model, col = "red", lwd = 2)  # lwd = line width

# Take user input for prediction
predict_num <- as.numeric(readline(prompt = "Enter the number of hours: "))
# Prepare new data for prediction
new_x <- data.frame(x = predict_num)

# Predict the Y value
predicted_marks <- predict(model, newdata = new_x)

# Print the predicted value
cat("Predicted Y for", predict_num, "hours is:", round(predicted_marks, 2), "\n")

# Add the predicted point on the plot
points(predict_num, predicted_marks, col = "green", pch = 19, cex = 1.5)

# Add text label for the predicted point
text(predict_num, predicted_marks, labels = round(predicted_marks, 2), pos = 3, col = "green")
