# 1. Split Data (80% train, 20% test)
set.seed(123)
train_index <- sample(1:nrow(data), 0.8 * nrow(data))
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# 2. Scale numeric predictors
train_data$Perimeter <- scale(train_data$Perimeter)
train_data$MajorAxisLength <- scale(train_data$MajorAxisLength)
train_data$MinorAxisLength <- scale(train_data$MinorAxisLength)

test_data$Perimeter <- scale(test_data$Perimeter)
test_data$MajorAxisLength <- scale(test_data$MajorAxisLength)
test_data$MinorAxisLength <- scale(test_data$MinorAxisLength)

# 3. Fit Logistic Regression Model
model <- glm(Class ~ MajorAxisLength + Perimeter + MinorAxisLength,
             data = train_data,
             family = binomial,
             control = glm.control(maxit = 500))

summary(model)

# 4. Predict probabilities on test data
predicted_prob <- predict(model, newdata = test_data, type = "response")

# 5. Convert probabilities to class labels (threshold = 0.5)
predicted_class <- ifelse(predicted_prob > 0.5, 1, 0)

# 6. Calculate Accuracy
accuracy <- mean(predicted_class == test_data$Class) * 100
cat("Model Accuracy (%):", round(accuracy, 2), "\n")

# 7. Combine actual and predicted into a data frame
output <- data.frame(
  Test_Class = test_data$Class,
  Predicted_Class = predicted_class,
  Perimeter = test_data$Perimeter,
  MajorAxisLength = test_data$MajorAxisLength,
  MinorAxisLength = test_data$MinorAxisLength,
  Predicted_Probability = round(predicted_prob, 3)
)

# Print the output
print(output)
