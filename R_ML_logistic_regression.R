# =========================================================
# LOGISTIC REGRESSION IN R (CLASSIFICATION MODEL)
# 1. BUILT-IN METHOD (glm)
# 2. MANUAL METHOD (WITHOUT BUILT-IN FUNCTIONS)
# =========================================================



# =========================================================
# METHOD 1 : BUILT-IN LOGISTIC REGRESSION (glm)
# =========================================================

# -------------------------------
# STEP 1: SPLIT DATA INTO TRAIN AND TEST SET
# -------------------------------
# We divide dataset into:
# 80% training data -> to build model
# 20% testing data  -> to evaluate model
set.seed(123)  # ensures same random split every time

train_index <- sample(1:nrow(data), 0.8 * nrow(data)) #sample(x, size)

train_data <- data[train_index, ]   # training dataset
test_data <- data[-train_index, ]   # testing dataset


# -------------------------------
# STEP 2: FEATURE SCALING (NORMALIZATION)
# -------------------------------
# Scaling makes all variables in same range (important for model performance)
# Formula: (x - mean) / standard deviation

train_data$Perimeter <- scale(train_data$Perimeter)
train_data$MajorAxisLength <- scale(train_data$MajorAxisLength)
train_data$MinorAxisLength <- scale(train_data$MinorAxisLength)

test_data$Perimeter <- scale(test_data$Perimeter)
test_data$MajorAxisLength <- scale(test_data$MajorAxisLength)
test_data$MinorAxisLength <- scale(test_data$MinorAxisLength)


# -------------------------------
# STEP 3: FIT LOGISTIC REGRESSION MODEL
# -------------------------------
# glm = Generalized Linear Model
# family = binomial means logistic regression (0/1 classification)

model <- glm(Class ~ MajorAxisLength + Perimeter + MinorAxisLength,
             data = train_data,
             family = binomial)

# Display model details (coefficients, p-values, significance)
summary(model)


# -------------------------------
# STEP 4: PREDICT PROBABILITIES
# -------------------------------
# type="response" gives probability values between 0 and 1

predicted_prob <- predict(model, newdata = test_data, type = "response")


# -------------------------------
# STEP 5: CONVERT PROBABILITY TO CLASS
# -------------------------------
# If probability > 0.5 => Class = 1
# Else => Class = 0

predicted_class <- ifelse(predicted_prob > 0.5, 1, 0)


# -------------------------------
# STEP 6: CALCULATE ACCURACY
# -------------------------------
# Accuracy = correct predictions / total predictions

accuracy <- mean(predicted_class == test_data$Class) * 100
cat("Model Accuracy (%):", round(accuracy, 2), "\n")


# -------------------------------
# STEP 7: FINAL OUTPUT TABLE
# -------------------------------
# Shows actual vs predicted values

output <- data.frame(
  Actual_Class = test_data$Class,
  Predicted_Class = predicted_class,
  Perimeter = test_data$Perimeter,
  MajorAxisLength = test_data$MajorAxisLength,
  MinorAxisLength = test_data$MinorAxisLength,
  Predicted_Probability = round(predicted_prob, 3)
)

print(output)



# =========================================================
# METHOD 2 : MANUAL LOGISTIC REGRESSION (NO BUILT-IN FUNCTIONS)
# =========================================================



# -------------------------------
# STEP 1: CREATE SAMPLE DATASET
# -------------------------------
# We manually create dataset for understanding logic

data2 <- data.frame(
  Perimeter = c(10, 12, 14, 16, 18, 20, 22, 24),
  MajorAxisLength = c(5, 6, 7, 8, 9, 10, 11, 12),
  MinorAxisLength = c(2, 3, 4, 5, 6, 7, 8, 9),
  Class = c(0, 0, 0, 0, 1, 1, 1, 1)
)

print(data2)


# -------------------------------
# STEP 2: TRAIN-TEST SPLIT
# -------------------------------
# Same idea as above but done manually

set.seed(123)

n <- nrow(data2)
train_index <- sample(1:n, 0.8 * n)

train <- data2[train_index, ]
test <- data2[-train_index, ]


# -------------------------------
# STEP 3: MANUAL SCALING (Z-SCORE NORMALIZATION)
# -------------------------------
# Formula: (x - mean) / sd

scale_manual <- function(x) {
  (x - mean(x)) / sd(x)
}

train$Perimeter <- scale_manual(train$Perimeter)
train$MajorAxisLength <- scale_manual(train$MajorAxisLength)
train$MinorAxisLength <- scale_manual(train$MinorAxisLength)

test$Perimeter <- scale_manual(test$Perimeter)
test$MajorAxisLength <- scale_manual(test$MajorAxisLength)
test$MinorAxisLength <- scale_manual(test$MinorAxisLength)


# -------------------------------
# STEP 4: INITIALIZE PARAMETERS (WEIGHTS)
# -------------------------------
# b0 = bias (intercept)
# b1, b2, b3 = weights for features

b0 <- 0
b1 <- 0
b2 <- 0
b3 <- 0

learning_rate <- 0.01   # controls learning speed
epochs <- 1000          # number of training iterations


# -------------------------------
# STEP 5: SIGMOID FUNCTION
# -------------------------------
# Converts linear output into probability (0 to 1)

sigmoid <- function(z) {
  1 / (1 + exp(-z))
}


# -------------------------------
# STEP 6: TRAIN MODEL USING GRADIENT DESCENT
# -------------------------------
# Model learns best values of b0, b1, b2, b3

for (e in 1:epochs) {
  for (i in 1:nrow(train)) {

    # Take one training sample
    x1 <- train$MajorAxisLength[i]
    x2 <- train$Perimeter[i]
    x3 <- train$MinorAxisLength[i]
    y  <- train$Class[i]

    # Linear equation: z = b0 + b1x1 + b2x2 + b3x3
    z <- b0 + b1*x1 + b2*x2 + b3*x3

    # Apply sigmoid to get probability
    p <- sigmoid(z)

    # Error = actual - predicted
    error <- y - p

    # Update weights (learning step)
    b0 <- b0 + learning_rate * error
    b1 <- b1 + learning_rate * error * x1
    b2 <- b2 + learning_rate * error * x2
    b3 <- b3 + learning_rate * error * x3
  }
}


# -------------------------------
# STEP 7: PREDICTION ON TEST DATA
# -------------------------------
# We compute probability for each test sample

predicted_prob <- c()

for (i in 1:nrow(test)) {

  x1 <- test$MajorAxisLength[i]
  x2 <- test$Perimeter[i]
  x3 <- test$MinorAxisLength[i]

  z <- b0 + b1*x1 + b2*x2 + b3*x3

  predicted_prob[i] <- sigmoid(z)
}


# -------------------------------
# STEP 8: CONVERT PROBABILITY TO CLASS
# -------------------------------
# Decision rule:
# probability > 0.5 → Class 1
# else → Class 0

predicted_class <- ifelse(predicted_prob > 0.5, 1, 0)


# -------------------------------
# STEP 9: CALCULATE ACCURACY
# -------------------------------
accuracy <- mean(predicted_class == test$Class) * 100
cat("Manual Model Accuracy (%):", round(accuracy, 2), "\n")


# -------------------------------
# STEP 10: FINAL OUTPUT TABLE
# -------------------------------
# Shows actual vs predicted results

output <- data.frame(
  Actual_Class = test$Class,
  Predicted_Class = predicted_class,
  Perimeter = test$Perimeter,
  MajorAxisLength = test$MajorAxisLength,
  MinorAxisLength = test$MinorAxisLength,
  Predicted_Probability = round(predicted_prob, 3)
)

print(output)


