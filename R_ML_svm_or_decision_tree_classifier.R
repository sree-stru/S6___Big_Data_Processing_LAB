# Load libraries
library(e1071)       # for SVM
library(rpart)       # for Decision Tree
library(rpart.plot)  # for plotting the tree

# 1. Load Dataset
data <- read.csv("/home/ds-da-35/tidha/bill_authentication.csv")

# 2. Assign Column Names
colnames(data) <- c("variance", "skewness", "curtosis", "entropy", "class")

# 3. Ensure numeric columns are numeric
num_cols <- c("variance", "skewness", "curtosis", "entropy")  # fixed typo: curtasis -> curtosis
data[num_cols] <- lapply(data[num_cols], function(x) as.numeric(as.character(x)))

# 4. Convert class to factor
data$class <- as.factor(data$class)

# 5. Split Data (70% Train, 30% Test)
set.seed(123)
sample_index <- sample(1:nrow(data), 0.7 * nrow(data))

train_data <- data[sample_index, ]
test_data <- data[-sample_index, ]

# -------------------------------
# SVM Model
# -------------------------------
svm_model <- svm(class ~ ., data = train_data, kernel = "linear")

svm_pred <- predict(svm_model, test_data)

svm_conf_matrix <- table(Actual = test_data$class, Predicted = svm_pred)
print("SVM Confusion Matrix:")
print(svm_conf_matrix)

# -------------------------------
# Decision Tree Model
# -------------------------------
dt_model <- rpart(class ~ ., data = train_data, method = "class")

# Plot the Decision Tree
rpart.plot(dt_model, main = "Decision Tree for Banknote Authentication")

dt_pred <- predict(dt_model, test_data, type = "class")

dt_conf_matrix <- table(Actual = test_data$class, Predicted = dt_pred)
print("Decision Tree Confusion Matrix:")
print(dt_conf_matrix)
