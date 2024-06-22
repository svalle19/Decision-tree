# Load necessary libraries
library(readxl)
library(caTools)
library(rpart) # For fitting decision trees
library(rpart.plot) # For plotting decision trees
library(caret) # For evaluating model performance
library(pROC) # For AUC calculation

# Set seed for reproducibility
set.seed(06192024)

# Read the Excel file
GS_5_23_24 <- read_excel("D:/GS 5.23.24.xlsx")
# View(GS_5_23_24)

# Remove rows with NA values in the input features or target variable
GS_5_23_24 <- na.omit(GS_5_23_24)

# Splitting dataset into 4:1 ratio for training data set
sample_data <- sample.split(GS_5_23_24$GS0or1, SplitRatio = 0.75)
train_data <- subset(GS_5_23_24, sample_data == TRUE)
test_data <- subset(GS_5_23_24, sample_data == FALSE)

# Convert GS0or1 to a factor for classification
train_data$GS0or1 <- as.factor(train_data$GS0or1)
test_data$GS0or1 <- as.factor(test_data$GS0or1)

# Define the formula for the classification tree
formula_tree <- GS0or1 ~ Ecadherin + Ncadherin + Bcatenin + phosphoRbS249 + Stage + Grade + GleasonGrade + Metastais + LymphNodes + TumorSize

# Train the classification tree
ctree.fit <- rpart(formula_tree, data = train_data, method = "class", control = rpart.control(minsplit = 30, cp = 0.01))

# Print the complexity parameter table
printcp(ctree.fit)

# Select the optimal cp value based on the lowest cross-validated error
optimal_cp <- ctree.fit$cptable[which.min(ctree.fit$cptable[,"xerror"]),"CP"]

# Print the selected optimal cp
cat("Selected optimal cp: ", optimal_cp, "\n")

# Prune the tree using the optimal cp
pruned_tree <- prune(ctree.fit, cp = optimal_cp)

# Print the pruned tree complexity parameter table
printcp(pruned_tree)

# Plot the pruned tree
rpart.plot(pruned_tree, main = "Pruned Classification Tree")

# Predict on the test set
pred_class <- predict(pruned_tree, newdata = test_data, type = "class")
pred_prob <- predict(pruned_tree, newdata = test_data, type = "prob")[,2]

# Confusion matrix to evaluate the classification performance
conf_matrix <- confusionMatrix(pred_class, test_data$GS0or1)
print(conf_matrix)

# Evaluate accuracy
accuracy <- sum(pred_class == test_data$GS0or1) / nrow(test_data)
cat("Accuracy for the classification tree: ", accuracy, "\n")

# Calculate AUC
roc_obj <- roc(test_data$GS0or1, pred_prob)
auc_value <- auc(roc_obj)
cat("AUC for the classification tree: ", auc_value, "\n")

# Plot the ROC curve
plot(roc_obj, main = "ROC Curve for Pruned Classification Tree", col = "blue")
