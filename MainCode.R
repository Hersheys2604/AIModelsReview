#Remove all objects from the workspace and load data
rm(list = ls())
Phish <- read.csv("PhishingData.csv")
set.seed(123456)
L <- as.data.frame(c(1:50))
L <- L[sample(nrow(L), 10, replace = FALSE),]
Phish <- Phish[(Phish$A01 %in% L),]
PD <- Phish[sample(nrow(Phish), 2000, replace = FALSE),] # sample of 2000 rows


#Q1
#Proportion  of phishing sites to legitimate sites
get_proportions = function(data, column, name){
  proportions = as.data.frame(table(data[, column])/nrow(data) * 100)
  colnames(proportions) = c("Class", "Proportion")
  write.csv(proportions, name, row.names = FALSE)
  return(proportions)
}
PD.Proportions = get_proportions(PD, 26, "ProportionsWithNA.csv")

# Calculate description of real-valued attributes
calculate_statistics = function(data, columns) {
  descriptions = data.frame()
  for (i in columns) {
    column_description = data.frame(
      Variable = colnames(data)[i],
      Mean = format(mean(data[, i], na.rm = TRUE), scientific = FALSE, digits = 4),
      SD = format(sd(data[, i], na.rm = TRUE), scientific = FALSE, digits = 4),
      Min = round(min(data[, i], na.rm = TRUE), 4),
      Q1 = round(quantile(data[, i], 0.25, na.rm = TRUE), 4),
      Median = round(median(data[, i], na.rm = TRUE), 4),
      Q3 = round(quantile(data[, i], 0.75, na.rm = TRUE), 4),
      Max = round(max(data[, i], na.rm = TRUE), 4)
    )
    
    descriptions = rbind(descriptions, column_description)
  }
  
  rownames(descriptions) = 1:nrow(descriptions)
  write.csv(descriptions, "Descriptions.csv", row.names = FALSE)
  return(descriptions)
}

#Choose columsn with greater than 2 values (3 with NA included)
columns = which(sapply(PD, function(x) length(unique(x)) > 3))
PD.Descriptions = calculate_statistics(PD, columns)

#Count number of NA values in each column
NA.Counts = data.frame(Predictor = names(PD), NA.Count = colSums(is.na(PD)))
NA.Counts = as.data.frame(t(NA.Counts))
write.csv(NA.Counts, "NACounts.csv")


#Q2
# Remove rows with NA values
PD = na.omit(PD)

# Calculate proportions without NA values
PD.Proportions.Without.NA = get_proportions(PD, 26, "ProportionsWithoutNA.csv")

# Convert Class to factor
PD$Class = as.factor(PD$Class)


#Q3
# Split data into 70% training and 30% testing sets
set.seed(33114064) #Student ID as random seed 
train.row = sample(1:nrow(PD), 0.7*nrow(PD)) 
PD.train = PD[train.row,]
PD.test = PD[-train.row,]


#Q4
# Load libraries
library(tree)
library(e1071)
library(randomForest)
library(adabag)

# Train classifiers and make predictions on test data to evaluate performance
PD.tree = tree(Class ~ ., data = PD.train) # Decision tree
tree.predict = predict(PD.tree, PD.test, type = "class")
plot(PD.tree)
text(PD.tree, pretty = 0)

PD.Bayes = naiveBayes(Class ~ ., data = PD.train) # Naïve Bayes
Bayes.predict = predict(PD.Bayes, PD.test, type = "class")

PD.Bag = bagging(Class ~ ., data = PD.train) # Bagging
Bag.predict = predict.bagging(PD.Bag, PD.test)

PD.Boost = boosting(Class ~ ., data = PD.train) # Boosting
Boost.predict = predict.boosting(PD.Boost, PD.test)

PD.rf = randomForest(Class ~ ., data = PD.train) # Random Forest
rf.predict = predict(PD.rf, PD.test)


#Q5
# Create confusion matrices and calculate accuracies of each model (saving to CSV)
confusion_matrix = function(actual, predicted, name) {
  matrix = table(actual = actual, predicted = predicted)
  accuracy = sum(diag(as.matrix(matrix))) / nrow(PD.test)
  write.csv(matrix, file = name, row.names = TRUE)
  return(list(matrix = matrix, accuracy = accuracy))
}
tree.matrix = confusion_matrix(actual = PD.test$Class, predicted = tree.predict, name = "TreeMatrix.csv")
Bayes.matrix = confusion_matrix(actual = PD.test$Class, predicted = Bayes.predict, name = "BayesMatrix.csv")
Bag.matrix = confusion_matrix(actual = PD.test$Class, predicted = Bag.predict$class, name = "BagMatrix.csv")
Boost.matrix = confusion_matrix(actual = PD.test$Class, predicted = Boost.predict$class, name = "BoostMatrix.csv")
rf.matrix = confusion_matrix(actual = PD.test$Class, predicted = rf.predict, name = "RFMatrix.csv")


#Q6
#ROC curve
library(ROCR)

#calculate confidence first
tree.conf = predict(PD.tree, PD.test, type = 'vector')
Bayes.conf = predict(PD.Bayes, PD.test, type = 'raw')
rf.conf = predict(PD.rf, PD.test, type = 'prob')

# create prediction object
tree.ROCPrediction = ROCR::prediction(tree.conf[,2], PD.test$Class)
Bayes.ROCPrediction = ROCR::prediction(Bayes.conf[,2], PD.test$Class)
Bag.ROCPrediction = ROCR::prediction(Bag.predict$prob[,2], PD.test$Class)
Boost.ROCPrediction = ROCR::prediction(Boost.predict$prob[,2], PD.test$Class)
rf.ROCPrediction = ROCR::prediction(rf.conf[,2], PD.test$Class)

#calculate TPR and FPR
tree.ROC = performance(tree.ROCPrediction, "tpr", "fpr")
Bayes.ROC = performance(Bayes.ROCPrediction, "tpr", "fpr")
Bag.ROC = performance(Bag.ROCPrediction, "tpr", "fpr")
Boost.ROC = performance(Boost.ROCPrediction, "tpr", "fpr")
rf.ROC = performance(rf.ROCPrediction, "tpr", "fpr")

#Plot ROC curves - needed to create a function to plot all curves for future questions
plot_default_ROC_curves = function() {
  plot(tree.ROC, col = "red", main = "ROC Curves", lwd = 2, cex.axis = 2, cex.main = 2)
  plot(Bayes.ROC, col = "green", add = TRUE, lwd = 2)
  plot(Bag.ROC, col = "blue", add = TRUE, lwd = 2)
  plot(Boost.ROC, col = "orange", add = TRUE, lwd = 2)
  plot(rf.ROC, col = "violet", add = TRUE, lwd = 2)
  abline(0,1, lwd = 2)
}

plot_default_ROC_curves()
legend("bottomright", legend = c("Decision Tree", "Naïve Bayes", "Bagging", "Boosting", "Random Forest"), col = c("red", "green", "blue", "orange", "violet"), lty = 1, lwd = 2)

# calculate AUC
calculate_AUC = function(prediction) {
  AUC = performance(prediction, "auc")
  return(as.numeric(AUC@y.values))
}

tree.AUC = calculate_AUC(tree.ROCPrediction)
Bayes.AUC = calculate_AUC(Bayes.ROCPrediction)
Bag.AUC = calculate_AUC(Bag.ROCPrediction)
Boost.AUC = calculate_AUC(Boost.ROCPrediction)
rf.AUC = calculate_AUC(rf.ROCPrediction)

# Save AUC values to CSV to compare
AUC.Comparison = data.frame(
  Classifier = c("Decision Tree", "Naïve Bayes", "Bagging", "Boosting", "Random Forest"),
  AUC = c(tree.AUC, Bayes.AUC, Bag.AUC, Boost.AUC, rf.AUC)
)
write.csv(AUC.Comparison, "AUCComparison.csv", row.names = FALSE)


#Q7
# Compare classifiers with accuracy, AUC and average (writing to csv for comparison)
compare.classifiers = data.frame(
  Classifier = c("Decision Tree", "Naïve Bayes", "Bagging", "Boosting", "Random Forest"),
  Accuracy = c(tree.matrix$accuracy, Bayes.matrix$accuracy, Bag.matrix$accuracy, Boost.matrix$accuracy, rf.matrix$accuracy),
  AUC = c(tree.AUC, Bayes.AUC, Bag.AUC, Boost.AUC, rf.AUC)
)
compare.classifiers$Avg = 0.5 * compare.classifiers$Accuracy + 0.5 * compare.classifiers$AUC
write.csv(compare.classifiers, "BaseClassifierPerformance.csv", row.names = FALSE)


#Q8
# Get importance scores for each model
summary(PD.tree)
PD.Bag$importance
PD.Boost$importance
PD.rf$importance

# Plot ensemble importances in a multivariate bar grapgh
plot_ensemble_importance = function(bag_model, boost_model, rf_model) {
  # Get importance scores
  bag_importance = bag_model$importance
  boost_importance = boost_model$importance
  rf_importance = rf_model$importance

  # Create data frames
  bag_df = data.frame(Variable = names(bag_importance), Importance = bag_importance, Model = "Bagging")
  boost_df = data.frame(Variable = names(boost_importance), Importance = boost_importance, Model = "Boosting")
  rf_df = data.frame(Variable = row.names(rf_importance), Importance = rf_importance[, 1], Model = "Random Forest")

  # Combine data frames
  importance_df = rbind(bag_df, boost_df, rf_df)

  # Create bar chart
  library(ggplot2)
  ggplot(importance_df, aes(x = Importance, y = Variable, fill = Model)) +
    geom_bar(stat = "identity", position = "dodge") +
    coord_flip() +
    labs(x = "Variable", y = "Importance", title = "Variable Importance") +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  # Center, enlarge, and bold the title
      legend.position = c(0.95, 0.95),  # Move the legend inside the plot at the top right
      legend.justification = c("right", "top"),  # Anchor the legend at its top right corner
      legend.background = element_rect(fill = "white", colour = "black")  # Add a background to the legend
    )
}
plot_ensemble_importance(PD.Bag, PD.Boost, PD.rf)

# Get top 10 most important variables of all ensemble models combined
get_top_10_variables = function(bag_model, boost_model, rf_model) {
  # Get importance scores
  importance_bag = bag_model$importance
  importance_boost = boost_model$importance
  importance_rf = rf_model$importance[,1]
  
  # Calculate average importance scores
  importance_avg = (importance_bag + importance_boost + importance_rf) / 3
  
  # Sort in descending order
  importance_sorted = sort(importance_avg, decreasing = TRUE)
  
  # Create a data frame
  top.predictors = data.frame(Variable = names(importance_sorted)[1:10],
                  AvgImportance = importance_sorted[1:10])

  write.csv(top.predictors, "Top10Predictors.csv", row.names = FALSE)
  
  # Return the data frame
  return(top.predictors)
}

top.variables = get_top_10_variables(PD.Bag, PD.Boost, PD.rf)
top.variables


#Q9
# Prune the decision tree to 2 leaf nodes and evaluate performance
tree.pruned.two = prune.tree(PD.tree, best = 2)
tree.ptwo.pred = predict(tree.pruned.two, PD.test, type = "class")
tree.ptwo.matrix = confusion_matrix(actual = PD.test$Class, predicted = tree.ptwo.pred, name = "PrunedTree2Matrix.csv")
tree.ptwo.conf = predict(tree.pruned.two, PD.test, type = 'vector')
tree.ptwo.ROCPrediction = ROCR::prediction(tree.ptwo.conf[,2], PD.test$Class)
tree.ptwo.AUC = calculate_AUC(tree.ptwo.ROCPrediction)

# Prune the decision tree to 3 leaf nodes and evaluate performance
tree.pruned.three = prune.tree(PD.tree, best = 3)
tree.pthree.pred = predict(tree.pruned.three, PD.test, type = "class")
tree.pthree.matrix = confusion_matrix(actual = PD.test$Class, predicted = tree.pthree.pred, name = "PrunedTree3Matrix.csv")
tree.pthree.conf = predict(tree.pruned.three, PD.test, type = 'vector')
tree.pthree.ROCPrediction = ROCR::prediction(tree.pthree.conf[,2], PD.test$Class)
tree.pthree.AUC = calculate_AUC(tree.pthree.ROCPrediction)

# Prune the decision tree to 4 leaf nodes and evaluate performance
tree.pruned.four = prune.tree(PD.tree, best = 4)
tree.pfour.pred = predict(tree.pruned.four, PD.test, type = "class")
tree.pfour.matrix = confusion_matrix(actual = PD.test$Class, predicted = tree.pfour.pred, name = "PrunedTree4Matrix.csv")
tree.pfour.conf = predict(tree.pruned.four, PD.test, type = 'vector')
tree.pfour.ROCPrediction = ROCR::prediction(tree.pfour.conf[,2], PD.test$Class)
tree.pfour.AUC = calculate_AUC(tree.pfour.ROCPrediction)

# Compare pruned tree performance for each pruned tree based on leaf nodes
compare.tree.pruned = data.frame(
  Classifier = c("Prune - 2", "Prune - 3", "Prune - 4"),
  Accuracy = c(tree.ptwo.matrix$accuracy, tree.pthree.matrix$accuracy, tree.pfour.matrix$accuracy),
  AUC = c(tree.ptwo.AUC, tree.pthree.AUC, tree.pfour.AUC)
)
compare.tree.pruned$Avg = 0.5 * compare.tree.pruned$Accuracy + 0.5 * compare.tree.pruned$AUC
compare.tree.pruned$AvgIncrease = c(0, diff(compare.tree.pruned$Avg)) # Calculate increase in average performance
write.csv(compare.tree.pruned, "PrunedTreePerformance.csv", row.names = FALSE)

# Plot pruned tree with 3 leaf nodes (best simple model)
plot(tree.pruned.three)
text(tree.pruned.three, pretty = 0)

# Calculate TPR, FPR and Plot ROC curves for pruned tree with 3 leaf nodes (best simple model)
tree.pthree.ROC = performance(tree.pthree.ROCPrediction, "tpr", "fpr")
plot_default_ROC_curves()
plot(tree.pthree.ROC, col = "brown", add = TRUE, lwd = 2)
legend("bottomright", legend = c("Decision Tree", "Naïve Bayes", "Bagging", "Boosting", "Random Forest", "Simple Tree"), col = c("red", "green", "blue", "orange", "violet", "brown"), lty = 1, lwd = 2)

# Save AUC and performance to compare with other models
write.csv(rbind(AUC.Comparison, data.frame(Classifier = "Simple Tree", AUC = tree.pthree.AUC)), "AUCComparisonSimpleTree.csv", row.names = FALSE)

# Calculate average performance and compare with base models from Q4.
tree.pthree.avg = 0.5 * tree.pthree.matrix$accuracy + 0.5 * tree.pthree.AUC
write.csv(rbind(compare.classifiers, data.frame(Classifier = "Simple Tree", AUC = tree.pthree.AUC, Accuracy = tree.pthree.matrix$accuracy, Avg = tree.pthree.avg)), "SimpleTreeClassifierPerformance.csv", row.names = FALSE)


#Q10
# Perform cross-validation to find best Random Forest Model
rf.cv = rfcv(PD.train[, -26], PD.train$Class, cv = 10, mtry = function(p) max(1, floor(sqrt(p))), step = 0.5, scale = "log", recursive = FALSE)

# Find best mtry value that reduces error
mtry.min.error = rf.cv$n.var[which.min(rf.cv$error.cv)]

# Save mtry values and errors to CSV to compare
write.csv(data.frame(mtry = rf.cv$n.var, Error = rf.cv$error.cv), "RFCVErrors.csv", row.names = FALSE)

# Train Random Forest model with best mtry value and ntree = 1000
best.tree = randomForest(Class ~ ., data = PD.train, mtry = mtry.min.error, ntree = 1000)

# Predict on test data and get confusion matrix and accuracy
best.tree.pred = predict(best.tree, PD.test)
best.tree.matrix = confusion_matrix(actual = PD.test$Class, predicted = best.tree.pred, name = "BestTreeMatrix.csv")

# Get Important Variables to check
best.tree$importance

# Calculate ROC curve
best.tree.conf = predict(best.tree, PD.test, type = 'prob')
best.tree.ROCPrediction = ROCR::prediction(best.tree.conf[,2], PD.test$Class)
best.tree.ROC = performance(best.tree.ROCPrediction, "tpr", "fpr")

# Plot ROC curves of all models
plot_default_ROC_curves()
plot(best.tree.ROC, col = "purple", add = TRUE, lwd = 2)
legend("bottomright", legend = c("Decision Tree", "Naïve Bayes", "Bagging", "Boosting", "Random Forest", "Best Tree"), col = c("red", "green", "blue", "orange", "violet", "purple"), lty = 1, lwd = 2)

# Calculate AUC and save to compare
best.tree.AUC = calculate_AUC(best.tree.ROCPrediction)
write.csv(rbind(AUC.Comparison, data.frame(Classifier = "Best Tree", AUC = best.tree.AUC)), "AUCComparisonBestTree.csv", row.names = FALSE)

# Calculate average performance and compare with base models from Q4.
best.tree.avg = 0.5 * best.tree.matrix$accuracy + 0.5 * best.tree.AUC
write.csv(rbind(compare.classifiers, data.frame(Classifier = "Best Tree", AUC = best.tree.AUC, Accuracy = best.tree.matrix$accuracy, Avg = best.tree.avg)), "BestTreeClassifierPerformance.csv", row.names = FALSE)


#Q11
library(neuralnet)

# Create new data set to fit Neural Network
PD.NN.Data = PD

# Scale data 
PD.NN.Data[, 1:25] = scale(PD.NN.Data[, 1:25])

#Convert Class from factor to numeric
PD.NN.Data$Class = as.numeric(PD.NN.Data$Class) - 1

# Create test / train data sets using same train.row from Q3
PD.nn.train = PD.NN.Data[train.row,]
PD.nn.test = PD.NN.Data[-train.row,]

# Train neural network
PD.nn = neuralnet(Class ~ ., data = PD.nn.train, hidden = c(3,3), threshold = 0.01)

# Predict on test data
nn.result = compute(PD.nn, PD.nn.test)
nn.predict = as.data.frame(ifelse(nn.result$net.result < 0.5, 0, 1))

# Create confusion matrix and calculate accuracy
nn.matrix = confusion_matrix(actual = PD.nn.test$Class, predicted = nn.predict$V1, name = "NNMatrix.csv")

# Calculate ROC curve
library(ROCR)
nn.ROCPrediction = ROCR::prediction(nn.result$net.result, PD.nn.test$Class)
nn.ROC = performance(nn.ROCPrediction, "tpr", "fpr")

# Plot ROC curve
plot_default_ROC_curves()
plot(best.tree.ROC, col = "purple", add = TRUE, lwd = 2)
plot(nn.ROC, col = "brown", add = TRUE, lwd = 2)
legend("bottomright", legend = c("Decision Tree", "Naïve Bayes", "Bagging", "Boosting", "Random Forest", "Best Tree", "Neural Network"), col = c("red", "green", "blue", "orange", "violet","purple", "brown"), lty = 1, lwd = 2)

# Calculate AUC and save to compare
nn.AUC = calculate_AUC(nn.ROCPrediction)
write.csv(rbind(AUC.Comparison, data.frame(Classifier = "Neural Network", AUC = nn.AUC)), "AUCComparisonNN.csv", row.names = FALSE)

# Calculate average performance and compare with base models from Q4.
nn.avg = 0.5 * nn.matrix$accuracy + 0.5 * nn.AUC
write.csv(rbind(compare.classifiers, data.frame(Classifier = "Neural Network", AUC = nn.AUC, Accuracy = nn.matrix$accuracy, Avg = nn.avg)), "NNClassifierPerformance.csv", row.names = FALSE)


#Q12
# Train SVM model
PD.svm = svm(Class ~ ., data = PD.train, probability = TRUE, scale = TRUE)

# Predict on test data
svm.predict = predict(PD.svm, PD.test)

# Create confusion matrix and calculate accuracy
svm.matrix = confusion_matrix(actual = PD.test$Class, predicted = svm.predict, name = "SVMMatrix.csv")

# Calculate ROC curve
svm.conf = attr(predict(PD.svm, PD.test, probability = TRUE), "probabilities")
svm.ROCPrediction = ROCR::prediction(svm.conf[,2], PD.test$Class)
svm.ROC = performance(svm.ROCPrediction, "tpr", "fpr")

# Plot ROC curve with all models
plot_default_ROC_curves()
plot(best.tree.ROC, col = "purple", add = TRUE, lwd = 2)
plot(nn.ROC, col = "brown", add = TRUE, lwd = 2)
plot(svm.ROC, col = "darkgreen", add = TRUE, lwd = 2)
legend("bottomright", legend = c("Decision Tree", "Naïve Bayes", "Bagging", "Boosting", "Random Forest", "Best Tree", "Neural Network", "SVM"), col = c("red", "green", "blue", "orange", "violet", "purple", "brown", "darkgreen"), lty = 1, lwd = 2)

# Calculate AUC and save to compare
svm.AUC = calculate_AUC(svm.ROCPrediction)
write.csv(rbind(AUC.Comparison, data.frame(Classifier = "SVM", AUC = svm.AUC)), "AUCComparisonSVM.csv", row.names = FALSE)

# Calculate average performance and compare with base models from Q4.
svm.avg = 0.5 * svm.matrix$accuracy + 0.5 * svm.AUC
write.csv(rbind(compare.classifiers, data.frame(Classifier = "SVM", AUC = svm.AUC, Accuracy = svm.matrix$accuracy, Avg = svm.avg)), "SVMClassifierPerformance.csv", row.names = FALSE)
