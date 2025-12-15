# Phishing Website Detection Analysis

See `Findings.pdf` for the full report and detailed analysis.

## Project Overview

This project analyzes a dataset of 2,000 websites to classify them as either "Phishing" or "Legitimate" using various machine learning algorithms. The analysis involves data pre-processing, feature importance identification, and a comparative performance review of multiple classifiers to determine the best model for this specific dataset.

## Dataset & Pre-processing

* **Source**: A provided dataset containing 2,000 rows and 25 predictors (A01-A25).
* **Class Distribution**: Initially 64.2% Legitimate / 35.8% Phishing.
* **Cleaning**: Rows containing `NA` values were removed, reducing the dataset to 1,567 rows to ensure accurate model fitting.
* **Split**: Data was divided into a 70% training set and a 30% test set.
* **Normalisation**: Predictors were normalised for the Artificial Neural Network (ANN) and Support Vector Machine (SVM) models.

## Models Implemented

The following classification techniques were implemented and evaluated in R:

* Decision Tree
* Naïve Bayes
* Ensemble Methods: Bagging, Boosting, and Random Forest
* **Simple Tree**: A pruned decision tree optimised for human readability.
* **Best Tree**: An optimised Random Forest model (tuned `mtry` and `ntree`).
* **Artificial Neural Network (ANN)**: A 3x3 network.
* **Support Vector Machine (SVM)**.

## Performance Results

The models were evaluated based on Accuracy and Area Under the Curve (AUC). An "Average Score" (0.5 × Accuracy + 0.5 × AUC) was used to rank overall performance.

| Classifier | Accuracy | AUC | Average Score |
|---|---|---|---|
| Best Tree (New RF) | 0.824 | 0.832 | 0.828 |
| Decision Tree | 0.817 | 0.837 | 0.827 |
| Random Forest (Base) | 0.811 | 0.839 | 0.825 |
| Bagging | 0.809 | 0.822 | 0.816 |
| SVM | 0.777 | 0.819 | 0.798 |
| Boosting | 0.758 | 0.792 | 0.775 |
| ANN | 0.766 | 0.771 | 0.769 |
| Simple Tree (Pruned) | 0.773 | 0.756 | 0.764 |
| Naïve Bayes | 0.338 | 0.743 | 0.540 |

## Key Findings

1. **Top Performers**: Tree-based models (Random Forest and Decision Tree) significantly outperformed other methods. The "Best Tree" (a tuned Random Forest) achieved the highest accuracy at 82.4%.

2. **Key Predictors**: Across all ensemble methods, the most important variables for predicting phishing sites were A01, A18, A23, and A22.

3. **Human Readability**: A "Simple Tree" was created using only variables A01 and A18. While its performance (Avg: 0.764) was slightly lower than complex models, it allowed for manual classification with reasonable accuracy.

4. **Poor Performers**:
   * **Naïve Bayes**: Performed worst with an accuracy of only 33.8%.
   * **ANN & SVM**: These models underperformed compared to tree-based methods, likely due to the small dataset size (approx. 1,000 training rows), which is often insufficient for these algorithms to converge optimally.

## Conclusion

For this specific dataset, Random Forest (specifically the parameter-tuned version) is the most robust classifier. However, the standard Decision Tree is a viable alternative if model interpretability is a priority, as it performs nearly as well as the Random Forest with much lower complexity.
