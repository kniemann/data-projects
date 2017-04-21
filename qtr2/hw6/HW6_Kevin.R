#HW 6 Kevin Niemann
setwd('C:/Users/kevin/Google Drive/datasci/HW6')
#install.packages("pls")
#install.packages("glmnet")
library(pls)
library(glmnet)

set.seed(20) # You will need to set the seed to 20 for this to work.

# Retrieve Breast Cancer Expression Data From the following Study:
#http://www.ncbi.nlm.nih.gov/pubmed/21532620


micro_data=read.table("MicroArray.txt", header=TRUE)

dim(micro_data)

# Normalize each column
micro_data = scale(micro_data)

# Breast Cancer Samples:
cancer_samples = c(0,0,0,0,0,1,0,0,0,1,0,1,0,0,1,0,0,0,1)

# Think of this as ~ 10,000 possible variables (genes) to predict 19 outcomes.

# Convert to data.frame
micro_frame = data.frame(t(micro_data))
micro_frame$outcomes = cancer_samples

##-----Lasso Regression-----
# user glmnet, where family = 'binomial'
inputs = model.matrix(outcomes ~ . - outcomes, data = micro_frame)
cancer_lasso = glmnet(inputs, micro_frame$outcomes, family='binomial', alpha = 1)

# See how the variables vary with the different regularization factor, lambda
coef(cancer_lasso)[,20][coef(cancer_lasso)[,20]>1e-10]
plot(cancer_lasso, xvar="lambda")

# Now use cv.glmnet to test different lasso cutoffs

cancer_lasso_cv = cv.glmnet(inputs,micro_frame$outcomes,alpha=1,family='binomial')
plot(cancer_lasso_cv)


# find the minumum lambda.min

best_lambda = cancer_lasso_cv$lambda.min


# Find the coefficients that are greater than zero
best_coef = coef(cancer_lasso)[,cancer_lasso$lambda == best_lambda]
best_coef = best_coef[best_coef > 1e-10]


# Plug this into the glm(...,family='binomial') to get the logistic outcome
inputFormula = paste(names(best_coef[-1]), collapse =" + ")
formula = as.formula(paste('outcomes ~', inputFormula, sep=""))

has_cancer = glm(formula,family='binomial', data=micro_frame)

summary(has_cancer)
# Compare with the real outcome, cancer_samples above
cutoff = 0.5
prediction = as.numeric(has_cancer$fitted.values>cutoff)

pred_cancer_actual_cancer = sum( (prediction == 1) & (cancer_samples == 1) )
pred_no_cancer_actual_no_cancer = sum( (prediction == 0) & (cancer_samples == 0) )

# Maybe say our accuracy is total right out of total:
accuracy = (pred_cancer_actual_cancer + pred_no_cancer_actual_no_cancer)/(nrow(micro_frame))

#Summary
#The breat cancer data was fit to a generalized linear model through a lasso method through the glmnet function.
#I then used cross validation for the glmnet function through cv.glmnet and determined the minimum lambda value.
#With that min/best lambda, I found the best coefficents produced by the glm function.
#When I plugged those coefficients into the right side of the formula of the glm function, I tested the new model to 
#predict which patients had cancer. It correctly predicted the 5 cancer cases and 14 non-cancer cases (surprisingly).
