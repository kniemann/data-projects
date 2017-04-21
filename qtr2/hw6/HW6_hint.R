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
xfactors = model.matrix(outcomes ~ ., data = micro_frame)
cancer_lasso = glmnet(x = xfactors, y = as.factor(micro_frame$outcomes), family='binomial', alpha = 1)

# See how the variables vary with the different regularization factor, lambda
coef(cancer_lasso)[,20][coef(cancer_lasso)[,20]>1e-10]
plot(cancer_lasso, xvar="lambda")

# Now use cv.glmnet to test different lasso cutoffs

cancer_lasso_cv = cv.glmnet(xfactors,micro_frame$outcomes,alpha=1,family='binomial')
plot(cancer_lasso_cv)


# find the minumum lambda.min

best_lambda = cancer_lasso_cv$lambda.min

best_coef = coef(cancer_lasso)[,cancer_lasso$lambda == best_lambda]
best_coef = best_coef[best_coef > 1e-10]

# Find the coefficients that are greater than zero


# Plug this into the glm(...,family='binomial') to get the logistic outcome

# Compare with the real outcome, cancer_samples above