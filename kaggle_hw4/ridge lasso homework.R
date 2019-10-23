##########################
#####Ridge Regression#####
##########################
library(ISLR)
Hitters = na.omit(Hitters)
help(Hitters)

#Need matrices for glmnet() function. Automatically conducts conversions as well
#for factor variables into dummy variables.
x = model.matrix(Salary ~ ., Hitters)[, -1]
head(x)#Dropping the intercept column.
y = Hitters$Salary

# use ridge/lsso to predict salary by other variables

# define labmda range
grid = 10^seq(5, -2, length = 100)

#Fitting the ridge regression. Alpha = 0 for lasso regression.
ridge.models = glmnet(x, y, alpha = 0, lambda = grid)

#Visualizing the ridge regression shrinkage.
plot(ridge.models, xvar = "lambda", label = TRUE, main = "Ridge Regression")

#Creating training and testing sets. Here we decide to use a 80-20 split with
#approximately 80% of our data in the training set and 20% of our data in the
#test set.
set.seed(0)
train = sample(1:nrow(x), 8*nrow(x)/10)
test = (-train)
y.test = y[test]
length(train)/nrow(x)
length(y.test)/nrow(x)

#Running 10-fold cross validation to find best lambda for ridge regression.
library(glmnet)
set.seed(0)
cv.ridge.out = cv.glmnet(x[train, ], y[train],
                         lambda = grid, alpha = 0, nfolds = 10)
plot(cv.ridge.out, main = "Ridge Regression\n")
bestlambda.ridge = cv.ridge.out$lambda.min
bestlambda.ridge
#148.4968
log(bestlambda.ridge)
#5.000564

#What is the test MSE associated with this best value of lambda?
ridge.bestlambdatrain = predict(ridge.models, s = bestlambda.ridge, newx = x[test, ])
mean((ridge.bestlambdatrain - y.test)^2)
#88883.4

#Refit the ridge regression on the overall dataset using the best lambda value
#from cross validation; inspect the coefficient estimates.
ridge.out = glmnet(x, y, alpha = 0)
predict(ridge.out, type = "coefficients", s = bestlambda.ridge)

#(Intercept)  1.238703e+01
#AtBat       -2.562245e-02
#Hits         1.164305e+00
#HmRun       -2.714614e-01
#Runs         1.144242e+00
#RBI          8.626580e-01
#Walks        1.970499e+00
#Years       -1.165527e+00
#CAtBat       1.064489e-02
#CHits        7.244512e-02
#CHmRun       4.931281e-01
#CRuns        1.437435e-01
#CRBI         1.531789e-01
#CWalks       2.610264e-04
#LeagueN      3.160150e+01
#DivisionW   -1.005605e+02
#PutOuts      2.100104e-01
#Assists      5.699454e-02
#Errors      -2.202061e+00
#NewLeagueN   4.357394e+00


##########################
#####Lasso Regression#####
##########################
#Fitting the lasso regression. Alpha = 1 for lasso regression.
lasso.models = glmnet(x, y, alpha = 1, lambda = grid)

#Visualizing the lasso regression shrinkage.
plot(lasso.models, xvar = "lambda", label = TRUE, main = "Lasso Regression")

#Running 10-fold cross validation to find the best lambda.
set.seed(0)
cv.lasso.out = cv.glmnet(x[train, ], y[train],
                         lambda = grid, alpha = 1, nfolds = 10)
plot(cv.lasso.out, main = "Lasso Regression\n")
bestlambda.lasso = cv.lasso.out$lambda.min
bestlambda.lasso
#2.154435
log(bestlambda.lasso)
#0.7675284

#What is the test MSE associated with this best value of lambda?
lasso.bestlambdatrain = predict(lasso.models, s = bestlambda.lasso, newx = x[test, ])
mean((lasso.bestlambdatrain - y.test)^2)
#82167.65

#Refit the lasso regression on the overall dataset using the best lambda value
#from cross validation; inspect the coefficient estimates.
lasso.out = glmnet(x, y, alpha = 1)
predict(lasso.out, type = "coefficients", s = bestlambda.lasso)

#Let's also inspect the MSE of our final lasso model on all our data.
lasso.bestlambda = predict(lasso.out, s = bestlambda.lasso, newx = x)
mean((lasso.bestlambda - y)^2)

#Refit the ridge regression on the overall dataset using the best lambda value
#from cross validation; inspect the coefficient estimates.
lasso.out = glmnet(x, y, alpha = 1)
predict(lasso.out, type = "coefficients", s = bestlambda.lasso)

#(Intercept)  1.361570e+02
#AtBat       -1.695228e+00
#Hits         5.980847e+00
#HmRun        8.866356e-02
#Runs         .           
#RBI          .           
#Walks        4.989180e+00
#Years       -1.038254e+01
#CAtBat      -8.295157e-06
#CHits        .           
#CHmRun       5.656298e-01
#CRuns        7.081846e-01
#CRBI         3.867245e-01
#CWalks      -5.885472e-01
#LeagueN      3.308113e+01
#DivisionW   -1.193837e+02
#PutOuts      2.762869e-01
#Assists      2.020514e-01
#Errors      -2.298847e+00
#NewLeagueN   .           

#In this scenario, the MSE of both the ultimate ridge and lasso regressions
#were comparable with the lasso being a bit smaller. The lasso has the substantial 
#advantage over ridge regression in that the resulting coefficient estimates are sparse.