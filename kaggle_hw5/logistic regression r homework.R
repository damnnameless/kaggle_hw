GradSchools = read.table("[06] Graduate Schools.txt")
GradSchools$rank = as.factor(GradSchools$rank)
#Fitting the logistic regression with all variables; the family parameter
#specifies the error distribution and link function to be used. For logistic
#regression, this is binomial.

logit.overall = glm(admit ~ gre + gpa + rank,
                    family = "binomial",
                    data = GradSchools)

#Residual plot for logistic regression with an added loess smoother; we would
#hope that, on average, the residual values are 0.
scatter.smooth(logit.overall$fit,
               residuals(logit.overall, type = "deviance"),
               lpars = list(col = "red"),
               xlab = "Fitted Probabilities",
               ylab = "Deviance Residual Values",
               main = "Residual Plot for\nLogistic Regression of Admission Data")
abline(h = 0, lty = 2)

library(car)
influencePlot(logit.overall) #Can still inspect the influence plot.

summary(logit.overall) #Investigating the overall fit of the model.

#Coefficient interpretations on the log odds scale:
#-Intercept: The log odds of a student getting admitted to a graduate school
#            when they attended a top tier undergraduate school and received a
#            0 on the GRE and a 0 as their GPA is approximately -3.990.
#-GRE: For every additional point a student scores on the GRE, their log odds
#      of being admitted to graduate school increase by approximately 0.002,
#      holding all other variables constant.
#-GPA: For every additional point a student raises their GPA, their log odds of
#      being admitted to graduate school increase by approximately 0.804, holding
#      all other variables constant.
#-Rank: The change in log odds associated with attending an undergraduate school
#       with prestige of rank 2, 3, and 4, as compared to a school with prestige
#       rank 1, is approximately -0.675, -1.340, and -1.552, respectively, holding
#       all other variables constant.

exp(logit.overall$coefficients)

#Coefficient interpretations on the odds scale:
#-Intercept: The odds of a student getting admitted to a graduate school
#            when they attended a top tier undergraduate school and received a
#            0 on the GRE and a 0 as their GPA is approximately 0.019.
#-GRE: For every additional point a student scores on the GRE, their odds
#      of being admitted to graduate school multiply by approximately 1.002,
#      holding all other variables constant.
#-GPA: For every additional point a student raises their GPA, their odds of
#      being admitted to graduate school multiply by approximately 2.235, holding
#      all other variables constant.
#-Rank: The multiplicative change in odds associated with attending an undergraduate school
#       with prestige of rank 2, 3, and 4, as compared to a school with prestige
#       rank 1, is approximately 0.509, 0.262, and 0.212, respectively, holding
#       all other variables constant.

#Inspecting the relationship between log odds and odds.
cbind("Log Odds" = logit.overall$coefficients,
      "Odds" = exp(logit.overall$coefficients))

confint(logit.overall) #For logistic regression objects, the confint() function
#defaults to using the log likelihood to generate confidence
#intervals; this is similar to inverting the likelihood
#ratio test.

confint.default(logit.overall) #To generate confidence intervals for logistic
#regression models based on the standard errors
#as we are accustomed to, we can use the
#confint.default() function.

#Generating confidence intervals for the coefficients on the odds scale.
exp(confint(logit.overall))
exp(confint.default(logit.overall))

#Do the categories for rank add any predictive power to the model? Let's
#conduct the drop in deviance test:
logit.norank = glm(admit ~ gre + gpa,
                   family = "binomial",
                   data = GradSchools)

reduced.deviance = logit.norank$deviance #Comparing the deviance of the reduced
reduced.df = logit.norank$df.residual    #model (the one without rank) to...

full.deviance = logit.overall$deviance #...the deviance of the full model (the
full.df = logit.overall$df.residual    #one with the rank terms).

pchisq(reduced.deviance - full.deviance,
       reduced.df - full.df,
       lower.tail = FALSE)

#The p-value is extremely small (<.0005); we have evidence to conclude that the
#model with the factors for rank is preferable to the model without the factors
#for rank.

#More simply, we can use the anova() function and set the test to "Chisq".
anova(logit.norank, logit.overall, test = "Chisq")

#Converting the fitted probabilities to binary:
admitted.predicted = round(logit.overall$fitted.values)

#Comparing the true values to the predicted values:
table(truth = GradSchools$admit, prediction = admitted.predicted)

#It seems like this model made a lot of mistakes (116 out of 400)! This is quite
#dreadful in this case. Let's do a little bit more exploring. We never looked at
#the overall test of deviance:
pchisq(logit.overall$deviance, logit.overall$df.residual, lower.tail = FALSE)

#The p-value for the overall test of deviance is <.05, indicating that this model
#is not a good overall fit!

#What about checking the McFadden's pseudo R^2 based on the deviance?
1 - logit.overall$deviance/logit.overall$null.deviance

#Only about 8.29% of the variability in admission appears to be explained by
#the predictors in our model; while the model is valid, it seems as though it
#isn't extremely informative.

#What have we found out? The overall model we created doesn't give us much
#predictive power in determining whether a student will be admitted to
#graduate school.
table(GradSchools$admit) #Our data contains 273 unadmitted students and 127
#admitted students.
table(admitted.predicted) #The model we created predicts that 351 students will
#not be admitted, and only 49 will be admitted.
table(truth = GradSchools$admit, prediction = admitted.predicted)

#The table of the truth against the prediction shows that we only have an accuracy
#of (254 + 30)/400 = 71%; yet, if we were to simply predict "unadmitted" for
#everyone uniformly, we would have an accuracy of 273/400 = 68.25%! No wonder
#why the overall test of deviance was insignificant -- predicting only the
#intercept of the baseline probability was just as sufficient!