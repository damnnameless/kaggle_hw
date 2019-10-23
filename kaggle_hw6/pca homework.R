library(Sleuth2)
case1701
View(case1701)
printer_data = case1701[, 1:11]
nrow(printer_data)

library(psych)
fa.parallel(printer_data, #The data in question.
            fa = "pc", #Display the eigenvalues for PCA.
            n.iter = 100) #Number of simulated analyses to perform.
abline(h = 1) #Adding a horizontal line at 1.
#Should extract 1 PC

pc = principal(printer_data, nfactors = 1, rotate = "none")

pc
#The variance of the extracted principal components is 10.61;
#these are also the eigenvalues associated with the principal components.

#The first principal component accounts for approximately 0.96% of the variability
#in the original dataset.

plot(pc$scores)

# transformed data set
pc$scores
