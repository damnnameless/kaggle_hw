###########################
#####One-Sample T-Test#####
###########################
set.seed(0)
#Randomly generating 100 normally
#distributed observations with a
#mean of 70 and standard deviation
#of 1.
heights = rnorm(n = 100, mean = 70, sd = 1) 

# test whether the mean value of height is 68

t.test(heights, mu = 68, alternative = "two.sided")

#The p-value for this test is smaller than 2.2e-16; so, we reject the null hypothesis


###########################
#####Two-Sample T-Test#####
###########################
set.seed(0)

#Randomly generating 100 normally distributed
#observations with a mean of 1550 and a
#standard deviation of 200.
SAT.Spring = rnorm(100, 1550, 200) 

#Randomly generating 80 normally distributed
#observations with a mean of 1500 and a standard
#deviation of 210.

SAT.Fall = rnorm(80, 1500, 210) 
# test whether mean values of SAT spring and fall are the same

t.test(SAT.Spring, SAT.Fall, alternative = "two.sided")

#The p-value for this test is 0.01215; so, we reject the null hypothesis if alpha is 0.05 and 
#cannot reject null hypothesis if alpha is 0.01

################
#####F-Test#####
################

# test whether variances of SAT spring and fall are the same


var.test(SAT.Spring, SAT.Fall, alternative = "two.sided")

#The p-value for this test is 0.1161; so, we cannot reject the null hypothesis

#######################
#####One-Way ANOVA#####
#######################
set.seed(0)
Low.Calorie = rnorm(200, 10, 1) #Randomly generating weight loss measurements
Low.Carb = rnorm(200, 8.5, 1)   #for various diet types.
Low.Fat = rnorm(200, 8, 1)
Control = rnorm(200, 0, 1)

Weight.Loss = c(Low.Calorie, Low.Carb, Low.Fat, Control) #Combining data into
Category = c(rep("Low Calorie", 200),                    #different consolidated
             rep("Low Carb", 200),                       #vectors.
             rep("Low Fat", 200),
             rep("Control", 200))

# test whether the mean values of the weight loss of different categories of diet are the same


summary(aov(Weight.Loss ~ Category))

#The p-value for this test is smaller than 2e-16. so We reject the null hypothesis
#null hypothesis is that they have the same mean


##################################
#####X^2 Test of Independence#####
##################################
quiz.data = matrix(c(44, 21, 12, 18), nrow = 2, ncol = 2, byrow = TRUE)
dimnames(quiz.data) = list(Attendance = c("Present", "Absent"),
                           Grade = c("Pass", "Fail"))

# test whether the attendance and grade variables are independent of one another


chisq.test(quiz.data)
#The p-value for this test is 0.02001. So if alpha is 0.05 null hypothesis is not rejected
#if alpha is 0.01 null hypothesis is rejected.
