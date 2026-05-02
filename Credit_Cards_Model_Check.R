# Load the dataset "credit_cards" which is stored by the R package "causaldata".
data(credit_cards, package="causaldata")
View(credit_cards)

# Fit a Generalised Linear Model to investigate the relationship between the total bill in April 2005 in thousands 
# of New Taiwan Dollars and whether the credit card payment is late in April 2005.
credit_cards$LateApril <- ifelse(credit_cards$LateApril == "TRUE", 1, 0) #Treat TRUE as 1 and FALSE as 0.

logr_bill <- glm(LateApril ~ BillApril, data = credit_cards, family = binomial) #The response variable is either 1 or 0, so we choose the "binomial" family. 
#This uses a logistic regression, which is, log(p/(1 - p)) = β0 + β1 * BillApril, where p = Pr(LateApril = 1).

summary(logr_bill)
coef(logr_bill)
exp(coef(logr_bill))
#These show that each unit of increase in the total bill in thousands of New Taiwan Dollars 
#increases the odds of paying late in April 2005 by about 0.28% (p < 0.001, significant).

qqnorm(credit_cards$BillApril)
qqline(credit_cards$BillApril, col = "red") #The normal Q-Q plot shows that the data is positively skewed and has positive kurtosis.
#So it violates the normality assumption.

# Test the quality of the model.
install.packages("performance")
library(performance)

png(filename = "checkmodel.png", width = 1500, height = 500)
check_model(logr_bill)
dev.off()
#The model fits reasonably well overall, but the binned residual plot suggests potential misspecification, 
#indicating that the relationship between BillApril and LateApril may not be adequately captured by the current model.

#Predict the probabilities of paying late in April 2005 based on the following total bill amounts.
install.packages("tibble")
library(tibble)

newdata <- tibble(BillApril=c(-100,0,20,100,500,750,1000))
prob.predictions <- predict(object = logr_bill, newdata = newdata)
prob.predictions #These are the log-odds.
prob.predictions <- predict(object = logr_bill, newdata = newdata, type = "response")
prob.predictions #These are the real probabilities.


