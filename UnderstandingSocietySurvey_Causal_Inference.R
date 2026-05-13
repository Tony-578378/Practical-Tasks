#Variables in the dataset:
#sex – sex (at birth) of the individual (1 – male, 2 – female)
#age – age of the respondent in years
#occupid – the importance of your occupation to who you are (Very important to my 
#sense of who I am 1-4 Not at all important to the sense of who I am)
#grossmpay – total monthly labour income (pounds, rounded)
#fiveclass – occupational class, 5-class taxonomy (Management and professional – 1,
#Intermediate – 2, Small employer & own account – 3, Lower supervisory & technical –
#4, Semi-routine & routine – 5)
#urban – living an urban or rural area (Rural area – 1, Urban area – 2)
#smoker – being a smoker (non-smoker – 1, smoker – 2)

## Research question:Does smoking reduce monthly labour income?

library(haven)
survey <- read_dta("USS_general_overview.dta")
View(survey)

library(psych)
describe(survey)

#Remove missing values
survey <- na.omit(survey[, c("grossmpay",
                             "smoker",
                             "age",
                             "sex",
                             "fiveclass",
                             "urban")])#occupid contains too many missing values, so we exclude it from our analysis.

## 1.Plot a Directed Acyclic Graph (DAG) to define a causal structure.
install.packages("dagitty")
install.packages("ggdag")
install.packages("tweenr")
install.packages("graphlayouts")

library(dagitty)
library(ggdag)
library(ggplot2)

png(file="Directed_Acyclic_Graph.png",
    width=350, height=400)

set.seed(10)
dag <- dagitty("
dag {
  age -> smoker
  age -> grossmpay
  sex -> smoker
  sex -> grossmpay
  fiveclass -> smoker
  fiveclass -> grossmpay
  urban -> smoker
  urban -> grossmpay
  smoker -> grossmpay
}
") #This defines a causal effect of smoking on monthly labour income.

ggdag(dag) +
  theme_dag()

dev.off()

adjustmentSets(dag, exposure = "smoker", outcome = "grossmpay")#I should control for age, fiveclass, sex, urban.

## 2.Build an OLS regression model, using grossmpay as the outcome variable.
#Factorise all categorical variables.
survey$sex <- factor(survey$sex, levels = c(1, 2), labels = c("Male", "Female"))
survey$fiveclass <- factor(survey$fiveclass, levels = c(1:5), labels = c("Management and professional",
                                                                         "Intermediate","Small employer & own account",
                                                                         "Lower supervisory & technical",
                                                                         "Semi-routine & routine"))
survey$urban <- factor(survey$urban, levels = c(1, 2), labels = c("Rural area", "Urban area"))
survey$smoker <- factor(survey$smoker, levels = c(1, 2), labels = c("non-smoker", "smoker"))
survey$smoker <- survey$smoker <- ifelse(survey$smoker == "smoker", 1, 0)#matching requires 0<=y<=1

#Build the model
model1 <- lm(grossmpay ~ smoker + sex + age + fiveclass + urban, data = survey)
summary(model1)#Being a smoker instead of a non-smoker decreases the total monthly
#income significantly (B = -156.95, p < 0.01).

## 3.Perform propensity score matching.
install.packages("MatchIt")
library(MatchIt)

m.out <- matchit(smoker ~ sex + age + fiveclass + urban, data = survey, method = "nearest")
summary(m.out)#2566 out of 16555 control units are matched.

#Check for common support
png(file="USS_PSMatchingCommonSupport.png", width=500, height=450)

plot(m.out, type = "hist")#0 < Pr(D=1|X=x) < 1 for all X holds

dev.off()

matched_survey <- match.data(m.out)

#Compare pre- and post-matching
model2 <- lm(grossmpay ~ smoker + sex + age + fiveclass + urban, data = matched_survey)
summary(model2)#The coefficient of smoker now slightly changes to -165.92, so it strenghthens the 
#the argument that the relationship is not purely due to observable confounding.

## 4.Sensitivity analysis.
library(cobalt)

png(file="USS_covariate_balance_plot.png", width=600, height=600)
love.plot(m.out)
dev.off()

#Smokers and non-smokers differed substantially in many variables before matching, but are much more
#similar after matching.So the matching is balanced.

#Calculate the E-value
install.packages("EValue")
install.packages("mathjaxr")
library(EValue)

evalues.OLS(est = model1$coef['smoker'], #use the coefficient of smoker in model1
            se = 50,
            sd = sd(survey$grossmpay))
#The point estimate of E-value is only 1.41, suggesting that relatively weak 
#unmeasured confounding could potentially explain away the observed association. 
#Therefore, although matching improved balance on observed covariates, the 
#estimated effect should be interpreted cautiously and not as strong causal evidence
#between smoking or not and the income.


