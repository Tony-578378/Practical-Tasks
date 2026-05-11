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

library(haven)
survey <- read_dta("USS_general_overview.dta")
View(survey)

library(psych)
describe(survey)

attach(survey)
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
  occupid -> smoker
  occupid -> grossmpay
  smoker -> grossmpay
}
") #This defines a causal relationship between smoking and monthly labour income.

ggdag(dag) +
  theme_dag()

dev.off()

adjustmentSets(dag, exposure = "smoker", outcome = "grossmpay")#I should control for age, fiveclass, occupid, sex, urban.




