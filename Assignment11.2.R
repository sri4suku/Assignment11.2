#Perform the below operations:
 # a. Is there any association between job and default?
  #b. Is there any significant difference in duration of last call between?
  #people having housing loan or not?
  #c. Is there any association between consumer price index and
#consumer?
 # d. Is the employment variation rate consistent across Job types?
#e. Is the employment variation rate same across Education?
 # f. Which group is more confident?

# a. Is there any association between job and default?
library(readr)
#"C:\Users\aruna\Desktop\Assignments\home work acadgild\bank-additional\bank-additional-full.csv"
setwd("C:\\Users\\DELL\\Desktop\\Assignments\\Assignment11")
bankaddfull1<- read.table("bank-additional-full.csv", header = T, sep=";" )

str(bankaddfull1)
if(length(which(is.na(bankaddfull1$campaign)==TRUE)>0))
{
  print("Missing Value found in the specified column")
} else
  print("All okay: No Missing Value found in the specified column")
if(length(which(is.na(bankaddfull1$age)==TRUE)>0)){
  print("Missing Value found in the specified column")
} else
  print("All okay: No Missing Value found in the specified column")

head(bankaddfull1)## Displays first 6 rows for each variable
str(bankaddfull1)## Describes each variables
summary(bankaddfull1)## Provides basic statistical information of each variable

## DATA EXPLORATION - Check for Missing Data
## Option 1
is.na(bankaddfull1) ## Displays True for a missing value
################3
##Option 2
require(Amelia)
missmap(bankaddfull1,main="Missing Data - Bank ", col=c("red","grey"),legend=FALSE)
## No red colour stripes are visible. hence no missing values.
summary(bankaddfull1) ## displays missing values if any under every variable
#The Pearson's chi-squared test of independence is one of the most basic and common hypothesis tests in the statistical analysis of categorical data. It is a significance test. Given two categorical random variables, X and Y, the chi-squared test of independence determines whether or not there exists a statistical dependence between them. Formally, it is a hypothesis test. The chi-squared test assumes a null hypothesis and an alternate hypothesis. The general practice is, if the p-value that comes out in the result is less than a pre-determined significance level, which is 0.05 usually, then we reject the null hypothesis.

#H0: The The two variables are independent

#H1: The The two variables are dependent

#The null hypothesis of the chi-squared test is that the two variables are independent and the alternate hypothesis is that they are related.
#To establish that two categorical variables (or predictors) are dependent, the chi-squared statistic must have a certain cutoff. This cutoff increases as the number of classes within the variable (or predictor) increases.
#i. Pearson's chi-squared test of independence (significance test)
#Is there any association between Job and default? 
with(bankaddfull1, chisq.test( job, default))
with(bankaddfull1, table( job, default) )
# OR
with(bankaddfull1, prop.table(table( job,default)))
#Pearson's Chi-squared test
#p-value = 8.008e-09
#Pearson's Chi-squared test
#since the p-value is < 2.2e-16 is less than the cu$t-off value of 0.05, we can reject the null hypothesis in favor of alternative hypothesis and conclude, that the variables,( job & default- p-value = 8.008e-09) are dependent to each other.

#b. Is there any significant difference in duration of last call between people having housing loan or not? 
with(bankaddfull1, chisq.test(duration,housing))
with(bankaddfull1, table( duration,housing) )
# OR
with(bankaddfull1, prop.table(table(duration, housing)))

#---------------------------------------------
#Is there any association between consumer price index and consumer?
with(bankaddfull1, chisq.test(cons.price.idx,cons.conf.idx))
with(bankaddfull1, table(cons.price.idx,cons.conf.idx))
# OR
with(bankaddfull1, prop.table(table(cons.price.idx,cons.conf.idx)))

#p-value < 2.2e-16 and it is very much less than 0.05.we can reject the null hypothesis in favor of alternative hypothesis and conclude, that the variables, (job & Marital-p-value < 2.2e-16),(con.price.idx , consumer- are dependent to each other.
#Chi-squared approximation may be incorrect
#Pearson's Chi-squared test
  
-------------------------------
  #Is the employment variation rate consistent across job types? 
 with(bankaddfull1, chisq.test( job,emp.var.rate))
 with(bankaddfull1, table( job,emp.var.rate) )
# OR
with(bankaddfull1, prop.table(table( job,emp.var.rate)))
##p-value < 2.2e-16 is very much less than 0.05


#Pearson's Chi-squared test

#data:  job and emp.var.rate
#X-squared = 4676.8, df = 99, p-value < 2.2e-16
#---------------------------------------------------------------
  #Is the employment variation rate same across education? 
  #Which group is more confident? 
  with(bankaddfull1, chisq.test( education,emp.var.rate))
with(bankaddfull1, table( education, emp.var.rate) )
# OR
with(bankaddfull1, prop.table(table( education,emp.var.rate)))

#Pearson's Chi-squared test

#data:  education and emp.var.rate
#X-squared = 1451.6, df = 63, p-value < 2.2e-16
#-------------------------------------------------------------------


  