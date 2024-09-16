adult <- read.csv('adult_sal.csv')
head(adult)

library(dplyr)
adult <- select(adult,-X)

str(adult)
summary(adult)

#Data Cleaning
# type_employer column

table(adult$type_employer)

unemp <- function(job){
  job <- as.character(job)
  if (job=='Never-worked' | job=='Without-pay') {
    return('Unempoyed')
  } else {
    return(job)
  }
}

adult$type_employer <- sapply(adult$type_employer, unemp)
table(adult$type_employer)

group_emp <- function(job) {
  if (job=='Local-gov' | job=='State-gov') {
    return('SL-gov')
  } else if (job=='Self-emp-inc' | job=='Self-emp-not-inc') {
    return('self-emp')
  } else {
    return(job)
  }
}

adult$type_employer <- sapply(adult$type_employer, group_emp)
table(adult$type_employer)

# martial column
group_marital <- function(mar){
  mar <- as.character(mar)
  
  # Not-Married
  if (mar=='Separated' | mar=='Divorced' | mar=='Widowed'){
    return('Not-Married')
    
    # Never-Married   
  }else if(mar=='Never-married'){
    return(mar)
    
    #Married
  }else{
    return('Married')
  }
}

adult$marital <- sapply(adult$marital, group_marital)
table(adult$marital)

# country column
levels(adult$country)
Asia <- c('China','Hong','India','Iran','Cambodia','Japan', 'Laos' ,
          'Philippines' ,'Vietnam' ,'Taiwan', 'Thailand')

North.America <- c('Canada','United-States','Puerto-Rico' )

Europe <- c('England' ,'France', 'Germany' ,'Greece','Holand-Netherlands','Hungary',
            'Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')

Latin.and.South.America <- c('Columbia','Cuba','Dominican-Republic','Ecuador',
                             'El-Salvador','Guatemala','Haiti','Honduras',
                             'Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru',
                             'Jamaica','Trinadad&Tobago')
Other <- c('South')

group_country <- function(ctry){
  if (ctry %in% Asia){
    return('Asia')
  }else if (ctry %in% North.America){
    return('North.America')
  }else if (ctry %in% Europe){
    return('Europe')
  }else if (ctry %in% Latin.and.South.America){
    return('Latin.and.South.America')
  }else{
    return('Other')      
  }
}
adult$country <- sapply(adult$country, group_country)
table(adult$country)

# Missing Data
library(Amelia)

adult[adult=='?'] <- NA
table(adult$type_employer)

adult$type_employer <- sapply(adult$type_employer,factor)
adult$country <- sapply(adult$country,factor)
adult$marital <- sapply(adult$marital,factor)
adult$occupation <- sapply(adult$occupation,factor)
adult$income <- sapply(adult$income,factor)
adult$education <- sapply(adult$education, factor)
adult$relationship <- sapply(adult$relationship, factor)
adult$race <- sapply(adult$race, factor)
adult$sex <- sapply(adult$sex, factor)

missmap(adult,y.at=c(1),y.labels = c(''),col=c('yellow','black'))

# May take awhile
adult <- na.omit(adult)
# Use missmap again to check if any missing values

#EDA
library(ggplot2)
library(dplyr)

# A histogram of ages, colored by income
ggplot(adult,aes(age)) + 
  geom_histogram(aes(fill=income),color='black',binwidth=1, 
                 position=position_stack(reverse=TRUE)) +
  theme_bw()

# A histogram of hours worked per week
ggplot(adult, aes(hr_per_week)) +
  geom_histogram() +
  theme_bw()

# Rename the country column
names(adult)[names(adult)=="country"] <- "region"

# A barplot of region with the fill color defined by income class
ggplot(adult, aes(region)) + 
  geom_bar(aes(fill=income), color='black', 
           position=position_stack(reverse=TRUE)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Building a Model

library(caTools)
set.seed(101)
sample <- sample.split(adult$income, SplitRatio = 0.7)
# Training data
train = subset(adult$income, sample==T)
# Testing data
test = subset(adult$income, sample==F)

model = glm(income ~ ., family = binomial(logit), data = train)
summary(model)

new.step.model <- step(model)
summary(new.step.model)

test$predicted.income = predict(model, newdata=test, type="response")
table(test$income, test$predicted.income > 0.5)

#the accuracy of our model
(6372+1423)/(6372+1423+548+872)
#recall
6732/(6372+548)
#precision
6732/(6372+872)
