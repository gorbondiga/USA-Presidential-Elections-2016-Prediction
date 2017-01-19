#####################################
options(warn=-1)
#Presidential Election Data Analysis
#Code to read in the data files to R
#Unfried 10/4/16
#####################################
rm(list=ls())
#Have fun! You can do the same thing to read in the fulldata.dta file
#Haven package has function to read .dta files into R
#You will need to install it before you load it
library(haven)
#Hmisc package has "describe" function used below
#You will need to install it before you load it
library(Hmisc)
library(gdata)
library(car)
library(boot)
library(caret)
library(ggplot2)


#read_dta function is from Haven package; choose.file() does not work
polldata <- read_dta("polldata.dta")
fulldata <- read_dta("fulldata_10112016.dta")
#otherwise polldata is a "tibble" instead of a data frame
pd <- as.data.frame(polldata)
fd <- as.data.frame(fulldata)

source("functions.R")


#Variables for global model:
#qual <- c("gender", "race", "bornus") 
#quant <- c("education", "age", "hhincome", "statereside")
globalV <- c("gender", "race", "bornus", "education", "age", "hhincome", "statereside") #<- rbind(qual, quant)


#Put as factor each necesary variable:
#fd$statereside = as.factor(fd$statereside)
fd$gender = as.factor(fd$gender)
fd$race = as.factor(fd$race)
fd$bornus = as.factor(fd$bornus)

#Get the data cleaned for fd dataset.
data = getNewData(fd, globalV)
data = as.data.frame(data)

#Loop for getting the model for each state:
states <- states_list(data) #get a list with the data of each state in each position of the list.
modelS = c()

states.prediction <- as.data.frame(matrix(0,1:56, 1:6))

for(i in 1:56) {
  id <- i
  if(i != 3 && i != 7 && i != 14 && i != 43 && i != 52 ) { #there are not state 3, 7...
    newdata = fixData(states[[i]]) # fix the data for not having varibles that not change their values in all of the row.
    
    model <- glm(formula = clint ~ . , family = binomial(), data = newdata)
    #cross <- cv.glm(newdata, model, K = 10)
    
    #keeping the prediction of each state.
    p <- predict(model, data = subset(newdata, select = c(1:length(newdata)-1)), type = 'response')
    p.states <- ifelse(p > 0.5, 1, 0)
    
    clinton <- sum(p.states == 1) / length(p)
    trump <- sum(p.states == 0) / length(p)
    prob <- mean(p)
    
    
    states.prediction[i, 1] <- id #states id number
    states.prediction[i, 2] <- NA #Addig Na so that we could add name of each state latter in this place. 
    states.prediction[i, 3] <- clinton #prob of clinton to winnig depends on the model
    states.prediction[i, 4] <- trump #prob of trump for winning depends on the model
    states.prediction[i, 5] <- prob #this is a probability of the mean of the odds for clinton winning
    a<-na.omit(newdata$clint)
    states.prediction[i, 6] <- sum(a==1) / length(a) #probability of clinto winning depending on the dataset, counting how many people says they will vote clinton.
    
    #Print every number i of state and summary of the model of the state[i].
##  print(i)
##  print(summary(model)$coef)
    #print(vif(model))
  }
}

states.prediction[1:2, 2] <- state.name[1:2]
states.prediction[4:6, 2] <- state.name[3:5]
states.prediction[8:10, 2] <- state.name[6:8]
states.prediction[11, 2] <- "District of Columbia"
states.prediction[12:13, 2] <- state.name[9:10]
states.prediction[15:42, 2] <- state.name[11:38]
states.prediction[44:51, 2] <- state.name[39:46]
states.prediction[53:56, 2] <- state.name[47:50]

states.prediction[states.prediction$names == "NA"] <- NA

names(states.prediction) <- c("id", "names", "clinton", "trump", "clinton_mean_prob"
                               , "no_model_mean")


#-------------------------------------------------------------------------------------------------

#Logistic regression model for clinton prediction if seh loses or wins. 
  modelG <- glm(formula = clint ~ ., family = binomial(), data = data)
  #cVG <- cv.glm(data, modelG)


#start prediction procees for the actual data:
global.pred <- predict(modelG, data = subset(data, select = c(1:length(data)-1)), type = 'response')
fitted.pred <- ifelse(global.pred > 0.5, 1, 0)


global.predictions <- data.frame(matrix(0, 1:56, 1:5))

#each vector means the same as sin the states.prediction, but this is for the global prediction of the model.
global.predictions[,1] <- 1
global.predictions[,2] <- sum(fitted.pred == 1) / length(fitted.pred)
global.predictions[,3] <- sum(fitted.pred == 0) / length(fitted.pred)
global.predictions[,4] <- mean(global.pred)
a<-na.omit(data$clint)
global.predictions[,5] <- sum(a==1) / length(a)


names(global.predictions) <- c("id", "clinton", "trump", "clinton_mean_prob", "no_model_mean")

##ROC curve analysis with cross validation test:
library(ROCR)
test <- as.data.frame(data[200000:length(data),])

p <- predict(modelG, newdata=subset(test,select=c(1:length(test)-1)), type="response")
pr <- prediction(p, test$clint)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]

##Graphics for every variable:
raceGraph <- ggplot(na.omit(data), aes(race))+geom_bar(aes(fill=clint), position="dodge")
ageGraph <- ggplot(na.omit(data), aes(age))+geom_bar(aes(fill=clint), position="dodge")
genderGraph <- ggplot(na.omit(data), aes(gender))+geom_bar(aes(fill=clint), position="dodge")
educationGraph <- ggplot(na.omit(data), aes(education))+geom_bar(aes(fill=clint), position="dodge")
bornusGraph <- ggplot(na.omit(data), aes(bornus))+geom_bar(aes(fill=clint), position="dodge")
hhincomeGraph <- ggplot(na.omit(data), aes(hhincome))+geom_bar(aes(fill=clint), position="dodge")
stateGraph <- ggplot(na.omit(data), aes(statereside))+geom_bar(aes(fill=clint), position="dodge")


##Create table of stadistics prediction results, with the auc value in each prediction: 

