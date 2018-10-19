# cargamos los documetos
rm(list = ls())

setwd("/Users/jobtoday/Downloads/")


# cargamos bas de datos

SurveyIncomplete <- read.csv("/Users/jobtoday/Downloads/SurveyIncomplete.csv")

#install.packages("caret")
#install.packages("rattle")
#install.packages("rpart.plot")
#install.packages("lattice")
#install.packages("ggplot2")

library(rattle)
library(rpart.plot)
library(caret)
library(rpart)
library(ggplot2)
library(randomForest)
library(lattice)
library(readxl)
library(randomForest)

SurveyComplete<- read_xlsx("/Users/jobtoday/Downloads/Survey_Key_and_Complete_Responses_excel.xlsx", "Survey Results Complete" )

#structura

str(SurveyComplete)

summary(SurveyComplete)

head(SurveyComplete)
tail(SurveyComplete)

# Pasamos de numerico a factor

SurveyComplete$car<- as.factor(SurveyComplete$car)

SurveyComplete$zipcode<- as.factor(SurveyComplete$zipcode)

SurveyComplete$brand<- as.factor(SurveyComplete$brand)

SurveyComplete$elevel = factor(x = SurveyComplete$elevel, levels = 0:4, ordered = T)

# Pasamos de numerico a factor en Survey Incomplete

SurveyIncomplete$car<- as.factor(SurveyIncomplete$car)

SurveyIncomplete$zipcode<- as.factor(SurveyIncomplete$zipcode)

SurveyIncomplete$brand<- as.factor(SurveyIncomplete$brand)

SurveyIncomplete$elevel = factor(x = SurveyIncomplete$elevel, levels = 0:4, ordered = T)


# Preproceso ( normalización)

preprocessParams <- preProcess(SurveyComplete[,c("credit", "age", "salary")], method=c("center", "scale"))
transformed <- predict(preprocessParams, SurveyComplete[,c("credit", "age", "salary")])
transformed

# creamos nuevo dataset normalizado para SurveyComplete

NormSurveyComplete <- cbind(transformed, SurveyComplete[c("elevel","zipcode","car","brand")])

# creamos nuevo dataset normalizado para SurveyIncomplete

preprocessParamsIncomplete <- preProcess(SurveyIncomplete[,c("credit", "age", "salary")], method=c("center", "scale"))
transformedIncomplete <- predict(preprocessParamsIncomplete, SurveyIncomplete[,c("credit", "age", "salary")])
transformedIncomplete
NormSurveyIncomplete <- cbind(transformedIncomplete, SurveyIncomplete[c("elevel","zipcode","car","brand")])

# preparamos el trainset del 75% 

inTraining <- createDataPartition(NormSurveyComplete$brand, p = .75, list = FALSE)

training <- NormSurveyComplete[inTraining,]

testing <- NormSurveyComplete[-inTraining,]

# creamos los 10 folds 

fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)


library(class)

# Generamos knn

ctrl <- trainControl(method="repeatedcv",repeats = 3) 
knnFit1 <- train(brand ~ ., data = training, method = "knn", trControl = ctrl, tuneLength = 10)


summary(knnFit1)

# make predictions

testknnFit1 <- predict(knnFit1, testing)

postResample(testknnFit1, testing$brand)

# quitamos variables 

knnFit2 <- train(brand ~age+salary, data = training, method = "knn", trControl = ctrl, tuneLength = 10)

testKnnFit2 <- predict(knnFit2, testing)

postResample(testKnnFit2, testing$brand)

# instalacion de paquetes para poder generar DT


library(rpart)

# generación del random forest 

ctrl <- trainControl(method="repeatedcv",number=10, repeats = 3)
rfFit1 <- train(brand ~ ., data = training, method = "rf", trControl = ctrl, ntree= 10, tuneLength = 10)
testrfFit1 <- predict(rfFit1, testing)
plot(rfFit)

# quitamos variables

rfFit2 <- train (brand~age+salary, data = training, method = "rf", trControl = ctrl, preProcess = c("center","scale"), ntree= 10, tuneLength = 10)

testrfFit2 <- predict(rfFit2, testing)

postResample(testrfFit2, testing$brand)




#confusionMatrixpara RF

confusionMatrix(testrfFit1, testing$brand )

#confusionMatrixpara RF

confusionMatrix(testrfFit2, testing$brand )


#confusionMatrixpara KnnFit

confusionMatrix(testknnFit1, testing$brand )

#confusionMatrixpara KnnFit

confusionMatrix(testKnnFit2, testing$brand )


# decision tree 

DTSurveyCompleted <- rpart(brand ~ ., data = training,  method ="class")
plot(DTSurveyCompleted)
text(DTSurveyCompleted)
fancyRpartPlot(DTSurveyCompleted)
print(DTSurveyCompleted)
# making predictions
testrFit <- predict(rfFit, testing)
postResample(testrFit, testing$brand)



# aplicamos modelo predictivo al SurveyIncomplete para rfFit2

postResample(testrfFit1, testing$brand)
postResample(testrfFit2, testing$brand)
postResample(testknnFit1, testing$brand)
postResample(testKnnFit2, testing$brand)
# hacemos predicciones 

FinalPredict <- predict(knnFit2, NormSurveyIncomplete)
summary(FinalPredict)



