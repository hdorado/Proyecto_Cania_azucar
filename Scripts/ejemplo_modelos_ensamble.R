

# Training and comparison of models
# Hugo Andres Dorado B.
# 2020

rm(list = ls())


library(caret)
library(caretEnsemble)
library(randomForest)
library(nnet)
library(kernlab)
library(plyr)
library(elasticnet)
library(monomvn)
library(brnn)
library(snowfall)
library(plyr)
library(tidyr)
library(agricolae)
library(ggplot2)

dataset <- read.table('Datos/wpbc.data',sep=',',row.names = 1)[,-34]

dataset$V2

summary(dataset)


#--------------------------Training model--------------------------------------

set.seed(123)

CBinTrainDP   <-  createDataPartition(y=dataset$V2,p=0.60,
                                        list=FALSE)
  
CBTraininig <- dataset[CBinTrainDP,]
  
CBTesting   <- dataset[-CBinTrainDP,]
  
  
my_control <- trainControl(
 method = "repeatedcv",
 number = 5,
 repeats = 3,
 classProbs=TRUE
)

dimMat <- 1
  
# (2, 18, 40). http://www3.ntu.edu.sg/eee/icis/cv/publications/HGB/HGBPaperinIEEE03Mar.pdf
  
model_list_big <- caretList(x = CBTraininig[,-dimMat], y = CBTraininig[,dimMat],
                              trControl = my_control,
                              tuneList = list(
                                mlp <- caretModelSpec(method = 'mlp',
                                                      preProcess = 'range',
                                                      tuneGrid=expand.grid(size=c(2,18,40))),
                                #brnn <- caretModelSpec(method = 'brnn', preProcess = 'range',
                                #                      tuneGrid=expand.grid(neurons=c(2,18,40))),
                                #las <- caretModelSpec(method = 'lasso',
                                #                      preProcess = 'range'),
                                rf  <- caretModelSpec(method ='rf'),
                                cf  <- caretModelSpec(method ='cforest')
                                # svm <- caretModelSpec(method ='svmRadial'),
                                # ridge <- caretModelSpec(method ='ridge'),
                                # lm <- caretModelSpec(method ='lm')
                              )
)

greedy_ensemble <- caretEnsemble(model_list_big)

summary(greedy_ensemble)

p <- as.data.frame(predict(model_list_big,newdata=CBTesting[,-dimMat]))
  
modelCor(resamples(model_list_big))
  
predictModels <- lapply(model_list_big, predict, newdata=CBTesting[,-dimMat])
  
predictModels$ensemble <- predict(greedy_ensemble,newdata=CBTesting[,-dimMat])
  
sapply(predictModels,function(x){postResample(x,CBTesting[,dimMat])})

# Solo para modelos condicionales

confusionMatrix(predictModels$rf,CBTesting[,dimMat])

confusionMatrix(predictModels$ensemble,CBTesting[,dimMat])



