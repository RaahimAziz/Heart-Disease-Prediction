data = read.csv(file.choose())
str(data)
anyNA(data)

#Changing the datatypes:

data$Sex = factor(data$Sex)
data$Sex = as.numeric(data$Sex)

data$ChestPainType = factor(data$ChestPainType)
data$ChestPainType=as.numeric(data$ChestPainType)

data$RestingECG = factor(data$RestingECG)
data$RestingECG = as.numeric(data$RestingECG)

data$ExerciseAngina = factor(data$ExerciseAngina)
data$ExerciseAngina=as.numeric(data$ExerciseAngina)

data$ST_Slope = factor(data$ST_Slope)
data$ST_Slope=as.numeric(data$ST_Slope)

#convert output into factor:
data$HeartDisease=factor(data$HeartDisease)

#finding the vip variable:
library(caret)
library(vip)

vipv = train(HeartDisease~., metod = "glm", data = data, family= "binomial",
             trControl = trainControl(method = "cv", number = 10))

plot = vip(vipv, num_features = 10)



#Makiing another dataframe only for significant variables:

heart_data = data[c("ST_Slope","ChestPainType","Oldpeak","MaxHR",
                    "Cholesterol","ExerciseAngina","Age",
                    "RestingBP","Sex", "HeartDisease")]
str(heart_data)

#Sampling:
set.seed(9654694)
smp = sample(2, nrow(heart_data), replace = TRUE, prob = c(0.8,0.2))
train= heart_data[smp==1,]
test = heart_data[smp==2,]


#logistic regresison:

logmod = train(HeartDisease~., data = train, family="binomial",
               method = "glm", trControl = trainControl(method = "cv",
                                                        number = 10))

logpred = predict(logmod, test)
logtab = table(logpred, test$HeartDisease)






#0.85


#Decision Tree:
library(party)

dectree = ctree(HeartDisease~., data = train,
                controls = ctree_control(mincriterion = 0.9999,
                                         minsplit = 6))
plot(dectree)

decpred = predict(dectree, test)
dectab = table(decpred, test$HeartDisease)

#0.77


#SVM:

trctrl = trainControl(method = "repeatedcv", number = 10, classProbs = F,
                      repeats = 3)

svm = train(HeartDisease~., method = "svmLinear", data = train,
            trainControl=trctrl, preProcess= c("center","scale"),
            tuneLength = 10)

svmpred = predict(svm, test)
svmtab = table(svmpred, test$HeartDisease)


#0.84















