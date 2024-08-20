#dataset of stresslevel.csv
u=read.csv(file.choose())
View(u)
table(u$stress_level)
u$stress_level=factor(u$stress_level,levels = c(0,1,2),labels = c(0,1,2))
sum(is.na(u))
u[-21]=scale(u[-21])
library(caTools)
set.seed(100)
sp=sample.split(u$stress_level,SplitRatio = 0.70)
trainu=subset(u,sp==T)
testu=subset(u,sp==F)
#knn
library(class)
iknn=knn(train=trainu,test=testu,cl=trainu$stress_level,k=25)
library(caret)
confusionMatrix(testu$stress_level,iknn)
#naive bayes
library(e1071)
inb=naiveBayes(trainu[-21],trainu$stress_level)
ipre=predict(inb,testu[-21])
confusionMatrix(ipre,testu$stress_level)
#decision tree
library(rpart)
idt=rpart(formula = stress_level~.,data=trainu)
idpre=predict(idt,testu[-21],type="class")
confusionMatrix(idpre,testu$stress_level)
#randomForest
library(randomForest)
irf=randomForest(x=trainu[-21],y=trainu$stress_level,ntrees=25)
irpre=predict(irf,testu[-21])
confusionMatrix(irpre,testu$stress_level)
#svm
library(e1071)
u3=svm(formula=stress_level~.,data=trainu,kernel="linear",type="C-classification")#kernel is fun or method which we use
pre=predict(c3,testu)
library(caret)
confusionMatrix(pre,testu$stress_level)
