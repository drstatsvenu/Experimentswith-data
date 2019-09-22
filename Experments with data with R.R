getwd()
setwd("C:/Users/Sm/Desktop/Ou elective SEM3/Python")
getwd()


train<-read.csv("train.csv")
test<-read.csv("test.csv")
str(train)
str(test)

# Selectibg variables

train_cont= subset(train, select=c(ID,Age, Hours.Per.Week))
train_cat= subset(train, select=-c(ID,Age, Hours.Per.Week))

str(train_cont)
str(train_cat)

# Sumamry
summary(train_cont)
library(pastecs)

options(scipen = 100)
options(digits=2)
stat.desc(train_cont)

#Categorical Variables:
apply(train_cat, 2, function(x){length(unique(x))})

#
table(train_cat$Race)
as.matrix((prop.table(table(train_cat$Race))))

#############################3
# Multivariate Analysis
#############################3

library(gmodels)
CrossTable(train$Sex, train$Income.Group)

library(ggplot2)
ggplot(train,aes(Sex,fill = Income.Group))+geom_bar()+labs(title="Stacked Bar", x="Sex", y="count")+theme_bw()

ggplot(train,aes(Sex,Hours.Per.Week))+geom_boxplot()+labs(title="Box Plot")

#Checking missing values
table(is.na(train))
colSums(is.na(train))

library(mlr)

imputed_data= impute(train,classes = list(factor = imputeMode()))
train<-imputed_data$data
colSums(is.na(train))
imputed_data= impute(test,classes = list(factor = imputeMode()))
test<-imputed_data$data
colSums(is.na(test))

#####################33
sapply(train,class)
as.matrix(prop.table(table(train$Workclass)))
library(car)
# combining classes
train$Workclass <- recode (train$Workclass, "c('State-gov','Self-emp-inc', 'Federal-gov' ,'Without-pay', 'Never-worked')= 'Others'")
test$Workclass <- recode (test$Workclass, "c('State-gov','Self-emp-inc', 'Federal-gov' ,'Without-pay', 'Never-worked')= 'Others'")
as.matrix(prop.table(table(train$Workclass)))

# Predictive Modeling
table(train$Income.Group )

train$Income.Group <-ifelse(train$Income.Group=="<=50K",0,1)
table(train$Income.Group)
train<-subset(train,select= -c(ID))
sapply(train,class)

library(rpart)

set.seed(333)
train.tree<-rpart( Income.Group ~ ., data=train, method= "class", control=rpart.control(minsplit = 20, minbucket = 100,maxdepth = 10), xval=5)
summary(train.tree)
library(rpart.plot)
rpart.plot(train.tree)

predictions_train<-predict(train.tree,newdata=train,type='class')
str(predictions_train)
library(e1071)


predictions_test<-predict(train.tree,newdata=test,type='class')

library(caret)
confusionMatrix(predictions_train, as.factor(train$Income.Group))

solution.frame=data.frame(ID=test$ID, Income.Group=predictions_test)
write.csv(solution.frame, file="Final Solution.csv")


