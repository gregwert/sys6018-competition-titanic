
library(tidyverse)
library(dplyr)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

train = read_csv("train.csv")
test = read_csv("test.csv")

# find titles
train1 = train
train1["Title"] = gsub('(.*, )|(\\..*)', '', train$Name)


#fill in missing age values
ageFit = rpart(Age~Pclass+Fare+SibSp+Parch+Sex+Embarked+Title,data=train1, method="anova")
agesPredictList = train1$Age
agePredictions = predict(ageFit, train1, method="anova")

train2 = train1
for (i in 1:length(train1$PassengerId)) {
  if (is.na(train$Age[i])) {
    train2$Age[i] = agePredictions[i]
  }
}

#Classify as chid or not
train3 = train2
train3["Child"] = 0
train3$Child[train3$Age >= 18] = 1


fit = rpart(Survived ~ Pclass+Sex+Age+SibSp+Parch+Fare+Embarked+Title+Child, data=train3, method="class")

fancyRpartPlot(fit)

# add test titles
test1 = test
test1["Title"] = gsub('(.*, )|(\\..*)', '', test$Name)

#Classify as chid or not (test)
test2 = test1
test2["Child"] = 0
test2$Child[test2$Age >= 18] = 1
test2[415,12] = "Miss"

prediction = predict(fit, test2, type = "class")

final = as.data.frame(test$PassengerId)
colnames(final) = "PassengerId"
final["Survived"] = prediction

write.csv(final, file= "gaw8pa-Titanic-predictions.csv", row.names=FALSE)


