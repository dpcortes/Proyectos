
install.packages("kernlab")
library(kernlab)
data("spam")
str(spam[, 1:5])
set.seed(3435)
trainIndicator <- rbinom(4601, size = 1, prob = 0.5)
table(trainIndicator)
trainIndicator
##0    1 
##2314 2287 
trainSpam <- spam[trainIndicator== 1, ]
testSpam <- spam[trainIndicator== 0, ]
names(trainSpam)
##[1] "make"              "address"           "all"               "num3d"             "our"              
##[6] "over"              "remove"            "internet"          "order"             "mail"             
##[11] "receive"           "will"              "people"            "report"            "addresses"        
##[16] "free"              "business"          "email"             "you"               "credit"           
##[21] "your"              "font"              "num000"            "money"             "hp"               
##[26] "hpl"               "george"            "num650"            "lab"               "labs"             
##[31] "telnet"            "num857"            "data"              "num415"            "num85"            
##[36] "technology"        "num1999"           "parts"             "pm"                "direct"           
##[41] "cs"                "meeting"           "original"          "project"           "re"               
##[46] "edu"               "table"             "conference"        "charSemicolon"     "charRoundbracket" ##
##[51] "charSquarebracket" "charExclamation"   "charDollar"        "charHash"          "capitalAve"       
##[56] "capitalLong"       "capitalTotal"      "type"             
head(trainSpam)
table(trainSpam$type)
plot(trainSpam$capitalAve ~ trainSpam$type)
plot(log10(trainSpam$capitalAve + 1) ~ trainSpam$type)
plot(log10(trainSpam[, 1:4]+ 1))
hCluster <- hclust(dist(t(trainSpam[ , 1:57])))
hClusterUpdated = hclust(dist(t(log10(trainSpam[ , 1:55]+ 1))))
plot(hClusterUpdated)
trainSpam$numType = as.numeric(trainSpam$type) - 1
costFunction = function(x,y) sum(x!= (y > 0.5))
cvError = rep(NA, 55)
library(boot)
for(i in 1:55){
  lmFormula = reformulate(names(trainSpam)[i], response = "numType")
  gmlFit = glm(lmFormula, family = "binomial", data = trainSpam)
  cvError[i] = cv.glm(trainSpam, gmlFit, costFunction, 2)$delta[2]
}
##Which predictor has minimun cross-validated error
names(trainSpam)[which.min(cvError)]
##[1] "charDollar"
##Use the best model from the group
predictionModel = glm(numType ~ charDollar, family = "binomial", data = trainSpam)
##Get predictions on the test set
predictionTest = predict(predictionModel, testSpam)
predictedSpam = predict(predictionModel, testSpam)
predictedSpam = rep("nonspam", dim(testSpam)[1])
##Clasify as spam for those with prob >0.5
predictedSpam[predictionModel$fitted > 0.5] = "spam"
##Clasification table
table(predictedSpam, testSpam$type)
##predictedSpam nonspam spam
##nonspam    1346  458
##spam         61  449
##Error Rate
(61+458)/(1346+61+458+449)
##[1] 0.2242869


v1 <- data.frame(typo=c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3), rnorm(15))