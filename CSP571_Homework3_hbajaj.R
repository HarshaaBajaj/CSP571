# CSP571
# Homework 3

# 1.Load in the auto mpg data set: https://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/auto-mpg.data
auto <- read.table('https://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/auto-mpg.data',
                       header=FALSE)

# 2. Identify all of the categorical variables, all of the numeric variables
# and all of the binary variables.
names(auto)<-c("mpg","cylinders","displacement","horsepower","weight","acceleration","model year","origin","car name")
str(auto)
auto$horsepower <- as.numeric(levels(auto$horsepower))[auto$horsepower]
numVars <- names(which(sapply(auto, is.numeric)))
catVars <- names(which(sapply(auto, is.factor)))
binaryVars <- names(which(sapply(auto, is.logical)))
stopifnot(length(numVars) + length(catVars) + length(binaryVars)== ncol(auto))

# 3. Identify the appropriate descriptive statistics and graph for this data set.
# Execute on those and use the comments to discuss relevant relationships or insights discovered.
library(Hmisc)
describe(auto) 

hist(auto$mpg) #mpg: not normal distribution
#cylinders: majority of the cars have four cylinders
library(ggplot2)
ggplot(auto,aes(x=mpg,fill=factor(auto$cylinders)))+
  geom_histogram(binwidth = 3)+
  ggtitle("mpg and cylinders")+
  xlab("mpg")+
  ylab("total count")+
  labs(fill="cylinders")  #having 4 cylinders results in better mpg

hist(auto$displacement)#displacement: not normally distributed as mean>median
hist(auto$horsepower)#horsepower: +vely skewed
hist(auto$weight)#weight: +vely skewed
hist(auto$acceleration)#acceleration: normal distribution as mean~median
#model year
which.max(table(auto$`model year`)) #most frequent is 73 
ggplot(auto,aes(mpg))+ 
  geom_bar()+
  facet_wrap(~`model year`)   #as model year increases the mpg also seems increases

hist(auto$origin) #as from des. stats and histogram, frequency of 1 is highest
ggplot(auto,aes(x=mpg,fill=factor(auto$origin)))+
  geom_histogram(binwidth = 3)+
  ggtitle("mpg and origin")+
  xlab("mpg")+
  ylab("total count")+
  labs(fill="origin") 
mean(auto[which(auto$origin==1),"mpg"]) #avg mpg of cars with origin as 1 is 20.08353
plot(auto$`car name`) #car name
which.max(table(auto$`car name`)) #most frequent is ford pinto 
mean(auto[which(auto$`car name`=="ford pinto"),"mpg"]) #avg mpg of ford pinto cars is 22.91667

plot(auto) #as displacement increases, mpg decreases; as horsepower increases, mpg 
#decreases; as weight increases, mpg decreases; as acceleration increases, mpg increases;

# 4. Create a correlation matrix for all of the numeric variables.
correlations = cor(auto[numVars],use = "pairwise.complete.obs")
library(corrplot)
corrplot(correlations,type="lower",method="number") 
#cylinders is highly correlated with weight and displacement, weight is also highly correlated with displacement

# 5. Identify the columns (if any) with missing data.
#from output of three we know that horse power has 6 missing values.
sum(is.na(auto$horsepower))
Null_Counter <- apply(auto, 2, function(x) length(which(x == "" | is.na(x) | x == "NA" | x == "999" | x == "0"))/length(x))
Null_Counter 
auto<-na.omit(auto)
# 6. Divide the data into a train/test set (80% and 20% respectively) using stratified sampling
trainPct=.8
testPct=.2
library('caret')
inTrain <- createDataPartition(y = auto$mpg, p = trainPct, list = FALSE)
autoTrain <- auto[inTrain,]
autoTest <- auto[-inTrain,]
stopifnot(nrow(autoTest) + nrow(autoTrain) == nrow(auto))

# 7. Fit a linear model to the data using the numeric variables only. Calculate the R**2 on the test set.
numVars
m1<-lm(mpg~cylinders+displacement+horsepower+weight+acceleration+`model year`+origin,data=autoTrain)
summary(m1)
# Calculate R**2 on the test data
autoTest$mpgPredictedm1<- predict(m1, autoTest)
SST1 <- sum((autoTest$mpg - mean(autoTest$mpg))^2)
SSR1 <- sum((autoTest$mpgPredictedm1 - mean(autoTest$mpg))^2)
Rsqm1<-SSR1/SST1

# 8. Programmatically identify and remove the non-significant variables (alpha = .05). Fit a new model with those variables removed.
# Calculate the R**2 on the test set with the new model. Did this improve performance?
sig_var<-names(which((summary(m1)$coeff[-1,4]<=.05)))
m2<-lm(mpg~displacement+weight+`model year`+origin, data=autoTrain)
summary(m2)
# Calculate R**2 on the test data
autoTest$mpgPredictedm2<- predict(m2, autoTest)
SST2 <- sum((autoTest$mpg - mean(autoTest$mpg))^2)
SSR2 <- sum((autoTest$mpgPredictedm2 - mean(autoTest$mpg))^2)
Rsqm2<-SSR2/SST2 
#R square has slightly decreased by decimal points, possibly because weight and displacement are highly correlated. 

# 9. Attempt to fit a model on all of the relevant independent variables (including carName).
# Then calculate the R**2 on a test set. You will likely encounter an error.
# Explain why this error occurs. Fix this error.
m3<-lm(mpg~.,data = autoTrain)
summary(m3)
autoTest$mpgPredictedm3<- predict(m3, autoTest)
#error because car name is facctor with 305 levels from the training set, when we try 
#to predict from test set it encounters unidentified levels. To fix this we can
#preinclude the test set levels and then predict
m3$xlevels[["car name"]] <- union(m3$xlevels[["car name"]], levels(autoTest$`car name`))
autoTest$mpgPredictedm3<- predict(m3, autoTest)
# Calculate R**2 on the test data
SST3 <- sum((autoTest$mpg - mean(autoTest$mpg))^2)
SSR3 <- sum((autoTest$mpgPredictedm3 - mean(autoTest$mpg))^2)
Rsqm3<-SSR3/SST3  

# 10. Determine the relationship between model year and mpg.
# Interpret this relationship.
# Theorize why this relationship might occur.
ggplot(autoTrain,aes(mpg))+
  geom_bar()+
  facet_wrap(~`model year`)  # it looks like as the model year increases, mpg is also 
#increasing which makes sense as we can infer that better technology was developed 
which.max(table(auto$`model year`)) #most frequent is 73 
mean(auto[which(auto$`model year`==73),"mpg"]) #avg mpg of cars with model year as 73 is 17.1

# 11. Build the best linear model you can (as measured by R**2 on the test data)
# Record the value obtained in the comments below. Make sure to show all your code.
m4<-lm(mpg~cylinders+horsepower+acceleration+`model year`+origin,data=autoTrain)
summary(m4) 
# Calculate R**2 on the test data
autoTest$mpgPredictedm4<- predict(m4, autoTest)
SST4 <- sum((autoTest$mpg - mean(autoTest$mpg))^2)
SSR4 <- sum((autoTest$mpgPredictedm4 - mean(autoTest$mpg))^2)
Rsqm4<-SSR4/SST4 

m5<-lm(mpg~cylinders+horsepower+`model year`+origin,data=autoTrain)
summary(m5) 
# Calculate R**2 on the test data
autoTest$mpgPredictedm5<- predict(m5, autoTest)
SST5 <- sum((autoTest$mpg - mean(autoTest$mpg))^2)
SSR5 <- sum((autoTest$mpgPredictedm5 - mean(autoTest$mpg))^2)
Rsqm5<-SSR5/SST5 

m6<-lm(mpg~weight+`model year`+origin,data=autoTrain)
summary(m6)
# Calculate R**2 on the test data
autoTest$mpgPredictedm6<- predict(m6, autoTest)
SST6 <- sum((autoTest$mpg - mean(autoTest$mpg))^2)
SSR6 <- sum((autoTest$mpgPredictedm6 - mean(autoTest$mpg))^2)
Rsqm6<-SSR6/SST6

m7<-lm(mpg~displacement+`model year`+origin,data=autoTrain)
summary(m7)
# Calculate R**2 on the test data
autoTest$mpgPredictedm7<- predict(m7, autoTest)
SST7 <- sum((autoTest$mpg - mean(autoTest$mpg))^2)
SSR7 <- sum((autoTest$mpgPredictedm7 - mean(autoTest$mpg))^2)
Rsqm7<-SSR7/SST7

m8<-lm(mpg~poly(weight,2)+`model year`+origin,data=autoTrain)
summary(m8)
# Calculate R**2 on the test data
autoTest$mpgPredictedm8<- predict(m8, autoTest)
SST8 <- sum((autoTest$mpg - mean(autoTest$mpg))^2)
SSR8 <- sum((autoTest$mpgPredictedm8 - mean(autoTest$mpg))^2)
Rsqm8<-SSR8/SST8

m9<-lm(mpg~poly(displacement,2)+`model year`+origin,data=autoTrain)
summary(m9) 
# Calculate R**2 on the test data
autoTest$mpgPredictedm9<- predict(m9, autoTest)
SST9 <- sum((autoTest$mpg - mean(autoTest$mpg))^2)
SSR9 <- sum((autoTest$mpgPredictedm9 - mean(autoTest$mpg))^2)
Rsqm9<-SSR9/SST9 

compmat<-matrix(c(summary(m1)$adj.r.squared,summary(m2)$adj.r.squared,summary(m3)$adj.r.squared,summary(m4)$adj.r.squared,summary(m5)$adj.r.squared,summary(m6)$adj.r.squared,summary(m7)$adj.r.squared,summary(m8)$adj.r.squared,summary(m9)$adj.r.squared,Rsqm1,Rsqm2,Rsqm3,Rsqm4,Rsqm5,Rsqm6,Rsqm7,Rsqm8,Rsqm9,
                  summary(m1)$terms[[3]],summary(m2)$terms[[3]],summary(m3)$terms[[3]],summary(m4)$terms[[3]],summary(m5)$terms[[3]],summary(m6)$terms[[3]],summary(m7)$terms[[3]],summary(m8)$terms[[3]],summary(m9)$terms[[3]]),nrow = 9)
rownames<- c("M1","M2","M3","M4","M5","M6","M7","M8","M9")
colnames<- c("Adj R Squared","Test R Squared","Formula")
comdf<-as.data.frame(compmat,row.names = rownames)
names(comdf)<-colnames
View(comdf)
#Even though the highest Adj R squared is acheived in M3, I think M8 would be better as the model complexity is low and it has a good trade off between Adj R sq and test R sq

#reference : http://stackoverflow.com/questions/22315394/factor-has-new-levels-error-for-variable-im-not-using
