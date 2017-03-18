# CSP/MATH 571

# Question 1: Create a variable named "myName" and assign to have a value of your
# preferred name. Create a varaible named "myEmail" and assign it to have a value
# of your email.
myName <- "Harshaa Bajaj"
myEmail <- "hbajaj@hawk.iit.edu"


# Question 2: Create a vector of integers from 100 to 10000 (inclusive). Assign
# the variable myVector. Return the sum, min, max, and median of this vector and assign it
# below.
a<-seq(100,10000)
myVector<-a
q2Sum<-sum(myVector)
q2Min<-min(myVector)
q2Max<-max(myVector)
q2Median<-median(myVector)

# Question 3: Write a function that accepts a number as an input returns
# TRUE if that number is divisible by 127 FALSE if that number is not divisible
# by 127.  For example, divis(127*5) should return TRUE and divis(80)
# should return FALSE. Hint: %% is the modulo operator in R.

divis <- function(num)
{
  if(num%%127==0)
    return (TRUE)
  else
    return (FALSE)
}

# Question 4: Using the function you wrote for Question 3 and the vector you
# defined in Question 2, deterine how many integers between 100 and 10000 are
# divisible by 127. Assign it to the variable below.
countDivis<-0
for(i in myVector){
  if (divis(i)== TRUE)
    countDivis= countDivis+1
}

# Question 5: Using the vector of names below, write code to return the 9
# last name in the vector.
names <- c("Kermit Chacko",
           "Eleonore Chien",
           "Genny Layne",
           "Willene Chausse",
           "Taylor Lyttle",
           "Tillie Vowell",
           "Carlyn Tisdale",
           "Antione Roddy",
           "Zula Lapp",
           "Delphia Strandberg",
           "Barry Brake",
           "Warren Hitchings",
           "Krista Alto",
           "Stephani Kempf",
           "Sebastian Esper",
           "Mariela Hibner",
           "Torrie Kyler")

substr(names[9],6,nchar(names[9]))

# Question 6: Using the vector "names" from Question 5, write code to
# determine how many last names start with L.

countLastNameStartsWithL<-length(names[grep(" L",names)])

# Question 7: Using the vector "names" from Question 5, write code to create a
# list that allows the user to input a first name and retrieve the last name.
# For example, nameMap["Krista"] should return "Alto".

nameMap<-function(x){
  sapply(strsplit(names[grep(x,names)]," "),"[",2)
}

# Question 8: Load in the "Adult" data set from the UCI Machine Learning
# Repository. http://archive.ics.uci.edu/ml/datasets/Adult
# Load this into a dataframe. Rename the variables to be the proper names
# listed on the website. Name the income attribute (">50K", "<=50K") to be
# incomeLevel

adult_data <- read_csv("/adult.data.csv", col_names = FALSE)
colnames(adult_data)=c("age","workclass","fnlwgt","education","education-num","marital-status","occupation","relationship","race","sex","capital-gain","capital-loss","hours-per-week","native-country","incomeLevel")

# Question 9: Create a new variable called workSector. Label all government
# employees as "government", all self-employeed employees as "selfEmployed",
# all Private employees as "Private" and everyone else as "Other".

adult_data$workclass=as.factor(adult_data$workclass)
sector=function(x){
  if((x=="State-gov")||(x=="Federal-gov")||(x=="Local-gov")){
    return("government")
  }
  else if((x=="Self-emp-not-inc")||(x=="Self-emp-inc")){
    return("selfEmployed")
  }
  else if(x=="Private"){
    return("Private")
  }
  else{
    return ("Other")
  }
}
class=NULL
for(i in 1:nrow(adult_data)){
  class=c(class,sector(adult_data[i,"workclass"]))
}
adult_data$workSector=class

# Question 10: Create a histogram of the 'age'. Hint: You'll need to convert
# age to be numeric first. Save this histogram and include it with your
# submission

adult_data$age=as.numeric(adult_data$age)
hist(adult_data$age,xlab = "Age" , main = "Histogram of Age",col = blues9)

# Question 11: Determine the top 3 occupations with the highest average hours-per-week
# Hint: One way to do this is to use tapply

avg=tapply(adult_data$`hours-per-week`,adult_data$occupation,mean)
head(avg[sort.list(avg,decreasing = T)],n=3)

#references:
https://www.r-bloggers.com/r-function-of-the-day-tapply-2/
http://stackoverflow.com/questions/2453326/fastest-way-to-find-second-third-highest-lowest-value-in-vector-or-column
