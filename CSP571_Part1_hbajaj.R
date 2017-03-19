# Please note that there are two  parts, an R file and a text document.
# Load in the Boston Housing data set using the code below.
install.packages('mlbench')
library('mlbench')
data(BostonHousing)

# 1. Create a scatterplot matrix of all variables in the data set. Save your output.
pairs(BostonHousing)

# 2. For each numeric variable in BostonHousing, create a separate boxplot using
# "Method 2" listed in the class notes. Do this programmatically; meaning do
# not simply hardcode the creation of every boxplot. Instead, loop over the
# approriate columns and create the boxplots. Save your output. Ensure your boxplots
# all have proper titles
bp<-function(x){
  for(i in 1:ncol(x)){
    a<-x[,i]
    if(class(a)=='factor')
      print("variable is non numeric")
    else 
      boxplot(a, main=names(x[i]))
  }
}
par(mfrow=c(1,3))
bp(BostonHousing)

# 3. Create a correlation matrix and correlation plot
# for the BostonHousing data set. Save your output.
install.packages("corrplot")
library('corrplot')
numeric_var <- sapply(BostonHousing, is.numeric)
cor_mat <- cor(BostonHousing[, numeric_var])
corrplot(cor_mat,type="lower", method = "number", diag = TRUE)

# 4. Identify the top 3 strongest absolute correlations in the data set. Save your output.
cor_mat[upper.tri(cor_mat,diag=TRUE)]=NA
cor_mat=as.data.frame(as.table(cor_mat))
cor_mat=na.omit(cor_mat)
cor_mat=cor_mat[order(-abs(cor_mat$Freq)),]
strong_cor<-cor_mat[1:3,]

# 5. Create a new variable call ageGroup quartiles. Divide the age variable
# into four even sections and assign it to one quartile.
BostonHousing$ageGroup_quartiles<-cut(BostonHousing$age,quantile(BostonHousing$age,probs = seq(0, 1, 0.25)))

# 6. Go to the website listed below. Convert the html table into a
# dataframe with columns NO, Player, Highlights
library('rvest')
library('tidyr')
url = 'http://www.espn.com/nfl/superbowl/history/mvps'
webpage <- read_html(url)
sports <- html_nodes(webpage, css = 'table')
superbowl <- html_table(sports)[[1]]
superbowl <- superbowl[-(1:2), ]
names(superbowl) <- c("NO", "Player", "Highlights")

# 7.Extract the names of the MVPs, Position and Team into columns
# MVP1, MVP2, Position, Team
superbowl <- separate(superbowl, Player, c('MVPs', 'Position','Team'), sep=', ', remove=TRUE)
superbowl <- separate(superbowl, MVPs, c('MVP1', 'MVP2'), sep=' &', remove=TRUE)

# 8. Determine the 90th%, 92.5th%, 95th%, 97.5th% and 99th% confidence intervals
# for the mean of passing yards
# (as listed in "Highlights" column) for quarterbacks.
# Note that there are a few intermediate steps you'll probably want to do
# in order to accomplish this. I am purposelly leaving that up to you, so that
# you are starting to develop the types of problem solving skills you'll need
# to tackle these problems in the wild.
superbowl <- separate(superbowl, Highlights, c('yards', 'Others'), sep=' yards passing, ', remove= FALSE)
superbowl$yards <- as.numeric(superbowl$yards)  #to just get the numbers cuz anythin else will become NA in R

t.test(superbowl$yards, conf.level = .99)
t.test(superbowl$yards, conf.level = .975)
t.test(superbowl$yards, conf.level = .95)
t.test(superbowl$yards, conf.level = .925)
t.test(superbowl$yards, conf.level = .90)

# 9. The following contains data on the calorie counts of four types
# of foods. Perform an ANOVA and determine the Pr(>F)
library(reshape2)
food1 <- c(164,   172,   168,   177, 	156, 	195)
food2 <- c(178,   191, 	197, 	182, 	185, 	177)
food3 <- c(175, 	193, 	178, 	171, 	163, 	176)
food4 <- c(155, 	166, 	149, 	164, 	170, 	168)
dftry<-data.frame(food1,food2,food3,food4)
dftry<-melt(dftry)
fit <- aov(value ~ variable,data=dftry)
summary(fit)

# 10. Install the lubridate package and familarize yourseslf with it.
# This is the preferred package for handling
# dates in R, which is an important skill.
# Practing handling dates by using the package to determine how many
# Tuesdays fell on the first of the month
# during the 19th century (1 Jan 1801 to 31 Dec 1901).
install.packages('lubridate')
library('lubridate')
days=seq(from=as.Date("1801/1/1"), to=as.Date("1901/12/31"), by="month")
sum(weekdays(days) == "Tuesday")

#reference: http://stackoverflow.com/questions/34575270/project-euler-19-in-r
