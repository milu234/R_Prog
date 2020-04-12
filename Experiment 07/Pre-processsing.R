data("iris")
dat <- iris
head(dat)
#*******************************************************************************
#For filling out the missing values
#dataset$column_header: Selects the column in the dataset specified after $ (sepal.Length, Sepal.Width, Petal.Length, Petal.width).
#is.na(dataset$column_header): This method returns true for all the cells in the specified column with no values.
#ave(dataset$column_header, FUN = function(x) mean(x, na.rm = 'TRUE')): Ths method calculates the average of the column passed as argument.

dat$Sepal.Length = ifelse(is.na(dat$Sepal.Length),ave(dat$Sepal.Length,FUN = function(x) mean(x, na.rm = 'TRUE')),dat$Sepal.Length)
dat$Sepal.Width = ifelse(is.na(dat$Sepal.Width),ave(dat$Sepal.Width,FUN = function(x) mean(x, na.rm = 'TRUE')),dat$Sepal.Width)
dat$Petal.Length = ifelse(is.na(dat$Petal.Length),ave(dat$Petal.Length,FUN = function(x) mean(x, na.rm = 'TRUE')),dat$Petal.Length)
dat$Petal.Width = ifelse(is.na(dat$Petal.Width),ave(dat$Petal.Width,FUN = function(x) mean(x, na.rm = 'TRUE')),dat$Petal.Width)
dat
#-------------------------------------------------------------------------------------


#*************************************************************************************
#Since we are not interested in having decimal places for the petals and sepal we will round it up using the roundup function
dat$Sepal.Length = as.numeric(format(round(dat$Sepal.Length,0)))
dat$Sepal.Width = as.numeric(format(round(dat$Sepal.Length,0)))
dat$Petal.Length = as.numeric(format(round(dat$Sepal.Length,0)))
dat$Petal.Width = as.numeric(format(round(dat$Sepal.Length,0)))
dat
#-------------------------------------------------------------------------------------

#*************************************************************************************
##Dealing With Categorical Data
#Categorical variables represent types of data which may be divided into groups. Examples of categorical variables are race, sex, age group, educational level etc.
#In our dataset, we have one categorical feature i.e. Species. In R we can use the factor method to convert texts into numerical codes.
dat$Species= factor(dat$Species, levels = c('setosa','versicolor','virginica'), labels = c(1,2,3))
dat
#-------------------------------------------------------------------------------------


#*************************************************************************************
install.packages('caTools') #install once
library(caTools)
set.seed(123)
split = sample.split(dat$Species, SplitRatio = 0.8)
training_set = subset(dat, split == TRUE)
test_set = subset(dat, split == FALSE)
#-------------------------------------------------------------------------------------

#*************************************************************************************
training_set[,1:4] = scale(training_set[,1:4])
training_set
test_set[,1:4] = scale(test_set[,1:4])
test_set
nrow(test_set)
