#EDA
# this gives the current wd
getwd()
#set the wkd dir
setwd("E:/eBooks/C-DAC/R Programming Lab")
getwd()
# PART 1 - Summary Analysis
# An approach to unearth, summarize 
# and visualize the important characteristics of a dataset.
# Important properties to look at:
#   - Dimensions and size of dataset
# - Structure and variables
# - Types of variables
# - Frequencies and Mode
# - Percentiles
# - Measures of location/central tendency: Mean, Median
# - Measures of spread: Range, Variation
# - Measures of shape: Skewness, Kurtosis
# - etc.
mtcars
class(mtcars)
dim(mtcars)
names(mtcars)
object.size(mtcars)
head(mtcars)
tail(mtcars)
summary(mtcars)
str(mtcars)
#returns mean. missing values are removed, if #na.rm=TRUE. 
mean(mtcars$mpg, na.rm=TRUE)
median(mtcars$mpg, na.rm=TRUE)
#returns range (minimum and maximum) of object. 
# missing values are removed, if #na.rm=TRUE. 
range(mtcars$mpg,na.rm=TRUE)
var(mtcars$mpg,na.rm=TRUE)
sd(mtcars$mpg, na.rm=TRUE)
quantile(mtcars$mpg, probs=seq(0,1,0.25),na.rm=TRUE)


# PART 2 - Descriptive Analysis
# Extension of Summary Analylsis.
# Generally, both overlap each other.
# Helps in analysing large amounts of data 
# in simple and structured manner.
# Involves numerical and graphical methods
# to analyse the dataset.

# Refers to meausres of distribution, shape, central tendancy 
# and variability of a dataset with respect 
# to continuous variables mainly.

# Skewness: Refers to the symmetry (or assymmetry) of a distribution.
# - Can be positive or negative.
# 
# - Positive value: Distribution is right-skewed 
# i.e. mean is greater than median.
# 
# - Negative value: Distribution is left-skewed
# i.e. mean is less than median.
#Calculating skewness 
milage<-mtcars$mpg #By using package "moments" 
install.packages("moments")
library(moments) 
skewness(milage) #function of moments package


# Kurtosis: Refers to the tailedness (heavy-tailed or light-tailed) 
# of data relative to a normal distribution.
# - Can be positive or negative.
# 
# - Positive value: Positive or high value kurtosis 
# indicates tails or outliers. Said to be leptokurtic.
# 
# - Negative value: Negative kurtosis indicates a 
# flat data distribution (light tails or lack of outliers). 
# Said to be platykurtic.
# 
# - The normal distribution has zero kurtosis 
# (or Pearson's kurtosis as 3). Said to be mesokurtic.

#calculation of kurtosis 
kurtosis(milage) #reports Pearson's (proper) kurtosis (> or < 3). 
# function of moments package

