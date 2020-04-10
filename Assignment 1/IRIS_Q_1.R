datasets::iris
colnames(iris)
data_ex1 <- iris
data_ex1
colnames(data_ex1)[colnames(data_ex1) == "Sepal.Length"] <- "SepalLength" # Rename column
colnames(data_ex1)[colnames(data_ex1) == "Sepal.Width"] <- "Sepalwidth" # Rename column
colnames(data_ex1)

#Display flowers details where species is setosa,Petalwidth is 0.2 and sepalwidth is greater than 3.
library(dplyr)
dat.query1 <- filter(data_ex1, Petal.Width == 0.2, Sepalwidth > 3, Species == "setosa")
dat.query1


#Display recordes in the descending order species and ascending order sepal length.
data_ex1
sort_data <- data_ex1[order(-as.numeric(as.factor(data_ex1[,'Species'])),data_ex1[,'SepalLength']),]
sort_data

#Display count of different types flowers
summary(data_ex1['Species'])

#Display min and max petal width,min and max sepal length
a = min(data_ex1['Petal.Width'])
b = max(data_ex1['Petal.Width'])
c = min(data_ex1['SepalLength'])
d = max(data_ex1['SepalLength'])
cat("For Petal Width [min : ", a ,  " | max :",b,"]" )
cat("For  Sepal Length [min : ", c ,  " | max :",d,"]" )

#Add new column ratio which stores ratio of sepal length and sepal width
data_ex1$new_column <- (data_ex1$SepalLength/data_ex1$Sepalwidth)
data_ex1