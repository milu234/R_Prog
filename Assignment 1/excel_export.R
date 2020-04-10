dataframe <- iris
dataframe
library(dplyr)
dataframe1 <- filter(dataframe, Species == "setosa")
dataframe2 <- filter(dataframe, Species == "versicolor")
dataframe3 <- filter(dataframe, Species == "virginica")
dataframe1
dataframe2
dataframe3

install.packages("xlsx" , dependencies = TRUE)
library(xlsx)
write.xlsx(dataframe1, file="IRIS.xlsx", sheetName="setosa", row.names=FALSE)
write.xlsx(dataframe2, file="IRIS.xlsx", sheetName="versicolor", append=TRUE, row.names=FALSE)
write.xlsx(dataframe3, file="IRIS.xlsx", sheetName="virginica", append=TRUE, row.names=FALSE)



my_data <- read.xlsx("IRIS.xlsx", sheetName = "setosa")
head(my_data,6)