
test_data <- read.csv("aps_failure_test_set.csv",
                      skip = 20, 
                      na.strings = "na")

training_data <- read.csv("aps_failure_training_set.csv",
                      skip = 20, 
                      na.strings = "na")


dim(training_data)
dim(test_data)
library(dplyr)
library(caret)
install.packages("caretEnsemble")
library(caretEnsemble)
install.packages("mice")
library(mice)
install.packages("doParallel")
library(doParallel)
library(car)

glimpse(training_data)
glimpse(test_data)

summary(training_data)

summary(test_data)

options(digits = 2)
prop.table(table(training_data$class))
prop.table(table(test_data$class))

options(scipen = 999)
summary_df <- do.call(cbind, lapply(training_data[, 
                                                  2:ncol(training_data)], summary))
summary_df_t <- as.data.frame(round(t(summary_df),0))
names(summary_df_t)[7] <- paste("Missing_values")
summary_df_t_2 <- summary_df_t %>% 
  mutate(obs = nrow(training_data),
         Missing_prop = Missing_values / obs)
print(summary_df_t_2)

summary_df_t_2 %>% summarise(Min = mean(Min.),
                             first_Q = mean(`1st Qu.`),
                             Median = median(Median),
                             Mean = mean(Mean),
                             third_Q = mean(`3rd Qu.`),
                             Max = max(Max.),
                             mean_MV = mean(Missing_values),
                             obs = mean(obs),
                             mean_MV_perc = mean_MV / obs)

#replicate our sets
training_data_bind <- training_data
test_data_bind <- test_data
#create a new column "set" to label the observations
training_data_bind$set <- "TRAIN"
test_data_bind$set <- "TEST"
#merge them into 1 single set
full_dataset <- rbind(training_data_bind, test_data_bind)
dim(full_dataset)

set.seed(123)
imputed_full <- mice(full_dataset, 
                     m=1, 
                     maxit = 5, 
                     method = "mean", 
                     seed = 500)

full_imputed <- complete(imputed_full, 1)
dim(full_imputed)

(na_count_full_imputed <-data.frame(sapply(full_imputed, function(y) sum(length(which(is.na(y)))))))

issue_columns <- subset(imputed_full$loggedEvents, 
                        meth == "constant" | meth == "collinear")
print(issue_columns)

#create vector of column names
issue_columns_names <- as.character(issue_columns[, "out"])
issue_columns_names <- issue_columns_names[-2]
print(issue_columns_names)

full_imputed_filtered <- full_imputed[ , !(names(full_imputed) %in% 
                                             issue_columns_names)]
dim(full_imputed_filtered)


#subset the full_imputed_filtered dataset
training_data_imp <- subset(full_imputed_filtered, set == "TRAIN")
test_data_imp <- subset(full_imputed_filtered, set == "TEST")
#drop the "set" column, we don't need it anymore
training_data_imp$set <- NULL
test_data_imp$set <- NULL
#check dimensions
dim(training_data_imp)
dim(test_data_imp)

cooksd <- cooks.distance(glm(class ~ ., 
                             family = "binomial", 
                             data = training_data_imp))

plot(cooksd, 
     pch="*", 
     cex=2, 
     main="Influential Obs by Cooks distance")  
abline(h = 4*mean(cooksd, na.rm=T), col="red")


outliers <- rownames(training_data_imp[cooksd > 4*mean(cooksd, na.rm=T), ])
print(outliers)

sum((cor(training_data_imp) > 0.5 | cor(training_data_imp) < -0.5) & cor(training_data_imp) < 1) / (162*162)


registerDoParallel(3)
getDoParWorkers()
registerDoParallel(3)
getDoParWorkers()
set.seed(123)
my_ctrl <- trainControl(method = "cv", 
                        number = 5,
                        classProbs = TRUE,
                        savePredictions = "final",
                        index = 
                          createResample(training_data_imp$class, 3),
                        sampling = "up",
                        allowParallel = TRUE)

model_list <- caretList(class ~ .,
                        data = training_data_imp,
                        methodList = c("glm", "nb"),
                        metric = "Kappa",
                        tuneList = NULL,
                        continue_on_fail = FALSE,  
                        preProcess = c("center", "scale"),
                        trControl = my_ctrl)

  
#Logistic Regression model
confusionMatrix(predict(model_list$glm,test_data_imp, type = "raw"), test_data_imp$class)
#Naive Bayes model
confusionMatrix(predict(model_list$nb,test_data_imp, type = "raw"), test_data_imp$class)


#**************************Clustering**********************************************************

wineUrl <- 'http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data'
wine <- read.table(wineUrl, header=FALSE, sep=',', 
                                       stringsAsFactors=FALSE, 
                                       col.names=c('Cultivar', 'Alcohol', 'Malic.acid', 
                                                                        'Ash', 'Alcalinity.of.ash', 
                                                                        'Magnesium', 'Total.phenols', 
                                                                        'Flavanoids', 'Nonflavanoid.phenols', 
                                                                        'Proanthocyanin', 'Color.intensity', 
                                                                        'Hue', 'OD280.OD315.of.diluted.wines', 
                                                                        'Proline' 
                                                     ))
head(wine)
wineTrain <- wine[, which(names(wine) != "Cultivar")]

set.seed(278613)
wineK3 <- kmeans(x=wineTrain, centers=3)

wineK3
install.packages("useful")
library(useful)

plot(wineK3, data=wineTrain)
plot(wineK3, data=wine, class="Cultivar")


set.seed(278613)
wineK3N25 <- kmeans(wineTrain, centers=3, nstart=25) 
wineK3$size
wineK3N25$size

wineBest <- FitKMeans(wineTrain, max.clusters=20, nstart=25, seed=278613)
wineBest

PlotHartigan(wineBest)

table(wine$Cultivar, wineK3N25$cluster)

plot(table(wine$Cultivar, wineK3N25$cluster),
     main="Confusion Matrix for Wine Clustering",
     xlab="Cultivar", ylab="Cluster")
library(cluster)
theGap <- clusGap(wineTrain, FUNcluster=pam, K.max=20) 



# logW curves
ggplot(gapDF, aes(x=1:nrow(gapDF))) +
  geom_line(aes(y=logW), color="blue") +
  geom_point(aes(y=logW), color="blue") +
  geom_line(aes(y=E.logW), color="green") +
  geom_point(aes(y=E.logW), color="green") +
  labs(x="Number of Clusters") 

# gap curve
ggplot(gapDF, aes(x=1:nrow(gapDF))) +
  geom_line(aes(y=gap), color="red") +
  geom_point(aes(y=gap), color="red") +
  geom_errorbar(aes(ymin=gap-SE.sim, ymax=gap+SE.sim), color="red") +
  labs(x="Number of Clusters", y="Gap")

wineH <- hclust(d=dist(wineTrain))
plot(wineH)


keep.cols <- which(!names(wbInfo) %in% c("iso2c", "country", "year", + "capital", "iso3c")) 
wbDaisy <- daisy(x=wbInfo[, keep.cols]) 
wbH <- hclust(wbDaisy) 


plot(wbH)

gapDF <- as.data.frame(theGap$Tab)
gapDF


wineH1 <- hclust(dist(wineTrain), method="single") 
wineH2 <- hclust(dist(wineTrain), method="complete")
wineH3 <- hclust(dist(wineTrain), method="average") 
wineH4 <- hclust(dist(wineTrain), method="centroid")
 plot(wineH1, labels=FALSE, main="Single")
 plot(wineH2, labels=FALSE, main="Complete")
 plot(wineH3, labels=FALSE, main="Average")
 plot(wineH4, labels=FALSE, main="Centroid")
 
 # plot the tree
 plot(wineH)
 # split into 3 clusters
 rect.hclust(wineH, k=3, border="red") 
 # split into 13 clusters
 rect.hclust(wineH, k=13, border="blue")
 
 diamonds
 
