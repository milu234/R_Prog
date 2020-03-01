install.packages('dplyr')
library('dplyr')
set.seed(1234)
data_frame <- tibble(  
  c1 = rnorm(50, 5, 1.5),   
  c2 = rnorm(50, 5, 1.5),  
  c3 = rnorm(50, 5, 1.5),
  c4 = rnorm(50, 5, 1.5), 	
  c5 = rnorm(50, 5, 1.5)
)

df

# Sort by c1
df <- data_frame[order(data_frame$c1),]
head(df)

# Sort by c3 and c4
df <- data_frame[order(data_frame$c3, data_frame$c4)]
head(df)


# Sort by c3(descending) and c4(acending)
df <-data_frame[order(-data_frame$c3, data_frame$c4),]
head(df)


