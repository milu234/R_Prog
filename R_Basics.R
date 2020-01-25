vtr <- c(150,200,250,300,350)
option <- "median"
switch(option,
       "mean" = print(mean(vtr)),
       "mode" = print(mode(vtr)),
       "median" = print(median(vtr)),
       print("Invalid Input"))
