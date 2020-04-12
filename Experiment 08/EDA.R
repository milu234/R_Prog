# Loading funModeling!
install.packages("funModeling")
library(funModeling)
library(dplyr)
data(heart_disease)
data_set <- heart_disease
View(data_set)

# Profiling the data input
df_status(heart_disease)


# Profiling the Data Input
my_data_status=df_status(heart_disease, print_results = F)

# Removing variables with 60% of zero values
vars_to_remove=filter(my_data_status, p_zeros > 60)  %>% .$variable
vars_to_remove

# Keeping all columns except the ones present in 'vars_to_remove' vector
heart_disease_2=select(heart_disease, -one_of(vars_to_remove))

#Ordering data by percentage of zeros

arrange(my_data_status, -p_zeros) %>% select(variable, q_zeros, p_zeros)


# Total rows
nrow(heart_disease)


# Total columns
ncol(heart_disease)

# Column names
colnames(heart_disease)

freq(data=heart_disease, input = c('thal','chest_pain'))

# Just keeping two variables to use in this example
heart_disease_3=select(heart_disease, thal, chest_pain)

# Profiling the data!
describe(heart_disease_3)


#******************************************************************************************************************


library(Hmisc)

# Loading data from the book repository without altering the format
data_world=read.csv(file = "https://goo.gl/2TrDgN", header = T, stringsAsFactors = F, na.strings = "..")

# Excluding missing values in Series.Code. The data downloaded from the web page contains four lines with "free-text" at the bottom of the file.
data_world=filter(data_world, Series.Code!="")

# The magical function that keeps the newest values for each metric. If you're not familiar with R, then skip it.
max_ix<-function(d) 
{
  ix=which(!is.na(d))
  res=ifelse(length(ix)==0, NA, d[max(ix)])
  return(res)
}

data_world$newest_value=apply(data_world[,5:ncol(data_world)], 1, FUN=max_ix)

# Printing the first three rows
head(data_world, 3)

View(data_world)

# Get the list of indicator descriptions.
names=unique(select(data_world, Series.Name, Series.Code))
head(names, 5)



# Convert a few
df_conv_world=data.frame(
  new_name=c("urban_poverty_headcount", 
             "rural_poverty_headcount", 
             "gini_index", 
             "pop_living_slums",
             "poverty_headcount_1.9"), 
  Series.Code=c("SI.POV.URHC", 
                "SI.POV.RUHC",
                "SI.POV.GINI",
                "EN.POP.SLUM.UR.ZS",
                "SI.POV.DDAY"), 
  stringsAsFactors = F)

# adding the new indicator value
data_world_2 = left_join(data_world, 
                         df_conv_world, 
                         by="Series.Code", 
                         all.x=T)

data_world_2 = 
  mutate(data_world_2, Series.Code_2=
           ifelse(!is.na(new_name), 
                  as.character(data_world_2$new_name), 
                  data_world_2$Series.Code)
  )


# The package 'reshape2' contains both 'dcast' and 'melt' functions
library(reshape2)

data_world_wide=dcast(data_world_2, Country.Name  ~ Series.Code_2, value.var = "newest_value")

#Now we have the final table to analyze

# Printing the first three rows
head(data_world_wide, 3)



library(Hmisc) # contains the `describe` function

vars_to_profile=c("gini_index", "poverty_headcount_1.9")
data_subset=select(data_world_wide, one_of(vars_to_profile))

# Using the `describe` on a complete dataset. # It can be run with one variable; for example, describe(data_subset$poverty_headcount_1.9)

describe(data_subset)

library(funModeling)

# Full numerical profiling in one function automatically excludes non-numerical variables
profiling_num(data_world_wide)

plot_num(data_world_wide)



# Loading needed libraries
library(funModeling) # contains heart_disease data

install.packages("minerva")
library(minerva) # contains MIC statistic
library(ggplot2)
library(dplyr)
library(reshape2) 
library(gridExtra) # allow us to plot two plots in a row
options(scipen=999) # disable scientific notation

correlation_table(data=heart_disease, target="has_heart_disease")

# Reading anscombe quartet data
anscombe_data = 
  read.delim(file="https://goo.gl/mVLz5L", header = T)

# calculating the correlation (R squared, or R2) for 
#every pair, every value is the same: 0.86.
cor_1 = cor(anscombe_data$x1, anscombe_data$y1)
cor_2 = cor(anscombe_data$x2, anscombe_data$y2)
cor_3 = cor(anscombe_data$x3, anscombe_data$y3)
cor_4 = cor(anscombe_data$x4, anscombe_data$y4)

# defining the function
plot_anscombe <- function(x, y, value, type)
{
  # 'anscombe_data' is a global variable, this is 
  # a bad programming practice ;)
  p=ggplot(anscombe_data, aes_string(x,y))  + 
    geom_smooth(method='lm', fill=NA) + 
    geom_point(aes(colour=factor(1), 
                   fill = factor(1)), 
               shape=21, size = 2
    ) + 
    ylim(2, 13) + 
    xlim(4, 19) + 
    theme_minimal() + 
    theme(legend.position="none") + 
    annotate("text", 
             x = 12, 
             y =4.5, 
             label = 
               sprintf("%s: %s", 
                       type, 
                       round(value,2)
               )
    )  
  
  return(p)
}

# plotting in a 2x2 grid
grid.arrange(plot_anscombe("x1", "y1", cor_1, "R2"), 
             plot_anscombe("x2", "y2", cor_2, "R2"), 
             plot_anscombe("x3", "y3", cor_3, "R2"), 
             plot_anscombe("x4", "y4", cor_4, "R2"), 
             ncol=2, 
             nrow=2)


x=seq(0, 20, length.out=500)
df_exp=data.frame(x=x, y=dexp(x, rate=0.65))
ggplot(df_exp, aes(x=x, y=y)) + geom_line(color='steelblue') + theme_minimal()

# position [1,2] contains the correlation of both variables, excluding the correlation measure of each variable against itself.

# Calculating linear correlation
res_cor_R2=cor(df_exp)[1,2]^2
sprintf("R2: %s", round(res_cor_R2,2))

install.packages("caret")
library(caret)

# selecting just a few variables
heart_disease_2 = 
  select(heart_disease, max_heart_rate, oldpeak, 
         thal, chest_pain,exer_angina, has_heart_disease)

# this conversion from categorical to a numeric is merely 
# to have a cleaner plot
heart_disease_2$has_heart_disease=
  ifelse(heart_disease_2$has_heart_disease=="yes", 1, 0)

# it converts all categorical variables (factor and 
# character for R) into numerical variables.
# skipping the original so the data is ready to use
dmy = dummyVars(" ~ .", data = heart_disease_2)

heart_disease_3 = 
  data.frame(predict(dmy, newdata = heart_disease_2))

# Important: If you recieve this message 
# `Error: Missing values present in input variable 'x'. 
# Consider using use = 'pairwise.complete.obs'.` 
# is because data has missing values.
# Please don't omit NA without an impact analysis first, 
# in this case it is not important. 
heart_disease_4=na.omit(heart_disease_3)

# compute the mic!
mine_res_hd=mine(heart_disease_4)

mine_res_hd$MIC[1:5,1:5]

# library wto plot that matrix
install.packages("corrplot")
library(corrplot) 

# to use the color pallete brewer.pal
library(RColorBrewer) 

# hack to visualize the maximum value of the 
# scale excluding the diagonal (variable against itself)
diag(mine_res_hd$MIC)=0

# Correlation plot with circles. 
corrplot(mine_res_hd$MIC, 
         method="circle",
         col=brewer.pal(n=10, name="PuOr"),
         # only display upper diagonal
         type="lower", 
         #label color, size and rotation
         tl.col="red",
         tl.cex = 0.9, 
         tl.srt=90, 
         # dont print diagonal (var against itself)
         diag=FALSE, 
         # accept a any matrix, mic in this case 
         #(not a correlation element)
         is.corr = F 
         
)

# Correlation plot with color and correlation MIC
corrplot(mine_res_hd$MIC, 
         method="color",
         type="lower", 
         number.cex=0.7,
         # Add coefficient of correlation
         addCoef.col = "black", 
         tl.col="red", 
         tl.srt=90, 
         tl.cex = 0.9,
         diag=FALSE, 
         is.corr = F 
)

cross_plot(heart_disease, input = "chest_pain", target = "has_heart_disease", plot_type = "percentual")