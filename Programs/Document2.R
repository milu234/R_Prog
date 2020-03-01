# Decision Making
# If Statement
# ??? Syntax :
#   if(condition)
#   {
#     // if condition is true
#   }

x <- 30L
if(is.integer(x)) {
  print("X is an Integer")
}


# If...Else Statement
# if(condition) {
#   // if condition is true
# } else {
#   // if condition is false.
# }
x <- c("what","is","truth")
if("Truth" %in% x) {
  print("Truth is found")
} else {
  print("Truth is not found")
}

# if...else if....else statement
x <- c("what","is","truth")
if("Truth" %in% x) {
  print("Truth is found the first time")
} else if ("truth" %in% x) {
  print("truth is found the second time")
} else {
  print("No truth found")
}


# Switch Statement
# Syntax :
#   switch(expression, case1, case2, case3 )

x <- switch(
  3,
  "first",
  "second",
  "third",
  "fourth"
)
print(x)


# Loops
# Repeat Loop
v <- c("Hello","loop")
cnt <- 2
repeat {
  print(v)
  cnt <- cnt+1
  if(cnt > 5) {
     break 
  }
}

# While                                                                                                                                                         loop
v <- c("Hello","while loop")
cnt <- 2
while (cnt < 7) {
  print(v)
  cnt = cnt + 1
}

?LETTERS
v <- LETTERS[1:4]
for ( i in v) {
  print(i)
}


# Next
# ??? Skip the current iteration.
v <- LETTERS[1:6]
for ( i in v) {
  if (i == "D") {
    next
  }
  print(i)
}


# Functions
mean(25:82)
sum(41:50)

# Similarly user can define their own
# functions, which increases code
# efficiency & reusability.
# ??? Create a function to print squares
# of numbers in sequence.
new.function <- function(a) {
  for(i in 1:a) {
    b <- i^2
    print(b)
  }
}
new.function(4)


new.function <- function(a,b,c)
{
  result <- a * b + c
  print(result)
}
# call by position.
new.function(5,3,11)
# call by name.
new.function(a = 11, b = 5, c =3)


# Math :
abs(-3.666)
sqrt(4)#   ??? abs(x) - Absolute value of x
sum(1,2,3,4)
k=c(3,3,2,1)
sum(k)
cos(35)
tan(34)
exp(100)
log(56)


# ??? Statistical functions :
mean(2,3,1,2,3,4)
median(2,3,1,2,3,4)
min(2,3,1,2,3,4)
max(2,3,1,2,3,4)
?quantile
quantile(c(2,3,1,2,3,4))
quantile(c(2,3,1,2,3,4),probs = seq(0, 1, 0.25))
quantile(c(2,3,1,2,3,4),probs = seq(0, 1, 0.10))
sd(c(2,3,1,2,3,4))
var(c(2,3,1,2,3,4))
?sample
m=c(1,2,3,4,5,6,7,8,9,10)
m
sample(m,3)
sample(m,3)
# replace:should sampling be with replacement?
sample(m,3,replace = FALSE)

z=c(1,2,3,4,5,6,7,8,9,10)
summary(z)


m
length(m)
range(m)
rep(1,6)
sign(-1.11)
sign(1.11)
sign(-31.11)
sign(0)

tolower("Jayshree")
toupper("Jayshree")
m=c(5,2,3,2,4,3,2,4,2,1,3,2,3,5,3,2)
length(m)
unique(m)
floor(3.9)
ceiling(1.1)
round(1.55)
round(1.5)

Sys.Date()
Sys.time()



# Packages in R
# R packages are a collection of R
# functions, complied code and
# sample data.
# ??? They are stored under a directory
# called "library" in the R
# environment.
# ??? By default, R installs a set of
# packages during installation. More
# packages are added later,
# when they are needed for some
# specific purpose
# ??? After installing package also have
# to load it in workspace using
# library('packagename')

# E.g : Install package "XML".
install.packages("XML")
# Loading the package
library('XML')

# List of all the packages installed
library()

getwd()
setwd("E:/eBooks/C-DAC/R Programming Lab")
getwd()


data <- read.csv("input.csv")
print(data)
#Today, just a glimpse of EDA
# Analyzingthe Data
# ???

# Extract only name & start date column.
print(is.data.frame(data))
print(ncol(data))
print(nrow(data))

# Get the max salary from data frame.
sal<-max(data$salary)
print(sal)

# Get the details of person in IT dept.
retval<-subset( data, dept == "IT")
print(retval)

# Get the details of person in IT dept having 
# salary greater than 600.
info <-subset(data, salary > 600 & dept == "IT")
print(info)

# Extract only name & start date of employee
S1 <-c(data$name, data$start_date)
print(S1)

# Reading from JSON
install.packages("rjson")
library("rjson")
# Give the input file name to the function.
result <- fromJSON(file ="input.json")
result

# Convert JSON file to a data frame.
Jsdf <- as.data.frame(result)
print(Jsdf)


