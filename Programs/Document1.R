# this gives the current wd
getwd()
#set the wkd dir
#setwd("E:/eBooks/C-DAC/R Programming Lab")
getwd()
dir()
ls()
help.start()
help("sum")
?mean.Date
?summary
?mean.Date
x<-1
x
print(x)
class(x)
typeof(x)
print(x)
x
x<-'c'
x
typeof(x)
is.character(x)
y<-'2'
as.integer(y)
y
typeof(y)
y<-as.integer(y)
y
typeof(y)


# R Data Types
# Vectors in R
N1 <- c('white', 'pink', 'blue')
N1
N2 <- c(15L,20L,25L)
N3 <- c(15,20,25)
N4 <- c(TRUE,FALSE,FALSE,TRUE)
N5 <- c(2+3i, 4+6i, 8+9i)
N6 <- charToRaw("Hello")
N6
print(N1)
length(N1)
typeof(N1)
class(N1)

v <- 5:13
print(v)

v <- 3.8:11.4
print(v)

# Create vector with elements from
# 5 to 9 incrementing by 0.4.
print(seq(5, 9, by = 0.4))

s <-c('apple','red',5,TRUE, 2+3i)
print(s)
class(s)
typeof(s)

# Accessing vector elements using
# position.
t <- c("Sun","Mon","Tue","Wed",
       "Thurs","Fri", "Sat")
u <- t[c(2,3,6)]
print(u)

# Accessing vector elements using
# logical indexing.
v <- t[c(TRUE, FALSE, FALSE,
         FALSE,FALSE,TRUE,FALSE)]
print(v)

# Accessing vector elements using
# 0/1 indexing.
y <- t[c(0,0,0,0,0,0,1)]
print(y)

# Accessing vector elements using
# range indexing.
u <- t[2:6]
u

# Single Element Update
v1 <- c(23,45,67,89,76)
v1[3] <- 101 # Update data at index 3
v1

# Multi Element Update
v1 <- c(23,45,67,89,76)
v1[2:4] <- 100
v1

# List in R
# Lists are the R objects which
# contain elements of different types
# like - numbers, strings,
# vectors and another list inside it.

list_data <- list("Red", "Green",
                  c(21,32,11), TRUE, 51.23, 119.1)
print(list_data)
class(list_data)

y<-c(4,5,6) #multiplication by a scalar 
5*y
5+y 
y^5
?matrix
# Creating Matrix: Two-dimensional array having
# elements of same class.
m<-matrix(c(11,12,13,55,60,65,66,72,78),nrow=3,ncol=3)
m
dim(m)
#attributes of matrix m 
attributes(m)


m<-matrix(c(11,12,13,55,60,65,66,72,78),nrow=3,ncol=3,
          byrow = TRUE)
m

m*3

x<-c(1,2,3)
y<-c(11,12,13) 
cbind(x,y)

rbind(x,y)

n<-matrix(c(4,5,6,14,15,16,24,25,26),nrow=3,ncol=3)
q<-m+n 
q

o<-matrix(c(4,5,6,14,15,16),nrow=3,ncol=2)
o
#matrix multiplication by using %*% 
r<-m %*% o 
r

#transpose of matrix 
mdash<-t(m) 
mdash

s<-matrix(c(4,5,6,14,15,16,24,25,26), nrow=3,ncol=3,byrow=TRUE)
s
#determinant of s
s_det<-det(s)
s_det
m
m_row_sum<-rowSums(m) 
m_row_sum

m_col_sum<-colSums(m) 
m_col_sum


#using list() function 
x<-list(1,"p",TRUE,2+4i) 
x

# Factor: Represents categorical data. Can be ordered or unordered.
status<-c("low","high","medium","high","low")
status
typeof(status)

#using factor() function 
x<-factor(status, ordered=TRUE,levels=c("low","medium","high")) 
x
class(x)

# Data frame: Used to store tabular data.
student_id<-c(1,2,3)
student_names<-c("Ram","Shyam","Laxman")
position<-c("First","Second","Third")
data<-data.frame(student_id,student_names,position)
data
data$position
data$position
data$student_names
#accessing a particular column
data$student_id
nrow(data$student_names)
nrow(data)
ncol(data)
names(data)

# Create two lists.
list1 <- list(1,2,3)
list2 <- list("Sun","Mon","Tue")
# Merge the two lists.
merged.list <- c(list1,list2)
# Print the merged list.
print(merged.list)


# Create lists.
list1 <- list(1:5)
list2 <-list(10:14)
# Convert the lists to vectors.
v1 <- unlist(list1)
v2 <- unlist(list2)
# Now add the vectors
result <- v1+v2
print(result)


M <- matrix(c(3:14), nrow = 3,
            byrow = TRUE)
print(M)

N <- matrix(c(3:14), nrow = 4,
            byrow = FALSE)
print(N)

# Defining Row & Column names
rownames = c("row1", "row2",
             "row3", "row4")
colnames = c("col1", "col2",
             "col3")
P <- matrix(c(3:14), nrow = 4,
            byrow = TRUE, dimnames =
              list(rownames, colnames))
print(P)
?matrix
# Access the element at 3rd column
# and 1st row.
print(P[1,3])
# Access the element at 2nd column
# and 4th row.
print(P[4,2])
# Access only the 2nd row.
print(P[2,])
# Access only the 3rd column.
print(P[,3])


# Creating an array
vector1 <- c(5,9,3)
vector2 <-
  c(10,11,12,13,14,15)
# Use vectors as input to the array.
result <- array(vector2,dim = c(3,3,2))
print(result)

# Create two vectors of different lengths.
vector1 <- c(5,9,3)
vector2 <- c(10,11,12,13,14,15)
col <- c("COL1","COL2","COL3")
row <- c("ROW1","ROW2","ROW3")
mat <- c("Matrix1","Matrix2")
# Use vectors as input to the array.
result <- array(c(vector1,vector2),
                dim = c(3,3,2),
                dimnames = list(row, col,mat))
print(result)


# Element in the 1st row and 3rd column
# of the 1st matrix.
print(result[1,3,1])


# Create a Vector
data <- c("East","West",
          "East","North", "North","East",
          "West","West", "West","East",
          "North")
# Use factor() to create factor
factor_data <- factor(data)
print(factor_data)

# Reorder the levels
new_order_data <- factor(factor_data,levels = c("East","West","North"))
print(new_order_data)

# Creating a Dataframe
student.data <-
  data.frame(
    student_id = c (1:5),
    student_name = c("Rick", "Dan", "Michelle","Ryan","Gary"),
  scores = c(62.3, 51.2, 61.0, 72.0,
  84.25),
  start_date = as.Date( c("2012-01-01",
  "2013-09-23","2014-11-15","2014-05-11","2015-03-27")),
  stringsAsFactors = FALSE)
?as.Date
# Print the data frame.
print(student.data)
student_id = c (1:5)
student_name = c("Rick", "Dan", "Michelle","Ryan","Gary")
scores = c(62.3, 51.2, 61.0, 72.0,
           84.25)
Student = data.frame(student_id, student_name, scores)
print(Student)
print(Student[1,2])




# Structure of the Data Frame
str(emp.data)
# ??? Summary of Data in Data Frame
summary(emp.data)
summary(emp.data$salary)
# Extract Specific columns.
print(emp.data$emp_name)
summary(emp.data$start_date)
# Extract first two rows.
print(emp.data[1:2,])

# Extract 3rd and 5th row with 2nd & 4th column.
result <- emp.data[c(3,5),c(2,4)]
print(result)


# Add the "dept" coulmn.
emp.data$dept <- c('IT' ,
                    'Operations' , 'IT' ,'HR',
                    'Finance')
print(emp.data)

class(emp.data$dept)



data.frame(df, stringsAsFactors = TRUE)
a <- c(10,20,30,40)
b <- c('book','pen','textbook','pencil_base')
c <- c(TRUE,FALSE,TRUE,FALSE)
d <- c(2.5, 8, 10, 7)

df <- data.frame(a,b,c,d)
print(df)
names(df) <- c('ID', 'items', 'store', 'price')
df
str(df)


#Slice data Frame
df[1,2]

df[1:2,]



