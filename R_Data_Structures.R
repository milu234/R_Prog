# Vectors in R
N1 <- c('red','green','yellow')
N2 <- c(15L,20L,25L)
N3 <- c(15,20,25)
N4 <- c(TRUE,FALSE,FALSE,TRUE)
N5 <- c(2+3i, 4+6i, 8+9i)
N6 <- charToRaw("Hello")
print(N1)
print(N2)
print(N3)
print(N4)
print(N5)
print(N6)
v <- 5:13
print(v)

v <- 3.8:11.4
print(v)


# Create vector with elements from
# 10 to 20 incrementing by 0.4.
print(seq(10, 20, by = 0.6))


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