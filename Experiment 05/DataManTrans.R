install.packages("tidyverse")
install.packages("nycflights13")
library(dplyr)
library(nycflights13)
library(tidyverse)
nycflights13::flights
flights
view(flights)

#Filter
filter(flights, month == 1 ,day == 1)

#Comparisons
filter(flights, month == 1)
filter(flights, dep_time > 600 , arr_time < 800)

#Logical Operators
filter(flights, month == 11 | month == 12)

nov_dec <- filter(flights, month %in% c(11, 12))
nov_dec
view(nov_dec)

filter(flights, !(arr_delay > 120 | dep_delay > 120))
filter(flights, arr_delay <= 120, dep_delay <= 120)

df <- tibble(x = c(1, NA, 3))
filter(df, x > 1)

#Arrange with Rows
arrange(flights,year,month,day)

#Use desc() to re-order by a column in descending order:
arrange(flights, desc(dep_delay))


#Select columns with select()

# Select columns by name
select(flights, year, month, day)

#select columns by day and year(inclusive)
select(flights, year:day)

#select columns except  day and year
select(flights, -(year:day))

#Another option is to use select() in conjunction with the everything() helper. This is useful if you have a handful of variables you'd like to move to the start of the data frame.
select(flights, time_hour, air_time, everything())


#Mutate add new columns by performing operation on new columns
flights_sml <- select(flights, 
                      year:day, 
                      ends_with("delay"), 
                      distance, 
                      air_time
)
mutate(flights_sml,
       gain = dep_delay - arr_delay,
       speed = distance / air_time * 60
)

#Mutate and modify with new columns
mutate(flights_sml,
       gain = dep_delay - arr_delay,
       hours = air_time / 60,
       gain_per_hour = gain / hours
)

#Transmute : Only keep the new added variables

transmute(flights,
          gain = dep_delay - arr_delay,
          hours = air_time / 60,
          gain_per_hour = gain / hours
)


#Operations
transmute(flights,
          dep_time,
          hour = dep_time %/% 100,
          minute = dep_time %% 100
)

#The last key verb is summarise(). It collapses a data frame to a single row:
summarise(flights, delay = mean(dep_delay, na.rm = TRUE))


by_day <- group_by(flights, year, month, day)
summarise(by_day, delay = mean(dep_delay, na.rm = TRUE))

#Combining multiple operations with the pipe
by_dest <- group_by(flights, dest)
delay <- summarise(by_dest,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE)
)
delay <- filter(delay, count > 20, dest != "HNL")
ggplot(data = delay, mapping = aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE)
#> `geom_smooth()` using method = 'loess' and formula 'y ~ x'



flights %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay))

flights %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay, na.rm = TRUE))

flights %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay, na.rm = TRUE))


not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay))

delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay)
  )

ggplot(data = delays, mapping = aes(x = delay)) + 
  geom_freqpoly(binwidth = 10)


delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay, na.rm = TRUE),
    n = n()
  )

ggplot(data = delays, mapping = aes(x = n, y = delay)) + 
  geom_point(alpha = 1/10)


