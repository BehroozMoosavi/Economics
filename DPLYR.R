##Purrr and a Bit of Dplyr
library(tidyverse)
#map(): list in, list out
#The map() function is an alternative to lapply(). It has the following simple form: map(x, f), where x is a list or vector, and f is a function. It always returns a list

my.list = list(nums=seq(0.1,0.6,by=0.1), chars=letters[1:12], 
               bools=sample(c(TRUE,FALSE), 6, replace=TRUE))
map(my.list, length)
lapply(my.list, length)

#The map_dbl() function is an alternative to sapply(). It has the form: map_dbl(x, f), where x is a list or vector, and f is a function that returns a numeric value (when applied to each element of x)
#Similarly:
#map_int() returns an integer vector
#map_lgl() returns a logical vector
#map_chr() returns a character vector
map_dbl(my.list, length)
as.numeric(unlist(lapply(my.list, length)))
vapply(my.list, FUN=length, FUN.VALUE=numeric(1))
library(repurrrsive) # Load Game of Thrones data set
class(got_chars)
class(got_chars[[1]])
names(got_chars[[1]])
map_chr(got_chars, function(x) { return(x$name) })
##### Extracting Element
map_chr(got_chars, "name")
map_lgl(got_chars, "alive")
sapply(got_chars, `[[`, "name")
sapply(got_chars, `[[`, "alive")
#########
#map_dfr() and map_dfc(): list in, data frame out
#The map_dfr() and map_dfc() functions iterate a function call over a list or vector, but automatically combine the results into a data frame. They differ in whether that data frame is formed by row-binding or column-binding
map_dfr(got_chars, `[`, c("name", "alive"))
class(map_dfr(got_chars, `[`, c("name", "alive")))
# Base R is much less convenient
data.frame(name = sapply(got_chars, `[[`, "name"),
           alive = sapply(got_chars, `[[`, "alive"))
# DPLYR
head(mtcars) # Built in data frame of cars data, 32 cars x 11 variables
#filter(): subset rows based on a condition
filter(mtcars, (mpg >= 20 & disp >= 200) | (drat <= 3))
#Base R is just as easy with subset(), more complicated with direct indexing
subset(mtcars, (mpg >= 20 & disp >= 200) | (drat <= 3))
mtcars[(mtcars$mpg >= 20 & mtcars$disp >= 200) | (mtcars$drat <= 3), ]
head(group_by(mtcars, cyl), 2)
# Ungrouped
summarize(mtcars, mpg = mean(mpg), hp = mean(hp))
# Grouped by number of cylinders
summarize(group_by(mtcars, cyl), mpg = mean(mpg), hp = mean(hp))























































