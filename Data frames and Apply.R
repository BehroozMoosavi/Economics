my.df = data.frame(nums=seq(0.1,0.6,by=0.1), chars=letters[1:6], 
                   bools=sample(c(TRUE,FALSE), 6, replace=TRUE))
my.df

# Recall, a list can have different lengths for different elements!
my.list = list(nums=seq(0.1,0.6,by=0.1), chars=letters[1:12], 
               bools=sample(c(TRUE,FALSE), 6, replace=TRUE))
my.list

my.df[,1] # Also works for a matrix 
my.df$nums # Doesn't work for a matrix, but works for a list


#### creating dataframe from matrix
class(state.x77) # Built-in matrix of states data, 50 states x 8 variables
head(state.x77) 
class(state.region)
head(state.region)

state.df = data.frame(state.x77, Region=state.region, Division=state.division)
class(state.df)

head(state.df) # Note that the first 8 columns name carried over from state.x77
### Adding columns to a data frame
# First way: use data.frame() to concatenate on a new column
state.df = data.frame(state.df, Cool=sample(c(T,F), nrow(state.df), rep=TRUE))
head(state.df, 4)
# Second way: just directly define a new named column
state.df$Score = sample(1:100, nrow(state.df), replace=TRUE)
head(state.df, 4)
###### Deleting columns To delete columns: we can either use negative integer indexing, or set a column to NULL
# First way: use negative integer indexing
state.df = state.df[,-ncol(state.df)]
head(state.df, 4)
# Second way: just directly set a column to NULL
state.df$Cool = NULL
head(state.df, 4)

# Second way: just directly set a column to NULL
state.df$Cool = NULL
head(state.df, 4)

# Compare the averages of the Frost column between states in New England and
# Pacific divisions
mean(state.df[state.df$Division == "New England", "Frost"]) 
mean(state.df[state.df$Division == "Pacific", "Frost"]) # Those wimps!
#### The subset() function provides a convenient alternative way of accessing rows for data frames
# Using subset(), we can just use the column names directly (i.e., no need for
# using $)
state.df.ne.1 = subset(state.df, Division == "New England")
# Get same thing by extracting the appropriate rows manually
state.df.ne.2 = state.df[state.df$Division == "New England", ]
all(state.df.ne.1 == state.df.ne.2)


##### apply
#apply(): apply a function to rows or columns of a matrix or data frame
#lapply(): apply a function to elements of a list or vector
#sapply(): same as the above, but simplify the output (if possible)
#tapply(): apply a function to levels of a factor vector
#apply(x, MARGIN=1, FUN=my.fun), to apply my.fun() across rows of a matrix or data frame x
#apply(x, MARGIN=2, FUN=my.fun), to apply my.fun() across columns of a matrix or data frame x

apply(state.x77, MARGIN=2, FUN=min) # Minimum entry in each column

apply(state.x77, MARGIN=2, FUN=max) # Maximum entry in each column

apply(state.x77, MARGIN=2, FUN=summary) # Summary of each col, get back matrix!


# Our custom function: trimmed mean
trimmed.mean = function(v) {  
  q1 = quantile(v, prob=0.1)
  q2 = quantile(v, prob=0.9)
  return(mean(v[q1 <= v & v <= q2]))
}

apply(state.x77, MARGIN=2, FUN=trimmed.mean) 

# Compute trimmed means, defining this on-the-fly
apply(state.x77, MARGIN=2, FUN=function(v) { 
  q1 = quantile(v, prob=0.1)
  q2 = quantile(v, prob=0.9)
  return(mean(v[q1 <= v & v <= q2]))
})

# Our custom function: trimmed mean, with user-specified percentiles
trimmed.mean = function(v, p1, p2) {
  q1 = quantile(v, prob=p1)
  q2 = quantile(v, prob=p2)
  return(mean(v[q1 <= v & v <= q2]))
}

apply(state.x77, MARGIN=2, FUN=trimmed.mean, p1=0.01, p2=0.99)


x = matrix(rnorm(9), 3, 3)
# Don't do this (much slower for big matrices)
apply(x, MARGIN=1, function(v) { return(sum(v > 0)) })

rowSums(x > 0)

my.list
lapply(my.list, FUN=mean) # Get a warning: mean() can't be applied to chars
lapply(my.list, FUN=summary)
sapply(my.list, FUN=mean) # Simplifies the result, now a vector
sapply(my.list, FUN=summary) # Can't simplify, so still a list
#### tapply The function tapply() takes inputs as in: tapply(x, INDEX=my.index, FUN=my.fun), to apply my.fun() to subsets of entries in x that share a common level in my.index
# Compute the mean and sd of the Frost variable, within each region
tapply(state.x77[,"Frost"], INDEX=state.region, FUN=mean)
tapply(state.x77[,"Frost"], INDEX=state.region, FUN=sd)
######
# Split up the state.x77 matrix according to region
state.by.reg = split(data.frame(state.x77), f=state.region)
class(state.by.reg) # The result is a list
names(state.by.reg) # This has 4 elements for the 4 regions
class(state.by.reg[[1]]) # Each element is a data frame
state.by.reg[[1]]
lapply(state.by.reg, FUN=head, 3) 
# For each region, average each of the 8 numeric variables
lapply(state.by.reg, FUN=function(df) { 
  return(apply(df, MARGIN=2, mean)) 
})