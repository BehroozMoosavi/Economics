set.seed(33) # For reproducibility
x.vec = rnorm(6) # Generate a vector of 6 random standard normals
x.vec
x.vec[3] # Third element
x.vec[-3]
x.mat = matrix(x.vec, 3, 2) # Fill a 3 x 2 matrix with those same 6 normals,
# column major order
x.mat
x.mat[2,] # Second row
x.list = list(x.vec, letters, sample(c(TRUE,FALSE),size=4,replace=TRUE))
x.list
x.list[[3]] # Third element of list
x.list[-1] # All but first element of list
x.vec[c(F,F,T,F,F,F)] # Third element
x.vec[c(T,T,F,T,T,T)] # All but third element
pos.vec = x.vec > 0 # Boolean vector indicating whether each element is positive
pos.vec
x.vec[pos.vec] # Pull out only positive elements
names(x.list) = c("normals", "letters", "bools")
x.list[["letters"]] # "letters" (third) element 
x.list[c("normals","bools")]
##### Control Flow
x = 0.5

if (x >= 0) {
  x
} else {
  -x
}
x = -2

if (x^2 < 1) {
  x^2 
} else if (x >= 1) {
  2*x-1
} else {
  -2*x+1
}
# In the ifelse() function we specify a condition, then a value if the condition holds, and a value if the condition fails
ifelse(x > 0, x, -x)

# Deciding between many options
# Instead of an if() statement followed by elseif() statements (and perhaps a final else), we can use switch(). We pass a variable to select on, then a value for each option
type.of.summary = "mode"

switch(type.of.summary,
       mean=mean(x.vec),
       median=median(x.vec),
       histogram=hist(x.vec),
       "I don't understand")
### Bolean operators
u.vec = runif(10, -1, 1)
u.vec
u.vec[-0.5 <= u.vec & u.vec <= 0.5] = 999 

(0 > 0) && all(matrix(0,2,2) == matrix(0,3,3)) 

(0 > 0) && (ThisVariableIsNotDefined == 0) 
### ITeration
n = 10
log.vec = vector(length=n, mode="numeric")
for (i in 1:n) {
  log.vec[i] = log(i)
}
log.vec
#####
n = 10
log.vec = vector(length=n, mode="numeric")
for (i in 1:n) {
  if (log(i) > 2) {
    cat("I'm outta here. I don't like numbers bigger than 2\n")
    break
  }
  log.vec[i] = log(i)
}
log.vec

for (str in c("Prof", "Ryan", "Tibs")) {
  cat(paste(str, "declined to comment\n"))
}

for (i in 1:4) {
  for (j in 1:i^2) {
    cat(paste(j,""))
  }
  cat("\n")
  
}

# A while() loop repeatedly runs a code block, again called the body, until some condition is no longer true
i = 1
log.vec = c()
while (log(i) <= 2) {
  log.vec = c(log.vec, log(i))
  i = i+1
}
log.vec

# while(TRUE) and repeat: both do the same thing, just repeat the body indefinitely, until something causes the flow to break. Example (try running in your console):



repeat {
  ans = readline("Who is the best Professor of Statistics at CMU? ")
  if (ans == "Tibs" || ans == "Tibshirani" || ans == "Ryan") {
    cat("Yes! You get an 'A'.")
    break
  }
  else {
    cat("Wrong answer!\n")
  } 
}