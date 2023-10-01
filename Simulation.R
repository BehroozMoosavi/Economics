sample(x=letters, size=10) # Without replacement, the default
sample(x=c(0,1), size=10, replace=TRUE) # With replacement
sample(x=10) # Arguments set as x=1:10, size=10, replace=FALSE
####rnorm(): generate normal random variables
####pnorm(): normal distribution function, Φ(x)=P(Z≤x)
####dnorm(): normal density function, ϕ(x)=Φ′(x)
####qnorm(): normal quantile function, q(y)=Φ−1(y), i.e., Φ(q(y))=y

n = 100000
z = rnorm(n, mean=0, sd=1) # These are the defaults for mean, sd
mean(z)
var(z)
#### Estimated Distribution function.
x = seq(-3, 3, length=100)
ecdf.fun = ecdf(z) # Create the ECDF
class(ecdf.fun)
ecdf.fun(0)
#### Plotting it
plot(x, ecdf.fun(x), lwd=2, col="red", type="l", ylab="CDF", main="ECDF")
lines(x, pnorm(x), lwd=2)
legend("topleft", legend=c("Empirical", "Actual"), lwd=2, 
       col=c("red","black"))

##### Interlude: Kolmogorov-Smirnov test
cat(" One of the most celebrated tests in statistics is due to Kolmogorov in 1933. The Kolmogorov-Smirnoff (KS) statistic is:
  $$
  \sqrt{\frac{n}{2}} \sup _x\left|F_n(x)-G_n(x)\right|
  $$
  Here $\mathrm{F}_{\mathrm{n}}$ is the ECDF of $\mathrm{X}_1, \ldots, \mathrm{X}_{\mathrm{n}} \sim \mathrm{F}$, and $\mathrm{G}_{\mathrm{n}}$ is the ECDF of $\mathrm{Y}_1, \ldots, \mathrm{Y}_{\mathrm{n}} \sim \mathrm{G}$. Under the null hypothesis $\mathrm{F}=\mathrm{G}$ (two distributions are the same), as $\mathrm{n} \rightarrow \infty$, the KS statistic approaches the supremum of a Brownian bridge:
  $$
  \sup _{t \in[0,1]}|B(t)|
  $$
")
n = 500
t = 1:n/n
Sig = t %o% (1-t)
Sig = pmin(Sig, t(Sig))
eig = eigen(Sig)
Sig.half = eig$vec %*% diag(sqrt(eig$val)) %*% t(eig$vec)
B = Sig.half %*% rnorm(n)
plot(t, B, type="l")
##### Two remarkable facts about the KS test:
  
### It is distribution-free, meaning that the null distribution doesn’t depend on F,G
  
###  We can actually compute the null distribution and use this test, e.g., via ks.test():
ks.test(rnorm(n), rt(n, df=1)) # Normal versus t1
ks.test(rnorm(n), rt(n, df=10)) # Normal versus t10
#### Estimated Density function
hist.obj = hist(z, breaks=30, plot=FALSE) 
class(hist.obj) # It's a list
hist.obj$breaks # These are the break points that were used
hist.obj$density # These are the estimated probabilities
# We can plot it
plot(hist.obj, col="pink", freq=FALSE, main="Histogram")
lines(x, dnorm(x), lwd=2)
legend("topleft", legend=c("Histogram", "Actual"), lwd=2, 
       col=c("pink","black"))
####Seed 
###All pseudorandom number generators depend on what is called a seed value

#This puts the random number generator in a well-defined “state”, so that the numbers it generates, from then on, will be reproducible
#The seed is just an integer, and can be set with set.seed()
#The reason we set it: so that when someone else runs our simulation code, they can see the same—albeit, still random—results that we do

# Getting the same 5 random normals over and over
set.seed(0); rnorm(5)
set.seed(0); rnorm(5)
set.seed(1); rnorm(5)
set.seed(0); rnorm(1)

###### Part III
###### Iteration and simulation
# Simulate, supposing 60 subjects in each group 
set.seed(0)
n = 60 
mu.drug = 2
mu.nodrug = runif(n, min=0, max=1)
x.drug = 100*rexp(n, rate=1/mu.drug) 
x.nodrug = 100*rexp(n, rate=1/mu.nodrug)
# Find the range of all the measurements together, and define breaks
x.range = range(c(x.nodrug,x.drug))
breaks = seq(min(x.range),max(x.range),length=20)

# Produce hist of the non drug measurements, then drug measurements on top
hist(x.nodrug, breaks=breaks, probability=TRUE, xlim=x.range, 
     col="lightgray", xlab="Percentage reduction in tumor size", 
     main="Comparison of tumor reduction")

# Plot a histogram of the drug measurements, on top
hist(x.drug, breaks=breaks, probability=TRUE, col=rgb(1,0,0,0.2), add=TRUE) 

# Draw estimated densities on top, for each dist
lines(density(x.nodrug), lwd=3, col=1)
lines(density(x.drug), lwd=3, col=2)
legend("topright", legend=c("No drug","Drug"), lty=1, lwd=3, col=1:2)

#### Repetition and reproducibility
####One single simulation is not always trustworthy (depends on the situation, of course)
####In general, simulations should be repeated and aggregate results reported—requires iteration!
####  To make random number draws reproducible, we must set the seed with set.seed()
####More than this, to make simulation results reproducible, we need to follow good programming practices
####Gold standard: any time you show a simulation result (a figure, a table, etc.), you have code that can be run (by anyone) to produce exactly the same result


####################Iteration and simulation (and functions): good friends

####Writing a function to complete a single run of your simulation is often very helpful
####This allows the simulation itself to be intricate (e.g., intricate steps, several simulation parameters), but makes running the simulation simple
####Then you can use iteration to run your simulation over and over again
####Good design practice: write another function for this last part (running your simulation many times)

# Function to do one simulation run
one.sim = function(param1, param2=value2, param3=value3) {
  # Possibly intricate simulation code goes here
}

# Function to do repeated simulation runs
rep.sim = function(nreps, param1, param2=value2, param3=value3, seed=NULL) {
  # Set the seed, if we need to
  if(!is.null(seed)) set.seed(seed)
  
  # Run the simulation over and over
  sim.objs = vector(length=nreps, mode="list")
  for (r in 1:nreps) {
    sim.objs[r] = one.sim(param1, param2, param3)
  }
  
  # Aggregate the results somehow, and then return something
  
}

##### Example
simulate_and_plot <- function(n_simulations, n = 60, mu.drug = 2) {
  set.seed(0)
  x.range <- NULL
  breaks <- NULL
  
  for (i in 1:n_simulations) {
    mu.nodrug <- runif(n, min = 0, max = 1)
    x.drug <- 100 * rexp(n, rate = 1 / mu.drug) 
    x.nodrug <- 100 * rexp(n, rate = 1 / mu.nodrug)
    
    if (is.null(x.range)) {
      x.range <- range(c(x.nodrug, x.drug))
      breaks <- seq(min(x.range), max(x.range), length = 20)
      hist(x.nodrug, breaks = breaks, probability = TRUE, xlim = x.range, 
           col = "lightgray", xlab = "Percentage reduction in tumor size", 
           main = "Comparison of tumor reduction")
    }
    
    hist(x.drug, breaks = breaks, probability = TRUE, col = rgb(1, 0, 0, 0.2), add = TRUE) 
    lines(density(x.nodrug), lwd = 3, col = 1)
    lines(density(x.drug), lwd = 3, col = 2)
  }
  
  legend("topright", legend = c("No drug", "Drug"), lty = 1, lwd = 3, col = 1:2)
}
simulate_and_plot(n_simulations = 1000)

####saveRDS(): allows us to save single R objects (like a vector, matrix, list, etc.), in (say) .rds format. E.g.,
#.    saveRDS(my.mat, file="my.matrix.rds")
####save(): allows us to save any number of R objects in (say) .rdata format. E.g.,

#.    save(mat.x, mat.y, list.z, file="my.objects.rdata")
####Corresponding to the two different ways of saving, we have two ways of loading things into R:

####readRDS(): allows us to load an object that has been saved by savedRDS(), and assign a new variable name. E.g.,
#.          my.new.mat = readRDS("my.matrix.rds")
####load(): allows us to load all objects that have been saved through save(), according to their original variables names. E.g.,
#.          load("my.objects.rdata")

  
