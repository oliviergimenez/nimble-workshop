# ---- R codes associated with NIMBLE workshop for GDR Ecostat 2022 
## Olivier Gimenez, Maud Quéroué, Valentin Lauret
# slides at https://oliviergimenez.github.io/nimble-workshop/

# ---- load packages ----
library(tidyverse)
library(nimble)
theme_set(theme_light())


# ---- Let's start with a Capture Recapture Example  ----

## we capture, mark and release n=57 animals 
## we recapture y=19 animals alive

y <- 19 # nb of success
n <- 57 # nb of attempts


# ---- 1. Build model ----

## Specify binomial likelihood and uniform prior on survival probability theta

model <- nimbleCode({
  # likelihood
  survived ~ dbinom(theta, released)
  # prior
  theta ~ dunif(0, 1)
  # derived quantity
  lifespan <- -1/log(theta) # expected lifespan 
                            # w/ constant survival
})

## check the model R object
model


# ---- 2. Read in data ----

## Constants are values that do not change
my.constants <- list(released = 57)

## Data are values that change, i.e. left of a ~
my.data <- list(survived = 19)


# ---- 3. Specify parameters ---- 

parameters.to.save <- c("theta", "lifespan")


# ---- 4. Initial values ----

init1 <- list(theta = 0.1)
init2 <- list(theta = 0.5)
initial.values <- list(init1, init2)

initial.values


## Alternative intitalization 
initial.values <- function() list(theta = runif(1,0,1))
initial.values()


# ---- 5. Provide MCMC details ----

n.iter <- 5000
n.burnin <- 1000
n.chains <- 2


# ---- RUN NIMBLE ----

## Do not focus on the warnings for now
# Running time : +/- 30 sec 

mcmc.output <- nimbleMCMC(code = model,
                          data = my.data,
                          constants = my.constants,
                          inits = initial.values,
                          monitors = parameters.to.save,
                          niter = n.iter,
                          nburnin = n.burnin,
                          nchains = n.chains)


# ---- INSPECT MCMC OUTPUT ----

str(mcmc.output)

## Dimensions
dim(mcmc.output$chain1)

## Return values
head(mcmc.output$chain1)


## Compute posterior summaries
mean(mcmc.output$chain1[,'theta'])


## 95% credible interval
quantile(mcmc.output$chain1[,'theta'], probs = c(2.5, 97.5)/100)


## Visualize posterior distribution with a histogram
mcmc.output %>%
  as_tibble() %>%
  ggplot() + 
  geom_histogram(aes(x = chain1[,"theta"]), color = "white") + 
  labs(x = "survival probability")

# ---- MCMC VISUALIZATION ----

## load package
library(MCMCvis)

## Numerical summaries
MCMCsummary(object = mcmc.output, round = 2)

## Caterpillar plot for posterior distribution
MCMCplot(object = mcmc.output, params = 'theta')

## Trace and posterior density
MCMCtrace(object = mcmc.output,
          pdf = FALSE, # no export to PDF
          ind = TRUE, # separate density lines per chain
          params = "theta")


## Add convergence diagnostic
MCMCtrace(object = mcmc.output,
          pdf = FALSE,
          ind = TRUE,
          Rhat = TRUE, # add Rhat
          n.eff = TRUE, # add eff sample size
          params = "theta")


# ---- DERIVED QUANTIITES -----

## pool theta samples between the three chains
theta_samples <- c(mcmc.output$chain1[,'theta'], 
                   mcmc.output$chain2[,'theta'],
                   mcmc.output$chain3[,'theta'])

## calculate lifespan from all theta samples
lifespan <- -1/log(theta_samples)

## obtain numerical summaries
mean(lifespan)

quantile(lifespan, probs = c(2.5, 97.5)/100)

## visualize lifespan posterior distribution
lifespan %>%
  as_tibble() %>%
  ggplot() +
  geom_histogram(aes(x = value), color = "white") +
  labs(x = "lifespan")


# ---- Programming NIMBLE FUNCTIONS ----

## `run` section gives the function to be executed.
##  `theta = double(0)` and `returnType(double(0))` arguments tell that input and output
##        are single numeric values (scalars).
## double(1) and double(2) would mean vectors and matrices

## write a function to compute lifespan  = - 1/log(theta) as in the CR example

computeLifespan <- nimbleFunction(
    run = function(theta = double(0)) { # type declarations
        ans <- -1/log(theta)
        return(ans)
        returnType(double(0))  # return type declaration
    } )

## try the function
computeLifespan(0.8)

## compile in C++
CcomputeLifespan <- compileNimble(computeLifespan)
CcomputeLifespan(0.8)

## Use Nimble function in a model

model <- nimbleCode({
  # likelihood
  survived ~ dbinom(theta, released)
  # prior
  theta ~ dunif(0, 1)
  # derived quantity
  lifespan <- computeLifespan(theta)
})


## The rest of the workflow remain the same 

my.data <- list(survived = 19, released = 57)
parameters.to.save <- c("theta", "lifespan")
initial.values <- function() list(theta = runif(1,0,1))
n.iter <- 5000
n.burnin <- 1000
n.chains <- 2
mcmc.output <- nimbleMCMC(code = model,
                          data = my.data,
                          inits = initial.values,
                          monitors = parameters.to.save,
                          niter = n.iter,
                          nburnin = n.burnin,
                          nchains = n.chains)
MCMCsummary(object = mcmc.output, round = 2)


# ---- Programming: CALLING R/C++ FUNCTIONS ----

## an example R function 

myfunction <- function(x) {
  -1/log(x)
}

## to use the function in Nimble
## use nimbleExternallCall() for a C/C++ function

Rmyfunction <- 
  nimbleRcall(prototype = function(x = double(0)){}, 
                           Rfun = 'myfunction',
                           returnType = double(0))

## call the function in a model

model <- nimbleCode({
  # likelihood
  survived ~ dbinom(theta, released)
  # prior
  theta ~ dunif(0, 1)
  lifespan <- Rmyfunction(theta)
})


## The rest of the workflow remain the same 

my.data <- list(survived = 19, released = 57)
parameters.to.save <- c("theta", "lifespan")
initial.values <- function() list(theta = runif(1,0,1))
n.iter <- 5000
n.burnin <- 1000
n.chains <- 2
mcmc.output <- nimbleMCMC(code = model,
                          data = my.data,
                          inits = initial.values,
                          monitors = parameters.to.save,
                          niter = n.iter,
                          nburnin = n.burnin,
                          nchains = n.chains)
MCMCsummary(object = mcmc.output, round = 2)


# ---- USER DEFINED DISTRIBUTIONS ----

## write a binomial distribution

# density
dmybinom <- nimbleFunction(
  run = function(x = double(0), 
                 size = double(0), 
                 prob = double(0), 
                 log = integer(0, default = 1)) {
    returnType(double(0))
    # compute binomial coefficient 
    lchoose <- lfactorial(size) - lfactorial(x) - lfactorial(size - x)
    # binomial density function
    logProb <- lchoose + x * log(prob) + (size - x) * log(1 - prob)
    if(log) return(logProb)
    else return(exp(logProb)) 
  })

# simulation using the coin flip method (p. 524 in Devroye 1986)
rmybinom <- nimbleFunction(
  run = function(n = integer(0, default = 1),
                 size = double(0),
                 prob = double(0)) {
    returnType(double(0))
    x <- 0
    y <- runif(n = size, min = 0, max = 1)
    for (j in 1:size){
      if (y[j] < prob){
        x <- x + 1
      }else{
        x <- x
      }
    }
    return(x)    
  })

# define the nimbleFunctions in R global environment
assign('dmybinom', dmybinom, .GlobalEnv)
assign('rmybinom', rmybinom, .GlobalEnv)


## try your function
rmybinom(n = 1, size = 5, prob = 0.1)

rmybinom(n = 1, size = 5, prob = 0.8)


## usual workflow

model <- nimbleCode({
  # likelihood
  survived ~ dmybinom(prob = theta, size = released)
  # prior
  theta ~ dunif(0, 1)
})

mcmc.output <- nimbleMCMC(code = model,
                          data = my.data,
                          inits = initial.values,
                          niter = n.iter,
                          nburnin = n.burnin,
                          nchains = n.chains)
MCMCsummary(mcmc.output)


# ---- DETAILED NIMBLE WORKFLOW ----

## As before: model code, read in data and pick initial values
    
model <- nimbleCode({
  # likelihood
  survived ~ dbinom(theta, released)
  # prior
  theta ~ dunif(0, 1)
  # derived quantity
  lifespan <- -1/log(theta)
})

my.data <- list(survived = 19, released = 57)
initial.values <- list(theta = 0.5)


## 1. Create the model as an R object

survival <- nimbleModel(code = model,
                        data = my.data,
                        inits = initial.values)


## Look at the values stored at each node

survival$getNodeNames()

survival$theta
survival$survived
survival$lifespan 
# this is 
-1/log(0.5)

## calculate the log-likelihood at the initial value for theta
survival$calculate()
# this is 
dbinom(x = 19, size = 57, prob = 0.5, log = TRUE)

## 2. Compile Nimble
#  generate C++ code that  can be used in R 

Csurvival <- compileNimble(survival)

Csurvival$theta

## Performing Maximum Likelihood Estimation with NIMBLE
# using either `survival` or `Csurvival`

# function for negative log-likelihood to minimize
f <- function(par) {
    Csurvival[['theta']] <- par # assign par to theta 
    ll <- Csurvival$calculate() # update log-likelihood with par value
    return(-ll) # return negative log-likelihood
}

# evaluate function at 0.5 and 0.9
f(0.5)
f(0.9)

## MLE
# minimize function
out <- optimize(f, interval = c(0,1))
round(out$minimum, 2)

## 3. MCMC configuration

survivalConf <- configureMCMC(survival)

# add a parameter to monitor
survivalConf$addMonitors(c("lifespan"))

survivalConf

## 4. Create MCMC function

# build MCMC function
survivalMCMC <- buildMCMC(survivalConf)

# compile MCMC function
CsurvivalMCMC <- compileNimble(survivalMCMC, project = survival)


## 5. Run Nimble

n.iter <- 5000
n.burnin <- 1000

# use runMCMC() 
samples <- runMCMC(mcmc = CsurvivalMCMC, 
                   niter = n.iter,
                   nburnin = n.burnin)

head(samples)

## obtain numerical summaries
samplesSummary(samples)


# ---- MANAGE MCMC SAMPLERS ----

## print samplers
# survivalConf <- configureMCMC(survival)
survivalConf$printSamplers()

# to see Nimble available samplers
?samplers

## change samplers
# removing default sampler
survivalConf$removeSamplers(c('theta'))
survivalConf$printSamplers()

# change it for a slice sampler
survivalConf$addSampler(target = c('theta'),
                        type = 'slice')
survivalConf$printSamplers()

## resume with the Nimble workflow
# build and compile MCMC
survivalMCMC2 <- buildMCMC(survivalConf)

CsurvivalMCMC2 <- compileNimble(survivalMCMC2, 
                                project = survival,
                                resetFunctions = TRUE) 

# run NIMBLE
samples2 <- runMCMC(mcmc = CsurvivalMCMC2, 
                    niter = n.iter,
                    nburnin = n.burnin)

# obtain numerical summaries:
samplesSummary(samples2)

# ---- TIPS AND TRICKS ----

## Updating MCMC chains

niter_ad <- 6000
CsurvivalMCMC$run(niter_ad, reset = FALSE)

# extract previous MCMC samples augmented with new samples
more_samples <- as.matrix(CsurvivalMCMC$mvSamples)
samplesSummary(more_samples)

# 4000 first iterations + 6000 added iterations
dim(more_samples)
