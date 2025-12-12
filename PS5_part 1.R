## PS 5
##Part 1: Simulation
##Step 0: Set up parameters and one dataset

set.seed(123)   

# Sample size for each dataset
N <- 500  

# True parameters
beta_0 <- 1      # Intercept
beta_X <- 0.5    # True treatment effect
beta_C <- 1.0    # True confounder effect
gamma  <- 0.7    # Effect of confounder on treatment
sigma_e <- 1     # SD of error in Y
sigma_u <- 1     # SD of noise in X

# Generate confounder C
C <- rnorm(N, mean = 0, sd = 1)

# Generate treatment X, which depends on confounder C (creates confounding)
u <- rnorm(N, mean = 0, sd = sigma_u)
X <- gamma * C + u

# Generate outcome Y using the true linear model
e <- rnorm(N, mean = 0, sd = sigma_e)
Y <- beta_0 + beta_X * X + beta_C * C + e

# Put into a data frame
dat <- data.frame(Y = Y, X = X, C = C)

# Fit the true model: Y ~ X + C
model_true <- lm(Y ~ X + C, data = dat)

# Print the summary table for the model
summary(model_true)


#Question part a
# Step 1: Function to run ONE simulation


simulate_beta_X <- function(N, beta_0, beta_X, beta_C, gamma,
                            sigma_e, sigma_u) {
  # Generate confounder
  C <- rnorm(N, mean = 0, sd = 1)
  
  # Generate treatment (depends on confounder)
  u <- rnorm(N, mean = 0, sd = sigma_u)
  X <- gamma * C + u
  
  # Generate outcome
  e <- rnorm(N, mean = 0, sd = sigma_e)
  Y <- beta_0 + beta_X * X + beta_C * C + e
  
  # Fit the same true model
  mod <- lm(Y ~ X + C)
  
  # Return the estimated coefficient on X
  coef(mod)["X"]
}


# Step 2: Repeat many simulations


n_sims <- 1000   # Number of simulations

# Run the simulation n_sims times and store the X coefficients
beta_X_hats <- replicate(
  n_sims,
  simulate_beta_X(N, beta_0, beta_X, beta_C, gamma, sigma_e, sigma_u)
)

# Check the mean of the estimated coefficients
mean(beta_X_hats)   

# Check the standard deviation of the estimated coefficients
sd(beta_X_hats)


# Step 3: Plot histogram of β̂_X to show approximate normality


hist(beta_X_hats,
     breaks = 30,
     main = "Sampling Distribution of beta_X_hat",
     xlab = "Estimated beta_X")


## Question part b
# Bootstrapped standard error

# Step 1: Since We already "collected data":
# dat = data.frame(Y, X, C) from part (a), 
# we should Define a function that resamples the data WITH replacement
#  and returns the coefficient of X

bootstrap_beta_X <- function(data) {
  n <- nrow(data)                          # sample size
  rows_boot <- sample(1:n, size = n,      # resample row indices
                      replace = TRUE)
  data_boot <- data[rows_boot, ]          # bootstrap sample
  
  model_boot <- lm(Y ~ X + C, data = data_boot)
  coef(model_boot)["X"]                   # return beta_hat for X
}

# Number of bootstrap samples
B <- 1000

# Step 2: Calculate the statistic for each sample
beta_X_boot <- replicate(B, bootstrap_beta_X(dat))

# Step 3: Bootstrapped standard error 
se_boot_X <- sd(beta_X_boot)

se_boot_X   # print the bootstrapped for the X coefficient


## Question part c
#  Omit the confounder in the model


# but fit a model that omits C: Y ~ X
simulate_beta_X_omitC <- function(N, beta_0, beta_X, beta_C, gamma,
                                  sigma_e, sigma_u) {
  # Generate confounder
  C <- rnorm(N, mean = 0, sd = 1)
  
  # Generate treatment (depends on confounder)
  u <- rnorm(N, mean = 0, sd = sigma_u)
  X <- gamma * C + u
  
  # Generate outcome from the TRUE model (still uses C)
  e <- rnorm(N, mean = 0, sd = sigma_e)
  Y <- beta_0 + beta_X * X + beta_C * C + e
  
  # WRONG estimated model: omit the confounder
  mod_wrong <- lm(Y ~ X)
  
  # Return the coefficient on X from the wrong model
  coef(mod_wrong)["X"]
}

# Number of simulations 
n_sims <- 1000

# Get sampling distribution of the WRONG coefficient
beta_X_hats_omitC <- replicate(
  n_sims,
  simulate_beta_X_omitC(N, beta_0, beta_X, beta_C, gamma, sigma_e, sigma_u)
)

# Look at mean and sd of this sampling distribution
mean(beta_X_hats_omitC)   # will NOT be close to 0.5
sd(beta_X_hats_omitC)

# Histogram of the treatment coefficient when C is omitted
hist(beta_X_hats_omitC,
     breaks = 30,
     main = "Sampling Dist. of beta_X_hat (confounder omitted)",
     xlab  = "Estimated beta_X (model without C)")


