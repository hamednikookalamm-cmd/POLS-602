set.seed(123)
n <- 2000   # sample size

## 1. Exogenous variables (no parents)
C  <- rnorm(n)   # confounder
Ey <- rnorm(n)   # Y-only exogenous variable
Z  <- rnorm(n)   # instrument (X-only exogenous)

## 2. Treatment
X  <- 0.8*C + 0.6*Z + rnorm(n)

## 3. Mediator
M  <- 0.7*X + 0.3*C + rnorm(n)

## 4. Outcome
Y  <- 0.5*X + 0.6*M + 0.8*C + 0.7*Ey + rnorm(n)

## 5. Collider
Coll <- 0.7*X + 0.7*Y + rnorm(n)

## Final dataset
dat <- data.frame(Y, X, C, M, Coll, Ey, Z)
head(dat)

# Model for the direct effect of X on Y
mod_direct <- lm(Y ~ X + C + M, data = dat)
summary(mod_direct)
