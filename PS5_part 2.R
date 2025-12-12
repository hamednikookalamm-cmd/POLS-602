## PS 5
## part 2
# Question a


# I want to Create a simple data set

set.seed(202) 

# total sample size
N <- 200              

# Binary group: 0 = control, 1 = treated
treat <- rbinom(N, size = 1, prob = 0.5)

# Outcome:
# control mean = 10
# treated mean = 12 (difference in means = 2)
Y <- 10 + 2 * treat + rnorm(N, mean = 0, sd = 3)

# Put into a data frame
dat2 <- data.frame(Y = Y, treat = treat)

## Difference in means test

# Look at group means first
tapply(dat2$Y, dat2$treat, mean)

# Two-sample t-test:
# H0: mean(Y | treat = 1) = mean(Y | treat = 0)
# HA: means are different (two-sided test), alpha = 0.05
t_test_out <- t.test(Y ~ treat,
                     data = dat2,
                     var.equal = TRUE)  # assume equal variances

t_test_out


## Question b

## Linear model

lm_out <- lm(Y ~ treat, data = dat2)
summary(lm_out)

