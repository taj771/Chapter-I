rm(list = ls())

---
title: "Monte Carlo Simulation Using For Loops (elasticity/standard error)"
     : two variables appear in the elasticity equation, cov has considered
output: html_notebook
---

This is a simulation design. 

```{r}
library(pacman)
pacman::p_load(data.table, fixest, stargazer, dplyr, magrittr, faux) 
```

Model: 
$$
Y =  (\beta_1+2\beta_2WQ)*WQ +U
$$

```{r OLS-n-100-M-100000}

## Parameters and seed
#beta_1 = 0.067
#beta_2 = - 0.001
set.seed(1)  # Seed
n = 1000     # Sample size
M = 100000   # Number of experiments/iterations
r = 0.9 # correlation

## Storage 
slope_1 <- rep(0,M)


## Begin Monte Carlo

for (i in 1:M){ #  M is the number of iterations
  
  # Generate data
  U_i = rnorm(n, mean = 0, sd = 1) # Error
  data = mvrnorm(n, mu=c(0.029,  0.00034), Sigma=matrix(c(1, r, r, 1), nrow=2), empirical=TRUE)
  X1_i = data[, 1]  # standard normal
  X2_i = data[, 2]  # standard normal
  Y_i = 15.75*X1_i + 2*X2_i*15.75*15.75 + U_i  # Dependent variable
  
  # Formulate data.table
  data_i = data.table(X1 = X1_i,X2 = X2_i, Y = Y_i)
  
    # Run regressions
  ols_i <- lm(data = data_i, Y ~ 1)
  
  # Extract slope coefficient and save
  slope_1[i] <- ols_i$coefficients[1]
}


# Summary statistics
estimates_DT <- data.table(beta_1 = slope_1)
stargazer(estimates_DT[, c("beta_1")], type = "text", digits = 5)




```


Model: SE
$$
Y =  (\beta_1+2\beta_2WQ)*WQ +U
$$

```{r OLS-n-100-M-100000}

## Parameters and seed
#beta_1 = 0.067
#beta_2 = - 0.001
set.seed(1)  # Seed
n = 1000     # Sample size
M = 10000   # Number of experiments/iterations
r = 1 # correlation

## Storage 
slope_1 <- rep(0,M)


## Begin Monte Carlo

for (i in 1:M){ #  M is the number of iterations
  
  # Generate data
  U_i = rnorm(n, mean = 0, sd = 1) # Error
  data = mvrnorm(n, mu=c(0.025, - 0.00023), Sigma=matrix(c(1, r, r, 1), nrow=2), empirical=TRUE)
  X1_i = data[, 1]  # standard normal (mu=0, sd=1)
  X2_i = data[, 2]  # standard normal (mu=0, sd=1)
  Y_i = 49.10*X1_i + 2*X2_i*49.10*49.10 + U_i  # Dependent variable
  
  # Formulate data.table
  data_i = data.table(X1 = X1_i,X2 = X2_i, Y = Y_i)
  
    # Run regressions
  ols_i <- lm(data = data_i, Y ~ 1)
  
  # Extract slope coefficient and save
  slope_1[i] <- ols_i$coefficients[1]
}


# Summary statistics
estimates_DT <- data.table(beta_1 = slope_1)
stargazer(estimates_DT[, c("beta_1")], type = "text", digits = 5)




```

