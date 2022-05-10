#** Load Package **
library("pwlmm")
library(ggplot2)
library(car)
library(Hmisc)
library(Rcpp)
library(RcppEigen)


#** Load Data **
data(dataw1)

#** Null Model
mod2L0 <- pwigls2 (Y ~ 1 + (1 | PSU), data = dataw1, wj, wi_j)
mod2L0

#** Two-level Random Intercept  model with weights **#
mod2L <- pwigls2 (Y ~ X1 + X2 + X3 + X4 + X5 + (1 | PSU), data = dataw1, wj, wi_j)

## To display results
mod2L

## Fixed effects & Standard Errors
mod2L$beta$coefficients
mod2L$beta$standard_errors

## Variance components
### Random intercepts variance
mod2L$theta$level2_variances
mod2L$theta$var_cov

### Level one
mod2L$theta$level1_variance

### Display 4 plots
## Residuals

plot(resid(mod2L))
qqPlot(resid(mod2L))
hist(resid(mod2L))

## Residuals Level 2
level2Res <- mod2L$group_residuals
uj1 <- level2Res$coefficients
ujse <- level2Res$standard_errors
lower <- uj1 - 1.96*ujse
upper <- uj1 + 1.96*ujse
perm <- order(uj1)
uj1_sort <- uj1[perm,]
upper_sort <- upper[perm,]
lower_sort <- lower[perm,]


### Caterpilar Plot
errbar(1:NROW(uj1) , uj1_sort, upper_sort, lower_sort,
       xlab = "u_rank", ylab = "Random intercepts" )
abline(h = 0, col = "red")
points(1:NROW(uj1), uj1_sort, col = "blue")

#** Two-level Random Coefficients model with weights **#
mod2LRC <- pwigls2 (Y ~ X1 + X2 + X3 + X4 + X5 + (X1 | PSU), data = dataw1, wj, wi_j)

## Fixed effects & Standard Errors
mod2LRC$beta$coefficients
mod2LRC$beta$standard_errors
mod2LRC

## Variance components
### Variances
mod2LRC$theta$level2_variances
mod2LRC$theta$level2_covariances
mod2LRC$theta$se_level2_covariances
mod2LRC$theta$var_cov

### Level one
mod2LRC$theta$level1_variance

# Residuals
level2Res <- mod2LRC$group_residuals

plot(mod2LRC$fitted_values, mod2LRC$individual_residuals )
plot(level2Res$coefficients[,1], level2Res$coefficients[,2], xlab = "u0j", ylab = "u1j")
abline(h = 0, col = "red")
abline(v = 0, col = "red")
hist(level2Res$coefficients[,1])
hist(level2Res$coefficients[,2])
