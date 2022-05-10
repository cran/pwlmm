#** Load Package **
library("pwlmm")
library(Rcpp)
library(RcppEigen)
library(ggplot2)
library(car)
library(Hmisc)
library(dplyr)

#** Load Data **
data(datamultv)

#** Two-level multivariate multilevel model with weights **#
#** Null Model - TOEPLITZ
modmm0 <- wmlmm(Y ~  1, data = datamultv, PSU, idd, wave, wj, wi_j, "toep")

#* To display results
 modmm0

#** Variance components
attributes(modmm0)

#* Toeplitz Matrix
modmm0$mix_teta_matrix

#* Variance Matrix
modmm0$tot_var_matrix


#** Complete model
modmm <- wmlmm(Y ~  X1 + X2 + X3 + X4 + X5, data = datamultv, PSU, idd, wave, wj, wi_j, "toep")

#* To display results
modmm

#** Variance components
#* Toeplitz Matrix
modmm$mix_teta_matrix

#* Variance Matrix
modmm$tot_var_matrix

## Fixed effects & Standard Errors
modmm$beta$coefficients
modmm$beta$standard_errors


### Display 4 plots
## Residuals
plot(resid(modmm))
abline(h = 0, col = "red")
qqPlot(resid(modmm))
hist(resid(modmm))

## Level 2 
## Residuals Level 2
level2Res <- modmm$group_residuals
uj1 <- level2Res$coefficients
ujse <- level2Res$standard_errors
lower <- uj1 - 1.96*ujse
upper <- uj1 + 1.96*ujse
perm <- order(uj1)
uj1_sort <- uj1[order(uj1)]
upper_sort <- upper[perm]
lower_sort <- lower[perm]
#** Caterpilar Plot
errbar(1:NROW(uj1) , uj1_sort, upper_sort, lower_sort,
       xlab = "Level 2 res", ylab = "Multivariate Model" )
abline(h = 0, col = "red")
points(1:NROW(uj1), uj1_sort, col = "blue")


#** General linear
#** Null Model - Genlin
modmm0 <- wmlmm(Y ~  1, data = datamultv, PSU, idd, wave, wj, wi_j, "genlin",
                r=c(1,1,1,1,1))

modmm0
#** Variance components
#* General Linear Matrix
modmm0$mix_teta_matrix

#* Variance Matrix
modmm0$tot_var_matrix

#** Complete model
modmm <- wmlmm(Y ~  X1 + X2 + X3 + X4 + X5, data = datamultv, PSU, idd, wave, wj, wi_j,
               "genlin", r=c(1,1,1,1,1))
modmm

#** Variance components
#* General Liner
modmm$mix_teta_matrix

#* Variance Matrix
modmm$tot_var_matrix

## Fixed effects & Standard Errors
modmm$beta$coefficients
modmm$beta$standard_errors

### Display 4 plots
## Residuals
plot(resid(modmm))
abline(h = 0, col = "red")
qqPlot(resid(modmm))
hist(resid(modmm))

## Level 2 
## Residuals Level 2
level2Res <- modmm$group_residuals
uj1 <- level2Res$coefficients
ujse <- level2Res$standard_errors
lower <- uj1 - 1.96*ujse
upper <- uj1 + 1.96*ujse
perm <- order(uj1)
uj1_sort <- uj1[order(uj1)]
upper_sort <- upper[perm]
lower_sort <- lower[perm]
#** Caterpilar Plot
errbar(1:NROW(uj1) , uj1_sort, upper_sort, lower_sort,
       xlab = "Level 2 res", ylab = "Multivariate Model" )
abline(h = 0, col = "red")
points(1:NROW(uj1), uj1_sort, col = "blue")



## Histogram for response variable
ggplot(  datamultv , aes(x = Y))+
  geom_histogram(color="black", fill="white")+
  facet_grid(wave ~ .)

