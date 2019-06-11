


contr.bayes(2)  ## Q_2 in Rouder et al. (2012, p. 363)
#            [,1]
# [1,] -0.7071068
# [2,]  0.7071068

contr.bayes(5)  ## equivalent to Q_5 in Rouder et al. (2012, p. 363)

## check decomposition
Q3 <- contr.bayes(3)
Q3 %*% diag(2) %*% t(Q3)
## 2/3 on diagonal and -1/3 on off-diagonal elements
