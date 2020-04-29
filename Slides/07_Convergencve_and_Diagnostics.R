## ---- opts.label = "dopurl"-------------------------------------------------------------------------------------------
library("mice")
imp0 <- mice(NHANES, maxit = 0,
             defaultMethod = c("norm", "logreg", "polyreg", "polr"))

meth <- imp0$method
meth["HyperMed"] <- ""
meth["BMI"] <- "~I(weight/height^2)"

pred <- imp0$predictorMatrix
pred[, "HyperMed"] <- 0

post <- imp0$post
post["creat"] <- "imp[[j]][,i] <- squeeze(imp[[j]][,i], c(0, 100))"

## ---- opts.label = "dopurl"-------------------------------------------------------------------------------------------
pred[c("weight", "height"), "BMI"] <- 0

