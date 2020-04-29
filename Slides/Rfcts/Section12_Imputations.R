projdir <- gsub("/Slides", "", getwd())

load(file.path(projdir, "Slides/workspaces/longDF2.RData"))
library(mice)
library(lme4)


## ---- mice_set-up -----------------------------------------------------------
imp0 <- mice(longDF2, maxit = 0)
meth <- imp0$method
pred <- imp0$predictorMatrix

## ---- mice_settings ----------------------------------------------------------
# meth[c("x2", "x3")] <- "2lonly.pmm"
meth[c("x3")] <- "2lonly.pmm"
# meth[c("x2")] <- "2lonly.pmm"
meth[c("x4")] <- "2lonly.norm"

pred[, "id"] <- -2  # identify id variable
pred[, "ti"] <- 0 # don't use time-point indicator

## ---- mice_imp ---------------------------------------------------------------
imp <- mice(longDF2, maxit = 10, method = meth,
            predictorMatrix = pred, printFlag = FALSE)


## ---- mice_analysis_long -----------------------------------------------------
library(lme4)
models <- with(imp, lmer(y ~ x1 + x2 + x3 + x4 + time + I(time^2) +
                           (time|id),
                         control = lmerControl(optimizer = "Nelder_Mead")
))
mice_longimp <- summary(pool(models), conf.int = TRUE)


## ----miceimp_longDF2_naive ---------------------------------------------------
pred[, c("id", "ti")] <- 0
impnaive <- mice(longDF2, predictorMatrix = pred, maxit = 10)
models2 <- with(impnaive,
                lmer(y ~ x1 + x2 + x3 + x4 + time + I(time^2) + (time|id)))

mice_longimp_naive <- summary(pool(models2), conf.int = TRUE)
```
