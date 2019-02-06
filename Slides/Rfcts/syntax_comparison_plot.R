library(mice)
library(ggplot2)

# simulate data ----------------------------------------------------------------
set.seed(2018)
N <- 200
x <- rnorm(N)
z <- rbinom(N, size = 1, prob = plogis(x))
y <- x + x^2 + z + x*z + rnorm(N, 0, 0.5)

DF_nonlin_orig <- data.frame(y = y, x = x, z = z)


# create missing values
DF_nonlin <- DF_nonlin_orig
DF_nonlin$x[sample(1:length(x), size = N/2)] <- NA


# naive imputation -------------------------------------------------------------
impnaive <- mice(DF_nonlin, printFlag = F)

# JAV imputation ---------------------------------------------------------------
DF2 <- DF_nonlin
DF2$xx <- DF2$x^2
DF2$xz <- DF2$x * DF2$z

impJAV <- mice(DF2, printFlag = F, maxit = 20)


# Fit the models ---------------------------------------------------------------
# model on complete data
mod_nonlin <- lm(y ~ x + I(x^2) + z + x:z, data = DF_nonlin_orig)
res_nonlin <- cbind(coef(mod_nonlin), confint(mod_nonlin))

# complete case analysis
mod_cc <- lm(y ~ x + I(x^2) + z + x:z, data = DF_nonlin)
res_cc <- cbind(coef(mod_cc), confint(mod_cc))

# model and result for naive imputation
mod_impnaive <- with(impnaive, lm(y ~ x + I(x^2) + z + x:z))
res_impnaive <- summary(pool(mod_impnaive))[, c("est", 'lo 95', 'hi 95')]

# model and result for JAV imputation
mod_JAV <- with(impJAV, lm(y ~ x + xx + z + xz))
res_JAV <- summary(pool(mod_impnaive))[, c("est", 'lo 95', 'hi 95')]


# combine the results ----------------------------------------------------------
res <- list(
  cc = as.data.frame(res_cc),
  naive = as.data.frame(res_impnaive),
  JAV = as.data.frame(res_JAV)
)

# set the column names and add variable names and imputation method as columns
for (i in 1:length(res)) {
  colnames(res[[i]]) <- c("coef", "lo", "up")
  res[[i]]$var <- rownames(res[[i]])
  res[[i]]$method <- names(res)[i]
}

# combine results from list into one data.frame for plotting
plotDF <- do.call(rbind, res)

# Get reference values from analysis of original (complete) data for blue area.
# This is actually a polygon in the plot and we need to specify the x-values
# so that it covers the whole x-axis, so we use 0.5 (the first result is plottet
# at x = 1) and "number of methods" + 0.5
refDF <- data.frame(x = rep(c(0.5, length(res) + 0.5, length(res) + 0.5, 0.5), nrow(res[[1]])),
                    # y is the confidence interval of the model on the original data
                    y = c(apply(confint(mod_nonlin), 1, rep, each = 2)),
                    # the variable names need to match the one in plotDF
                    var = rep(c("(Intercept)", "x", "I(x^2)", "z", "x:z"),
                              each = 4),
                    # for the horizontal line we get the coefficients from the model
                    # on the original data
                    est = rep(coef(mod_nonlin), each = 4)
)



# plot -------------------------------------------------------------------------
ggplot(plotDF, aes(x = method, y = coef)) +
  # draw the blue polygon
  geom_polygon(data = refDF,
               aes(x = x, y = y), fill = 'blue', alpha = 0.3) +
  # add the horizontal line
  geom_hline(data = refDF,
             aes(yintercept = est), lty = 2) +
  # points for the pooled estimates from the different imputations
  geom_point() +
  # confidence intervals from the imputations
  geom_errorbar(aes(ymin = lo, ymax = up), width = 0.2) +
  # make differet plots per variable
  facet_wrap("var", scales = 'free') +
  # specify the order of the methods on the x-axis
  scale_x_discrete(limits = c("cc", "naive", "JAV")) +
  # label for the y-axis
  ylab("estimate & 95% confidence interval")
