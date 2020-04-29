###############################################################################
# Example data
###############################################################################
projdir <- gsub("/Slides", "", getwd())

library(splines)

# longitudinal data ------------------------------------------------------------
# * longDF1 ------------------------------------------------------------------
set.seed(2020)
N <- 50
id <- 1:N
nj <- sample(3:5, N, replace = T)
ti <- sapply(nj, sample, x = seq(1, 9, by = 2), prob = c(0.1, rep(0.2, 4)))
time <- lapply(ti, function(x) sort(runif(length(x), min = x - 1, max = x)))

x1 <- rnorm(N, 28, 2.5) # maternal age
x2 <- factor(sample(c("boy", "girl"), N, replace = T)) # gender
x3 <- factor(sample(c("low", "mid", "high"), N, replace = T)) # income/educ
x4 <- runif(N, min = 37, max = 42)

longDF1_orig <- data.frame(id = rep(1:N, sapply(time, length)),
                            time = unlist(time),
                            tp = unlist(sapply(time, seq_along)),
                            ti = unlist(sapply(ti, sort)),
                            x1 = rep(x1, sapply(time, length)),
                            x2 = rep(x2, sapply(time, length)),
                            x3 = rep(x3, sapply(time, length)),
                            x4 = rep(x4, sapply(time, length))
)

D <- matrix(nrow = 2, ncol = 2, data = c(0.2, -0.02, -0.02, 0.005))
b <- MASS::mvrnorm(N, c(0,0), D)
Z <- model.matrix(~1 + time, longDF1_orig)

betas <- c(Intercept = 15,
           time1 = -0.4, time2 = 0.3, time3 = 0.6,
           matage = 0.3,
           gender = -0.9,
           educ2 = -0.4, educ3 = -0.9,
           gestbir = 0.3)

fmla <- ~ ns(time, df = 3) + x1 + x2 + x3 + x4
X <- model.matrix(fmla, model.frame(fmla, longDF1_orig, na.action = "na.pass"))

longDF1_orig$y <- as.numeric(X %*% betas +
                                rowSums(Z * b[match(longDF1_orig$id,
                                                    unique(longDF1_orig$id)), , drop = F]) +
                                rnorm(nrow(longDF1_orig), 0, 0.1))

longDF1 <- longDF1_orig[, c("id", paste0("x", 1:4), "y", "time", "tp", "ti")]


# create missing values
longDF1$x2[longDF1$id %in% c(1, 4, sample(5:N, size = N/2))] <- NA
longDF1$x3[longDF1$id %in% c(2, 3, sample(5:N, size = N/2))] <- NA
longDF1$x4[longDF1$id %in% c(2, 3, sample(5:N, size = N/2))] <- NA

wideDF1 <- reshape(longDF1, direction = 'wide',
                        v.names = c("y", "time"), idvar = "id",
                        timevar = 'ti', drop = "tp")

# * DFexlong2 ------------------------------------------------------------------
# sim data2 longitudinal --------------------------------------------------------
set.seed(1234)
N <- 150
id <- 1:N
t1 <- runif(N, 0, 10)
nj <- sample(5:10, N, replace = T)
time <- lapply(1:N, function(x) sort(runif(nj[x], min = t1[x], max = t1[x] + nj[x])))

x1 <- rnorm(N, 28, 2.5) # maternal time
x2 <- factor(sample(c(0, 1), N, replace = T)) # gender
x3 <- factor(sample(c(1:3), N, replace = T)) # income/educ
x4 <- rnorm(N, 35, 4)

longDF2_orig <- data.frame(id = rep(1:N, sapply(time, length)),
                           time = unlist(time),
                           ti = unlist(lapply(nj, seq, from = 1, by = 1)),
                           x1 = rep(x1, sapply(time, length)),
                           x2 = rep(x2, sapply(time, length)),
                           x3 = rep(x3, sapply(time, length)),
                           x4 = rep(x4, sapply(time, length))
)
longDF2_orig$time <- longDF2_orig$time - mean(longDF2_orig$time)

D <- matrix(nrow = 2, ncol = 2,
            data = c(0.2, -0.005, -0.005, 0.001))

b <- MASS::mvrnorm(N, c(0,0), D)
Z <- model.matrix(~1 + time, longDF2_orig)

betas <- c(Intercept = 15,
           time1 = -0.7, time2 = 0.3, time3 = 0.8,
           matage = 0.3,
           gender = -0.3,
           educ2 = -0.4, educ3 = -0.9,
           gestbir = 0.01)


fmla <- ~ ns(time, df = 3) + x1 + x2 + x3 + x4
X <- model.matrix(fmla, model.frame(fmla, longDF2_orig, na.action = "na.pass"))


longDF2_orig$y <- as.numeric(X %*% betas +
                               rowSums(Z * b[match(longDF2_orig$id,
                                                   unique(longDF2_orig$id)), , drop = F]) +
                               rnorm(nrow(longDF2_orig), 0, 0.03))

longDF2 <- longDF2_orig


# create missing values
longDF2$x2[longDF2$id %in% c(sample.int(N, size = 0.3 * N))] <- NA
longDF2$x3[longDF2$id %in% c(sample.int(N, size = 0.3 * N))] <- NA
longDF2$x4[longDF2$id %in% c(sample.int(N, size = 0.3 * N))] <- NA

save(longDF2,file = file.path(projdir, "Slides/workspaces/longDF2.RData"))

# DFexlong2_sub <- DFexlong2[DFexlong2$id %in% c(1, 2, 6, 15, 16), ]

# ggplot(subset(longDF2, id < 10), aes(x = time, y = y, color = factor(id))) +
#   geom_line() +
#   theme(legend.position = 'none')


# coefDFexlong2 <- as.data.frame(t(sapply(lapply(split(DFexlong2_orig,
#                                                      DFexlong2_orig$id),
#                                                lm, formula = y ~ age), coef)))
#
# coefDFexlong22 <- as.data.frame(t(sapply(lapply(split(DFexlong2_orig, DFexlong2$id),
#                                                 lm, formula = y ~ age + I(age^2)), coef)))


# ```{r simlong2 data, echo = FALSE, fig.with = 6, fig.height = 4}
# coefDFexlong2 <- as.data.frame(t(sapply(lapply(split(DFexlong2, DFexlong2$id),
#                                                lm, formula = y ~ time), coef)))
# 
# coefDFexlong22 <- as.data.frame(t(sapply(lapply(split(DFexlong2, DFexlong2$id),
#                                                 lm, formula = y ~ time + I(time^2)), coef)))
# ```



# Survival example -------------------------------------------------------------

library(MASS)
library(splines)
n <- 300 # number of subjects

# parameters for the survival model
phi <- 1.6458 # shape for the Weibull baseline hazard
mean.Cens <- 12 # mean of the exponential distribution for the censoring mechanism

################################################
gammas <- c("(Intercept)" = -5.7296, "x2" = 2.4092, "x1" = -2, x3 = 0.2) # coefficients for baseline covariates

set.seed(2019)
x2 <- rep(0:1, each = n/2) # group indicator, i.e., '0' placebo, '1' active treatment
x1 <- rnorm(n)
x3 <- rnorm(n)

# design matrix for the survival model
W <- cbind("(Intercept)" = 1,
           "x2" = x2,
           "x1" = x1,
           "x3" = x3)

################################################

# simulate event times
eta.t <- as.vector(W %*% gammas)
invS <- function(t, u, i) {
  h <- function(s) {
    x20 <- 1 - x2[i]
    x21 <- x2[i]
    exp(log(phi) + (phi - 1) * log(s) + eta.t[i])
  }
  integrate(h, lower = 0, upper = t)$value + log(u)
}
u <- runif(n)
trueTimes <- numeric(n)
for (i in 1:n) {
  Up <- 50
  tries <- 5
  Root <- try(uniroot(invS, interval = c(1e-05, Up), u = u[i], i = i)$root, TRUE)
  while (inherits(Root, "try-error") && tries > 0) {
    tries <- tries - 1
    Up <- Up + 200
    Root <- try(uniroot(invS, interval = c(1e-05, Up), u = u[i], i = i)$root, TRUE)
  }
  trueTimes[i] <- if (!inherits(Root, "try-error")) Root else NA
}
na.ind <- !is.na(trueTimes)
trueTimes <- trueTimes[na.ind]
W <- W[na.ind, , drop = FALSE]

n <- length(trueTimes)

# simulate censoring times from an exponential distribution,
# and calculate the observed event times, i.e., min(true event times, censoring times)
Ctimes <- runif(n, 0, 2 * mean.Cens)
Time <- pmin(trueTimes, Ctimes)
event <- as.numeric(trueTimes <= Ctimes) # event indicator

survdat_orig <- data.frame(Time = Time,
                           event = event,
                           x2 = x2[na.ind],
                           x1 = x1[na.ind],
                           x3 = x3[na.ind])



survdat <- survdat_orig
N = n
survdat$x1[sample(1:N, N*0.3)] <- NA
survdat$x3[sample(1:N, N*0.3)] <- NA
survdat$x2[sample(1:N, N*0.3)] <- NA
survdat$x2 <- factor(survdat$x2)
survdat_orig$x2 <- factor(survdat_orig$x2)

save(survdat, survdat_orig, file = file.path(projdir, "Slides/workspaces/survdat.RData"))
