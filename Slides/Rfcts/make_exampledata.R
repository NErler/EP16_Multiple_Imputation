###############################################################################
# Example data
###############################################################################

library(splines)

# longitudinal data ------------------------------------------------------------
# * DFexlong1 ------------------------------------------------------------------
set.seed(2018)
N <- 50
id <- 1:N
nj <- sample(3:5, N, replace = T)
ti <- sapply(nj, sample, x = seq(1, 9, by = 2), prob = c(0.1, rep(0.2, 4)))
time <- lapply(ti, function(x) sort(runif(length(x), min = x - 1, max = x)))

x1 <- rnorm(N, 28, 2.5) # maternal age
x2 <- factor(sample(c("boy", "girl"), N, replace = T)) # gender
x3 <- factor(sample(c("low", "mid", "high"), N, replace = T)) # income/educ
x4 <- runif(N, min = 37, max = 42)

DFexlong_orig <- data.frame(id = rep(1:N, sapply(time, length)),
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
Z <- model.matrix(~1 + time, DFexlong_orig)

betas <- c(Intercept = 15,
           time1 = -0.4, time2 = 0.3, time3 = 0.6,
           matage = 0.3,
           gender = -0.9,
           educ2 = -0.4, educ3 = -0.9,
           gestbir = 0.3)

fmla <- ~ ns(time, df = 3) + x1 + x2 + x3 + x4
X <- model.matrix(fmla, model.frame(fmla, DFexlong_orig, na.action = "na.pass"))

DFexlong_orig$y <- as.numeric(X %*% betas +
                                rowSums(Z * b[match(DFexlong_orig$id,
                                                    unique(DFexlong_orig$id)), , drop = F]) +
                                rnorm(nrow(DFexlong_orig), 0, 0.1))

DFexlong <- DFexlong_orig[, c("id", paste0("x", 1:4), "y", "time", "tp", "ti")]


# create missing values
DFexlong$x2[DFexlong$id %in% c(6, 18, sample.int(N, size = 0.3 * N))] <- NA
DFexlong$x3[DFexlong$id %in% c(6, sample.int(N, size = 0.3 * N))] <- NA
DFexlong$x4[DFexlong$id %in% sample.int(N, size = 0.3 * N)] <- NA

DFexlongwide <- reshape(DFexlong, direction = 'wide',
                        v.names = c("y", "time"), idvar = "id",
                        timevar = 'ti', drop = "tp")



IDs <- c(5, 6, 7, 8, 18)
subIDs <- IDs[IDs != 7]
subDFexlong <- DFexlong[DFexlong$id %in% IDs, ]


subDFexlongwide <- DFexlongwide[DFexlongwide$id %in% subIDs, ]


# * DFexlong2 ------------------------------------------------------------------
# sim data2 longitudinal --------------------------------------------------------
set.seed(2241)
N <- 150
id <- 1:N
t1 <- runif(N, 0, 10)
nj <- sample(5:10, N, replace = T)
time <- lapply(1:N, function(x) sort(runif(nj[x], min = t1[x], max = t1[x] + nj[x])))


x1 <- rnorm(N, 28, 2.5) # maternal time
x2 <- factor(sample(c(0, 1), N, replace = T)) # gender
x3 <- factor(sample(c(1:3), N, replace = T)) # income/educ
x4 <- rnorm(N, 35, 4)

DFexlong2_orig <- data.frame(id = rep(1:N, sapply(time, length)),
                             time = unlist(time),
                             ti = unlist(lapply(nj, seq, from = 1, by = 1)),
                             x1 = rep(x1, sapply(time, length)),
                             x2 = rep(x2, sapply(time, length)),
                             x3 = rep(x3, sapply(time, length)),
                             x4 = rep(x4, sapply(time, length))
)

D <- matrix(nrow = 2, ncol = 2,
            data = c(0.2, -0.005, -0.005, 0.001))

b <- MASS::mvrnorm(N, c(0,0), D)
Z <- model.matrix(~1 + time, DFexlong2_orig)

betas <- c(Intercept = 15,
           time1 = -0.7, time2 = 0.3, time3 = 0.8,
           matage = 0.3,
           gender = -0.9,
           educ2 = -0.4, educ3 = -0.9,
           gestbir = 0.01)


fmla <- ~ ns(time, df = 3) + x1 + x2 + x3 + x4
X <- model.matrix(fmla, model.frame(fmla, DFexlong2_orig, na.action = "na.pass"))


DFexlong2_orig$y <- as.numeric(X %*% betas +
                                 rowSums(Z * b[match(DFexlong2_orig$id,
                                                     unique(DFexlong2_orig$id)), , drop = F]) +
                                 rnorm(nrow(DFexlong2_orig), 0, 0.03))

DFexlong2 <- DFexlong2_orig

# create missing values
DFexlong2$x2[DFexlong2$id %in% c(sample.int(N, size = 0.3 * N))] <- NA
DFexlong2$x3[DFexlong2$id %in% c(sample.int(N, size = 0.3 * N))] <- NA
DFexlong2$x4[DFexlong2$id %in% c(sample.int(N, size = 0.3 * N))] <- NA

DFexlong2_sub <- DFexlong2[DFexlong2$id %in% c(1, 2, 6, 15, 16), ]
# ggplot(DFexlong2_sub, aes(x = time, y = y, color = factor(id))) +
#   geom_line() +
#   theme(legend.position = 'none')

# coefDFexlong2 <- as.data.frame(t(sapply(lapply(split(DFexlong2_orig,
#                                                      DFexlong2_orig$id),
#                                                lm, formula = y ~ age), coef)))
#
# coefDFexlong22 <- as.data.frame(t(sapply(lapply(split(DFexlong2_orig, DFexlong2$id),
#                                                 lm, formula = y ~ age + I(age^2)), coef)))
