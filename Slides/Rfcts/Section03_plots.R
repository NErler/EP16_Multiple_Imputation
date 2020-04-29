projdir <- gsub("/Slides", "", getwd())


library(mice)
library(ggplot2)
library(plyr)


set.seed(123)
N <- 100
p <- 4
m <- 3

S <- diag(c(0.5, 5, 1, 1))
s <- c(0.7, -0.5, 0.15, 0.15, 0.1, 0.2)
S[upper.tri(S)] <- s
S[lower.tri(S)] <- s

X <- MASS::mvrnorm(n = N,
                   mu = c(0.2, 12, 3, 0.2),
                   Sigma = S)
colnames(X) <- paste0("x", 1:4)

DF_orig <- as.data.frame(cbind(id = 1:N, X))
DF_orig$id <- as.integer(DF_orig$id)

p1 <- rbinom(n = nrow(DF_orig), size = 1, prob = plogis(-5 + 2*DF_orig$x3))
p2 <- rbinom(n = nrow(DF_orig), size = 1, prob = plogis(4 - DF_orig$x3))
p3 <- rbinom(n = nrow(DF_orig), size = 1, prob = plogis(4 - DF_orig$x3))

p1[1:3] <- c(0, 1, 0)
p2[1:3] <- c(1, 0, 1)
p3[1:3] <- c(1, 0, 0)

DF_orig[p1 == 0, "x2"] <- NA
DF_orig[p2 == 0, "x3"] <- NA
DF_orig[p3 == 0, "x4"] <- NA

imp <- mice(DF_orig, m = m)
# round(complete(imp, 1)[1:3, ], 1)
res <- with(imp, lm(x1 ~ x2 + x3 + x4))

for (i in 1:3) {
  subres <- round(summary(res$analyses[[i]])$coef[,1:2], 2)
  pr <- paste0(paste0("$\\beta_", 0:3, "$"), " & ",
               apply(subres, 1, function(x) {
                 paste0(sprintf("%.2f", x), collapse = " & ")}
                 ), collapse = "\\\\\n")
  
  assign(paste0("print", i), pr)
}


reslist <- lapply(seq_along(res$analyses),
                  function(i) {
                    x <- res$analyses[[i]]
                    a <- as.data.frame(cbind(coef(x), confint(x)))
                    names(a) <- c("coef", "lo", "hi")
                    a$imp <- i
                    a$var <- rownames(a)
                    a
                  })

poolDF <- do.call(rbind, reslist)

# poolcalc ----------------------------------------------------------------------
seDF <- do.call(rbind,
                lapply(seq_along(res$analyses), function(i){
                  ses <- as.data.frame(summary(res$analyses[[i]])$coef[, 2, drop = F])
                  ses$var <- rownames(ses)
                  ses$imp <- i
                  ses
                })
)
means <- ddply(poolDF, "var", summarize, coefmean = mean(coef))
ses <- ddply(seDF, "var", summarize, semean = mean(`Std. Error`))

CIs <- ddply(poolDF, "var", summarize, himean = mean(hi), lomean = mean(lo))

pooled <- as.data.frame(summary(pool(res), conf.int = TRUE)[, c("estimate", "2.5 %", "97.5 %")])
pooled$imp <- "pooled"
pooled$var <- rownames(reslist[[1]])




# poolplot1a ---------------------------------------------------------------------
ppool0 <- ggplot(poolDF, aes(x = coef, y = imp)) +
  geom_point() +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.25) +
  facet_grid(~var, scales = 'free') +
  theme(strip.text = element_text(size = 10, face = "bold"),
        panel.background = element_rect(fill = grey(0.98), color = grey(0.85)),
        strip.background = element_rect(fill = grey(0.85), color = grey(0.85)),
        panel.grid = element_blank()) +
  scale_y_discrete(limits = c(1, 2, 3),
                   labels = c(paste("imp", 1:3))) +
  ylab("") +
  scale_x_continuous(breaks = NULL) +
  xlab("parameter estimate & 95% confidence interval")

ggsave(ppool0,
       file = file.path(projdir, 'Slides/graphics/Section03', 'poolplot1a.pdf'),
       width = 6, height = 1.5)


# poolplot1b ------------------------------------------------------------------
ppool1 <- ppool0 +
  geom_vline(data = ddply(poolDF, "var", summarize, coefmean = mean(coef)),
             aes(xintercept = coefmean), lty = 2)
ggsave(ppool1,
       file = file.path(projdir, 'Slides/graphics/Section03', 'poolplot1b.pdf'),
       width = 6, height = 1.5)

# poolplot1c -------------------------------------------------------------------
ppool2 <- ppool0 +
  geom_vline(data = ddply(poolDF, "var", summarize, coefmean = mean(coef)),
             aes(xintercept = coefmean), lty = 2) +
  geom_vline(data = CIs, aes(xintercept = himean), col = 2, lty = 2) +
  geom_vline(data = CIs, aes(xintercept = lomean), col = 2, lty = 2)

ggsave(ppool2,
       file = file.path(projdir, 'Slides/graphics/Section03', 'poolplot1c.pdf'),
       width = 6, height = 1.5)



# cis_final ---------------------------------------------------------------------
CIDF <- data.frame(
  x = rep(unlist(t(pooled[, 2:3])), each = 2),
  y = rep(c(0.5, 3.5)[c(1,2,2,1)], nrow(pooled)),
  var = rep(pooled$var, each = 4)
)

cis_final <- ggplot(poolDF, aes(x = coef, y = as.numeric(imp))) +
  geom_polygon(data = CIDF, aes(x = x, y = y), fill = "blue4", alpha = 0.2) +
  geom_point() +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.25) +
  facet_grid(~var, scales = 'free') +
  theme(strip.text = element_text(size = 10, face = "bold"),
        panel.background = element_rect(fill = grey(0.98), color = grey(0.85)),
        strip.background = element_rect(fill = grey(0.85), color = grey(0.85)),
        panel.grid = element_blank()) +
  scale_y_discrete(limits = c(1, 2, 3),
                   labels = c(paste("imp", 1:3))) +
  ylab("") +
  scale_x_continuous(breaks = NULL) +
  xlab("parameter estimate & 95% confidence interval") +
  geom_vline(data = ddply(poolDF, "var", summarize, coefmean = mean(coef)),
             aes(xintercept = coefmean), lty = 2) +
  geom_vline(data = CIs, aes(xintercept = himean), col = 2, lty = 2, alpha = 0.5) +
  geom_vline(data = CIs, aes(xintercept = lomean), col = 2, lty = 2, alpha = 0.5)


ggsave(cis_final,
       file = file.path(projdir, 'Slides/graphics/Section03', 'cis_final.pdf'),
       width = 6, height = 1.5)






# m <- 3
# alpha = 0.05
# Q <- means[,2]
# B <- 1/(m - 1) * colSums(matrix(nrow = m, data = (poolDF[, 1] - rep(Q, m))^2, byrow = T))
# U <- ses[, 2]^2
# T <- U + B + B/m
# 
# r <- (B + B/m)/U
# 
# nu = (m - 1) * (1 + mic$r^{-1})^2
# 
# gamma = (r + 2/(nu+3))/(r+1)
# 
# Q + qt(alpha/2, df = nu) * T^{1/2}
# Q - qt(alpha/2, df = nu) * T^{1/2}
# 
# 1 - pf(Q^2/T, df1 = 1, df2 = nu)
