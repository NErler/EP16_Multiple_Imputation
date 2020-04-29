projdir <- gsub("/Slides", "", getwd())

set.seed(2020)

png(file.path(projdir, 'Slides/graphics/Section04', 'BayesImp_eps.png'),
    width = 900, height = 600, res = 300)
par(mar = c(1.5, 0.1, 0.1, 0.1), mgp = c(-1.5, 0, 0))
plot(density(rnorm(1e3, 0, 0.2)), main = '', xlab = expression(sigma),
     cex.lab = 3, ylab = '', yaxt = 'n',
     xaxt = 'n', lwd = 2, bty = 'n')
dev.off()


png(file.path(projdir, 'Slides/graphics/Section04', 'BayesImp_beta1.png'),
    width = 900, height = 600, res = 300)
par(mar = c(1.5, 0.1, 0.1, 0.1), mgp = c(-1.5, 0, 0))
plot(density(rnorm(1e3)), main = '', xlab = expression(beta[1]),
     cex.lab = 2.5, ylab = '', yaxt = 'n', bty = 'n',
     xaxt = 'n', lwd = 2)
dev.off()


png(file.path(projdir, 'Slides/graphics/Section04', 'BayesImp_beta3.png'),
    width = 900, height = 600, res = 300)
par(mar = c(1.5, 0.1, 0.1, 0.1), mgp = c(-1.5, 0, 0))
plot(density(rnorm(1e3)), main = '', xlab = expression(beta[3]),
     cex.lab = 2.5, ylab = '', yaxt = 'n', bty = 'n',
     xaxt = 'n', lwd = 2)
dev.off()



set.seed(2020)

x <- rep(seq(-0.5, 0.5, by = 0.3), each = 4)
y <- rep(seq(-0.5, 0.5, by = 0.3), 4)

png(file.path(projdir, 'Slides/graphics/Section04', 'BayesImp_betahat1.png'),
    width = 900, height = 900, bg = 'transparent', res = 300)
par(mar = rep(0.4, 4), mgp = c(0, 0, 0), xpd = T)
plot(x, y, type = 'n', bty = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '',
     xlim = range(x) * 1.5,
     ylim = range(y) * 1.5)
text(jitter(y, 2) ~ jitter(x, 2), labels = expression(hat(beta)[1]), cex = 2.5)
dev.off()

png(file.path(projdir, 'Slides/graphics/Section04', 'BayesImp_betahat3.png'),
    width = 900, height = 900, bg = 'transparent', res = 300)
par(mar = rep(0.4, 4), mgp = c(0, 0, 0), xpd = T)
plot(x, y, type = 'n', bty = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '',
     xlim = range(x) * 1.5,
     ylim = range(y) * 1.5)
text(jitter(y, 2) ~ jitter(x, 2), labels = expression(hat(beta)[3]), cex = 2.5)
dev.off()

png(file.path(projdir, 'Slides/graphics/Section04', 'BayesImp_sighat.png'),
    width = 900, height = 900, bg = 'transparent', res = 300)
par(mar = rep(0.4, 4), mgp = c(0, 0, 0), xpd = T)
plot(y ~ x,
     type = 'n', bty = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '',
     xlim = range(x) * 1.5,
     ylim = range(y) * 1.5)
text(jitter(y, 2) ~ jitter(x, 2), labels = expression(hat(sigma)), cex = 3)
dev.off()


y2 <- y + rep(c(0, 0.1), each = 4)

png(file.path(projdir, 'Slides/graphics/Section04', 'BayesImp_Eymis.png'),
    width = 900, height = 900, bg = 'transparent', res = 300)
par(mar = rep(0.4, 4), mgp = c(0, 0, 0), xpd = T)
plot(y2 ~ x,
     type = 'n', bty = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '',
     xlim = range(x) * 1.5,
     ylim = range(y) * 1.5)
text(jitter(y2, 1) ~ jitter(x, 1), labels = expression(E(y[mis])), cex = 1.5)
dev.off()

png(file.path(projdir, 'Slides/graphics/Section04', 'BayesImp_ymis.png'),
    width = 900, height = 900, bg = 'transparent', res = 300)
par(mar = rep(0.4, 4), mgp = c(0, 0, 0), xpd = T)
plot(y2 ~ x,
     type = 'n', bty = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '',
     xlim = range(x) * 1.5,
     ylim = range(y) * 1.5)
text(jitter(y2, 1) ~ jitter(x, 1), labels = expression(hat(y)[mis]), cex = 1.7)
dev.off()


set.seed(2020)
mean <- rnorm(20, 0, 0.3)
sd <- runif(20, 1.5, 1.9)

xx <- seq(-8, 8, length = 100)

png(file.path(projdir, 'Slides/graphics/Section04', 'BayesImp_distr.png'),
    width = 1200, height = 700, bg = 'transparent', res = 300)
par(mar = rep(0.4, 4), mgp = c(0, 0, 0), xpd = T)
plot(xx, dnorm(xx, mean[1], sd[1]), type = 'l', ylim = c(0, 0.26), xlim = c(-7.5, 7.5),
     bty = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')

for (i in seq_along(mean)) {
    lines(xx, dnorm(xx, mean[i], sd[i]))
}
dev.off()
