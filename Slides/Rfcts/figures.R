library(MASS)
library(RColorBrewer)
library(ggplot2)
library(gganimate)


# simulate data ----------------------------------------------------------------
set.seed(2019)

N = 50
x <- runif(N, -2, 2)
y <- rnorm(N, 0, 0.9) + x

DF0 <- data.frame(x = x, y = y)
DF0$yorig <- DF0$y
DF0$y[order(DF0$x)[c(0.25, 0.45, 0.7, 0.95) * N]] <- NA

m <- 5
lm1 <- lm(y ~ x, DF0)
xmis <- DF0$x[is.na(DF0$y)]
imps <- predict(lm1, newdata = DF0[is.na(DF0$y), ])

DF0$y2 <- DF0$y
DF0$y2[is.na(DF0$y)] <- -3.5
DF0$ismis <- is.na(DF0$y)


# baseplot ---------------------------------------------------------------------
baseplot_min <- ggplot(DF0) +
  theme_light() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_text(size = 16),
        panel.border = element_blank(),
        legend.position = c(0.12, 0.92),
        legend.background = element_blank(),
        legend.text = element_text(size = 14)) +
  xlab(expression(X[1])) +
  ylab(expression(X[2]))


baseplot <- baseplot_min +
  scale_color_manual(name = "",
                     limits = c(FALSE, TRUE),
                     values = c(grey(0.3), 2),
                     labels = c("observed", "missing")) +
  scale_shape_manual(name = "",
                     limits = c(FALSE, TRUE),
                     values = c(19, 1),
                     labels = c("observed", "missing"))


# plot + points ----------------------------------------------------------------
dataplot <- baseplot +
  geom_rect(xmin = -2.15, xmax = 1.97, ymin = -3.5, ymax = 3,
            fill = 'transparent', color = grey(0.7)) +
  geom_point(aes(x = x, y = y2, shape = ismis, color = ismis),
             na.rm = T, size = 2, stroke = 1.2)

# add regression line ----------------------------------------------------------
reglineplot <- dataplot +
  geom_abline(intercept = coef(lm1)[1], slope = coef(lm1)[2], lwd = 1)

# regimp_movie -----------------------------------------------------------------
nstep <- 50

yvals <- sapply(imps, function(k) {
  s <- seq(-3.5, to = k, by = 5/(0.75*nstep))
  c(s, rep(k, nstep - length(s)))
})

linedat <- do.call(rbind, lapply(2:nrow(yvals), function(k) {
  data.frame(yline = c(yvals[c(1, k), ]),
             xline = rep(xmis, each = 2),
             group = rep(1:ncol(yvals), each = 2),
             time = k-1)
}))

dotdat <- data.frame(xline = rep(xmis, colSums(apply(yvals, 2, duplicated)) + 1),
                     yline = rep(imps,
                                 colSums(apply(yvals, 2, duplicated)) + 1),
                     time = as.numeric(unlist(sapply(colSums(apply(yvals, 2,
                                                                   duplicated)) + 1,
                                          function(k) (nstep-k):(nstep-1))))
)



regimp_movie <- reglineplot +
  geom_point(data = dotdat,
             aes(x = xline, y = yline),
             color = 2, shape = 19, size = 2) +
  geom_line(data = linedat,
            aes(x = xline, y = yline, group = group), color = 2, lty = 2) +
  transition_manual(time)



# multiple regression lines ----------------------------------------------------
set.seed(2019)
reglines_DF <- as.data.frame(MASS::mvrnorm(10, coef(lm1), vcov(lm1)))
reglines_DF$time <- as.numeric(1:nrow(reglines_DF))

regimps_DF <- do.call(rbind, lapply(xmis, function(k) {
  data.frame(x = k,
             y = reglines_DF$`(Intercept)` + k * reglines_DF$x,
             time = reglines_DF$time)
})
)
regimps_DF$id <- match(regimps_DF$x, unique(regimps_DF$x))

reglines_movie <- dataplot +
  geom_abline(data = reglines_DF, aes(intercept = `(Intercept)`,
                                      slope = x, group = time)) +
  transition_components(time, enter_length = 0.7,
                        exit_length = 1) +
  enter_fade() + exit_fade()


# regimps_movie ----------------------------------------------------------------
regimps_movie <- dataplot +
  geom_abline(data = reglines_DF, aes(intercept = `(Intercept)`,
                                      slope = x, group = time)) +
  geom_point(data = regimps_DF, aes(x = x, y = y, group = interaction(time, id)),
             size = 2, col = 2) +
  transition_components(time, enter_length = 0.7,
                        exit_length = 1) +
  enter_fade() + exit_fade()

# regimps_movie <- dataplot +
#   geom_abline(data = reglines_DF, aes(intercept = `(Intercept)`,
#                                       slope = x, group = time)) +
#   geom_point(data = regimps_DF, aes(x = x, y = y, group = time), size = 2, col = 2) +
#   transition_time(time = time)# + enter_fade() + exit_fade()


# add random error -------------------------------------------------------------

test <- regimps_DF[regimps_DF$time %in% c(2, 4, 6, 8, 10), ]
test$group <- match(test$x, unique(test$x))
s <- seq(-2, 2, length = 20)
dn <- dnorm(s, 0, summary(lm1)$sigma)

densdat <- do.call(rbind, lapply(seq_along(test$x), function(k) {
  data.frame(xval = rep(test$x[k] - dn + 0.05, 2),
             yval = rep(test$y[k] + s, 2),
             group = test$group[k],
             time = test$time[k] - rep(c(1, 0), each = length(s)),
             col = test$time[k])
}))

impdat <- do.call(rbind, lapply(seq_along(test$x), function(k) {
  data.frame(xval = test$x[k],
             yval = c(NA, rep(rnorm(1, test$y[k], summary(lm1)$sigma),
                      max(test$time) - test$time[k] + 1)),
             group = test$group[k],
             time = c(test$time[k] - 1, test$time[k]:max(test$time)),
             col = test$time[k])
}))

reglines_DFnew <- reglines_DF[rep(c(2, 4, 6, 8, 10), each = 2), ]
reglines_DFnew$col <- as.character(reglines_DFnew$time)
reglines_DFnew$time <- 1:10


ranerrplot <- baseplot_min +
  geom_rect(xmin = -2.15, xmax = 1.97, ymin = -3.5, ymax = 3.9,
            fill = 'transparent', color = grey(0.7)) +
  geom_point(aes(x = x, y = y2, shape = as.character(ismis),
                 color = as.character(ismis)),
             na.rm = T, size = 1.5, stroke = 1.2) +
  geom_abline(data = reglines_DFnew[reglines_DFnew$time == 1, ],
              aes(intercept = `(Intercept)`,
                  slope = x, group = time, color = col)) +
  geom_polygon(data = densdat[densdat$time == 1, ],
               aes(xval, yval, group = interaction(group, time),
                   fill = as.character(col)),
               alpha = 0.1) +
  geom_path(data = densdat[densdat$time == 1, ], na.rm = TRUE,
            aes(xval, yval, group = interaction(group, time),
                color = as.character(col))) +
  scale_y_continuous(limits = c(-3.6, 3.9), expand = c(0, 0)) +
  scale_color_manual(name = "", guide = FALSE,
                     limits = c("FALSE", "TRUE", 2, 4, 6, 8, 10),
                     values = c(grey(0.7), 'red', brewer.pal(5, 'Dark2'))) +
  scale_fill_manual(guide = FALSE, limits = c(2, 4, 6, 8, 10),
                    values = brewer.pal(5, 'Dark2')) +
  scale_shape_manual(name = "",
                     limits = c('FALSE', "TRUE"),
                     labels = c('observed', 'missing'),
                     values = c(19, 1))

ranerr_movie <- baseplot_min +
  geom_rect(xmin = -2.15, xmax = 1.97, ymin = -3.5, ymax = 3.9,
            fill = 'transparent', color = grey(0.7)) +
  geom_point(aes(x = x, y = y2, shape = as.character(ismis),
                 color = as.character(ismis)),
             na.rm = T, size = 1.5, stroke = 1.2) +
  geom_abline(data = reglines_DFnew,
              aes(intercept = `(Intercept)`,
                  slope = x, group = time, color = col)) +
  geom_polygon(data = densdat,
               aes(xval, yval, group = interaction(group, time),
                   fill = as.character(col)),
                  alpha = 0.1) +
  geom_path(data = densdat, na.rm = TRUE,
            aes(xval, yval, group = interaction(group, time),
                color = as.character(col))) +
  geom_point(data = impdat, size = 2, na.rm = TRUE, stroke = 1.2,
             aes(xval, yval, group = interaction(group, time),
                 color = as.character(col), shape = "imp")) +
  scale_y_continuous(limits = c(-3.6, 3.9), expand = c(0, 0)) +
  transition_manual(time) +
  scale_color_manual(name = "", guide = FALSE,
                     limits = c("FALSE", "TRUE", 2, 4, 6, 8, 10),
                     values = c(grey(0.7), 'red', brewer.pal(5, 'Dark2'))) +
  scale_fill_manual(guide = FALSE, limits = c(2, 4, 6, 8, 10),
                    values = brewer.pal(5, 'Dark2')) +
  scale_shape_manual(name = "",
                     limits = c('FALSE', "TRUE", "imp"),
                     values = c(19, 1, 8),
                     labels = c('observed', 'missing', 'imputed'))

ranerrimpsplot <- baseplot_min +
  geom_rect(xmin = -2.15, xmax = 1.97, ymin = -3.5, ymax = 3.9,
            fill = 'transparent', color = grey(0.7)) +
  geom_point(aes(x = x, y = y2, shape = as.character(ismis),
                 color = as.character(ismis)),
             na.rm = T, size = 1.5, stroke = 1.2) +
  geom_point(data = impdat, size = 2, na.rm = TRUE, stroke = 1.2,
             aes(xval, yval, group = interaction(group, time),
                 color = as.character(col), shape = "imp")) +
  scale_y_continuous(limits = c(-3.6, 3.9), expand = c(0, 0)) +
  scale_color_manual(name = "", guide = FALSE,
                     limits = c("FALSE", "TRUE", 2, 4, 6, 8, 10),
                     values = c(grey(0.7), 'red', brewer.pal(5, 'Dark2'))) +
  scale_shape_manual(name = "",
                     limits = c('FALSE', "TRUE", "imp"),
                     values = c(19, 1, 8),
                     labels = c('observed', 'missing', 'imputed'))
