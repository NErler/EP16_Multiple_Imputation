# quadratic exampe -------------------------------------------------------------
# * simuate data ---------------------------------------------------------------
set.seed(2)
N <- 200
x <- runif(N, -1, 1)
y <- -1 - 0.6 * x + 0.5 * x^2 + rnorm(N, 0, 0.2)

DFexqdr <- data.frame(y = y, x = x)
DFexqdr$xmis <- DFexqdr$x
DFexqdr$xmis[sample(1:length(x), size = N/2, prob = plogis(5 * DFexqdr$y))] <- NA

impmod <- lm(xmis ~ y, DFexqdr)
imps <- predict(impmod, newdata = DFexqdr[is.na(DFexqdr$xmis), ]) +
  rnorm(sum(is.na(DFexqdr$xmis)), 0, summary(impmod)$sigma)

DFexqdr$ximp <- DFexqdr$xmis
DFexqdr$ximp[is.na(DFexqdr$xmis)] <- imps

lm0_qdr <- lm(y ~ x + I(x^2), DFexqdr)
lm_imp_qdr <- lm(y ~ ximp + I(ximp^2), DFexqdr)


# * plot 0: base ---------------------------------------------------------------
p_qdr0 <- ggplot(DFexqdr, aes(x = x, y = y,
                              color = is.na(xmis),
                              shape = is.na(xmis))) +
  geom_point(data = DFexqdr[is.na(DFexqdr$xmis), ], na.rm = TRUE,
             aes(x = ximp, y = y, shape = 'shapeimp', color = 'colimp')) +
  geom_point(size = 2) +
  theme_light() +
  theme(legend.position = c(0.1, 0.15),
        legend.background = element_rect(fill = 'transparent'),
        legend.text = element_text(size = 13),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_text(size = 15),
        axis.ticks = element_blank())


# * plot 1: obs + mis ------------------------------------------------------------
p_qdr1 <- p_qdr0 +
  scale_color_manual(name = "",
                     limits = c(F, T),
                     values = c("black", "darkgrey"),
                     labels = c("observed", "missing")) +
  scale_shape_manual(name = "",
                     limits = c(F, T),
                     values = c(19, 1),
                     labels = c("observed", "missing"))


# * plot 2: obs, mis + imp -------------------------------------------------------
p_qdr2 <- p_qdr0 +
  scale_color_manual(name = "",
                     limits = c(F, T, 'colimp'),
                     values = c("black", "darkgrey", "black"),
                     labels = c("observed", "missing", "imputed")) +
  scale_shape_manual(name = "",
                     limits = c(F, T, 'shapeimp'),
                     values = c(19, 1, 8),
                     labels = c("observed", "missing", "imputed"))


# * plot 3: reglines -------------------------------------------------------------
p_qdr3 <- p_qdr0 +
  geom_smooth(aes(group = "1", linetype = "ltycompl"), color = 'darkred',
              method = "lm", formula = y~x + I(x^2), se = F, lwd = 1.5) +
  geom_smooth(data = DFexqdr, aes(x = ximp, y = y, group = '1',
                                  linetype = "ltyimp"),
              color = 'darkred', se = F, lwd = 1.5,
              method = "lm", formula = y~x + I(x^2)) +
  scale_color_manual(name = "",
                     limits = c(F, T, 'colimp'),
                     values = c("black", "darkgrey", "black"),
                     labels = c("observed", "missing", "imputed")) +
  scale_shape_manual(name = "",
                     limits = c(F, T, 'shapeimp'),
                     values = c(19, 1, 8),
                     labels = c("observed", "missing", "imputed")) +
  scale_linetype_manual(name = "",
                        limits = c('ltycompl', 'ltyimp'),
                        values = c(1, 2),
                        labels = c("fit on complete", "fit on imputed")) +
  theme(legend.key.width = unit(1.2,"cm"),
        legend.title = element_blank(),
        legend.position = c(0.15, 0.19),
        legend.spacing.y = unit(-0.3, "lines")
  )



# Interaction example ----------------------------------------------------------
# * simulate data --------------------------------------------------------------
set.seed(2)
N <- 200
x <- runif(N, -1, 1)
z <- rbinom(N, size = 1, prob = 0.5)
y <- -1 - 0.6 * x + 0.5 * z + x*z + rnorm(N, 0, 0.2)

DFexint <- data.frame(y = y, x = x, z = z)
DFexint$xmis <- DFexint$x
DFexint$xmis[sample(1:length(x), size = N/2, prob = plogis(2 * DFexint$y))] <- NA

impmod <- lm(xmis ~ y + z, DFexint)
impmod2 <- lm(xmis ~ y*z, DFexint)

impresid <- rnorm(sum(is.na(DFexint$xmis)), 0, summary(impmod)$sigma)

imps <- predict(impmod, newdata = DFexint[is.na(DFexint$xmis), ]) + impresid
imps2 <- predict(impmod2, newdata = DFexint[is.na(DFexint$xmis), ]) + impresid

DFexint$ximp <- DFexint$ximp2 <- DFexint$xmis
DFexint$ximp[is.na(DFexint$xmis)] <- imps
DFexint$ximp2[is.na(DFexint$xmis)] <- imps2

lm0_int <- lm(y ~ x*z, DFexint)
lm_imp_int <- lm(y ~ ximp*z, DFexint)
lm_imp2_int <- lm(y ~ ximp2*z, DFexint)

plotexint <- reshape2::melt(DFexint, id.vars = c("y", "z", "ximp2", "xmis"))
plotexint$combi <- paste0(plotexint$variable, "_",
                          ifelse(is.na(plotexint$xmis), "mis", "obs"), plotexint$z)


# * plot 0: base ---------------------------------------------------------------
p_int0 <- ggplot(plotexint,
                 aes(x = value, y = y, color = combi, shape = combi, alpha = combi)) +
  theme_light() +
  theme(legend.position = c(0.15, 0.16),
        legend.background = element_rect(fill = 'transparent'),
        legend.key = element_rect(fill = 'transparent'),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 13),
        legend.spacing.x = unit(-0.1,  "lines"),
        legend.spacing.y = unit(-1.5, "lines"),
        legend.key.width = unit(1,"cm"),
        legend.box = "horizontal") +
  xlab("x")


# * plot 1: obs + mis ------------------------------------------------------------
p_int1 <- p_int0 +
  geom_point(size = 2, na.rm = TRUE) +
  scale_shape_manual(name = "",
                     limits = c("x_mis0", "x_mis1", "x_obs0", "x_obs1"),
                     values = c(1, 1, 19, 19),
                     labels = c("missing (z = 0)", "missing (z = 1)",
                                "observed (z = 0)", "observed (z = 1)")) +
  scale_alpha_manual(name = "",
                     limits = c("x_mis0", "x_mis1", "x_obs0", "x_obs1"),
                     values = c(0.5, 0.5, 1, 1),
                     labels = c("missing (z = 0)", "missing (z = 1)",
                                "observed (z = 0)", "observed (z = 1)")) +
  scale_color_manual(name = "",
                     limits = c("x_mis0", "x_mis1", "x_obs0", "x_obs1"),
                     values = c("blue", "chartreuse4", "blue", "chartreuse4"),
                     labels = c("missing (z = 0)", "missing (z = 1)",
                                "observed (z = 0)", "observed (z = 1)"))


# * plot 2: obs, mis + imp -------------------------------------------------------
p_int2 <- p_int0 +
  geom_point(na.rm = TRUE, size = 2) +
  scale_shape_manual(name = "",
                     limits = c("x_mis0", "x_mis1",
                                "x_obs0", "x_obs1",
                                "ximp_mis0", "ximp_mis1"),
                     values = c(1, 1, 19, 19, 8, 8),
                     labels = c("missing (z = 0)", "missing (z = 1)",
                                "observed (z = 0)", "observed (z = 1)",
                                "imputed (z = 0)", "imputed (z = 1)")) +
  scale_color_manual(name = "",
                     limits = c("x_mis0", "x_mis1",
                                "x_obs0", "x_obs1",
                                "ximp_mis0", "ximp_mis1"),
                     values = rep(c("blue", "chartreuse4"), 3),
                     labels = c("missing (z = 0)", "missing (z = 1)",
                                "observed (z = 0)", "observed (z = 1)",
                                "imputed (z = 0)", "imputed (z = 1)")) +
  scale_alpha_manual(name = "",
                     limits = c("x_mis0", "x_mis1",
                                "x_obs0", "x_obs1",
                                "ximp_mis0", "ximp_mis1"),
                     values = c(0.5, 0.5, 1, 1, 1, 1),
                     labels = c("missing (z = 0)", "missing (z = 1)",
                                "observed (z = 0)", "observed (z = 1)",
                                "imputed (z = 0)", "imputed (z = 1)"))



# * plot 3: reglines -----------------------------------------------------------
p_int3 <- p_int0 +
  geom_point(size = 2, na.rm = TRUE, alpha = 0.3) +
  geom_smooth(data = plotexint[plotexint$z == "0", ], method = "lm", se = F,
              aes(x = value, y = y, linetype = variable,
                  group = variable), color = 'blue') +
  geom_smooth(data = plotexint[plotexint$z == "1", ], method = "lm", se = F,
              aes(x = value, y = y, linetype = variable,
                  group = variable), color = 'chartreuse4') +
  theme(legend.position = c(0.26, 0.19)) +
  expand_limits(y = -2.5) +
  scale_shape_manual(name = "",
                     limits = c("x_mis0", "x_mis1",
                                "x_obs0", "x_obs1",
                                "ximp_mis0", "ximp_mis1"),
                     values = c(1, 1, 19, 19, 8, 8),
                     labels = c("missing (z = 0)", "missing (z = 1)",
                                "observed (z = 0)", "observed (z = 1)",
                                "imputed (z = 0)", "imputed (z = 1)")) +
  scale_color_manual(name = "",
                     limits = c("x_mis0", "x_mis1",
                                "x_obs0", "x_obs1",
                                "ximp_mis0", "ximp_mis1"),
                     values = rep(c("blue", "chartreuse4"), 3),
                     labels = c("missing (z = 0)", "missing (z = 1)",
                                "observed (z = 0)", "observed (z = 1)",
                                "imputed (z = 0)", "imputed (z = 1)")) +
  scale_linetype_manual(name = "",
                        limits = c("x", "ximp"),
                        values = c(1, 2),
                        labels = c(" true",  " imputed"))



# Longitudinal example --------------------------------------------------------
# * simulate data
IDs <- c(5, 6, 7, 8, 18)
subIDs <- IDs[IDs != 7]
subDFexlong <- DFexlong[DFexlong$id %in% IDs, ]

DFexlongwide <- reshape(DFexlong, direction = 'wide', v.names = c("y", "time"),
                        idvar = "id",
                        timevar = 'ti', drop = "tp")
subDFexlongwide <- DFexlongwide[DFexlongwide$id %in% subIDs, ]


coefDFexlong <- as.data.frame(t(sapply(lapply(split(subDFexlong, subDFexlong$id),
                                              lm, formula = y ~ time), coef)))
coefDFexlong$ID <- IDs


ltDFexlong <- subDFexlong[subDFexlong$id != 7,
                          c("id", "y", paste0("x", 1:4), "time")]
ltDFexlong$time <- sprintf(ltDFexlong$time, fmt = "%.2f")
ltDFexlong$y <- "\\checkmark"
ltDFexlong$x1 <- "\\checkmark"
ltDFexlong$x2 <- as.character(ltDFexlong$x2)
ltDFexlong$x2[!is.na(ltDFexlong$x2)] <- "\\checkmark"
ltDFexlong$x3 <- as.character(ltDFexlong$x3)
ltDFexlong$x3[!is.na(ltDFexlong$x3)] <- "\\checkmark"
ltDFexlong$x4[!is.na(ltDFexlong$x4)] <- "\\checkmark"

ltDFexlong <- rbind(ltDFexlong, rep("\\vdots", 7))


colvec <- brewer.pal(length(IDs), "Dark2")


# * plong1_1 -------------------------------------------------------------------
plong1_0 <- ggplot(subDFexlong, aes(x = time, y = y, color = factor(id))) +
  theme(legend.position = "none",
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 12)) +
  scale_color_brewer(palette = "Dark2") +
  scale_y_continuous(breaks = NULL,
                     minor_breaks = seq(from = 4, to = 6, by = 0.02))

plong1_1 <- plong1_0 +
  geom_line(lwd = 1) +
  geom_point(lwd = 2)




# survival example -------------------------------------------------------------
library(mice)
imp00 <- mice(survdat, maxit = 0)

meth01 <- imp00$method
meth01[c("x1")] <- "norm"
meth01[c("x3")] <- "norm"

impsurv_naive <- mice(survdat, maxit = 10)

cox <- with(survdat_orig, coxph(Surv(Time, as.numeric(event)) ~ x1 + x2 + x3))
cox01 <- with(impsurv_naive, coxph(Surv(Time, as.numeric(event)) ~ x1 + x2 + x3))

rescox01 <- as.data.frame(summary(pool(cox01), conf.int = TRUE)[, c("estimate", "2.5 %", "97.5 %")])
rescox <- as.data.frame(cbind(cox$coef, confint(cox)))

rescox01$meth <- "norm"
rescox$meth <- "orig"

rescox$var = rownames(rescox)
rescox01$var = rownames(rescox)

names(rescox) <- names(rescox01)
plotcox <- rbind(rescox, rescox01)
