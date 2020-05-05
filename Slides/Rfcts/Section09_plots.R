#+ setup, purl = FALSE
projdir <- gsub("/Slides", "", getwd())


knitr::opts_chunk$set(opts.label = 'nopurl', purl = FALSE)

EMCdark <- "#0c2074"
EMC60 <- "#6d79ac"
EMC80 <- "#3d4d90"

# knitr::knit_hooks$set(purl = knitr::hook_purl)
# knitr::opts_template$set(nopurl = list(purl = FALSE))
# knitr::opts_template$set(dopurl = list(purl = TRUE))



library(ggplot2)

# quadratic example -------------------------------------------------------------
# * simulate data ---------------------------------------------------------------
#+ simdata_quadratic, purl = TRUE, opts.label = "dopurl"
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
#+
p_qdr0 <- ggplot(DFexqdr, aes(x = x, y = y,
                              color = is.na(xmis),
                              shape = is.na(xmis))) +
  geom_point(data = DFexqdr[is.na(DFexqdr$xmis), ], na.rm = TRUE,
             aes(x = ximp, y = y, shape = 'shapeimp', color = 'colimp')) +
  geom_point(size = 2) +
  theme_light() +
  theme(legend.position = c(0.1, 0.15),
        legend.background = element_rect(fill = 'transparent'),
        legend.text = element_text(size = 11),
        panel.grid = element_blank(),
        legend.key.size = unit(0.7, 'lines'),
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

ggsave(p_qdr1, file = file.path(projdir, 'Slides/graphics/Section09', 'p_qdr1.pdf'),
       width = 6, height = 3.7)

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

ggsave(p_qdr2, file = file.path(projdir, 'Slides/graphics/Section09', 'p_qdr2.pdf'),
       width = 6, height = 3.7)

# * plot 3: reglines -------------------------------------------------------------
p_qdr3 <- p_qdr0 +
  geom_smooth(aes(group = "1", linetype = "ltycompl"), color = EMCdark,
              method = "lm", formula = y~x + I(x^2), se = F, lwd = 1.5) +
  geom_smooth(data = DFexqdr, aes(x = ximp, y = y, group = '1',
                                  linetype = "ltyimp"),
              color = EMCdark, se = F, lwd = 1.5,
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
        legend.position = c(0.15, 0.15),
        legend.spacing.y = unit(-0.3, "lines")
  )

ggsave(p_qdr3, file = file.path(projdir, 'Slides/graphics/Section09', 'p_qdr3.pdf'),
       width = 6, height = 4)




# Interaction example ----------------------------------------------------------
# * simulate data --------------------------------------------------------------
#+ sim_interact_data, purl = TRUE
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

#+
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
        legend.text = element_text(size = 11),
        legend.spacing.x = unit(-0.1, "lines"),
        legend.spacing.y = unit(-1.5, "lines"),
        legend.key.width = unit(1,"cm"),
        legend.key.size = unit(0.7, 'lines'),
        legend.box = "horizontal") +
  xlab("x")


# * plot 1: obs + mis ------------------------------------------------------------
p_int1 <- p_int0 +
  geom_point(size = 2, na.rm = TRUE) +
  scale_shape_manual(name = "",
                     limits = c("x_mis0", "x_mis1", "x_obs0", "x_obs1"),
                     values = c(1, 2, 19, 17),
                     labels = c("missing (z = 0)", "missing (z = 1)",
                                "observed (z = 0)", "observed (z = 1)")) +
  scale_alpha_manual(name = "",
                     limits = c("x_mis0", "x_mis1", "x_obs0", "x_obs1"),
                     values = c(0.5, 0.5, 1, 1),
                     labels = c("missing (z = 0)", "missing (z = 1)",
                                "observed (z = 0)", "observed (z = 1)")) +
  scale_color_manual(name = "",
                     limits = c("x_mis0", "x_mis1", "x_obs0", "x_obs1"),
                     values = c(EMC80, "black", EMC80, "black"),
                     labels = c("missing (z = 0)", "missing (z = 1)",
                                "observed (z = 0)", "observed (z = 1)"))

ggsave(p_int1, file = file.path(projdir, 'Slides/graphics/Section09', 'p_int1.pdf'),
       width = 6, height = 3.7)


# * plot 2: obs, mis + imp -------------------------------------------------------
p_int2 <- p_int0 +
  geom_point(na.rm = TRUE, size = 2) +
  scale_shape_manual(name = "",
                     limits = c("x_mis0", "x_mis1",
                                "x_obs0", "x_obs1",
                                "ximp_mis0", "ximp_mis1"),
                     values = c(1, 2, 19, 17, 8, 8),
                     labels = c("missing (z = 0)", "missing (z = 1)",
                                "observed (z = 0)", "observed (z = 1)",
                                "imputed (z = 0)", "imputed (z = 1)")) +
  scale_color_manual(name = "",
                     limits = c("x_mis0", "x_mis1",
                                "x_obs0", "x_obs1",
                                "ximp_mis0", "ximp_mis1"),
                     values = rep(c(EMC60, "black"), 3),
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

ggsave(p_int2, file = file.path(projdir, 'Slides/graphics/Section09', 'p_int2.pdf'),
       width = 6, height = 3.7)


# * plot 3: reglines -----------------------------------------------------------
p_int3 <- p_int0 +
  geom_point(size = 2, na.rm = TRUE, alpha = 0.3) +
  geom_smooth(data = plotexint[plotexint$z == "0", ], method = "lm", se = F,
              aes(x = value, y = y, linetype = variable,
                  group = variable), color = EMC80) +
  geom_smooth(data = plotexint[plotexint$z == "1", ], method = "lm", se = F,
              aes(x = value, y = y, linetype = variable,
                  group = variable), color = "black") +
  theme(legend.position = c(0.26, 0.19)) +
  expand_limits(y = -2) +
  scale_shape_manual(name = "",
                     limits = c("x_mis0", "x_mis1",
                                "x_obs0", "x_obs1",
                                "ximp_mis0", "ximp_mis1"),
                     values = c(1, 2, 19, 17, 8, 8),
                     labels = c("missing (z = 0)", "missing (z = 1)",
                                "observed (z = 0)", "observed (z = 1)",
                                "imputed (z = 0)", "imputed (z = 1)")) +
  scale_color_manual(name = "",
                     limits = c("x_mis0", "x_mis1",
                                "x_obs0", "x_obs1",
                                "ximp_mis0", "ximp_mis1"),
                     values = rep(c(EMC80, "black"), 3),
                     labels = c("missing (z = 0)", "missing (z = 1)",
                                "observed (z = 0)", "observed (z = 1)",
                                "imputed (z = 0)", "imputed (z = 1)")) +
  scale_linetype_manual(name = "",
                        limits = c("x", "ximp"),
                        values = c(1, 2),
                        labels = c(" true",  " imputed"))


ggsave(p_int3, file = file.path(projdir, 'Slides/graphics/Section09', 'p_int3.pdf'),
       width = 6, height = 4)





# Longitudinal example --------------------------------------------------------
# * simulate data
source(file.path(projdir, 'Slides/Rfcts/Section09_simulate_data.R'))


longDF1_tab <- subset(longDF1, id < 5, select = c("id", "y", paste0("x", 1:4), "time"))
longDF1_tab$time <- sprintf(longDF1_tab$time, fmt = "%.2f")
longDF1_tab$y <- "\\checkmark"
longDF1_tab$x1 <- "\\checkmark"
longDF1_tab$x2 <- as.character(longDF1_tab$x2)
longDF1_tab$x2[!is.na(longDF1_tab$x2)] <- "\\checkmark"
longDF1_tab$x3 <- as.character(longDF1_tab$x3)
longDF1_tab$x3[!is.na(longDF1_tab$x3)] <- "\\checkmark"
longDF1_tab$x4[!is.na(longDF1_tab$x4)] <- "\\checkmark"

longDF1_tab <- rbind(longDF1_tab, rep("\\vdots", 7))


# fill in naively imputed values
 
longDF1_tab2 <- longDF1_tab

longDF1_tab2$x2[is.na(longDF1_tab2$x2)] <- sample(c('boy', 'girl'),
                                                  size = sum(is.na(longDF1_tab2$x2)),
                                                  replace = TRUE)

longDF1_tab2$x3[is.na(longDF1_tab2$x3)] <- sample(c('low', 'mid', 'high'),
                                                  size = sum(is.na(longDF1_tab2$x3)),
                                                  replace = TRUE)

longDF1_tab2$x4[is.na(longDF1_tab2$x4)] <- sprintf("%.1f", 
                                                   rnorm(sum(is.na(longDF1_tab2$x4)),
                                                  40, 0.5))

colvec <- RColorBrewer::brewer.pal(length(unique(longDF1$id)), "Dark2")
colvec30 <- sapply(colvec, function(x) {
  rgb(t(col2rgb(x)), alpha = 0.3, maxColorValue = 255)
})

# * plong1_1 -------------------------------------------------------------------
plong1_0 <- ggplot(subset(longDF1, id < 9),
                   aes(x = time, y = y, color = factor(id))) +
  theme(legend.position = "none",
        panel.background = element_rect(fill = grey(0.98), color = grey(0.85)),
        panel.grid = element_blank(),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 12)) +
  scale_color_brewer(palette = "Dark2") +
  scale_y_continuous(breaks = NULL,
                     minor_breaks = seq(from = 4, to = 6, by = 0.02))

plong1_1 <- plong1_0 +
  geom_line(lwd = 1) +
  geom_point(lwd = 2)


ggsave(plong1_1, file = file.path(projdir, 'Slides/graphics/Section09', 'plong1_1.pdf'),
       width = 6, height = 4.5)

# plong1_1b----------------------------------------------------------------------
plong1_1b <- plong1_1 +
  geom_vline(xintercept = seq(1, 9, 2), lty = 2) +
  scale_x_continuous(breaks = seq(1, 9, 2))

ggsave(plong1_1b, file = file.path(projdir, 'Slides/graphics/Section09', 'plong1_1b.pdf'),
       width = 6, height = 4.5)



# * LMM results ------------------------------------------------------------------
library(lme4)
library(mice)

implong <- mice(longDF1, seed = 123)
impDFlong <- complete(implong, 3)


lme0 <- lmer(y ~ x1 + x2 + x3 + x4 + ns(time, df = 3) +
                     (time|id), data = longDF1_orig)

lme_imp <- with(implong, lmer(y ~ x1 + x2 + x3 + x4 + ns(time, df = 3) +
                                (time|id),
                              control = lmerControl(optimizer = 'Nelder_Mead')
))

res0 <- data.frame(term = names(fixef(lme0)),
                   estimate = fixef(lme0),
                   confint(lme0, parm = names(fixef(lme0)),
                           method = "Wald"),
                   mod = 'orig',
                   check.names = FALSE)
resimp <- cbind(
  summary(pool(lme_imp),
          conf.int = TRUE)[, c("term", "estimate", "2.5 %", "97.5 %")],
  mod = 'imp')

res <- rbind(res0, resimp)

 
gg_longex1 <- ggplot(res[!res$term %in% grep("time", res$term, value = TRUE), ],
       aes(x = mod, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(min = `2.5 %`, max = `97.5 %`), width = 0.2) +
  facet_wrap("term", scales = 'free',
             labeller = as_labeller(c("x2girl" = "x2",
                                      "(Intercept)" = "Intercept",
                                      "x1" = "x1",
                                      "x3low" = "x3 (low)",
                                      "x3mid" = "x3 (mid)",
                                      "x4" = "x4"))) +
  scale_x_discrete(limits = c("orig", "imp"),
                   labels = c("original", "imputed")) +
  xlab("") +
  ylab("estimate & 95% CI") +
  theme(panel.background = element_rect(fill = grey(0.98), color = grey(0.85)),
        strip.background = element_rect(fill = grey(0.85), color = grey(0.85)),
        panel.grid = element_blank())

ggsave(gg_longex1, file = file.path(projdir, 'Slides/graphics/Section09/gglongex1.pdf'),
       width = 5, height = 4)



# * imputation wide --------------------------------------------------------------
impwide <- mice(wideDF1, printFlag = FALSE, maxit = 10, seed = 123)

implist <- mice::complete(impwide, 'long') %>% split(., .$.imp)

longList <- lapply(implist, reshape, direction = 'long',
                   varying = list(y = paste0("y.", seq(1, 9, 2)),
                                  time = paste0("time.", seq(1, 9, 2))),
                   v.names = c("y", "time"),
                   timevar = 'tp', drop = ".imp")

midsobj <- miceadds::datalist2mids(longList)

lme_impwide <- with(midsobj, lmer(y ~ x1 + x2 + x3 + x4 + ns(time, df = 3) + (time|id),
                                  control = lmerControl(optimizer = "Nelder_Mead")))

reswide <- cbind(
  summary(pool(lme_impwide), conf.int = TRUE)[, c("term", "estimate", "2.5 %", "97.5 %")],
  mod = 'impwide')

res2 <- rbind(res, reswide)

gg_wideex1 <- ggplot(res2[!res2$term %in% grep("time", res2$term, value = T), ],
       aes(x = mod, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(min = `2.5 %`, max = `97.5 %`), width = 0.2) +
  facet_wrap("term", scales = 'free',
             labeller = as_labeller(c("x2girl" = "x2",
                                      "(Intercept)" = "Intercept",
                                      "x1" = "x1",
                                      "x3low" = "x3 (low)",
                                      "x3mid" = "x3 (mid)",
                                      "x4" = "x4"))) +
  # geom_hline(yintercept = 0, lty = 2) +
  scale_x_discrete(limits = c("orig", "imp", "impwide"),
                   labels = c("orig.", "imp.\nlong", "imp.\nwide")) +
  xlab("") +
  ylab("estimate & 95% CI") +
  theme(panel.background = element_rect(fill = grey(0.98), color = grey(0.85)),
        strip.background = element_rect(fill = grey(0.85), color = grey(0.85)),
        panel.grid = element_blank())


ggsave(gg_wideex1, file = file.path(projdir, 'Slides/graphics/Section09/gg_wideex1.pdf'),
       width = 5, height = 4)


# Longitudinal 2 example -------------------------------------------------------
# ```{r, echo = FALSE, fig.width = 6, fig.height = 4}

longDF2_sub <- subset(longDF2, id < 15)

plong2_0 <- ggplot(longDF2_sub,
                   aes(x = time, y = y, alpha = factor(id))) +
  theme_light() +
  theme(legend.position = "none",
        axis.title = element_text(size = 14)) +
  scale_y_continuous(breaks = NULL) +
  scale_x_continuous(name = 'time', breaks = NULL) +
  theme(panel.background = element_rect(fill = grey(0.98), color = grey(0.85)),
        panel.grid = element_blank())

plong2_1 <- plong2_0 +
  geom_line(lwd = 1, color = 'darkblue') +
  geom_point(lwd = 2, color = 'darkblue')
plong2_1

ggsave(plong2_1, file = file.path(projdir, 'Slides/graphics/Section09/plong2_1.pdf'),
       width = 6, height = 4)



plong2_2 <- plong2_1 +
  geom_line(data = data.frame(x = c(min(longDF2_sub$time), max(longDF2_sub$time),
                                    min(longDF2_sub$time), max(longDF2_sub$time)),
                              y = c(min(longDF2_sub$y), max(longDF2_sub$y),
                                    max(longDF2_sub$y), min(longDF2_sub$y)),
                              id = c(1, 1, 2, 2)),
            aes(x = x, y = y, group = factor(id)), color = "red", lwd = 2, alpha = 1) +
  theme(axis.title = element_text(size = 16))

ggsave(plong2_2, file = file.path(projdir, 'Slides/graphics/Section09/plong2_2.pdf'),
       width = 6, height = 3.2)


plong2_3 <- plong2_0 +
  geom_point(data = longDF2_sub[longDF2_sub$ti == 1, ],
             aes(x = time, y = y), alpha = 0.7,
             size = 8, shape = 21, color = 'darkblue', fill = 'darkblue') +
  geom_line(lwd = 1, color = 'darkblue') +
  geom_point(lwd = 2, color = 'darkblue') +
  theme(axis.title = element_text(size = 16))

ggsave(plong2_3, file = file.path(projdir, 'Slides/graphics/Section09/plong2_3.pdf'),
       width = 6, height = 3.2)




lm4 <- lm(y ~ time, data = subset(longDF2_sub, id == 4))
lm14 <- lm(y ~ time, data = subset(longDF2_sub, id == 14))

lm4_ns <- lm(y ~ ns(time, df = 2), data = subset(longDF2_sub, id == 4))
lm14_ns <- lm(y ~ ns(time, df = 2), data = subset(longDF2_sub, id == 14))


coefdat <- as.data.frame(rbind(
  c(id = 4, coef(lm4)),
  c(id = 14, coef(lm14))
))



rdi_ns <- data.frame(id = c(4, 14),
           x = -6, 
           y = c(predict(lm4_ns, newdata = data.frame(time = -6)),
                 predict(lm14_ns, newdata = data.frame(time = -6))
           )
)



slopedat <- data.frame(x = c(-6, -2, -2),
                       y = c(coef(lm14)[1] + c(-6, -6, -2) * coef(lm14)[2]),
                       id = 14)




plong2_4 <- plong2_0 +
  geom_point(data = coefdat,
             aes(x = -6, y = `(Intercept)` - 6 * time), alpha = 0.7,
             size = 8, shape = 21, color = 'darkblue', fill = 'darkblue') +
  geom_line(lwd = 1, color = 'darkblue', aes(group = factor(id),
                                             alpha = id %in% c(4, 14))) +
  geom_smooth(data = subset(longDF2_sub, id %in% c(4, 14)), fullrange = TRUE,
              method = 'lm', formula = y ~ x, se = FALSE,
              aes(alpha = factor(id)), color = 'darkblue', lty = 2) +
  geom_point(lwd = 2, color = 'darkblue', aes(alpha = id %in% c(4, 14))) +
  geom_line(data = slopedat, aes(x = x, y = y), color = 'darkblue',
            size = 2, alpha = 0.5) +
  theme(axis.title = element_text(size = 16)) +
  scale_alpha_manual(limits = c(TRUE, FALSE), values = c(0.6, 0.1))


ggsave(plong2_4, file = file.path(projdir, 'Slides/graphics/Section09/plong2_4.pdf'),
       width = 5.5, height = 4)



plong2_5 <- plong2_0 +
  geom_point(data = rdi_ns,
             aes(x = x, y = y), alpha = 0.7,
             size = 8, shape = 21, color = 'darkblue', fill = 'darkblue') +
  geom_line(lwd = 1, color = 'darkblue', aes(group = factor(id),
                                             alpha = id %in% c(4, 14))) +
  geom_smooth(data = subset(longDF2_sub, id %in% c(4, 14)), fullrange = TRUE,
              method = 'lm', formula = y ~ ns(x, df = 2), se = FALSE,
              aes(alpha = factor(id)), color = 'darkblue', lty = 2) +
  geom_point(lwd = 2, color = 'darkblue', aes(alpha = id %in% c(4, 14))) +
  theme(axis.title = element_text(size = 16)) +
  geom_curve(aes(x = 3.5, y = 22.45, xend = 9, yend = 23.1), 
             curvature = 0.15, lwd = 2, alpha = 0.2, color = 'darkblue',
             arrow = arrow(length = unit(0.03, "npc"))) +
  geom_curve(aes(x = -1, y = 21, xend = 4.5, yend = 21), 
             curvature = 0.25, lwd = 2, alpha = 0.2, color = 'darkblue',
             arrow = arrow(length = unit(0.03, "npc"))) +
  scale_alpha_manual(limits = c(TRUE, FALSE), values = c(0.6, 0.1)) +
  coord_cartesian(ylim = c(20.7, 24.5))


ggsave(plong2_5, file = file.path(projdir, 'Slides/graphics/Section09/plong2_5.pdf'),
       width = 5.5, height = 4)





# survival example -------------------------------------------------------------
library(survival)
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


p_cox <- ggplot(plotcox, aes(x = meth, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(min = `2.5 %`, max = `97.5 %`), width = 0.2) +
  theme(axis.text = element_text(size = 12),
        panel.background = element_rect(fill = grey(0.98), color = grey(0.85)),
        strip.background = element_rect(fill = grey(0.85), color = grey(0.85)),
        panel.grid = element_blank(),
        strip.text = element_text(size = 12)) +
  facet_wrap("var", scales = 'free',
             labeller = labeller(var = c("x21" = "x2 (binary)",
                                         "x1" = "x1 (continuous)",
                                         "x3" = "x3 (continuous)"))) +
  scale_x_discrete(name = '', limits = c("orig", "norm"),
                   labels = c("original", "naive\nimputation")) +
  ylab("estimate & 95% CI")
  

ggsave(p_cox, file = file.path(projdir, 'Slides/graphics/Section09/p_cox.pdf'),
       width = 7, height = 3)
