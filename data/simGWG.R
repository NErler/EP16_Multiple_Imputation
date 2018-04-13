# load data
data.dir <- "\\\\storage/v/vcl13/EPBI/Data/Users/228015(N.Erler)/ErasmusAGE/Myrte/myrte-gwg-bayes/Data"
# data.dir <- "Data"

library(foreign)
data <- read.spss(paste(data.dir, "/Tielemans dietary patterns_GWG_Jan2015_n3474.sav", sep = ""), to.data.frame=T)
data2 <- read.spss(paste(data.dir, "/Tielemans dietary patterns_GWG_Oct2015_n3474.sav", sep = ""), to.data.frame=T)
extra <- read.spss(paste(data.dir, "/GR1019-A_01072012.sav", sep=""), to.data.frame = T)

Data <- subset(data, select=c(IDM, IDC))

# outcome (should not be imputed)
Data$weight <- data$Weight_all

# time variable (don't impute)
Data$ga_time <- data$G_GA_all

# exposure (don't impute)
exposure <- data.frame(dp_a = data$DPA_A,
                       dp_b = data$DPA_B,
                       dp_c = data$DPA_C,
                       dhdi = data$DHDI_EXCL_ALC_SDS)

# covariates that need imputation
covars <- data.frame(gender = data$GENDER,  # gender of the child
                     parity = data$PARITY_DICHOTOMOUS,
                     BMI_int = data$BMI_0,
                     height_int = data$HEIGHT_1,  # height at intake
                     educ_m = as.ordered(data$EDUCM_Categorized),
                     alc = as.ordered(data$alc0),
                     smoke = as.ordered(data$SMOKE_PRE),
                     income = data$INCOME5,
                     gsi = data$gsi,
                     age_m = data$AGE_M,
                     time = data$time)


covars2 <- data.frame(gravid = data$GRAVID_DICHOTOMOUS,
                      BMI_1 = data$BMI_1,
                      kcal = data$kcal,
                      nausea = data$Nausea_pregnancy,
                      vomit = data$Vomiting_pregnancy,
                      # folium = as.ordered(data$FOLIUM_VALIDATED),
                      # age_p = data$AGE_P,
                      # BMI_p = data$BMI_P,
                      # educ_p = as.ordered(data$EDUCP_Categorized),
                      # bweight = data$WEIGHT,
                      gage_b = data$GESTBIR,
                      preterm = data$Preterm_birth,
                      SDSbweight = data$SDSNIKL,
                      gestint = data$GESTINT,  # gestational age at intake
                      folium_add = data$Folaat_g1  # Foliumzuur <18 weeks: nmol/l; to impute folium
                      # g1height = data$G1HEIGHT,  # height mother gestage 1
                      # g2height = data$G2HEIGHT,  # height mother gestage 2
                      # g3height = data$G3HEIGHT,  # height mother gestage 3
                      # educ_m_add = as.ordered(data$EDUCM5_Categorized)
                      )  # education of the mother from 5 year questionnaire


Data <- cbind(Data, exposure, covars, covars2)

Data <- Data[order(Data$IDC), ]

wide <- reshape(Data, direction="wide", v.names = c("weight", "ga_time"), idvar = "IDM", timevar = "time")
# wide <- wide[order(wide$IDC), ]
#

widenum <- wide[, sapply(wide, is.numeric)]
par(mfrow = c(4, 5), mar = c(3, 3.2, 0.5, 0.5), mgp = c(2, 0.6, 0))
for (i in 1:ncol(widenum)) {
  hist(widenum[, i], nclass = 30, xlab = names(widenum)[i], main = "")
  legend("topright", bty = "n",
         legend = paste0(round(mean(is.na(widenum[, i]))*100, 2), "% NA"))
}

# for factors
widefac <- wide[, sapply(wide, is.factor)]
par(mfrow = c(3, 4), mar = c(3, 3.2, 2.5, 0.5), mgp = c(2, 0.6, 0))
for (i in 1:ncol(widefac)) {
  tab <- table(widefac[, i], exclude = NULL)
  names(tab)[is.na(names(tab))] <- "NA"
  barplot(tab, main = paste0(names(widefac)[i]," (",
                             round(mean(is.na(widefac[, i]))*100,
                                   2), "% NA)"))
}



library(ggplot2)
library(lme4)
library(splines)
library(ordinal)

lme_weight <- lmer(weight ~ age_m + gender + I(kcal/1000) + ns(ga_time, df = 2) + (ns(ga_time, df = 2)|IDC), data = Data)

ggplot(Data, aes(x = ga_time, y = weight, group = factor(IDC), color = kcal)) + geom_line() +
  theme(legend.position = "none") +
  geom_line(data = data.frame(ga_time = 0:40,
                              weight = predict(lme_weight,
                                               newdata = data.frame(ga_time = 0:40,
                                                                    gender = factor("boy"),
                                                                    kcal = 2120,
                                                                    gestint = 14,
                                                                    age_m = 31),
                                           re.form = ~0)
  ), aes(x = ga_time, y = weight, group = 1), color = 2)



VC_weight <- lme4::VarCorr(lme_weight)
D_weight <- VC_weight$IDC
b_weight <- lme4::ranef(lme_weight)$IDC
names(b_weight) <- paste0("b_weight", 1:ncol(b_weight))

weights <- paste0("weight.", 1:4)
gages <- paste0("ga_time.", 1:4)


usevars <- c("IDC", "gender", "parity", "income", "gsi", "preterm", "gage_b", "BMI_int",
            "smoke", "educ_m", "kcal", "age_m")


DF <- wide[, usevars]

DF$income <- factor(DF$income, ordered = T)
DF$gsi[which(DF$gsi == 0)] <- 0.001

Data <- cbind(DF, wide[, c(weights, gages)])

modGender <- glm(gender ~ ., DF, family = "binomial")
modParity <- arm::bayesglm(parity ~ ., DF, family = "binomial")
modIncome <- clm(income ~ ., data = DF)
modGSI <- glm(gsi ~ ., data = DF, family = Gamma("log"))

modagem <- lm(age_m ~ ., data = DF)

modPreterm <- arm::bayesglm(preterm ~ .-gage_b, data = DF, family = "binomial")
modGESTBIR <- lm(gage_b ~ ., data = DF)

modkcal <- lm(kcal ~ ., data = DF)
modBMI <- glm(BMI_int ~ ., data = DF, family = Gamma("identity"))


clmSMOKE <- clm(smoke ~ ., data = DF)
clmEDUC <- clm(educ_m ~ ., data = DF)



N <- 1000# nrow(Data)
b_weight_ <- MASS::mvrnorm(N, rep(0, ncol(D_weight)), D_weight)
colnames(b_weight_) <- names(b_weight)


timemeans <- sapply(wide[, gages], mean, na.rm = T)
timevars <- sapply(wide[, gages], var, na.rm = T)

ranges <- round(sapply(wide[, gages], range, na.rm = T))

newage <- MASS::mvrnorm(N, timemeans, diag(timevars))

test <- newage >= t(outer(ranges[1, ], rep(1, nrow(newage)))) &
  newage <= t(outer(ranges[2, ], rep(1, nrow(newage))))
crit <- sum(apply(!test, 1, any))

while (crit > 0) {
  newage[apply(!test, 1, any), ] <- MASS::mvrnorm(sum(apply(!test, 1, any)),
                                                  timemeans, diag(timevars))
  test <- newage >= t(outer(ranges[1, ], rep(1, nrow(newage)))) &
    newage <= t(outer(ranges[2, ], rep(1, nrow(newage))))
  crit <- sum(apply(!test, 1, any))
}


classes <- sapply(DF, class)
classes["IDC"] <- "integer"
varmeans <- sapply(DF[classes == "numeric"], function(x) mean(x, na.rm = T))
varsds <- sapply(DF[classes == "numeric"], function(x) sd(x, na.rm = T))
props <- lapply(DF[sapply(classes, function(x) "factor" %in% x)], function(x)
  prop.table(table(x)))


set.seed(2018)
newDF <- cbind(data.frame(ID = 1:N),
               as.data.frame(mapply(rnorm, N, varmeans, varsds)),
               as.data.frame(sapply(props, function(x)
                 factor(sample(names(x), size = N, replace = T, prob = x))))
)
names(newDF) <- c("IDC", names(varmeans), names(props))
newDF$parity <- factor(newDF$parity, levels = levels(DF$parity))
newDF$preterm <- factor(newDF$preterm, levels = levels(DF$preterm))


newDF <- cbind(newDF, b_weight_)

M <- 20
arrDF <- vector("list", M)
set.seed(2018)
for (m in 1:M) {

  newDF$kcal <- predict(modkcal, newdata = newDF) + rnorm(N, 0, summary(modkcal)$sigma)
  # newDF$BMI_int <- predict(modBMI, newdata = newDF, type = "response") + rnorm(N, 0, summary(modBMI)$sigma)
  # newDF$BMI_int <- exp(predict(modBMI, newdata = newDF) + rnorm(N, 0, summary(modBMI)$dispersion))
  newDF$BMI_int <- predict(modBMI, newdata = newDF, type = "response") +
    rnorm(N, -1.5, 90*summary(modBMI)$dispersion)

  # newDF$gsi <- predict(modGSI, newdata = newDF, type = "response") + rnorm(N, 0, summary(modGSI)$dispersion)
  newDF$gsi <- exp(predict(modGSI, newdata = newDF) + rnorm(N, 0, summary(modGSI)$dispersion))

  newDF$gage_b <- predict(modGESTBIR, newdata = newDF, type = "response") +
    rnorm(N, 0, summary(modGESTBIR)$sigma)

  probGender <- predict(modGender, newdata = newDF, type = "response")
  newDF$gender <- factor(levels(DF$gender)[1 + rbinom(N, size = 1, prob = probGender)])

  # probPreterm <- predict(modPreterm, newdata = newDF, type = "response")
  # newDF$preterm <- factor(levels(DF$preterm)[1 + rbinom(N, size = 1, prob = probGender)])
  newDF$preterm <- factor(levels(DF$preterm)[as.numeric(newDF$gage_b < 37) + 1],
                          levels = levels(DF$preterm))

  probPARITY <- predict(modParity, newdata = newDF, type = "response")
  newDF$parity <- factor(levels(DF$parity)[1 + rbinom(N, size = 1, prob = probPARITY)],
                         levels = levels(DF$parity))

  probEDUC <- predict(clmEDUC, newdata = subset(newDF, select = -educ_m),
                      type = "prob")$fit
  newDF$educ_m <- factor(apply(probEDUC, 1, sample, x = colnames(probEDUC),
                             size = 1, replace = F),
                       levels = levels(DF$educ_m), ordered = T)

  probSMOKE <- predict(clmSMOKE, newdata = subset(newDF, select = -smoke),
                       type = "prob")$fit
  newDF$smoke <- factor(apply(probSMOKE, 1, sample, x = colnames(probSMOKE),
                              size = 1, replace = F),
                        levels = levels(DF$smoke), ordered = T)

  probINCOME <- predict(modIncome, newdata = subset(newDF, select = -income),
                       type = "prob")$fit
  newDF$income <- factor(apply(probINCOME, 1, sample, x = colnames(probINCOME),
                              size = 1, replace = F),
                        levels = levels(DF$income), ordered = T)
  newDF$age_m <- predict(modagem, newdata = newDF) + rnorm(N, 0, summary(modagem)$sigma)
  arrDF[[m]] <- newDF
}



DFnum <- DF[, sapply(DF, is.numeric)]
par(mfrow = c(2, 2), mar = c(3, 3.2, 0.5, 0.5), mgp = c(2, 0.6, 0))
for (i in 2:ncol(DFnum)) {
  hist(DFnum[, i], nclass = 30, xlab = names(DFnum)[i], main = "", freq = F,
       xlim = range(sapply(arrDF, "[", i), DFnum[, i], na.rm = T))
  legend("topright", bty = "n",
         legend = paste0(round(mean(is.na(DFnum[, i]))*100, 2), "% NA"))
  for (k in 1:sum(!sapply(arrDF, is.null))) {
    lines(density(arrDF[[k]][, i], na.rm = T), col = k,
          lwd = c(1,2)[as.numeric(k == M) + 1])
  }
}


DFcat <- DF[, sapply(DF, is.factor)]
par(mfrow = c(2, 3), mar = c(3, 3.2, 2.5, 0.5), mgp = c(2, 0.6, 0))
for (i in names(DFcat)) {
  matplot(t(sapply(arrDF[!sapply(arrDF, is.null)], function(x) prop.table(table(x[, i])))),
          type = "l", lty = 1, main = i)
  abline(h = prop.table(table(DFcat[, i])), lty = 2, col = 1:length(levels(DFcat[, i])))
}


newlong <- reshape(cbind(newDF, newage), direction = "long",
                   varying = list(gages), v.names = c("ga_time"),
                   times = c(0, 1, 2, 3),
                   idvar = "IDC", drop = c(colnames(b_weight_)))

# newlong$IDC <- paste0("id", newlong$IDC)

newlong$weight <- simulate(lme_weight, newdata = newlong, re.form = NA,
                        allow.new.levels = T, seed = 2018, nsim = 1)$sim_1


library(ggplot2)
ggplot(newlong, aes(ga_time, weight, color = factor(IDC))) + geom_line() +
  theme(legend.position = "none")


newwide <- reshape(newlong, direction = "wide", v.names = c('ga_time', 'weight'),
                   idvar = "IDC", timevar = "time", varying = list(gages, weights))



# create missing values --------------------------------------------------------
M <- 10
DFmis <- newwide


NADF <- sapply(newwide, as.numeric) * 0
navec <- matrix(nrow = M, ncol = ncol(newwide), dimnames = list(c(), names(DFmis)))

set.seed(2018)
for (m in 1:M) {
  for (i in names(DF)[colSums(is.na(DF)) > 0 & !names(DF) %in% c(grep("ga_time", names(DF), value = T))]) {
    var <- i
    vars <- names(DF)[!names(DF) %in% c(var, "IDC", grep("ga_time", names(DF), value = T))]
    compvars <- vars[colSums(is.na(DF[, vars])) == 0]
    # compvars <- compvars[compvars != "ga_time.1"]
    misvars <- vars[colSums(is.na(DF[, vars])) > 0]
    fmla <- as.formula(paste("is.na(", var, ") ~",
                             paste0("is.na(", misvars, ")", collapse = " + "), "+",
                             paste0(compvars, collapse = " + ")
    ))

    mod <- arm::bayesglm(fmla, data = DF, family = "binomial")

    NADF[, var] <- rbinom(nrow(DFmis), size = 1,
                          prob = predict(mod, newdata = DFmis, type = "response"))

    DFmis[, var] <- newwide[, var]
    DFmis[NADF[, var] == 1, var] <- NA
  }
  navec[m, ] <- colMeans(is.na(DFmis))
}


matplot(navec, type = "l")

cbind(colMeans(is.na(Data)), colMeans(navec)[match(names(Data), colnames(navec))])


DFmis[sample(N, N*mean(is.na(wide[, gages[2]]))), gages[2]] <- NA
DFmis[sample(N, N*mean(is.na(wide[, gages[3]]))), gages[3]] <- NA
DFmis[sample(N, N*mean(is.na(wide[, gages[4]]))), gages[4]] <- NA

DFmis[sample(N, 0.05 * N), c("preterm", "gage_b")] <- NA

DFmis$bd_mom <- sample(seq(from = as.Date("1975-01-01"),
                           to = as.Date("1995-12-12"), by = 1), N, replace = T)
DFmis$date_incl <- DFmis$bd_mom + sample(seq(from = 17*365,
                              to = 35*365, by = 1), N, replace = T)

DFmis$date_incl[DFmis$date_incl > as.Date("2016-01-06")] <-
  sample(seq(from = as.Date("2010-01-01"),
             to = as.Date("2016-12-12"), by = 1),
         sum(DFmis$date_incl > as.Date("2016-01-06")), replace = T)


simLong <- reshape(DFmis, direction = "long", varying = list(gages, weights),
                   v.names = c("gage", "weight"),
                   times = c(0, 1, 2, 3),
                   idvar = "ID")


simLong <- simLong[!is.na(simLong$gage), ]


GWGDF <- with(simLong, data.frame(
  id = IDC,
  date_incl = date_incl,
  gender = gender,
  parity = parity,
  gestbir = gage_b,
  weight = weight,
  gsi = gsi,
  income = income,
  BMI = BMI_int,
  kcal = kcal,
  bd_mom = bd_mom,
  preterm = preterm,
  smoke = smoke,
  educ = educ_m
))

GWGDF$visit_center <- factor(1, levels = c(0, 1))
GWGDF$educ <- factor(GWGDF$educ, levels = levels(GWGDF$educ)[c(2,1,3)])

