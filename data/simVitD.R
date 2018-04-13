# load packages
library(foreign)

# set working directory
setwd("\\\\storage/v/vcl13/EPBI/Data/Users/228015(N.Erler)/ErasmusAGE/Audry/Vitamin_D_during_pregnancy_and_childhhod_bone_mass")

# load dataset
dat <- read.spss("Nicole_data_2.sav", to.data.frame = T)

# specify outcomes and confounders
out <- c("TotBMD_NHeadmg", "TOTbmc_NH", "BMC_res_adj", "TOTarea_NH")

out.names <-   c(expression(BMD~(mg/cm^2)), "BMC (g)", "aBMC (g)", expression(paste("Bone area ", (cm^2))))

mod1 <- c("vitdc", "vitdc2", "seasong2", "sun_exposure_before_maternal_blood_sample",
          "ETHNINF_africanF", "ETHNINF_asianF", "length5child_DXA", "DXA_fat_lean_mass",
          "lean_mass_fraction", "ageDXAyears", "GENDER")

mod2 <- c("playing_sports_recF",
          "SDSNIKL")

summary(data[, c(mod1, mod2, out[c(2,4)])])

# select relevant variables and subset of data
data <- subset(dat, Imputation_ == "Original data")


vars <- c("IDC",
          "maternal_10_units",
          "TOTbmc_NH",
          "GENDER",
          "playing_sports_recF",
          "SDSNIKL",
          "season_maternal",
          "sun_exposure_before_maternal_blood_sample",
          "sun_before_birth",
          "ETHNINF_groupsF",
          "length5child_DXA",
          "DXA_fat_lean_mass",
          "lean_mass_fraction")


mydat <- data[, vars]
mydat$IDC <- as.integer(mydat$IDC)

library(splines)
mymod1 <- lm(TOTbmc_NH ~ ., data = mydat)
mymod <- lm(TOTbmc_NH ~ ns(maternal_10_units, df = 2) + . - maternal_10_units, data = mydat)


mydatnum <- mydat[, sapply(mydat, is.numeric)]
par(mfrow = c(3, 3), mar = c(3, 3.2, 0.5, 0.5), mgp = c(2, 0.6, 0))
for (i in 1:ncol(mydatnum)) {
  hist(mydatnum[, i], nclass = 30, xlab = names(mydatnum)[i], main = "")
  legend("topright", bty = "n",
         legend = paste0(round(mean(is.na(mydatnum[, i]))*100, 2), "% NA"))
}

# for factors
mydatfac <- mydat[, sapply(mydat, is.factor)]
par(mfrow = c(2, 2), mar = c(3, 3.2, 2.5, 0.5), mgp = c(2, 0.6, 0))
for (i in 1:ncol(mydatfac)) {
  tab <- table(mydatfac[, i], exclude = NULL)
  names(tab)[is.na(names(tab))] <- "NA"
  barplot(tab, main = paste0(names(mydatfac)[i]," (",
                             round(mean(is.na(mydatfac[, i]))*100,
                                   2), "% NA)"))
}

mydat$vitDsqrt <- sqrt(mydat$maternal_10_units)
mydat$vitDsqrt[mydat$vitDsqrt == 0] <- 0.01



# data models
library(splines)
modVitD <- stepAIC(lm(vitDsqrt ~ (.-IDC-maternal_10_units)^2, mydat))
# modVitD <- glm(vitDsqrt ~ (.- IDC)^2, mydat, family = Gamma(link = 'log'))
modout <- lm(TOTbmc_NH ~ (.-maternal_10_units - vitDsqrt - IDC)^2 + ns(maternal_10_units, df = 3), data = mydat)
modGender <- glm(GENDER ~ (.- IDC- vitDsqrt)^2, mydat, family = "binomial")
modSport <- glm(playing_sports_recF ~ (.-IDC- vitDsqrt)^2, mydat, family = "binomial")
modSDSNIKL <- lm(SDSNIKL ~ (.-IDC- vitDsqrt)^2, mydat)
modSeason <- nnet::multinom(season_maternal ~ (.-IDC- vitDsqrt)^2, data = mydat)
modSunExpoMat <- lm(sun_exposure_before_maternal_blood_sample ~ (.-IDC- vitDsqrt)^2, mydat)
modSunBirth <- lm(sun_before_birth ~ (.-IDC- vitDsqrt), mydat)
modEthn <- nnet::multinom(ETHNINF_groupsF ~ (.-IDC- vitDsqrt)^2, data = mydat)
modLength <- lm(length5child_DXA ~ (.-IDC- vitDsqrt)^2, data = mydat)
modFatLean <- glm(DXA_fat_lean_mass ~ (.-IDC- vitDsqrt)^2, data = mydat, family = Gamma(link = 'log'))
# modLeanMass <- lm(lean_mass_fraction ~ (.-IDC)^2, mydat)

modLeanMass <- betareg(lean_mass_fraction ~ (.-IDC- vitDsqrt)^2, mydat, link = "cloglog")
# modLeanMass2 <- betareg(lean_mass_fraction ~ (.-IDC)^2, mydat, link = "log")



classes <- sapply(mydat, class)

means <- sapply(mydat[classes == "numeric"], function(x) mean(x, na.rm = T))

sds <- sapply(mydat[classes == "numeric"], function(x) sd(x, na.rm = T))

props <- sapply(mydat[classes == "factor"], function(x)
  prop.table(table(x)))


N <- 1000

set.seed(2018)
newDF <- cbind(data.frame(ID = 1:N),
               as.data.frame(mapply(rnorm, N, means, sds)),
               as.data.frame(sapply(props, function(x)
                 factor(sample(names(x), size = N, replace = T, prob = x))))
)
names(newDF) <- c("IDC", names(means), names(props))
newDF$ETHNINF_groupsF <- relevel(newDF$ETHNINF_groupsF, ref = "european")
newDF$season_maternal <- factor(newDF$season_maternal, levels = levels(mydat$season_maternal))


M <- 20
arrDF <- vector("list", M)

for (m in 1:M) {
  probGENDER <- predict(modGender, newdata = newDF, type = "response")
  newDF$GENDER <- factor(levels(newDF$GENDER)[1 + rbinom(N, size = 1, prob = probGENDER)])

  probSport <- predict(modSport, newdata = newDF, type = "response")
  newDF$playing_sports_recF <- factor(levels(newDF$playing_sports_recF)[
    1 + rbinom(N, size = 1, prob = probSport)])

  probSeason <- predict(modSeason, newdata = newDF, type = "probs")
  newDF$season_maternal <- factor(apply(probSeason, 1, sample,
                                        x = colnames(probSeason),
                                        size = 1, replace = F),
                                  levels = levels(mydat$season_maternal))

  probETHN <- predict(modEthn, newdata = newDF, type = "probs")
  newDF$ETHNINF_groupsF <- factor(apply(probETHN, 1, sample, x = colnames(probETHN),
                                        size = 1, replace = F),
                                  levels = levels(mydat$ETHNINF_groupsF))

  newDF$vitDsqrt <- predict(modVitD, newdata = newDF, type = "response") +
    rnorm(N, 0, summary(modVitD)$sigma)# + runif(N, 0.5, 4)
  newDF$vitDsqrt[newDF$vitDsqrt > 5] <- runif(sum(newDF$vitDsqrt > 5), 2, 4)
  # newDF$maternal_10_units <- pmin(3, newDF$vitDsqrt)^2
  newDF$maternal_10_units <- newDF$vitDsqrt^2

  newDF$TOTbmc_NH <- pmax(predict(modout, newdata = newDF) +
    rnorm(N, 0, summary(modout)$sigma), 100)

  newDF$SDSNIKL <- predict(modSDSNIKL, newdata = newDF) +
    rnorm(N, 0, summary(modSDSNIKL)$sigma)

  newDF$sun_exposure_before_maternal_blood_sample <- pmax(0, predict(modSunExpoMat, newdata = newDF) +
    rnorm(N, 0, summary(modSunExpoMat)$sigma))

  newDF$sun_before_birth <- predict(modSunBirth, newdata = newDF) +
    runif(N, 0, summary(modSunBirth)$sigma)

  newDF$length5child_DXA <- pmax(50, predict(modLength, newdata = newDF) +
    rnorm(N, 0, summary(modLength)$sigma))

  newDF$DXA_fat_lean_mass <- predict(modFatLean, newdata = newDF, type = "response") +
    rnorm(N, 0, summary(modFatLean)$dispersion)

  newDF$lean_mass_fraction <- predict(modLeanMass, newdata = newDF) +
    rnorm(N, 0, 0.03)

  arrDF[[m]] <- newDF
}



mydatnum <- mydat[, sapply(mydat, is.numeric)]
par(mfrow = c(3, 4), mar = c(3, 3.2, 0.5, 0.5), mgp = c(2, 0.6, 0))
for (i in 2:ncol(mydatnum)) {
  hist(mydatnum[, i], nclass = 30, xlab = names(mydatnum)[i], main = "", freq = F,
       xlim = range(sapply(arrDF, "[", i), mydatnum[, i], na.rm = T))
  legend("topright", bty = "n",
         legend = paste0(round(mean(is.na(mydatnum[, i]))*100, 2), "% NA"))
  for(k in 1:sum(!sapply(arrDF, is.null))) {
    lines(density(arrDF[[k]][, i], na.rm = T), col = k,
          lwd = c(1,2)[as.numeric(k==M) + 1])
  }
}


mydatcat <- mydat[, sapply(mydat, is.factor)]
par(mfrow = c(2, 2), mar = c(3, 3.2, 2.5, 0.5), mgp = c(2, 0.6, 0))
for (i in names(mydatcat)) {
  matplot(t(sapply(arrDF[!sapply(arrDF, is.null)], function(x) prop.table(table(x[, i])))),
          type = "l", lty = 1, main = i)
  abline(h = prop.table(table(mydatcat[, i])), lty = 2, col = 1:length(levels(mydatcat[, i])))
}

# create missing values --------------------------------------------------------
M <- 6
DFmis <- newDF

NADF <- sapply(newDF, as.numeric) * 0
navec <- matrix(nrow = M, ncol = ncol(newDF), dimnames = list(c(), names(DFmis)))


set.seed(2018)
for (m in 1:M) {
  for (i in names(mydat)[colSums(is.na(mydat)) > 0]) {
    var <- i
    vars <- names(mydat)[!names(mydat) %in% c(var, "IDC")]
    compvars <- vars[colSums(is.na(mydat[, vars])) == 0]
    misvars <- vars[colSums(is.na(mydat[, vars])) > 0]
    fmla <- as.formula(paste("is.na(", var, ") ~",
                             paste0("is.na(", misvars, ")", collapse = " + "), "+",
                             paste0(compvars, collapse = " + ")
    ))

    mod <- arm::bayesglm(fmla, data = mydat, family = "binomial")

    probs <- predict(mod, newdata = DFmis, type = "response")
    cat(var, ": ", mean(probs), "\n")
    if (mean(probs) < 0.01 & mean(is.na(mydat[, var])) > 0)
      probs <- plogis(logit(probs) + 2)

    NADF[, var] <- rbinom(nrow(DFmis), size = 1,
                          prob = probs)

    DFmis[, var] <- newDF[, var]
    DFmis[NADF[, var] == 1, var] <- NA
  }
  navec[m, ] <- colMeans(is.na(DFmis))
}


matplot(navec, type = "l")

cbind(colMeans(is.na(mydat)), colMeans(navec)[match(names(mydat), colnames(navec))])


VitDdat <- with(DFmis,
                data.frame(
                  ID = IDC,
                  sex = GENDER,
                  ethn = ETHNINF_groupsF,
                  vitD = maternal_10_units,
                  season = season_maternal,
                  BMC = TOTbmc_NH,
                  birthwgt = SDSNIKL,
                  sun = sun_exposure_before_maternal_blood_sample,
                  sun_birth = sun_before_birth,
                  length = length5child_DXA,
                  weight = DXA_fat_lean_mass,
                  leanfrac = lean_mass_fraction,
                  sports = playing_sports_recF
                )
)

set.seed(2018)
VitDdat$bdate <- sample(seq(from = as.Date("2005-01-01"),
                           to = as.Date("2010-12-12"), by = 1), N, replace = T)
VitDdat$sun[sample(which(!is.na(VitDdat$sun_birth)), 90)] <- NA

VitDdat[sample(1:N, size = 0.05*N), c("leanfrac", "weight")] <- NA
VitDdat$ethn <- factor(VitDdat$ethn, ordered = T)
VitDdat$parity <- factor("nulliparity", levels = c("nulliparity", "multiparity"))

VitDdat <- VitDdat[, c(1,2, sample(3:ncol(VitDdat), ncol(VitDdat) - 2))]
