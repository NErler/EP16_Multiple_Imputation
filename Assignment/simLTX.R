load("N:/MDL/Thijmen Visseren/ELTR/Data/CleanedData.RData")

varying <- list(Date = c(paste0("LTx", 1:4, "_Date")),
                Outcome = c(paste0("TH", 1:4, "_Outcome")),
                Cause_of_fail1 = c(paste0("TH", 1:4, "_Cause_of_fail_1_New")),
                outcome_date = c(paste0("TH", 1:4, "_Outcome_date")))

dat$maxoutdate <- apply(longDF[, paste0("TH", 1:4, "_Outcome_date")], 1, max, na.rm = T)
dat$maxdate <- apply(longDF[, paste0("LTx", 1:4, "_Date")], 1, max, na.rm = T)

long <- reshape(dat, direction = "long", varying = varying,
                v.names = names(varying), times = 1:4)

long$Outcome <- factor(long$Outcome)
long$time <- factor(long$time)

long$etime_surv <- long$etime_LTx1


vars <- c(#"IDPAT",
          "etime_surv",
          "Outcome",
          "TH1_Type_of_Donor",
          "TH1_Type_of_Graft",
          "Recipient_Blood_Group",
          "TH1_Donor_Blood_Group",
          "blood_match",
          "Rec_Sex",
          "TH1_Total_ischemic_time",
          "TH1_Donor_age",
          "year",
          "Rec_Age_at_LTx")

DF <- droplevels(subset(long,
             select = vars,
             subset = !is.na(outcome_date) & # exclude patients with missing Outcome date
               !is.na(Outcome) & # exclude patients with missing outcome status
               outcome_date == maxoutdate & # only use last outcome
               Date == maxdate &
               TH1_Total_ischemic_time < 2000) # only use last outcome
)

DF$etime_surv[DF$etime_surv == 0] <- 0.001
DF$DonorGraftCat <- factor(paste0(DF$TH1_Type_of_Donor, DF$TH1_Type_of_Graft),
                           levels = unique(paste0(DF$TH1_Type_of_Donor,
                                                  DF$TH1_Type_of_Graft)[
                                                    !is.na(DF$TH1_Type_of_Donor) &
                                                      !is.na(DF$TH1_Type_of_Graft)]))

library(survival)
library(splines)
modcox <- coxph(Surv(etime_surv, as.numeric(Outcome)) ~ DonorGraftCat +
# Recipient_Blood_Group + TH1_Donor_Blood_Group +
  blood_match +
  Rec_Sex + TH1_Total_ischemic_time +
  TH1_Donor_age + ns(year, df = 3) + ns(Rec_Age_at_LTx, df = 2), data = DF)

modsurv <- glm(etime_surv ~ DonorGraftCat +
      blood_match +
      Rec_Sex + TH1_Total_ischemic_time + Outcome +
      TH1_Donor_age + ns(year, df = 3) + ns(Rec_Age_at_LTx, df = 2), data = DF,
    family = "Gamma")

modout <- glm(Outcome ~ DonorGraftCat +
                blood_match +
                Rec_Sex + TH1_Total_ischemic_time + etime_surv +
                TH1_Donor_age + ns(year, df = 3) + ns(Rec_Age_at_LTx, df = 2), data = DF,
              family = "binomial")

# DGCmod <- nnet::multinom(DonorGraftCat ~ . -TH1_Type_of_Graft-TH1_Type_of_Donor, data = DF)
Donmod <- nnet::multinom(TH1_Type_of_Donor ~ . -DonorGraftCat, data = DF)
Graftmod <- nnet::multinom(TH1_Type_of_Graft ~ . -DonorGraftCat, data = DF)
RecBGmod <- nnet::multinom(Recipient_Blood_Group ~ . -TH1_Type_of_Graft-TH1_Type_of_Donor, data = DF)
DonBGmod <- nnet::multinom(TH1_Donor_Blood_Group ~ . -TH1_Type_of_Graft-TH1_Type_of_Donor, data = DF)

RecSexmod <- glm(Rec_Sex ~ . -TH1_Type_of_Graft-TH1_Type_of_Donor, data = DF, family = "binomial")

# Iscmod <- glm(TH1_Total_ischemic_time ~ .-DonorGraftCat-TH1_Type_of_Graft-TH1_Type_of_Donor- year + ns(year, df = 3),
              # data = DF, family = Gamma(link = 'log'))
Iscmod <- lm(TH1_Total_ischemic_time ~ .-DonorGraftCat-TH1_Type_of_Graft-TH1_Type_of_Donor- year + ns(year, df = 3),
              data = DF)


DonAgemod <- lm(TH1_Donor_age ~ .-TH1_Type_of_Graft-TH1_Type_of_Donor, data = DF)
# yearmod <- lm(year ~ .-TH1_Type_of_Graft-TH1_Type_of_Donor, data = DF)
# yearmod <- truncreg::truncreg(year ~ .-TH1_Type_of_Graft-TH1_Type_of_Donor, data = DF,
#                               direction = "right", point = 2013)
# yearmod <- VGAM::vglm(year ~ .-TH1_Type_of_Graft-TH1_Type_of_Donor, data = DF,
#                               tobit(Upper = 2013))
yearmod <- glm(I(2014 - year) ~ .-TH1_Type_of_Graft-TH1_Type_of_Donor -
                 TH1_Total_ischemic_time - Rec_Age_at_LTx+ ns(TH1_Total_ischemic_time, df = 3)+
                 ns(Rec_Age_at_LTx, df = 3) + ns(TH1_Donor_age, df = 3) - TH1_Donor_age, data = DF,
               family = Gamma(link = "log"))
yearmod <- lm(year ~ .-TH1_Type_of_Graft-TH1_Type_of_Donor -
                 TH1_Total_ischemic_time - Rec_Age_at_LTx+ ns(TH1_Total_ischemic_time, df = 3)+
                 ns(Rec_Age_at_LTx, df = 3) + ns(TH1_Donor_age, df = 3) - TH1_Donor_age, data = DF)


RecAgemod <- lm(Rec_Age_at_LTx ~ .-TH1_Type_of_Graft-TH1_Type_of_Donor, data = DF)
# RecAgemod <- glm(Rec_Age_at_LTx ~ .-TH1_Type_of_Graft-TH1_Type_of_Donor, data = DF,
                 # family = Gamma(link = 'log'))


classes <- sapply(DF, class)
# classes["IDPAT"] <- "integer"
varmeans <- sapply(DF[classes == "numeric"], function(x) mean(x, na.rm = T))
varsds <- sapply(DF[classes == "numeric"], function(x) sd(x, na.rm = T))
props <- lapply(DF[sapply(classes, function(x) "factor" %in% x)], function(x)
  prop.table(table(x)))



set.seed(2018)
newDF <- cbind(#data.frame(ID = 1:N),
               as.data.frame(mapply(rnorm, N, varmeans, varsds)),
               as.data.frame(sapply(props, function(x)
                 factor(sample(names(x), size = N, replace = T, prob = x))))
)
names(newDF) <- c(names(varmeans), names(props))

newDF$Rec_Sex <- factor(newDF$Rec_Sex,
                        levels = levels(DF$Rec_Sex))
newDF$TH1_Type_of_Donor <- factor(newDF$TH1_Type_of_Donor,
                                  levels = levels(DF$TH1_Type_of_Donor))
newDF$TH1_Type_of_Graft <- factor(newDF$TH1_Type_of_Graft,
                                  levels = levels(DF$TH1_Type_of_Graft))
newDF$Recipient_Blood_Group <- factor(newDF$Recipient_Blood_Group,
                                  levels = levels(DF$Recipient_Blood_Group))
newDF$TH1_Donor_Blood_Group <- factor(newDF$TH1_Donor_Blood_Group,
                                  levels = levels(DF$TH1_Donor_Blood_Group))



M <- 10
arrDF <- vector("list", M)
set.seed(2018)
for (m in 1:M) {
  newDF$etime_surv <- predict(modsurv, newdata = newDF, type = 'response') +
    rnorm(N, 0, summary(modsurv)$dispersion)

  newDF$TH1_Total_ischemic_time <- pmax(10, predict(Iscmod, newdata = newDF, type = 'response') +
    rnorm(N, 0, summary(Iscmod)$sigma))

  newDF$TH1_Donor_age <- pmax(3, predict(DonAgemod, newdata = newDF, type = 'response')) +
    rnorm(N, 0, summary(DonAgemod)$sigma)

  # newDF$year <- pmax(1980, 2014 - predict(yearmod, newdata = newDF, type = 'response')) +
  #   rnorm(N, 0, 10*summary(yearmod)$dispersion)
  newDF$year <- pmin(rnorm(N, 2014, 0.5), pmax(1980, predict(yearmod, newdata = newDF, type = 'response')) +
    rnorm(N, 0, summary(yearmod)$sigma))

  newDF$Rec_Age_at_LTx <- pmax(3, predict(RecAgemod, newdata = newDF, type = 'response')) +
    rnorm(N, 0, summary(RecAgemod)$sigma)

  probOut <- predict(modout, newdata = newDF, type = "response")
  newDF$Outcome <- factor(levels(DF$Outcome)[1 + rbinom(N, size = 1, prob = probOut)])


  Graftprob <- predict(Graftmod, newdata = newDF, type = "probs")
  newDF$TH1_Type_of_Graft <- factor(apply(Graftprob, 1, sample, x = colnames(Graftprob),
                                          size = 1, replace = F),
                                    levels = levels(newDF$TH1_Type_of_Graft))

  Donprob <- predict(Donmod, newdata = newDF, type = "probs")
  newDF$TH1_Type_of_Donor <- factor(apply(Donprob, 1, sample, x = colnames(Donprob),
                                          size = 1, replace = F),
                                    levels = levels(newDF$TH1_Type_of_Donor))
  newDF$TH1_Type_of_Donor[newDF$TH1_Type_of_Graft == "L"] <- "L"
  newDF$TH1_Type_of_Graft[newDF$TH1_Type_of_Donor == "L"] <- "L"
  newDF$TH1_Type_of_Donor[newDF$TH1_Type_of_Graft == "R"] <- "H"

  newDF$DonorGraftCat <- factor(paste0(newDF$TH1_Type_of_Donor, newDF$TH1_Type_of_Graft),
                                levels = levels(DF$DonorGraftCat))


  RecBG <- predict(RecBGmod, newdata = newDF, type = "probs")
  newDF$Recipient_Blood_Group <- factor(apply(RecBG, 1, sample, x = colnames(RecBG),
                                          size = 1, replace = F),
                                    levels = levels(newDF$Recipient_Blood_Group))

  DonBG <- predict(DonBGmod, newdata = newDF, type = "probs")
  newDF$TH1_Donor_Blood_Group <- factor(apply(DonBG, 1, sample, x = colnames(DonBG),
                                              size = 1, replace = F),
                                        levels = levels(newDF$TH1_Donor_Blood_Group))

  newDF$blood_match <- factor(newDF$Recipient_Blood_Group == newDF$TH1_Donor_Blood_Group)

  sexprob <- predict(RecSexmod, newdata = newDF, type = 'response')
  newDF$Rec_Sex <- factor(levels(newDF$Rec_Sex)[1 + rbinom(N, size = 1, prob = sexprob)])


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
par(mfrow = c(3, 3), mar = c(3, 3.2, 2.5, 0.5), mgp = c(2, 0.6, 0))
for (i in names(DFcat)) {
  matplot(t(sapply(arrDF[!sapply(arrDF, is.null)], function(x) prop.table(table(x[, i], exclude = NULL)))),
          type = "l", lty = 1, main = i)
  abline(h = prop.table(table(DFcat[, i])), lty = 2, col = 1:length(levels(DFcat[, i])))
}




# create missing values --------------------------------------------------------
M <- 20
DFmis <- newDF

NADF <- sapply(newDF, as.numeric) * 0
navec <- matrix(nrow = M, ncol = ncol(newDF), dimnames = list(c(), names(DFmis)))


set.seed(2018)
for (m in 1:M) {
  for (i in c("TH1_Type_of_Donor", names(DF)[colSums(is.na(DF)) > 0 & !names(DF) %in% c("blood_match", "DonorGraftCat")])) {
    var <- i
    vars <- names(DF)[!names(DF) %in% c(var, "blood_match", "DonorGraftCat")]
    compvars <- vars[colSums(is.na(DF[, vars])) == 0]
    misvars <- vars[colSums(is.na(DF[, vars])) > 0]
    fmla <- as.formula(paste("is.na(", var, ") ~",
                             paste0("is.na(", misvars, ")", collapse = " + "), "+",
                             paste0(compvars, collapse = " + ")
    ))

    mod <- arm::bayesglm(fmla, data = DF, family = "binomial")

    probs <- predict(mod, newdata = DFmis, type = "response")
    # cat(var, ": ", mean(probs), "\n")
    if (mean(probs) < 0.05 & mean(is.na(DF[, var])) > 0)
      probs <- plogis(logit(probs) + 6)

    NADF[, var] <- rbinom(nrow(DFmis), size = 1,
                          prob = probs)

    DFmis[, var] <- newDF[, var]
    DFmis[NADF[, var] == 1, var] <- NA
  }
  navec[m, ] <- colMeans(is.na(DFmis))
}


matplot(navec, type = "l", ylim = c(0, 0.8))

cbind(colMeans(is.na(DF)), navec[M, match(names(DF), colnames(navec))])
