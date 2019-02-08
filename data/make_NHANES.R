#####################################################################
# Select the NHANES example data for the Multiple Imputation Course #
#####################################################################

# The original data files were downloaded from the NHANES web site and
# are stored in a folder "SAS" within "data" but are ignored in git.

# Packages outside base R that are needed for a specific function are
# - haven::read_xpt (version 2.0.0)
# - plyr::mapvalues (version 0.7.8)


datadir <- file.path(getwd(), "data", "SAS")


# help function to re-code missing values
setNA <- function(DF, vars, vals) {
  for (i in vars) {
    DF[DF[, i] %in% vals, i] <- NA
  }
  return(DF)
}



# * demographics data ------------------------------------------------------------
demo_orig <- as.data.frame(haven::read_xpt(file.path(datadir, "DEMO_G.XPT")))
demo <- data.frame(ID = demo_orig$SEQN,
                   gender = factor(demo_orig$RIAGENDR, levels = c(1,2),
                                   labels = c("male", "female")),
                   age = demo_orig$RIDAGEYR,
                   race = factor(demo_orig$RIDRETH1, levels = 1:5,
                                 labels = c("Mexican American", "Other Hispanic",
                                            "Non-Hispanic White",
                                            "Non-Hispanic Black", "other")),
                   Educ = factor(demo_orig$DMDEDUC2, levels = 1:5,
                                 labels = c("Less than 9th grade",
                                            "9-11th grade",
                                            "High school graduate",
                                            "some college",
                                            "College or above"))
                   # Race3 = factor(demo_orig$RIDRETH3,
                   #                levels = c(1:4, 6:7),
                   #                labels = c("Mexican American", "Other Hispanic",
                   #                           "Non-Hispanic White",
                   #                           "Non-Hispanic Black",
                   #                           "Non-Hispanic Asian", "other")),
                   # Preg = factor(demo_orig$RIDEXPRG, levels = 1:3,
                   #               labels = c("yes", "no", "maybe"))
)


weight_orig <- as.data.frame(haven::read_xpt(file.path(datadir, "WHQ_G.XPT")))
weight <- data.frame(ID = weight_orig$SEQN,
                     hgt = weight_orig$WHD010*2.54/100, # inches to meter
                     wgt = weight_orig$WHD020*0.45359237 # pound to kg
)

weight <- setNA(weight,
                vars = c("hgt", "wgt"),
                vals = c(7777, 9999))

# occumpation data -------------------------------------------------------------
occup_orig <- as.data.frame(haven::read_xpt(file.path(datadir, "OCQ_G.XPT")))
occup <- data.frame(ID = occup_orig$SEQN,
                    working = factor(occup_orig$OCD150,
                                     levels = c(1:4),
                                     labels = c('working at a job or business',
                                                'With a job or business but not at work',
                                                'looking for work',
                                                'not working at a job or business'))
)

occup <- setNA(occup, vars = c("working"), vals = c(7, 9))



# * blood pressure data ----------------------------------------------------------
bpq_orig <- as.data.frame(haven::read_xpt(file.path(datadir, "BPQ_G.XPT")))
bpx_orig <- as.data.frame(haven::read_xpt(file.path(datadir, "BPX_G.XPT")))
bpq <- data.frame(ID = bpq_orig$SEQN,
                  EverHyper = factor(bpq_orig$BPQ020, levels = 1:2,
                                     labels = c("yes", "no")),
                  TwiceHyper = factor(bpq_orig$BPQ030, levels = 1:2,
                                      labels = c("yes", "no")),
                  EverHyperMed = factor(bpq_orig$BPQ040A, levels = 1:2,
                                        labels = c("yes", "no")),
                  NowHyperMed = factor(bpq_orig$BPQ050A, levels = 1:2,
                                       labels = c("yes", "no"))
)


bpx <- data.frame(ID = bpx_orig$SEQN,
                  SBP1 = bpx_orig$BPXSY1,
                  SBP2 = bpx_orig$BPXSY2,
                  SBP3 = bpx_orig$BPXSY3,
                  SBP4 = bpx_orig$BPXSY4,
                  DBP1 = bpx_orig$BPXDI1,
                  DBP2 = bpx_orig$BPXDI2,
                  DBP3 = bpx_orig$BPXDI3,
                  DBP4 = bpx_orig$BPXDI4)
bpx$SBP <- rowMeans(bpx[, paste0("SBP", 1:4)], na.rm = T)
bpx$DBP <- rowMeans(bpx[, paste0("DBP", 1:4)], na.rm = T)



# * smoking data -----------------------------------------------------------------
smoke_orig <- as.data.frame(haven::read_xpt(file.path(datadir, "SMQ_G.XPT")))
smoke <- data.frame(ID = smoke_orig$SEQN,
                    smoke100life = factor(smoke_orig$SMQ020, levels = 1:2,
                                          labels = c("yes", "no")),
                    smokeNow = factor(smoke_orig$SMQ040, levels = 1:3,
                                      labels = c("every day", "some days", "not at all"))
)


# * alcohol data -----------------------------------------------------------------
alc_orig <- as.data.frame(haven::read_xpt(file.path(datadir, "ALQ_G.XPT")))
alc <- data.frame(ID = alc_orig$SEQN,
                  DrinkFreq = alc_orig$ALQ120Q,
                  FreqUnit = factor(alc_orig$ALQ120U, levels = 1:3,
                                    labels = c("week", "month", "year")),
                  Drinks = alc_orig$ALQ130
)

alc <- setNA(alc, vars = c("DrinkFreq", "Drinks"), vals = c(777, 999))

alc$DrinkDayPwk <- NA
alc$DrinkDayPwk[which(alc$FreqUnit == "year")] <- alc$DrinkFreq[which(alc$FreqUnit == "year")]/52
alc$DrinkDayPwk[which(alc$FreqUnit == "month")] <- alc$DrinkFreq[which(alc$FreqUnit == "month")]/4.3
alc$DrinkDayPwk[which(alc$FreqUnit == "week")] <- alc$DrinkFreq[which(alc$FreqUnit == "week")]
alc$DrinkDayPwk[which(alc$DrinkFreq == 0)] <- 0
alc$DrinksPwk <- alc$DrinkDayPwk * alc$Drinks
alc$DrinksPwk[which(alc$DrinkFreq == 0)] <- 0




# * biochem data -----------------------------------------------------------------
biochem_orig <- as.data.frame(haven::read_xpt(file.path(datadir, "BIOPRO_G.XPT")))
biochem <- data.frame(ID = biochem_orig$SEQN,
                      bili = biochem_orig$LBXSTB,
                      creat = biochem_orig$LBXSCR,
                      # ALT = biochem_orig$LBXSATSI,
                      # AST = biochem_orig$LBXSASSI,
                      albu = biochem_orig$LBXSAL,
                      # gluc_nonf1 = biochem_orig$LBXSGL,
                      # gluc_nonf2 = biochem_orig$LBDSGLSI,
                      chol = biochem_orig$LBXSCH,
                      uricacid = biochem_orig$LBXSUA
                      # UricAcid2 = biochem_orig$LBDSUASI
)


# * hemoglobin data --------------------------------------------------------------
# hemo_orig <- as.data.frame(haven::read_xpt(file.path(datadir, "CBC_G.XPT")))
# hemo <- data.frame(ID = hemo_orig$SEQN,
#                    hemo = hemo_orig$LBXHGB)

# * cholesterol data -------------------------------------------------------------
tchol_orig <- as.data.frame(haven::read_xpt(file.path(datadir, "TCHOL_G.XPT")))
tchol <- data.frame(ID = tchol_orig$SEQN,
                    tchol1 = tchol_orig$LBXTC,
                    tchol2 = tchol_orig$LBDTCSI
                    )

# * hdl data ---------------------------------------------------------------------
hdl_orig <- as.data.frame(haven::read_xpt(file.path(datadir, "HDL_G.XPT")))
hdl <- data.frame(ID = hdl_orig$SEQN,
                  HDL1 = hdl_orig$LBDHDD,
                  HDL2 = hdl_orig$LBDHDDSI)


# * diabetes data ---------------------------------------------------------------
diab_orig <- as.data.frame(haven::read_xpt(file.path(datadir, "DIQ_G.XPT")))
diab <- data.frame(ID = diab_orig$SEQN,
                   EverDM = factor(diab_orig$DIQ010, levels = 1:3,
                                   labels = c("yes", "no", "borderline")),
                   TakeInsulin = factor(diab_orig$DIQ050, levels = 1:2,
                                        labels = c("yes", "no")),
                   DMMed = factor(diab_orig$DIQ070, levels = 1:2,
                                  labels = c("yes", "no"))
)


# * glucose data -----------------------------------------------------------------
gluc_orig <- as.data.frame(haven::read_xpt(file.path(datadir, "GLU_G.XPT")))
gluc <- data.frame(ID = gluc_orig$SEQN,
                   gluc_fast1 = gluc_orig$LBXGLU,
                   gluc_fast2 = gluc_orig$LBDGLUSI
)

# * body measures data -----------------------------------------------------------
bm_orig <- as.data.frame(haven::read_xpt(file.path(datadir, "BMX_G.XPT")))
bm <- data.frame(ID = bm_orig$SEQN,
                 WC = bm_orig$BMXWAIST,
                 WCcomment = bm_orig$BMIWAIST)


# * medical conditions data ----------------------------------------------------
# med_orig <- as.data.frame(haven::read_xpt(file.path(datadir, "MCQ_G.XPT")))
# med <- data.frame(ID = med_orig$SEQN,
#                   EverStroke = factor(med_orig$MCQ160F, levels = 1:2,
#                                       labels = c("yes", "no")),
#                   EverLiverCond = factor(med_orig$MCQ160L, levels = 1:2,
#                                          labels = c("yes", "no")),
#                   StillLiverCond = factor(med_orig$MCQ170L, levels = 1:2,
#                                           labels = c("yes", "no"))
# )


# * physical funcitoning data --------------------------------------------------
# pf_orig <- as.data.frame(haven::read_xpt(file.path(datadir, "PFQ_G.XPT")))
# lbls <- c("Arthritis/rheumatism",
#           "Back or neck problem",
#           "Birth defect",
#           "Cancer",
#           "Depression/anxiety/emotional problem",
#           "Other developmental problem (such as cerebral palsy)",
#           "Diabetes",
#           "Fractures, bone/joint injury",
#           "Hearing problem",
#           "Heart problem",
#           "Hypertension/high blood pressure",
#           "Lung/breathing problem",
#           "Mental retardation",
#           "Other injury",
#           "Senility",
#           "Stroke problem",
#           "Vision/problem seeing",
#           "Weight problem",
#           "Other impairment/problem")
# pf <- data.frame(ID = pf_orig$SEQN,
#                  Limitations1 = factor(pf_orig$PFQ049, levels = 1:2),
#                  LimitWork = factor(pf_orig$PFQ051, levels = 1:2),
#                  WalkAid = factor(pf_orig$PFQ054, levels = 1:2),
#                  Confusion = factor(pf_orig$PFQ057, levels = 1:2),
#                  Limitations2 = factor(pf_orig$PFQ059, levels = 1:2),
#                  HealthProblem1 = factor(pf_orig$PFQ063A, levels = 10:28, labels = lbls),
#                  HealthProblem2 = factor(pf_orig$PFQ063B, levels = 10:28, labels = lbls),
#                  HealthProblem3 = factor(pf_orig$PFQ063C, levels = 10:28, labels = lbls),
#                  HealthProblem4 = factor(pf_orig$PFQ063D, levels = 10:28, labels = lbls),
#                  HealthProblem5 = factor(pf_orig$PFQ063E, levels = 10:28, labels = lbls),
#                  RequireEquipment = factor(pf_orig$PFQ090, levels = 1:2)
# )


#  merge datasets --------------------------------------------------------------
DF <- Reduce(function(x, y) merge(x, y, all = F),
             list(demo, bpq, bpx, smoke, alc, biochem, weight,
                  # hemo,
                  tchol, hdl, diab,
                  gluc, bm,
                  # med,
                  # pf,
                  occup)
)


# compute variables ------------------------------------------------------------
# * occupation
DF$occup <- plyr::mapvalues(DF$working, from = levels(DF$working),
                            to = c("working", "other", 'looking for work', 'not working'))

# * hypertension ---------------------------------------------------------------
DF$hypertension <- factor(apply(cbind(DF$EverHyper == "yes",
                                      DF$SBP >= 140,
                                      DF$DBP >= 90), 1, any),
                          labels = c("no", "yes"))

# * alcohol --------------------------------------------------------------------
DF$alc_cat <- cut(DF$DrinksPwk, c(-Inf, 0, 1, 3,  7, Inf), ordered_result = T,
                  labels = c("0", "<=1", "1-3", "3-7", ">7"))

DF$alc_bin <- cut(DF$DrinksPwk, c(-Inf, 1, Inf), ordered_result = F, right = F,
                  labels = c("<1", ">=1"))

# * smoking --------------------------------------------------------------------
DF$smoke <- as.character(DF$smokeNow)
DF$smoke[DF$smoke100life == "no"] <- "never"
DF$smoke[which(DF$smoke %in% c("some days", "every day"))] <- "current"

DF$smoke <- factor(DF$smoke, ordered = T,
                   levels = c("never", "not at all", "current"),
                   labels = c("never", "former", "current"))

# * education ------------------------------------------------------------------
# DF$educ <- plyr::mapvalues(DF$Educ, from = levels(DF$Educ),
#                            to = rep(c("low", "high"), c(3,2)))

# * hypercholesterol -----------------------------------------------------------
DF$hyperchol <- factor(DF$tchol1 >= 240, labels = c("no", "yes"))

# * diabetes -------------------------------------------------------------------
DF$DM <- factor(rowSums(cbind(DF$EverDM == "yes",
                              DF$DMMed == "yes",
                              DF$TakeInsulin == "yes",
                              DF$gluc_nonf1 >= 200,
                              DF$gluc_fast1 >= 126), na.rm = T) > 0,
                labels = c("no", "yes"))


# * liver disease --------------------------------------------------------------
# DF$LiverDisease <- as.character(DF$EverLiverCond)
# DF$LiverDisease[which(DF$StillLiverCond == "yes")] <- "current"
# DF$LiverDisease[which(DF$LiverDisease == "yes")] <- "former"
# DF$LiverDisease <- factor(DF$LiverDisease)

# * stroke outcome -------------------------------------------------------------
# DF$StrokeOutcome <- factor(apply(
#   DF[, paste0("HealthProblem", 1:5)] == "Stroke problem", 1, any),
#   labels = c("no", "yes"))


# make subset ------------------------------------------------------------------
NHANES <- subset(DF,
                 subset = c(age >= 20 &
                              age < 80
                              #!Preg %in% c("yes", "maybe")
                            #              !is.na(DBP) &
                            #              NowHyperMed %in% c("no", NA) &
                            #              EverHyperMed %in% c("no", NA)
                 ),
                 select = c(age, gender, race, bili, hgt, wgt,
                            # tchol1,
                            tchol2,
                            # HDL1,
                            HDL2,
                            hypertension, hyperchol, DM, smoke,
                            # alc_bin,
                            alc_cat,
                            # LiverDisease,
                            # EverStroke,
                            # StrokeOutcome,
                            Educ, SBP, DBP,
                            NowHyperMed, EverHyperMed,
                            creat, albu, uricacid, WC, occup))

# re-code
NHANES$HyperMed <- factor(NA, levels = c("no", "previous", "yes"), ordered = T)
NHANES$HyperMed[which(NHANES$EverHyperMed == "no")] <- "no"
NHANES$HyperMed[which(NHANES$NowHyperMed == "yes")] <- "yes"
NHANES$HyperMed[which(NHANES$NowHyperMed == "no" &
                        NHANES$EverHyperMed == "yes")] <- "previous"

NHANES$occup[NHANES$occup == 'other'] <- NA
NHANES <- droplevels(NHANES)

# re-name variables
names(NHANES) <- gsub('hyperchol', 'hypchol', names(NHANES))
names(NHANES) <- gsub('hypertension', 'hypten', names(NHANES))
names(NHANES) <- gsub('tchol2', 'chol', names(NHANES))
names(NHANES) <- gsub('HDL2', 'HDL', names(NHANES))
names(NHANES) <- gsub('alc_cat', 'alc', names(NHANES))
names(NHANES) <- gsub('Educ', 'educ', names(NHANES))

NHANES$BMI <- NHANES$wgt/NHANES$hgt^2

NHANES <- NHANES[, !names(NHANES) %in% c("NowHyperMed", "EverHyperMed")]

#####################
# data for lectures #
#####################
save(NHANES, file = "data/NHANES_for_lectures.RData")

######################
# data for practical #
######################
set.seed(2018)
sub <- sample(nrow(NHANES), 1000, prob = c(2/3, 1/3)[as.numeric(complete.cases(NHANES)) + 1])
NHANES <- NHANES[sub, ]
NHANES$cohort <- "2011"

NHANES <- NHANES[, sample(ncol(NHANES))]

save(NHANES, file = "data/NHANES_for_practicals.RData")

file.copy("data/NHANES_for_practicals.RData",
          "practicals/IncompleteData/www/NHANES_for_practicals.RData",
          overwrite = TRUE)

file.copy("data/NHANES_for_practicals.RData",
          "practicals/MImice/www/NHANES_for_practicals.RData",
          overwrite = TRUE)
