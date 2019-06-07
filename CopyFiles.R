# Quiz ---------------------------------------------
if (!dir.exists("EP16website/static/practical/Quiz"))
  dir.create("EP16website/static/practical/Quiz")

file.copy('Practicals/Quiz_PartI/Quiz_PartI.html',
          'EP16website/static/practical/Quiz', overwrite = TRUE)

webshot::webshot('Practicals/Quiz_PartI/Quiz_PartI.html',
                 'EP16website/static/practical/Quiz/image.png',
                 vwidth = 800, vheight = 450,
                 cliprect = 'viewport', zoom = 2)




# Practical: Incomplete Data ---------------------------------------------------
if (!dir.exists("EP16website/static/practical/IncompleteData"))
  dir.create("EP16website/static/practical/IncompleteData")

file.copy('Practicals/IncompleteData/EP16_IncompleteData.html',
          'EP16website/static/practical/IncompleteData', overwrite = TRUE)

webshot::webshot('Practicals/IncompleteData/EP16_IncompleteData.html',
                 'EP16website/static/practical/IncompleteData/image.png',
                 vwidth = 800, vheight = 450,
                 cliprect = 'viewport', zoom = 2)

# Practical: MI with mice ------------------------------------------------------
if (!dir.exists("EP16website/static/practical/MImice"))
  dir.create("EP16website/static/practical/MImice")

file.copy('Practicals/MImice/EP16_MImice.html',
          'EP16website/static/practical/MImice', overwrite = TRUE)

webshot::webshot('Practicals/MImice/EP16_MImice.html',
                 'EP16website/static/practical/MImice/image.png',
                 vwidth = 800, vheight = 450,
                 cliprect = 'viewport', zoom = 2)



# Practical: Check imputation --------------------------------------------------
if (!dir.exists("EP16website/static/practical/MIcheck"))
  dir.create("EP16website/static/practical/MIcheck")

file.copy('Practicals/MIcheck/EP16_MIcheck.html',
          'EP16website/static/practical/MIcheck', overwrite = TRUE)

webshot::webshot('Practicals/MIcheck/EP16_MIcheck.html',
                 'EP16website/static/practical/MIcheck/image.png',
                 vwidth = 800, vheight = 450,
                 cliprect = 'viewport', zoom = 2)


# Practical: Analysis of MI data -----------------------------------------------
if (!dir.exists("EP16website/static/practical/AnalysisMI"))
  dir.create("EP16website/static/practical/AnalysisMI")

file.copy('Practicals/AnalysisMI/EP16_AnalysisMI.html',
          'EP16website/static/practical/AnalysisMI', overwrite = TRUE)

webshot::webshot('Practicals/AnalysisMI/EP16_AnalysisMI.html',
                 'EP16website/static/practical/AnalysisMI/image.png',
                 vwidth = 800, vheight = 450,
                 cliprect = 'viewport', zoom = 2)

# Practical: MI with non-linear effects ------------------------------------------------------
if (!dir.exists("EP16website/static/practical/MInonlin"))
  dir.create("EP16website/static/practical/MInonlin")

file.copy('Practicals/MInonlin/EP16_MInonlin.html',
          'EP16website/static/practical/MInonlin', overwrite = TRUE)

webshot::webshot('Practicals/MInonlin/EP16_MInonlin.html',
                 'EP16website/static/practical/MInonlin/image.png',
                 vwidth = 800, vheight = 450,
                 cliprect = 'viewport', zoom = 2)


# Practical: MI with longitudinal data ----------------------------------------
if (!dir.exists("EP16website/static/practical/MIlong"))
  dir.create("EP16website/static/practical/MIlong")

file.copy('Practicals/MIlong/EP16_MIlong.html',
          'EP16website/static/practical/MIlong', overwrite = TRUE)

webshot::webshot('Practicals/MIlong/EP16_MIlong.html',
                 'EP16website/static/practical/MIlong/image.png',
                 vwidth = 800, vheight = 450,
                 cliprect = 'viewport', zoom = 2)


# Practical: MI with survival data ---------------------------------------------
if (!dir.exists("EP16website/static/practical/MIsurv"))
  dir.create("EP16website/static/practical/MIsurv")

file.copy('Practicals/MIsurv/EP16_MIsurv.html',
          'EP16website/static/practical/MIsurv', overwrite = TRUE)

webshot::webshot('Practicals/MIsurv/EP16_MIsurv.html',
                 'EP16website/static/practical/MIsurv/image.png',
                 vwidth = 800, vheight = 450,
                 cliprect = 'viewport', zoom = 2)


# Practical: Convert MI --------------------------------------------------------
if (!dir.exists("EP16website/static/practical/MIconvert"))
  dir.create("EP16website/static/practical/MIconvert")

file.copy('Practicals/MIconvert/EP16_MIconvert.html',
          'EP16website/static/practical/MIconvert', overwrite = TRUE)

webshot::webshot('Practicals/MIconvert/EP16_MIconvert.html',
                 'EP16website/static/practical/MIconvert/image.png',
                 vwidth = 800, vheight = 450,
                 cliprect = 'viewport', zoom = 2)


# Data -------------------------------------------------------------------------
# imps from MImice
file.copy('Practicals/MIcheck/www/imps.RData',
          'EP16website/static/practical/data/imps.RData', overwrite = TRUE)

file.copy('Practicals/MIcheck/www/NHANES_for_practicals.RData',
          'EP16website/static/practical/data/NHANES_for_practicals.RData', overwrite = TRUE)

file.copy('Practicals/MInonlin/www/NHANES_for_practicals_2.RData',
          'EP16website/static/practical/data/NHANES_for_practicals_2.RData', overwrite = TRUE)

file.copy('Practicals/MIlong/www/pbclong.RData',
          'EP16website/static/practical/data/pbclong.RData', overwrite = TRUE)

file.copy('Practicals/MIsurv/www/pbcdat.RData',
          'EP16website/static/practical/data/pbcdat.RData', overwrite = TRUE)


# Slides -----------------------------------------------------------------------
if (!dir.exists("EP16website/static/slide/lecture"))
  dir.create("EP16website/static/slide/lecture")

file.copy('Slides/MICourse_Slides.pdf',
          'EP16website/static/slide/lecture', overwrite = TRUE)



# run all practicals -----------------------------------------------------------
# need to check first that html version is chosen!!!!
#
files <- c("IncompleteData",
           'MImice',
           "MIcheck",
           "AnalysisMI",
           "MInonlin", #5
           "MIlong", # 6
           "MIsurv",
           "MIconvert")

for (i in files) {
  unlink(paste0('Practicals/', i, '/', i, '_cache'), recursive = TRUE)
}


for (i in files) {
  rmarkdown::render(input = paste0('Practicals/', i, '/', i, '.Rmd'))
}

# shiny version
for (i in files) {
  unlink(paste0('Practicals/', i, '/', i, '_cache'), recursive = TRUE)
  rmarkdown::shiny_prerendered_clean(paste0('Practicals/', i, '/', i, '.Rmd'))
}

i <- files[2]
rmarkdown::run(file = paste0('Practicals/', i, '/', i, '.Rmd'))

rsconnect::deployApp(appDir = file.path('Practicals', i),
                     appFiles = c(paste0(i, '.Rmd'), 'www'),
                     appName = paste0('EP16_', i),
                     account = 'emcbiostatistics',
                     server = 'shinyapps.io',
                     forceUpdate = TRUE)
