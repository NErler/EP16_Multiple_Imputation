# Practical: Incomplete Data ---------------------------------------------------
if (!dir.exists("EP16website/static/practical/IncompleteData"))
  dir.create("EP16website/static/practical/IncompleteData")

file.copy('Practicals/IncompleteData/IncompleteData.html',
          'EP16website/static/practical/IncompleteData', overwrite = TRUE)

webshot::webshot('Practicals/IncompleteData/IncompleteData.html',
                 'EP16website/static/practical/IncompleteData/image.png',
                 vwidth = 800, vheight = 450,
                 cliprect = 'viewport', zoom = 2)

# Practical: MI with mice ------------------------------------------------------
if (!dir.exists("EP16website/static/practical/MImice"))
  dir.create("EP16website/static/practical/MImice")

file.copy('Practicals/MImice/MImice.html',
          'EP16website/static/practical/MImice', overwrite = TRUE)

webshot::webshot('Practicals/MImice/MImice.html',
                 'EP16website/static/practical/MImice/image.png',
                 vwidth = 800, vheight = 450,
                 cliprect = 'viewport', zoom = 2)



# Practical: Check imputation --------------------------------------------------
if (!dir.exists("EP16website/static/practical/MIcheck"))
  dir.create("EP16website/static/practical/MIcheck")

file.copy('Practicals/MIcheck/MIcheck.html',
          'EP16website/static/practical/MIcheck', overwrite = TRUE)

webshot::webshot('Practicals/MIcheck/MIcheck.html',
                 'EP16website/static/practical/MIcheck/image.png',
                 vwidth = 800, vheight = 450,
                 cliprect = 'viewport', zoom = 2)


# Practical: Analysis of MI data -----------------------------------------------
if (!dir.exists("EP16website/static/practical/AnalysisMI"))
  dir.create("EP16website/static/practical/AnalysisMI")

file.copy('Practicals/AnalysisMI/AnalysisMI.html',
          'EP16website/static/practical/AnalysisMI', overwrite = TRUE)

webshot::webshot('Practicals/AnalysisMI/AnalysisMI.html',
                 'EP16website/static/practical/AnalysisMI/image.png',
                 vwidth = 800, vheight = 450,
                 cliprect = 'viewport', zoom = 2)

# Practical: MI with non-linear effects ------------------------------------------------------
if (!dir.exists("EP16website/static/practical/MInonlin"))
  dir.create("EP16website/static/practical/MInonlin")

file.copy('Practicals/MInonlin/MInonlin.html',
          'EP16website/static/practical/MInonlin', overwrite = TRUE)

webshot::webshot('Practicals/MInonlin/MInonlin.html',
                 'EP16website/static/practical/MInonlin/image.png',
                 vwidth = 800, vheight = 450,
                 cliprect = 'viewport', zoom = 2)


# Practical: MI with longitudinal data ----------------------------------------
if (!dir.exists("EP16website/static/practical/MIlong"))
  dir.create("EP16website/static/practical/MIlong")

file.copy('Practicals/MIlong/MIlong.html',
          'EP16website/static/practical/MIlong', overwrite = TRUE)

webshot::webshot('Practicals/MIlong/MIlong.html',
                 'EP16website/static/practical/MIlong/image.png',
                 vwidth = 800, vheight = 450,
                 cliprect = 'viewport', zoom = 2)


# Practical: MI with survival data ---------------------------------------------
if (!dir.exists("EP16website/static/practical/MIsurv"))
  dir.create("EP16website/static/practical/MIsurv")

file.copy('Practicals/MIsurv/MIsurv.html',
          'EP16website/static/practical/MIsurv', overwrite = TRUE)

webshot::webshot('Practicals/MIsurv/MIsurv.html',
                 'EP16website/static/practical/MIsurv/image.png',
                 vwidth = 800, vheight = 450,
                 cliprect = 'viewport', zoom = 2)



# Slides -----------------------------------------------------------------------
if (!dir.exists("EP16website/static/slide/lecture"))
  dir.create("EP16website/static/slide/lecture")

file.copy('Slides/MICourse_Slides.pdf',
          'EP16website/static/slide/lecture', overwrite = TRUE)
