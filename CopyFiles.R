# Practical: Incomplete Data ---------------------------------------------------
if (!dir.exists("EP16website/static/practical/IncompleteData"))
  dir.create("EP16website/static/practical/IncompleteData")

file.copy('Practicals/IncompleteData/Practical_IncompleteData.html',
          'EP16website/static/practical/IncompleteData', overwrite = TRUE)

webshot::webshot('Practicals/IncompleteData/Practical_IncompleteData.html',
                 'EP16website/static/practical/IncompleteData/image.png',
                 vwidth = 800, vheight = 450,
                 cliprect = 'viewport', zoom = 2)

# Practical: MI with mice ------------------------------------------------------
if (!dir.exists("EP16website/static/practical/MImice"))
  dir.create("EP16website/static/practical/MImice")

file.copy('Practicals/MImice/Practical_MImice.html',
          'EP16website/static/practical/MImice', overwrite = TRUE)

webshot::webshot('Practicals/MImice/Practical_MImice.html',
                 'EP16website/static/practical/MImice/image.png',
                 vwidth = 800, vheight = 450,
                 cliprect = 'viewport', zoom = 2)



# Slides -----------------------------------------------------------------------
if (!dir.exists("EP16website/static/slide/lecture"))
  dir.create("EP16website/static/slide/lecture")

file.copy('Slides/MICourse_Slides.pdf',
          'EP16website/static/slide/lecture', overwrite = TRUE)
