# Practical: Incomplete Data ---------------------------------------------------
file.copy('materials/Practicals/IncompleteData/Practical_IncompleteData.html',
          'static/practical/IncompleteData', overwrite = TRUE)

webshot::webshot('materials/Practicals/IncompleteData/Practical_IncompleteData.html',
                 'static/practical/IncompleteData/image.png',
                 vwidth = 800, vheight = 450,
                 cliprect = 'viewport', zoom = 2)

# Practical: MI with mice ------------------------------------------------------
file.copy('materials/Practicals/MImice/Practical_MImice.html',
          'static/practical/MImice', overwrite = TRUE)

webshot::webshot('materials/Practicals/MImice/Practical_MImice.html',
                 'static/practical/MImice/image.png',
                 vwidth = 800, vheight = 450,
                 cliprect = 'viewport', zoom = 2)

