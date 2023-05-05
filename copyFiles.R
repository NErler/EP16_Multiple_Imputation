# helper functions

write_Practicals_md <- function(x) {
  file <- gsub('^Practicals/', '', x)
  title <- gsub('.html$', '', gsub("_", ' ', sub("_", ' ', file)))
  img <- gsub('.html$', '.png', file)
  
  
  cat(
    paste0("---\n",
           "title: '", title, "'\n",
           "link: /practical/", file, "\n",
           "image: /practical/", img, "\n",
           "---\n\n"
    ), file = paste0('website/content/practical/', gsub('.html$', '', file), '.md')
  )
}


write_Slides_md <- function(x) {
  file <- gsub('^Slides/', '', x)
  title <- gsub('.pdf$', '', gsub("_", ' ', sub("_", ' ', file)))
  img <- gsub('.pdf$', '.png', file)
  
  
  cat(
    paste0("---\n",
           "title: '", title, "'\n",
           "link: /slide/", tolower(file), "\n",
           "image: /slide/", tolower(img), "\n",
           "---\n\n"
    ), file = paste0('website/content/slide/', gsub('.pdf$', '', tolower(file)), '.md')
  )
}





## Practicals ------------------------------------------------------------------
Rmd_files <- grep('.Rmd$', dir('Practicals', recursive = FALSE, full.names = TRUE), value = TRUE)
html_files <- grep('.html$', dir('Practicals', recursive = FALSE, full.names = TRUE), value = TRUE)

# Render all .Rmd files in Practicals to html
# file.remove(html_files)
# for (k in Rmd_files) {
#   rmarkdown::render(k)
# }

html_files <- grep('.html$', dir('Practicals', recursive = FALSE, full.names = TRUE), value = TRUE)

# remove content of website/content/practical and website/static/practical
unlink('website/content/practical/*')
unlink('website/static/practical/*', recursive = TRUE)

# Copy all .html files in folder Practicals to the corresponding folders in website/static/practical
x <- gsub('Practicals/', '', html_files)

file.copy(from = file.path(getwd(), 'Practicals', x),
          to = file.path('website/static/practical', x),
          overwrite = TRUE)



for (x in html_files) {
  write_Practicals_md(x)
  
  img <- gsub('Practicals/', '', gsub('.html$', '.png', x))
  
  # the following gives an error/message but works anyway
  
  webshot::webshot(x, paste0('website/static/practical/', img),
                   vwidth = 800, vheight = 450, delay = 2,
                   cliprect = 'viewport', zoom = 2)
}



file.copy('Practicals/style/image.jpg',
          'website/static/practical/image.jpg',
          overwrite = TRUE)


cat(
  paste0("---\n",
         "title: 'Data & Imputations'\n",
         "description: Download the data and mids objects.\n",
         "link: /practical/data/index.html", "\n",
         "image: /practical/image.jpg\n",
         "markup: blackFriday\n",
         "---\n\n",
         
         "\n\n",
         "<style>
.btn {
padding: 2px 10px; 
cursor: pointer; 
font-weight: bold;
background: #fff; 
color: #485167;
border-radius: 5px; 
border: 1px solid #485167; 
font-size: 100%;
}


/* Darker background on mouse-over */
.btn:hover {
  color: #fff;
background: #485167;
border: 1px solid #485167;
}
</style>", "\n\n",
         
         "## Data\n\n",
         paste0('Download a .zip file with all datasets for the practcals: 
         <a href="/practical/EP16_MultipleImputation_data.zip">
         <button class="btn"><i class="fa fa-download"></i></button>
                </a>'), "\n",
         
         
         "\n\n## Imputed Data Objects\n\n",
         paste0('Download a .zip file with all mids objects from the practicals:
         <a href="/practical/EP16_MultipleImputation_imps.zip">
         <button class="btn"><i class="fa fa-download"></i></button>
                </a>'), "\n"),
  file = paste0('website/content/practical/data.md'))



## Slides ---------------------------------------------------------------------

# compile all slides
Rmdfiles <- grep('.Rmd$', dir('Slides', full.names = TRUE), value = TRUE)
# sapply(Rmdfiles, rmarkdown::render)

# remove unnecessary files created during compilation 
sapply(c('.log', '.tex', '.aux', '.out', '.vrb', '.snm', '.nav', '.toc'),
       function(k) {
         file.remove(gsub('.Rmd$', k, Rmdfiles))
       })

unlink('website/content/slide/*')
unlink('website/static/slide/*')


# Copy all .pdf files in folder Slides to the corresponding folders in website/static/slide
pdfs <- grep("[[:digit:]]{2}[[:print:]]+.pdf$",
             dir('Slides', recursive = FALSE,
                 full.names = TRUE), value = TRUE)
file.copy(from = pdfs,
          to = file.path('website/static/slide'),
          overwrite = TRUE)


# write .md files for website/content/slide
for (x in pdfs) {
  write_Slides_md(x)
  
  img <- gsub('Slides/', '', gsub('.pdf$', '.png', x))
  
  # the following gives an error/message but works anyway
  pdftools::pdf_convert(x, pages = 1, dpi = 150,
                        filenames = paste0('website/static/slide/', img))
}


file.rename(from = grep(".pdf$|.png$", dir('website/static/slide',
                                           full.names = TRUE), value = TRUE),
            to = tolower(grep(".pdf$|.png$", dir('website/static/slide',
                                                 full.names = TRUE), value = TRUE))
)


## .zip files ------------------------------------------------------------------

practicals <- grep('.html$', dir('Practicals', recursive = FALSE, full.names = TRUE),
                   value = TRUE)
data <- dir('Practicals/data', full.names = TRUE)
imps <- dir('Practicals/workspaces', full.names = TRUE)
slides <- grep("[[:digit:]]{2}[[:print:]]+.pdf$", 
               dir('Slides', recursive = FALSE, full.names = TRUE), value = TRUE)


# create a .zip
zip(zipfile = 'website/static/slide/EP16_MultipleImputation',
    files = unlist(c(practicals, slides, data, imps)))



wd <- getwd()
setwd('Practicals/data')
zip(zipfile = file.path(wd, 'website/static/practical/EP16_MultipleImputation_data'),
    files = list.files(full.names = TRUE))
setwd(wd)

setwd('Practicals/workspaces/')
zip(zipfile = file.path(wd, 'website/static/practical/EP16_MultipleImputation_imps'),
    files = list.files(full.names = TRUE))

setwd(wd)


## update website---------------------------------------------------------------

# remove docs folder
unlink("docs", recursive = TRUE)

# Build website
system2('hugo', args = "-s website")

# does not work, but I don't know why...
# system2('hugo server', args = "-s website")
# Works when running in the Terminal though: hugo server -s website
