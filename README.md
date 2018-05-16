# Missing Values in Clinical Research: Multiple Imputation
This repository contains slides and materials for the Multiple Imputation part of NIHES course
[Missing Values in Clinical Research (EP16)](https://www.nihes.com/course/ep16_missing_values_in_clinical_research/).

Slides are available [here](https://github.com/NErler/MultipleImputationCourse/blob/master/Slides/MICourse_Slides.pdf).

Practicals are available as interactive [learnr](https://rstudio.github.io/learnr/) tutorials as well as in a static html version:
* [Imputation using the mice package](https://github.com/NErler/MultipleImputationCourse/blob/master/Practicals/MICE/Practical_MICE.html)
* [Imputation in complex settings](https://github.com/NErler/MultipleImputationCourse/blob/master/Practicals/MIadvanced/Practical_MIadvanced.html)

The practicals require [R](https://cran.r-project.org/), [JAGS](https://sourceforge.net/projects/mcmc-jags/files/) and the following packages:
(JAGS (and **rjags**) are only necessary for the parts of the practical that use the **JointAI** package.)
* [mice](https://cran.r-project.org/web/packages/mice) (version >= 2.46.0)
* [miceadds](https://cran.r-project.org/web/packages/miceadds) (version >= 2.10-14)
* [mitools](https://cran.r-project.org/web/packages/mitools) (version >= 2.3)
* [JointAI](https://cran.r-project.org/web/packages/JointAI) (version >= 0.1.0)
* [rjags](https://cran.r-project.org/web/packages/rjags) (version >= 4-6)
* [smcfcs](https://cran.r-project.org/web/packages/smcfcs) (version >= 1.3.0)
* [jomo](https://cran.r-project.org/web/packages/jomo) (version >= 2.6-2)

For **JointAI** it is suggested to use the development version available on GitHub
```r
devtools::install_github("JointAI", "NErler")
```
