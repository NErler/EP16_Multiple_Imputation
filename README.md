# Missing Values in Clinical Research: Multiple Imputation

This repository contains slides and materials for the Multiple Imputation part of NIHES course
[EP16: Missing Values in Clinical Research](https://www.nihes.com/course/ep16_missing_values_in_clinical_research/).

This repo has a [website](https://nerler.github.io/EP16_Multiple_Imputation)
where you can find slides and practicals.

The practicals require [R](https://cran.r-project.org/) and
[JAGS](https://sourceforge.net/projects/mcmc-jags/files/) to be installed
and use the following packages:
(JAGS (and **rjags**) are only necessary for the parts of the practical that use the **JointAI** package.)

* [mice](https://cran.r-project.org/package=mice) (version 3.4.0)
* [miceadds](https://cran.r-project.org/package=miceadds) (version 3.2-48)
* [mitools](https://cran.r-project.org/package=mitools) (version 2.3)
* [JointAI](https://cran.r-project.org/package=JointAI) (version 0.5.1)
* [rjags](https://cran.r-project.org/package=rjags) (version 4-8)

For **JointAI** it is suggested to use the most recent version available on GitHub
```r
devtools::install_github("NErler/JointAI")
```
