---
date: 2019-02-09
title: "EP16: Missing Values in Clinical Research"
description: Multiple Imputation
---

<div style="color:red;">
<h3>[This website is currently under construction]</h3>
</div>

This website contains slides and materials for the Multiple Imputation part of
NIHES course [Missing Values in Clinical Research (EP16)](https://www.nihes.com/course/ep16_missing_values_in_clinical_research/).

Practicals are available as interactive [learnr](https://rstudio.github.io/learnr/)
tutorials (links will follow) as well as in a static html version.

The practicals require [R](https://cran.r-project.org/) and
[JAGS](https://sourceforge.net/projects/mcmc-jags/files/) to be installed and
use the following R packages:
(JAGS (and **rjags**) are only necessary for the parts of the practical that use the **JointAI** package.)

* [mice](https://cran.r-project.org/web/packages/mice) (version 3.3.0)
* [miceadds](https://cran.r-project.org/web/packages/miceadds) (version 3.0-16)
* [mitools](https://cran.r-project.org/web/packages/mitools) (version 2.3)
* [JointAI](https://cran.r-project.org/web/packages/JointAI) (version 0.4.0)
* [rjags](https://cran.r-project.org/web/packages/rjags) (version 4-8)
* [smcfcs](https://cran.r-project.org/web/packages/smcfcs) (version >= 1.3.1)
* [jomo](https://cran.r-project.org/web/packages/jomo) (version >= 2.6-7)

For **JointAI** it is suggested to use the most recent version available on GitHub
```r
devtools::install_github("NErler/JointAI")
```
