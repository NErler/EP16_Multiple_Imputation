# Missing Values in Clinical Research: Multiple Imputation

This repository contains slides and materials for the Multiple Imputation part of NIHES course
[EP16: Missing Values in Clinical Research](https://www.nihes.com/course/ep16_missing_values_in_clinical_research/).

This repo has a [website](https://nerler.github.io/EP16_Multiple_Imputation)
where you can find slides and practicals.


## Software
The practicals require [R](https://cran.r-project.org/), and
[JAGS](https://sourceforge.net/projects/mcmc-jags/files/) (necessary for the parts of the practical that use the **JointAI** package) needs to be installed.


### R

You can download R here:

* Windows: [https://cran.r-project.org/bin/windows/base/](https://cran.r-project.org/bin/windows/base/)
* Mac OS: [https://cran.r-project.org/bin/macosx/](https://cran.r-project.org/bin/macosx/)
* Linux: [https://cran.r-project.org/bin/linux/](https://cran.r-project.org/bin/linux/) 


It is strongly recommended to use a recent version of R, i.e., version 4.0.0 or later.

To check the version number of R, you can use the following syntax (in R):
``` r
R.version.string
```

### JAGS
To download the latest version of JAGS, go to [https://sourceforge.net/projects/mcmc-jags/files/](https://sourceforge.net/projects/mcmc-jags/files/).

The JAGS version should be 4.2.0 or later.

### R packages
You will also need the following R packages:

``` r
install.packages("mice")
install.packages("JointAI")
install.packages("ggplot2")
install.packages("corrplot")
```

For the packages **mice** and **JointAI** it is important that you have 
recent versions installed.

* **mice** version 3.8.0 or later
* **JointAI** version 3.6.1

To check which version of a package you have installed, you can use the function
`packageVersion()`, for example

```r 
packageVersion("JointAI")
packageVersion("mice")
```

Once you have the package **JointAI** installed<sup>[1](#myfootnote1)</sup>, you can use the following
syntax to check if R can find JAGS, and which version of JAGS is installed:

```r
rjags::jags.version()
```


There are a number of additional packages that may be useful:
``` r
install.packages("VIM")
install.packages("naniar")
install.packages("visdat")
```

<br><br><hr>
<a name="myfootnote1">1</a>: This function is not actually part of **JointAI**, but
of the package **rjags**. 
Since **JointAI** depends on **rjags**, **rjags** will be installed automatically
with **JointAI**.
