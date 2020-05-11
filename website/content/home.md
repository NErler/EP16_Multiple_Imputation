---
date: 2020-04-24
title: "EP16: Missing Values in Clinical Research"
description: Multiple Imputation
markup: blackFriday
---

<style>
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
</style>


This website contains materials for the NIHES course [Missing Values in Clinical Research (EP16)](https://www.nihes.com/course/ep16_missing_values_in_clinical_research/):

* lecture slides
* practicals

Download a .zip file with all materials: <a href="/slide/EP16_MultipleImputation_2020.zip">
<button class="btn"><i class="fa fa-download"></i></button>
</a>


The practicals require [R](https://cran.r-project.org/), and
[JAGS](https://sourceforge.net/projects/mcmc-jags/files/) (necessary for the parts of the practical that use the **JointAI** package) needs to be installed.

To download and install **R**, go to [https://cran.r-project.org/](https://cran.r-project.org/).

To download JAGS, go to [https://sourceforge.net/projects/mcmc-jags/files/](https://sourceforge.net/projects/mcmc-jags/files/).


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
* **JointAI** version 0.6.1

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
