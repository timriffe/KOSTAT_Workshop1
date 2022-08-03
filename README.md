## 9th KOSTAT-UNFPA Summer Seminar on Population
# Workshop 1 "R in Demography"

27 July - 5 August, 2022. See you in Daejeon or in Zoom!

## Contents

- `Data/`
   1. `demo_pjan.xlsx`
   2. `demo_fasec.xlsx`
   3. `LT_inputs.csv`
   4. `LT_results.csv`
   5. `Korea_births_fwf_metadata.csv`

- `01` (Wednesday, 27 July 2022) "Introduction to `R`, `RStudio`, and `R markdown`"
   1. `01_handout` (`.Rmd`, `.pdf`)
   2. `01_session.Rmd` (notes from session)
- `02` (Thursday, 28 July 2022) "The tidy data approach" 
   1. `02_handout` (`.Rmd`, `.pdf`)
   2. `02_session.Rmd` (notes from session)
- `03` (Friday, 29 July 2022) "Worked example of tidy data processing: birth data"
   1. `03_handout` (`.Rmd`, `.pdf`)
   2. `03_session.Rmd`
   3. `03_solutions.Rmd`
   4. `03_fertility_ad_hoc.R`
- `04` (Monday, 1 August 2022) "Writing functions: the lifetable"
   1. `04_data_prep` (`.Rmd`, `.pdf`)
   2. `04_handout` (`.Rmd`, `.pdf`)
   3. `04_session.Rmd` (continued next day)
   4. `04_lifetable_functions.R` (used in follow-up to session)
- `05` (Tuesday, 2 August 2022) "Visualizing data using `ggplot2`"
   1. `05_handout` (`.Rmd`, `.pdf`)
   2. `05_session.Rmd`
- `06` (Wednesday, 3 August 2022) "Processing and visualizing South Korean fertility microdata"
   1. `06_handout` (`.Rmd`, `.pdf`) 
   2. `06_session.Rmd`
   3. `06_solutions.Rmd`
   4. `06_git_session.Rmd` (option git / github miniworkshop)
   5. `06_data_prep` (`.Rmd`, `.pdf`)
- `07` (Thursday, 4 August 2022) "Demographic standardization and decomposition"
   1. `07_handout` (`.Rmd`, `.pdf`) *in progress*
- `08` (Thursday, 6 August 2022) "Advanced pipelines: Data chosen by participants"
   1. `08_handout` (`.Rmd`, `.pdf`) *designed toward end of workshop*
      
## Things to install

* Rstudio Desktop, preview version [https://www.rstudio.com/products/rstudio/download/preview/](https://www.rstudio.com/products/rstudio/download/preview/)
Installing this will automatically install the latest `R` version `4.1.0`, which we need.

optional, but recommended to install:
* `git` [https://git-scm.com/downloads]()https://git-scm.com/downloads No need for a separate `git` program. We'll just use it from `Rstudio` a bit.
* `latex` [https://www.latex-project.org/get/](https://www.latex-project.org/get/)  Alternatively we can install it straight from `R` using the `tinytex` package.

## Further resources that might help installation:

Here are some links that exhaustively cover the topic of installation and update of R on different platforms.

* Install R and tidyverse for Mac, Windows. With some update guides
[https://uvastatlab.github.io/phdplus/installR.html](https://uvastatlab.github.io/phdplus/installR.html)
[https://courses.edx.org/courses/UTAustinX/UT.7.01x/3T2014/56c5437b88fa43cf828bff5371c6a924/](https://courses.edx.org/courses/UTAustinX/UT.7.01x/3T2014/56c5437b88fa43cf828bff5371c6a924/)

* Update R, R packages and R studio on different platforms 
[https://bootstrappers.umassmed.edu/bootstrappers-courses/courses/rCourse/Additional_Resources/Updating_R.html](https://bootstrappers.umassmed.edu/bootstrappers-courses/courses/rCourse/Additional_Resources/Updating_R.html)

* Full tutorial for updating R on Windows with screenshots
[https://www.r-statistics.com/2015/06/a-step-by-step-screenshots-tutorial-for-upgrading-r-on-windows/](https://www.r-statistics.com/2015/06/a-step-by-step-screenshots-tutorial-for-upgrading-r-on-windows/)

Installation guide for r and rstudio for Windows
[https://www.youtube.com/watch?v=NZxSA80lF1I&ab_channel=TechDecodeTutorials](https://www.youtube.com/watch?v=NZxSA80lF1I&ab_channel=TechDecodeTutorials)

RTools guide, for those still having problems on Windows:
[https://www.youtube.com/watch?v=FXWLR2DGgI8&t=34s](https://www.youtube.com/watch?v=FXWLR2DGgI8&t=34s)
