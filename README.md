## 12th KOSTAT-UNFPA Summer Seminar on Population
# Workshop 1 "R in Demography"

23 - 27 June, 2025 at the Statistics Human Resources Development Institute of KOSTAT in Daejeon, South Korea. For each session, a handout was prepared in advance (files `*_handout.pdf` and the markdown file where it was designed `*_handout.Rmd`). Sessions began with a presentation (files `*_presentation.pdf` and code for figures `*_pres_prep.R`). Live coding and notes from sessions is given in files `*_session.Rmd`. There is material covered in sessions that was not given in the handouts, and there is also material covered in handouts that was not given in the sessions.

## Contents 

- `Data/`

- `01` (Monday, 23 June 2025) "Basic Demographic Data and Concepts"
   1. `01_handout` (`.Rmd`, `.pdf`)
   2. `01_presentation.pdf`
   3. `01_pres_prep.R`
   4. `01_session.Rmd`
- `02` (Tuesday, 24 June 2025) "Mortality and Fertility" 
   1. `02_handout` (`.Rmd`, `.pdf`)
   2. `02_presentation.pdf`
   3. `02_pres_prep.R`
   4. `02_session.Rmd`
- `03` (Wednesday, 25 June 2025) "Population Structure"
   1. `03_handout` (`.Rmd`, `.pdf`)
   2. `03_presentation.pdf`
   3. `03_pres_prep.R`
   4. `03_session.Rmd`
- `04` (Thursday, 26 June 2025) "Population Growth"
   1. `04_handout` (`.Rmd`, `.pdf`)
   2. `04_presentation.pdf`
   3. `04_pres_prep.R`
   4. `04_session.Rmd`
- `05` (Friday, 27 June 2025) "Projection"
   1. `05_handout` (`.Rmd`, `.pdf`)
   2. `05_presentation.pdf`
   3. `05_pres_prep.R`
   4. `05_session.Rmd`

      
## Things to install

* Rstudio Desktop, from the daily builds [[https://www.rstudio.com/products/rstudio/download/preview/](https://dailies.rstudio.com/)]([https://www.rstudio.com/products/rstudio/download/preview/](https://dailies.rstudio.com/))
Installing this will automatically install the latest `R` version, which we need.

* Windows users: RTools will be required, please install it:
[https://cran.r-project.org/bin/windows/Rtools/](https://cran.r-project.org/bin/windows/Rtools/):
if you're having trouble, see:
[https://www.youtube.com/watch?v=FXWLR2DGgI8&t=34s](https://www.youtube.com/watch?v=FXWLR2DGgI8&t=34s)

* Mac users: Xquartz may be required, please install it:
[https://www.xquartz.org/](https://www.xquartz.org/)

## optional, but recommended to install:
* `git` [https://git-scm.com/downloads]()https://git-scm.com/downloads No need for a separate `git` program. We'll just use it from `Rstudio` a bit.
* `latex` if you don't have it, you can straight from `R` using the `tinytex` package.
  ```
  library(tinytex)
  install_tinytex()
  ```

## Further resources:
( how to do a full version upgrade, including all packages)
* Update R, R packages and R studio on different platforms 
[https://bootstrappers.umassmed.edu/bootstrappers-courses/courses/rCourse/Additional_Resources/Updating_R.html](https://bootstrappers.umassmed.edu/bootstrappers-courses/courses/rCourse/Additional_Resources/Updating_R.html)


