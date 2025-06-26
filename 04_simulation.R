library(tidyverse)

a2 <- .0003
b <-.07
age <- 30:100
gompmx <- function(a,b,x){
  a * exp(b * x)
}
mx_to_lx_cheap <- function(mx){
  n <- length(mx)
  c(1,exp(-cumsum(mx[-n])))
}
mx1 <- gompmx(a1,b,age)
mx2 <- gompmx(a2,b,age)
lx1 <- mx_to_lx_cheap(mx1)
lx2 <- mx_to_lx_cheap(mx2)


lx_tidy <- tibble(age, lx1, lx2)
prev30 <- c(rep(.01,70), rep(.1,7), rep(.2,7), rep(.3,7), rep(.4,70))

cohorts <- seq_along(prev30) + 1900
cohorts_tidy <- tibble(cohort = cohorts, prev_high = prev30)


lx_data <- cross_join(cohorts_tidy,lx_tidy) 
prev_data <- 
  lx_data |> 
  mutate(prev1 = prev_high * lx1,
         prev2 = (1 - prev_high) * lx2,
         year = cohort + age) |> 
  select(year, age, prev1, prev2) |> 
  group_by(year) |> 
  mutate(n = n()) |> 
  ungroup() |> 
  filter(n == 71) |> 
  select(-n)