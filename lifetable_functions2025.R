# functions created on 24 June, 2025 for the second session of 
# the Introduction to Demography bloack of the KOSTAT-UNFPA Sum...
library(tidyverse)
qx_to_lx <- function(qx, radix = 1e5){
  n <- length(qx)
  qx <- c(0,qx[-n])
  lx <- radix * cumprod(1-qx)
  return(lx)
}

calc_Lx <- function(lx,dx,ax){
  n     <- length(lx)
  Lx    <- lx - (dx * (1 - ax))
  Lx[n] <- lx[n] * ax[n]
  return(Lx)
}

calc_ex <- function(Lx, lx){
  # this one is cumbersome:
  # Tx <- sum(Lx) - c(0,cumsum(Lx[-111]))  
  # this one makes your head spin:
  Tx <- Lx |> rev() |> cumsum() |> rev()
  ex <- Tx / lx # condition it! (scale it up)
  return(ex)
}

calc_LT <- function(mx, ax, age, radix = 1e5){
  lt <- tibble(age, mx, ax) |> 
    mutate(qx = mx_to_qx(mx = mx, ax = ax),
           lx = qx_to_lx(qx = qx, radix = radix),
           dx = qx * lx,
           Lx = calc_Lx(lx = lx, dx = dx, ax = ax),
           ex = calc_ex(Lx = Lx,lx = lx))
  return(lt)
} 










