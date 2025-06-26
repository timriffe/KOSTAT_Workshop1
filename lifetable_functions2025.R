# functions created on 24 June, 2025 for the second session of 
# the Introduction to Demography block of the KOSTAT-UNFPA Sum...
library(tidyverse) # used by lifetable function

# calculate qx using the identity between ax, mx, qx
mx_to_qx <- function(mx, ax){
  qx              <- mx / (1 + (1 - ax) * mx)
  # make sure it obeys the rules!
  qx[qx > 1]      <- 1
  # no one gets out!
  qx[length(qx)]  <- 1
  return(qx)
}

mx_to_ax <- function(mx, age, sex){
  # this copied in from session 4, why do this more than once?
  ax = case_when(
    sex == "male" & age == 0 & between(mx, 0, 0.02300) ~ 0.14929 - 1.99545 * mx,
    sex == "male" & age == 0 & between(mx, 0.02300001, 0.08307) ~ 0.02832 + 3.26021 * mx,
    sex == "male" & age == 0 & between(mx, 0.08307001, 1) ~ 0.29915,
    sex == "female" & age == 0 & between(mx, 0, 0.01724) ~ 0.14903 - 2.05527 * mx,
    sex == "female" & age == 0 & between(mx, 0.01724001, 0.06891) ~ 0.04667 + 3.88089 * mx,
    sex == "female" & age == 0 & between(mx, 0.06891001,1) ~ 0.31411,
    age == max(age) ~ 1 / mx, # constant mortality for closeout (the mean survival time)
    TRUE ~ .5                 # if subject to constant mortality is the inverse of the rate
  )
}

# convert qx to lx
qx_to_lx <- function(qx, radix = 1e5){
  n  <- length(qx)
  qx <- c(0,qx[-n])
  lx <- radix * cumprod(1-qx)
  return(lx)
}

# calculate Lx 
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

# put the pieces together
calc_LT <- function(mx, ax, age, radix = 1e5){
  lt <- tibble(age, mx, ax) |> 
    mutate(qx = mx_to_qx(mx = mx, ax = ax),
           lx = qx_to_lx(qx = qx, radix = radix),
           dx = qx * lx,
           Lx = calc_Lx(lx = lx, dx = dx, ax = ax),
           ex = calc_ex(Lx = Lx,lx = lx))
  return(lt)
} 










