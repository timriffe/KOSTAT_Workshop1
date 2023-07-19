calc_nqx <- function(nMx, nAx, n){
  qx         <- (n * nMx) / (1 - (n - nAx) * nMx)
  
  # these are kludges, necessary to ensure nqx results as a probability
  qx[qx > 1] <- 1
  qx[qx < 0] <- 0
  qx
}

calc_lx <- function(nqx, radix = 1){
  npx <- 1 - nqx
  n   <- length(nqx)
  lx  <- cumprod(npx)
  # shift it back 1, as we start with 100%!
  # also ensure outgoing vector is the same length.
  lx  <- radix * c(1, lx[-n])
  lx
}

calc_ndx <- function(nqx, lx){
  nqx * lx
}

calc_nLx <- function(lx, ndx, nAx, n){
  N        <- length(lx)
  nLx      <- n[-N] * lx[-1] + nAx[-N] * ndx[-N]
  # special treatment for open age
  nLx[N]	 <- lx[N] * nAx[N] 
  nLx
}


calc_Tx <- function(nLx){
  # to understand this, look at the nLx curve,
  # then imagine integrating from the right 
  # to the left. Then compare with the formula!
  nLx |> 
    rev() |> 
    cumsum() |> 
    rev()
}
calc_ex <- function(Tx, lx){
  Tx / lx
}

calc_LT <- function(nMx, nAx, n, radix){
  N <- length(nMx)
  nqx  <- calc_nqx(nMx, nAx, n)
  lx   <- calc_lx(nqx = nqx, radix = 1)
  ndx  <- calc_ndx(nqx = nqx, lx = lx)
  nLx  <- calc_nLx(lx = lx, ndx = ndx, nAx = nAx, n = n)
  Tx   <- calc_Tx(nLx = nLx)
  ex   <- calc_ex(Tx = Tx, lx = lx)
  Age  <- cumsum(c(0,n))[1:N]
  
  
  tibble(age = Age,
         nMx = nMx,
         nAx = nAx,
         nqx = nqx,
         lx = lx,
         ndx = ndx,
         nLx = nLx,
         Tx = Tx,
         ex = ex)
}

# data.frame in, data.frame out!
calc_LT_tidy <- function(data, radix){
  # this is hacky, but works.
  # just pick out the needed vectors from the group of data
  calc_LT(nMx = data$nMx,
          nAx = data$nAx,
          n = data$n,
          radix = radix)
}









