
# This R script contains just the simple lifetable identity functions that
# we made in class on Monday 1 Aug, 2022. I made some minor changes to
# 1) simplify function names
# 2) handle the open age group in some cases

# NOTE: function arguments refer to lifetable columns
#       that need to have been previously created. These
#       are not 

# NEW name

# this is a common formula for getting
# probabilities from rates.
get_nqx <- function(nMx, nAx, n){
  nqx <- (n * nMx) / ( 1 + (n - nAx) * nMx)
  
  # these are called "kludge"s
  nqx[nqx > 1] <- 1
  nqx[nqx < 0] <- 0
  
  # NEW close out with 1 
  nqx[length(nqx)] <- 1
  
  return(nqx)
}

# NEW name
get_lx <- function(nqx, radix){
  # cumulative product of the conditional
  # survival probabilities in each age.
  lx <- cumprod(1 - nqx)
  lx <- c(1, lx)
  lx <- lx[-length(lx)]
  lx <- radix * lx
  return(lx)
}
# NEW name
get_ndx <- function( nqx, lx){
  nqx * lx
}

# NEW a second version of get_ndx() according to 
# student petition
get_ndx_2 <- function( nqx, lx){
  -diff(c(lx, 0))
}

# NEW name 
get_nLx <- function(lx, nAx, ndx, n){
  nLx      <- n * lx - (n - nAx) * ndx
  
  # NEW care for closeout
  N        <- length(nLx)
  nLx[N]	 <- lx[N] * nAx[N] 
  nLx
}

# same
get_Tx <- function(nLx){
  nLx %>% rev() %>% cumsum() %>% rev()
}

# same
get_ex <- function(Tx, lx){
  Tx / lx
}

# we'll want this for day 7!
my_lifetable <- function(Data, radix = 100000){
  out <-
    Data %>% 
    mutate(
      # convert death rates to probabilities
      nqx = get_nqx(nMx = nMx,
                    nAx = nAx,
                    n = AgeInt),
      # derive survival function
      lx = get_lx(nqx = nqx,
                  radix = radix),
      # derive lifetable deaths distribution
      ndx = get_ndx(nqx = nqx,
                    lx = lx),
      # lifetable exposure
      nLx = get_nLx(lx = lx,
                    ndx = ndx,
                    nAx = nAx,
                    n = AgeInt),
      # total exposure above exact age x
      Tx = get_Tx(nLx = nLx),
      # conditional average remaining lifetime
      ex = get_ex(Tx = Tx,
                  lx = lx)) 
  return(out)
}
