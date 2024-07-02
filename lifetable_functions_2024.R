
mx_to_qx <- function(mx, ax, n = 1){
  num   <- (n * mx) 
  denom <- (1 - (n - ax) * mx)
  qx    <- num / denom
  n     <- length(qx)
  qx[n] <- 1
  return(qx)
}

qx_to_lx <- function(qx, radix = 1){
  
  # the essential bit
  lx <- c(1, cumprod(1 - qx))
  
  # ensure the same length
  lx <- lx[-length(lx)] 
  
  # scale it up, potentially
  lx <- lx * radix
  return(lx)
}


qxlx_to_dx <- function(qx, lx){
  dx <- lx * qx
  return(dx)
}

lxdx_to_Lx <- function(lx,dx,ax,n=1){
  Lx <- (lx * n) - (dx * (n - ax))
  return(Lx)
}

Lx_to_Tx <- function(Lx){
  Tx <- 
    Lx |> 
    rev() |> 
    cumsum() |> 
    rev()
  return(Tx)
}

Txlx_to_ex <- function(Tx, lx){
  ex <- Tx / lx
  return(ex)
}