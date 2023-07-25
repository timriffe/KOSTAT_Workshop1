calc_qx <- function(mx, ax, n){ # function body starts here
  qx    <- (n * mx) / (1 + (n - ax) * mx)
  return(qx)
} # ends here

calc_lx <- function(qx, radix = 1){
  
  lx <- cumprod(1 - qx)
  lx <- c(1, lx)
  lx <- lx[1:111]
  lx <- radix * lx
  return(lx)
}

calc_dx <- function(lx,qx){
  qx[111] <- 1
  
  dx <- lx * qx
  return(dx)
}

calc_Lx <- function(lx,dx,ax,n){
  Lx <- lx * n - (n - ax) * dx
  return(Lx)
}

calc_Tx <- function(Lx){
  Tx <- Lx |> 
    rev() |> 
    cumsum() |> 
    rev()
  return(Tx)
}

calc_ex <- function(Tx,lx){
  ex <- Tx / lx
  return(ex)
}