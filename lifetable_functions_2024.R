
mx_to_qx <- function(mx, ax, n = 1){
  num   <- (n * mx) 
  denom <- (1 - (n - ax) * mx)
  qx    <- num / denom
  n     <- length(qx)
  qx[n] <- 1
  
  qx[qx > 1] <- 1
  qx[qx < 0] <- 0
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


calc_LT <- function(mx, ax, n, radix){
  N    <- length(mx)
  qx   <- mx_to_qx(mx, ax, n)
  lx   <- qx_to_lx(qx = qx, radix = 1)
  dx   <- qxlx_to_dx(qx = qx, lx = lx)
  Lx   <- lxdx_to_Lx(lx = lx, dx = dx, ax = ax, n = n)
  Tx   <- Lx_to_Tx(Lx = Lx)
  ex   <- Txlx_to_ex(Tx = Tx, lx = lx)
  Age  <- 1:N - 1
  
  
  tibble(age = Age,
         mx = mx,
         ax = ax,
         qx = qx,
         lx = lx,
         dx = dx,
         Lx = Lx,
         Tx = Tx,
         ex = ex)
}

# data.frame in, data.frame out!
calc_LT_tidy <- function(data, radix){
  # this is hacky, but works.
  # just pick out the needed vectors from the group of data
  calc_LT(mx = data$mx,
          ax = data$ax,
          n = data$n,
          radix = radix)
}





