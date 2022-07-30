
# Carl Schmertmann's TOPALS fertility graduation approach,
# code modified by TR
graduate_CS_df <- function(data, K8, S){
  
  Fx <- data %>% pull(ASFR)
  Age <- data %>% pull(Age)
  
  # columns of K8
  ka  <- seq(10,45,by=5)
  
  # keep ind
  ind <- ka >= min(Age) & ka <= max(Age)
  
  # scale ASFR to interval width
  #	Fx5 <- Fx * AgeInt
  
  # turn to column matrix
  f5 <- matrix(Fx)
  f1 <- S %*% K8[,ind] %*% f5
  
  # no negatives allowed!
  f1[f1 < 0] <- 0
  
  # return object:
  data.frame(Age = as.integer(rownames(f1)),
             ASFR = f1)
  
}

# loads K8 object needs for method
K8 <- read_csv("https://raw.githubusercontent.com/timriffe/KOSTAT_Workshop1/master/Data/K8.csv") %>% 
  as.matrix()
rownames(K8) <- seq(12.25,54.75,by=.5) %>% round(2)

# creates object S, required by method
S <- rep(c(.5,rep(0,42),.5,rep(0,43)),43) %>% 
  matrix(nrow=43) %>% 
  '['(,-87) 
rownames(S) <- 12:54
colnames(S) <- seq(12.25,54.75,by=.5) %>% round(2)


