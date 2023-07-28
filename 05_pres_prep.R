library(tidyverse)
library(colorspace)

hmd <- read_csv("Data/hmd.csv.gz")

AUS <- hmd |> 
  filter(country == "Australia",
         sex=="f") |> 
  mutate(lmx = log(mx))

# 1) visualize

AUS |> 
  ggplot(mapping = aes(x = year, 
                       y = age, 
                       z = lmx)) +
  geom_contour_filled() 

# 2) summarize
# how much can we explain with 2 parameters?

lmx_lm <-
  AUS %>% 
  lm(lmx ~ age, data = .) |> 
  predict(newdata = tibble(age = 0:110)) |> 
  as_tibble() |> 
  rename(lmx_lm = value) |> 
  mutate(age = 0:110) 
  

mlmx <- mean(AUS$lmx)

AUS |> 
  ggplot(aes(x = age, y = lmx)) +
  geom_line(mapping = aes(color = year, group = year), alpha = .5) +
  scale_color_continuous_sequential("Rocket") +
  geom_line(data = lmx_lm, mapping = aes(x = age, y = lmx_lm)) +
  theme_minimal() +
  geom_hline(yintercept = mlmx)

# Variance explained?
AUS |> 
  full_join(lmx_lm, by = c("age")) |> 
  filter(!is.infinite(lmx_lm)) |> 
  mutate(mlmx = mean(lmx),
         res = (lmx - lmx_lm)^2) |> 
  summarize(lmx = sqrt(sum((lmx - mlmx)^2)),
            lmx_lm = sqrt(sum(res))) |> 
  mutate(explained = paste(round(100 * (lmx - lmx_lm) / lmx,1),"%"))
  
# 3) How much can we explain with 3?

# how much can we explain with 3 parameters?
years <- AUS$year |> unique() |> sort()
xt <- expand_grid(age = 0:110,year=years)
lmx_lmt <-
  AUS %>% 
  lm(lmx ~ age + year, data = .) |> 
  predict(newdata = xt) |> 
  as_tibble() |> 
  rename(lmx_lm = value) |> 
  bind_cols(xt) 

AUS |> 
  full_join(lmx_lmt, by = c("age","year")) |> 
  filter(!is.infinite(lmx_lm)) |> 
  mutate(mlmx = mean(lmx),
         res = (lmx - lmx_lm)^2) |> 
  summarize(lmx = sqrt(sum((lmx - mlmx)^2)),
            lmx_lm = sqrt(sum(res))) |> 
  mutate(explained = paste(round(100 * (lmx - lmx_lm) / lmx,1),"%"))

# 4) allow for tilt (4 parameters)

lmx_lmt2 <-
  AUS %>% 
  lm(lmx ~ age * year, data = .) |> 
  predict(newdata = xt) |> 
  as_tibble() |> 
  rename(lmx_lm = value) |> 
  bind_cols(xt) 

AUS |> 
  full_join(lmx_lmt2, by = c("age","year")) |> 
  filter(!is.infinite(lmx_lm)) |> 
  mutate(mlmx = mean(lmx),
         res = (lmx - lmx_lm)^2) |> 
  summarize(lmx = sqrt(sum((lmx - mlmx)^2)),
            lmx_lm = sqrt(sum(res))) |> 
  mutate(explained = paste(round(100 * (lmx - lmx_lm) / lmx,1),"%"))

# 5) what if we allow each age  to progress independently?
xt2 <- xt |> 
  mutate(agec =as.character(age))
lmx_lmt3 <-
  AUS %>% 
  mutate(agec = as.character(age)) %>%
  lm(lmx ~ agec * year, data = .) |> 
  predict(newdata = xt2) |> 
  as_tibble() |> 
  rename(lmx_lm = value) |> 
  bind_cols(xt2) |> 
  select(-agec)

AUS |> 
  full_join(lmx_lmt3, by = c("age","year")) |> 
  filter(!is.infinite(lmx_lm)) |> 
  mutate(mlmx = mean(lmx),
         res = (lmx - lmx_lm)^2) |> 
  summarize(lmx = sqrt(sum((lmx - mlmx)^2)),
            lmx_lm = sqrt(sum(res))) |> 
  mutate(explained = paste(round(100 * (lmx - lmx_lm) / lmx,1),"%"))

# or each year to have a line:

xt3 <- xt |> 
  mutate(yearc =as.character(year))
lmx_lmt4 <-
  AUS %>% 
  mutate(yearc = as.character(year)) %>%
  lm(lmx ~ age * yearc, data = .) |> 
  predict(newdata = xt3) |> 
  as_tibble() |> 
  rename(lmx_lm = value) |> 
  bind_cols(xt3) |> 
  select(-yearc)

AUS |> 
  full_join(lmx_lmt4, by = c("age","year")) |> 
  filter(!is.infinite(lmx_lm)) |> 
  mutate(mlmx = mean(lmx),
         res = (lmx - lmx_lm)^2) |> 
  summarize(lmx = sqrt(sum((lmx - mlmx)^2)),
            lmx_lm = sqrt(sum(res))) |> 
  mutate(explained = paste(round(100 * (lmx - lmx_lm) / lmx,1),"%"))


# 6) now what about LC method?


# 6.1 alpha is just the average:

alpha <- 
  AUS |> 
  group_by(age) |> 
  summarize(alphax = mean(lmx))

# Alone, it explains:
AUS |> 
  mutate(mlmx = mean(lmx)) |> 
  group_by(age) |> 
  mutate(mlmxx = mean(lmx),
         res = (lmx - mlmxx)^2) |> 
  ungroup() |> 
  summarize(lmx = sqrt(sum((lmx - mlmx)^2)),
            lmx_lm = sqrt(sum(res))) |> 
  mutate(explained = paste(round(100 * (lmx - lmx_lm) / lmx,1),"%"))

# 6.2 Center lmx, put in AP matrix

cent_lM <-
  AUS |> 
  left_join(alpha,by="age") |> 
  mutate(lmx_centered = lmx - alphax) |> 
  select(age, year, lmx_centered) |> 
  pivot_wider(names_from = year,
              values_from = lmx_centered) |> 
  column_to_rownames("age") |> 
  as.matrix()
  
# 6.3 do svd on the matrix

svd_lM <- svd(cent_lM)

U <- svd_lM$u
D <- svd_lM$d
V <- svd_lM$v

u      <- svd_lM$u[, 1]
v      <- svd_lM$v[, 1]
d      <- svd_lM$d[1]


(U %*% diag(D) %*% t(V)) -

  image(U)
plot(D)
image(V)

plot(U[,1])
plot(V[,1])
#Explained variance
# exp.var<- cumsum((svd_lM$d)^2 / sum((svd_lM$d)^2))[1]
# exp.var

# age pattern of mortality change
Bx <- u / sum(u)

# secular change
Kt <- v * sum(u) * d

# put in tables:
beta <- 
  tibble(betax = Bx,
       age = 0:110)

kappa <- 
  tibble(kappat = Kt,
         year = years)

lK     <- length(Kt)
drift  <- (Kt[lK] - Kt[1]) / (lK - 1)
  
ggplot(alpha, mapping = aes(x = age, y = alphax)) +
  geom_line() +
  theme_minimal()

ggplot(beta, mapping = aes(x = age, y = betax)) +
  geom_line() +
  theme_minimal()

ggplot(kappa, mapping = aes(x = year, y = kappat)) +
  geom_line() +
  theme_minimal()

# combine to predict:
cross_join(beta, kappa) |> 
  left_join(alpha, by = "age") |> 
  mutate(lmx_lc = alphax + betax * kappat) |> 
  left_join(AUS, by = c("age","year")) |> 
  mutate(mlmx = mean(lmx),
         res = (lmx - lmx_lc)^2) |> 
  summarize(lmx = sqrt(sum((lmx - mlmx)^2)),
            lmx_lc = sqrt(sum(res))) |> 
  mutate(explained = paste(round(100 * (lmx - lmx_lc) / lmx,1),"%"))


