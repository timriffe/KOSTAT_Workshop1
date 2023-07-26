
# TTD pyramid:
library(tidyverse)
library(colorspace)
source("lifetable_utils.R")
KOR <- read_csv("Data/KOR2014.csv", show_col_types = FALSE)

LT <- 
  KOR |> 
  select(-births, - year) |> 
  group_by(sex) |> 
  mutate(mx = deaths / exposure,
         ax = c(.14,rep(.5,109),1.4),
         n = rep(1,111),
         qx = calc_qx(mx,ax,n),
         lx = calc_lx(qx),
         dx = calc_dx(lx,qx)) |> 
  ungroup() |> 
  select(sex,age, exposure, dx) |> 
  filter(sex != "total")
Mna0 <- function(M){
            M[is.na(M)]  <- 0
            M[is.nan(M)] <- 0
            M
}
Minf0 <- function(M){
            M[is.infinite(M)]  <- 0
            M
        }
ThanoSimple <- function(Px, dx){
           ay      <- 0:110
           dx      <- Mna0(dx)
           dx      <- c(dx, dx * 0) / sum(dx) # pad out with 0s
           EDx     <- matrix(dx[col(matrix(nrow = 111, ncol = 111)) + ay], 
                    nrow = 111, ncol = 111, dimnames = list(Ex = ay, Age = ay))
            t(Px * Minf0(Mna0(EDx / rowSums(EDx))))
        }
Eaym <- ThanoSimple(Px = LT$exposure[LT$sex == "male"], dx = LT$dx[LT$sex=="male"]) |> 
  as.data.frame() |> 
  rownames_to_column("ttd") |> 
  pivot_longer(-ttd, names_to = "age", values_to = "Eay") |> 
  mutate(sex = "male")
Eayf <- ThanoSimple(Px = LT$exposure[LT$sex == "female"], dx = LT$dx[LT$sex=="female"]) |> 
  as.data.frame() |> 
  rownames_to_column("ttd") |> 
  pivot_longer(-ttd, names_to = "age", values_to = "Eay") |> 
  mutate(sex = "female")

Eay <- bind_rows(Eaym, Eayf) |> 
  mutate(age = as.integer(age),
         ttd = as.integer(ttd))

Age_pyramid <-LT |> 
  select(-dx) |> 
  rename(pop = exposure)

TTD_pyramid <- Eay |> 
  group_by(sex, ttd) |> 
  summarize(pop = sum(Eay), 
            .groups = "drop")

Age_pyramid_w_heterogeneity <-
  Eay |> 
  mutate(ttd = ttd - ttd %% 10) |> 
  group_by(sex, age, ttd) |> 
  summarize(pop = sum(Eay), 
            .groups = "drop")

TTD_pyramid_w_heterogeneity <-
  Eay |> 
  mutate(age = age - age %% 10) |> 
  group_by(sex, age, ttd) |> 
  summarize(pop = sum(Eay), 
            .groups = "drop")
options(scipen = 999)

# 1 standard age pyramid
Age_pyramid |> 
  mutate(pop = ifelse(sex == "male",-pop,pop)) |> 
  ggplot(aes(x = age, y = pop)) +
  geom_col(width=1, fill = gray(.7)) +
  coord_flip() +
  theme_minimal() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18)) +
  scale_y_continuous(labels = abs, limits = c(-500000,500000)) +
  geom_hline(yintercept = seq(-400000,400000,by=200000), color = "white") +
  geom_vline(xintercept = seq(20,100,by=20), color = "white") +
  labs(y = "")

# 1 same pyramid, pointing out ttd heterogeneity
Age_pyramid_w_heterogeneity |> 
  mutate(pop = ifelse(sex == "male",-pop,pop)) |> 
  ggplot(aes(x = age, y = pop, fill = ttd)) +
  geom_col(width=1) +
  coord_flip() +
  theme_minimal() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16)) +
  scale_y_continuous(labels = abs, limits = c(-500000,500000)) +
  geom_hline(yintercept = seq(-400000,400000,by=200000), color = "white") +
  geom_vline(xintercept = seq(20,100,by=20), color = "white") +
  scale_fill_continuous_sequential("ag_GrnYl") +
  guides(fill = "none")+
  labs(y = "")

# TTD_pyramid |> 
#   mutate(pop = ifelse(sex == "male",-pop,pop)) |> 
#   ggplot(aes(x = ttd, y = pop)) +
#   geom_col(width=1, fill = gray(.7)) +
#   coord_flip() +
#   theme_minimal() +
#   theme(axis.text = element_text(size = 14),
#         axis.title = element_text(size = 16)) +
#   scale_y_continuous(labels = abs, limits = c(-500000,500000))+
#   geom_hline(yintercept = seq(-400000,400000,by=200000), color = "white") +
#   geom_vline(xintercept = seq(10,100,by=20), color = "white")

TTD_pyramid_w_heterogeneity |> 
  mutate(ttd10 = ttd - ttd %% 10) |> 
  mutate(pop = ifelse(sex == "male",-pop,pop)) |> 
  ggplot(aes(x = ttd, y = pop, fill = ttd10)) +
  geom_col(width=1) +
  coord_flip() +
  theme_minimal() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16)) +
  scale_y_continuous(labels = abs, limits = c(-500000,500000)) +
  geom_hline(yintercept = seq(-400000,400000,by=200000), color = "white") +
  geom_vline(xintercept = seq(20,100,by=20), color = "white") +
  scale_fill_continuous_sequential("ag_GrnYl",rev=TRUE)+
  guides(fill = "none")+
  labs(y = "")

TTD_pyramid_w_heterogeneity |> 
  mutate(pop = ifelse(sex == "male",-pop,pop)) |> 
  ggplot(aes(x = ttd, y = pop, fill = age)) +
  geom_col(width=1) +
  coord_flip() +
  theme_minimal() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16)) +
  scale_y_continuous(labels = abs, limits = c(-500000,500000)) +
  geom_hline(yintercept = seq(-400000,400000,by=200000), color = "white") +
  geom_vline(xintercept = seq(20,100,by=20), color = "white") +
  scale_fill_continuous_sequential("Sunset",rev=TRUE)+
  guides(fill = "none")+
  labs(y = "")


# ThanoSimple(Px = LT$lx[LT$sex == "male"], dx = LT$dx[LT$sex=="male"]) |> rowSums()

mx_gomp <- function(a,b,x){
  a * exp(x*b)
}

plot(mx_gomp(.001,.07,0:110), log = 'y')


mx2lx <- function(mx){
  n = length(mx)
  lx = exp(-cumsum(mx))
  c(1,lx[-c(n)])
}
mx1 <- mx_gomp(1,.09,0:110)
mx2 <- mx_gomp(.1,.09,0:110)
mx3 <- mx_gomp(.01,.09,0:110)
mx4 <- mx_gomp(.001,.09,0:110)
mx5 <- mx_gomp(.0001,.09,0:110)
mx6 <- mx_gomp(.00001,.09,0:110)

library(tidyverse)

initial_conditions <- tibble(mx1 = .01,
                             mx2 = .03,
                             mx3 = .06,
                             mx4 = .15,
                             mx5 = .6,
                             mx6 = .05) |> 
  pivot_longer(everything(),names_to = "frailty group", values_to = "init") 
initial_conditions$init |> sum()
fmx <-
tibble(age=0:110,mx1,mx2,mx3,mx4,mx5,mx6) |> 
  pivot_longer(-age,names_to = "frailty group", values_to = "mx") |> 
  arrange(`frailty group`,age) |> 
  group_by(`frailty group`) |> 
  mutate(lx = mx2lx(mx)) |> 
  ungroup() |> 
  left_join(initial_conditions,by = "frailty group") |> 
  mutate(lxw = lx * init) |> 
  group_by(age) |> 
  mutate(w = lxw / sum(lxw),
         `frailty group` = parse_number(`frailty group`))

options(scipen = 999)
fmx |> 
  ggplot(aes(x = age, y = mx, group = `frailty group`)) +
  geom_line(linewidth = 2) +
  scale_y_log10() +
  theme_minimal()

fmx |> 
  ggplot(aes(x = age, y = lx, group = `frailty group`)) +
  geom_line(linewidth = 2) +
  # scale_y_log10() +
  theme_minimal()

fmx |> 
  ggplot(aes(x = age, y = lxw, fill = as.factor(`frailty group`))) +
  geom_area(linewidth = 1) +
  # scale_y_log10() +
  theme_minimal() + 
  ylim(0,1)

fmx |> 
  ggplot(aes(x = age, y = w, fill = as.factor(`frailty group`))) +
  geom_area(linewidth = 1) +
  # scale_y_log10() +
  theme_minimal() + 
  ylim(0,1)

avgmx <-
  fmx |> 
  group_by(age) |> 
  summarize(mx = sum(mx * w))

fmx |> 
ggplot(aes(x = age, y = mx)) +
  geom_line(mapping=aes(group = `frailty group`),linewidth = 2) +
  scale_y_log10() +
  theme_minimal() +
  geom_line(data = avgmx, 
            mapping = aes(x = age, y = mx),
            color = "red",
            linewidth = 2)
avgmx |> 
  ggplot(aes(x=age,y=mx)) +
  geom_line() 

hmd <- read_csv("https://github.com/timriffe/KOSTAT_Workshop1/raw/master/Data/hmd.csv.gz")

library(wpp2022)
data("popAge1dt")
pop1dt <- popAge1dt
pop1dt |> write_csv("Data/pop1dt.csv")
library(utils)

test <- read_csv("https://github.com/timriffe/KOSTAT_Workshop1/raw/master/Data/pop1dt.csv.gz")
test |> head()
