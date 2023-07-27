


50000000 * .5 ^ (1:10)
6*33

library(tidyverse)
library(HMDHFDplus)
coale_r <- function(fxf, Lx, G = 29, maxit = 50, tol = 10^-12){
  R0 <- sum(fxf * Lx)
  ri <- log(R0) / G
  x  <- 0:(length(fxf)-1)
  for (i in 1:maxit){
    deltai <- sum(exp(-ri * x) * fxf * Lx) - 1
    ri     <- ri + (deltai / (G - (deltai / ri)))
    
    # this is just like delti, isn't it?
    resid <- abs(sum(exp(-ri * x) * fxf * Lx) - 1)
    if (resid < tol){
      break
    }
  }
  return(ri)
}
mlt <- readHMDweb("KOR","mltper_1x1",username = Sys.getenv("us"), password = Sys.getenv("pw"))
flt <- readHMDweb("KOR","fltper_1x1",username = Sys.getenv("us"), password = Sys.getenv("pw"))

B <-
read_csv("Data/BirthsKORHFD.csv") |> 
  select(year = Year,
         age = Age,
         births = Total) |> 
  mutate(age = parse_number(age)) |> 
  filter(year == max(year))|> 
  select(-year)

E <- read_csv("Data/ExposuresKORHMD.csv") |> 
  select(year = Year,
         age = Age,
         exposure = Female) |> 
  filter(year == max(year)) |> 
  select(-year)

BE <- left_join(E,B, by = "age") |> 
  mutate(births = if_else(is.na(births),0,births),
         fx = births / exposure) |> 
  select(age, fx)

mst <-
  mlt |> 
  filter(Year == max(Year)) |> 
  select(age = Age, pop = Lx) |> 
  mutate(sex = "male")
fst <-
  flt |> 
  filter(Year == max(Year)) |> 
  select(age = Age, pop = Lx) |> 
  mutate(sex = "female")
PFB <- 1 / (1 + 223356/212079)
fstb <-
  fst |> 
  select(-sex) |> 
  rename(Lx = pop) |> 
  left_join(BE, by = "age") |> 
  mutate(Lx = Lx / 1e5,
         fxf = fx * PFB) 
r_2020 <-
  fstb |> 
  summarize(r = coale_r(fxf,Lx,32),
            R0 = sum(fxf * Lx),
            G = log(R0) / r)
r_use <- r_2020$r

popst <- 
  bind_rows(mst, fst)|> 
  mutate(
    pop = 100 * pop / sum(pop),
    pop = ifelse(sex == "male",-pop,pop))

pop_stb_2020 <-
  popst |> 
  mutate(popstb = pop * exp(-r_use * (age + .5)),
         popstb = 100 * popstb / sum(popstb),
         popstb = ifelse(sex == "male",-popstb,popstb)) 
  
r_set <- tibble(r = seq(-.03,.03,by = .01))

pop_stb <-
  popst |> 
  cross_join(r_set) |>
  mutate(
    a = age + .5,
    pop_stb = exp(-r * a) * pop) |> 
  arrange(r,age) |> 
  group_by(r) |> 
  mutate(pop = 100 * pop_stb / sum(pop_stb)) |> 
  ungroup()

pop <- readHMDweb("KOR","Population",username = Sys.getenv("us"), password = Sys.getenv("pw"))  |> 
  filter(Year == 2020) |> 
  select(age = Age,
         male = Male2,
         female = Female2) |> 
  pivot_longer(-age, names_to = "sex", values_to = "pop")

options(scipen = 999)
# 1) show actual pyramid
pop |> 
  mutate(pop = ifelse(sex == "male",-pop,pop)) |> 
  ggplot(aes(x = age, y = pop)) +
  geom_col(width=1, fill = gray(.7)) +
  coord_flip() +
  theme_minimal() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18)) +
  scale_y_continuous(labels = abs(seq(-400,400,by=200)), limits = c(-500000,500000),
                     breaks = seq(-400000,400000,by=200000)) +
  scale_x_continuous(breaks = seq(0,100,by=20)) +
  geom_hline(yintercept = seq(-400000,400000,by=200000), color = "white") +
  geom_vline(xintercept = seq(20,100,by=20), color = "white") +
  labs(y = "population (1000s)")

# 2) rescale to 100%
pop |> 
  mutate(
    pop = 100 * pop / sum(pop),
    pop = ifelse(sex == "male",-pop,pop)) |> 
  ggplot(aes(x = age, y = pop)) +
  geom_col(width=1, fill = gray(.7)) +
  coord_flip() +
  theme_minimal() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18)) +
  scale_y_continuous(labels = abs, limits = c(-1,1)) +
  scale_x_continuous(breaks = seq(0,100,by=20)) +
  geom_hline(yintercept = seq(-1,1,by=.5), color = "white") +
  geom_vline(xintercept = seq(20,100,by=20), color = "white") +
  labs(y = "population (%)")

# 3) what's the stationary population structure?
popst  |> 
  ggplot(aes(x = age, y = pop)) +
  geom_col(width=1, fill = gray(.7)) +
  coord_flip() +
  theme_minimal() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18)) +
  scale_y_continuous(labels = abs, limits = c(-1,1)) +
  scale_x_continuous(breaks = seq(0,100,by=20)) +
  geom_hline(yintercept = seq(-1,1,by=.5), color = "white") +
  geom_vline(xintercept = seq(20,100,by=20), color = "white") +
  labs(y = "population (%)")

# 3) what are stable population structures?
pop_stb_plot <-
  pop_stb |> 
  group_by(r) |> 
  mutate(
    pop_stb = 100 * pop_stb / sum(pop_stb),
    pop_stb = ifelse(sex == "male",-pop_stb,pop_stb)) |> 
  ungroup() 

library(colorspace)

pop_stb_plot |> 
  ggplot(aes(x = age, y = pop_stb, color = r, group = interaction(sex,r))) +
  geom_hline(yintercept = seq(-1,1,by=.5), color = gray(.9), linewidth = .5) +
  geom_vline(xintercept = seq(20,100,by=20), color = gray(.9), linewidth = .5) +
  geom_line(linewidth=1, alpha = .7) +
  coord_flip() +
  theme_minimal() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18)) +
  scale_y_continuous(labels = abs, limits = c(-1.6,1.6)) +
  scale_x_continuous(breaks = seq(0,100,by=20)) +
  scale_color_binned_diverging("Berlin")+
  labs(y = "population (%)")


pop_stb_2020 

  pop |> 
  mutate(
    pop = 100 * pop / sum(pop),
    pop = ifelse(sex == "male",-pop,pop)) |> 
  ggplot(aes(x = age, y = pop)) +
  geom_col(width=1, fill = gray(.7)) +
    geom_hline(yintercept = seq(-1,1,by=.5), color = "white") +
    geom_vline(xintercept = seq(20,100,by=20), color = "white") +
  geom_line(data = pop_stb_2020,
            mapping = aes(x=age,y=popstb,group = sex)) +
    geom_line(data = popst,
              mapping = aes(x=age,y=pop,group = sex)) +
  coord_flip() +
  theme_minimal() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18)) +
  scale_y_continuous(labels = abs, limits = c(-1.2,1.2)) +
  scale_x_continuous(breaks = seq(0,100,by=20)) +
  labs(y = "population (%)")



