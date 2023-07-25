
#
library(tidyverse)
source("04_lifetable_functions.R")

KOR <- read_csv("Data/KOR2014.csv")
LT <-
KOR |> 
  mutate(nMx = deaths / exposure,
         nAx = if_else(age == 0, .1, .5),
         AgeInt = 1) |> 
  group_by(sex) |> 
  group_modify(~my_lifetable(Data = .x)) |> 
  ungroup()

library(scales)
options(scipen=999)
p<-
LT |> 
  filter(sex == "total") |> 
  select(age, nMx, nqx) |> 
  pivot_longer(-1, names_to = "measure", values_to = "value") |> 
  ggplot(aes(x=age,y=value,color=measure)) +
  geom_line(linewidth=2) +
  scale_y_log10() +
  theme_minimal() +
  theme(axis.text= element_text(size=16),
        axis.title = element_text(size = 18))+
  labs(y="measure of force") +
  guides(color = "none")
p
p+
KOR |> 
  filter(sex == "total") |> 
  mutate(age = case_when(between(age,1,4) ~ 1,
                         TRUE ~ age - age %% 5)) |> 
  group_by(sex, age) |> 
  summarize(deaths = sum(deaths),
            exposure = sum(exposure),
            .groups = "drop") |> 
  mutate(AgeInt = case_when(age == 0 ~ 1,
                            age == 1 ~ 4,
                            TRUE ~ 5), 
         nMx = deaths / exposure,
         nAx = DemoTools::lt_id_morq_a_greville(nMx,
                                                Age=age,
                                                AgeInt)) |> 
  group_modify(~my_lifetable(Data = .x)) |> 
  select(age, nMx, nqx) |> 
  pivot_longer(-age,names_to = "measure", values_to = "value") %>%
  geom_step(data=.,
            mapping = aes(x = age, y = value, color = measure),
            linewidth = 2)

KOR |> 
  filter(sex == "total") |> 
  ggplot(aes(x=age,y=-exposure)) +
  geom_col(width= 1, fill = gray(.8)) +
  theme_minimal() +
  geom_col(data = KOR %>% filter(sex == "total"),
           mapping = aes(x=age,y=deaths),
           width = 1, fill = "#d65a1c") + 
  labs(y = "counts") +
  theme(axis.title = element_text(size=16),
        axis.text = element_text(size=14)) +
  scale_y_continuous(labels = abs)
p <-
LT |> 
  filter(sex == "total") |> 
  ggplot(aes(x=age,y=lx)) +
  geom_line(linewidth=2) +
  theme_minimal() +
  theme(axis.title = element_text(size=16),
        axis.text = element_text(size=14)) 


LT |> 
  filter(sex == "total") |> 
  ggplot(aes(x=age,y=ndx)) +
  geom_line(linewidth=2) +
  theme_minimal() +
  theme(axis.title = element_text(size=16),
        axis.text = element_text(size=14)) 
LTt <- LT |> filter(sex == "total")
LT |> 
  filter(sex == "total") |> 
  ggplot(aes(x=age,y=lx)) +
  geom_line(linewidth=2) +
  theme_minimal() +
  theme(axis.title = element_text(size=16),
        axis.text = element_text(size=14)) +
  geom_line(data = filter(LT,sex == "total"),
            mapping = aes(x=age,y=ndx),
            linewidth=2,color = 'red')

LT |> 
  filter(sex == "total") |> 
  ggplot(aes(x=age,y=ndx)) +
  geom_line(linewidth=2) +
  theme_minimal() +
  theme(axis.title = element_text(size=16),
        axis.text = element_text(size=14)) +
  geom_line(data = filter(LT,sex == "total")%>%mutate(deaths  = 1e5*deaths/sum(deaths)),
            mapping = aes(x=age,y=deaths),
            linewidth=2,color = 'red')
LT |> 
  filter(sex == "female") |> 
  ggplot(aes(x=age,y=ndx)) +
  geom_line(linewidth=2) +
  theme_minimal() +
  theme(axis.title = element_text(size=16),
        axis.text = element_text(size=14)) +
  geom_line(data = filter(LT,sex == "female")%>%mutate(deaths  = 1e5*deaths/sum(deaths)),
            mapping = aes(x=age,y=deaths),
            linewidth=2,color = 'red')

e0 <- LT |> 
  filter(sex=="total",
         age==0) |> 
  pull(ex)
e0


LT |> 
  filter(sex == "total") |> 
  ggplot(aes(x=age,y=ndx)) +
  geom_line(linewidth=2) +
  theme_minimal() +
  theme(axis.title = element_text(size=16),
        axis.text = element_text(size=14)) +
  geom_vline(xintercept = e0, col = "red") 


B <-
  KOR |> 
  filter(births > 0) |> 
  select(age, births)

B |> 
  ggplot(aes(x = age, y = births)) +
  geom_col(width = 1, fill = gray(.7)) +
  theme_minimal() +
  theme(axis.title = element_text(size=16),
        axis.text = element_text(size=14)) +
  labs(y = "birth counts")

BE <- KOR |> 
  filter(sex == "female") |> 
  select(age, exposure) |> 
  right_join(B,by="age")
BE |> 
ggplot(aes(x=age,y=-exposure)) +
  geom_col(width= 1, fill = gray(.8)) +
  theme_minimal() +
  geom_col(mapping = aes(x=age,y=births),
           width = 1, fill = "#47a874") + 
  labs(y = "counts") +
  theme(axis.title = element_text(size=16),
        axis.text = element_text(size=14)) +
  scale_y_continuous(labels = abs)

BE |> 
  mutate(Fx = births / exposure) |> 
  ggplot(aes(x = age, y = Fx)) +
  geom_line(linewidth = 2) +
  theme_minimal() +
  labs(y = "birth rates") +
  theme(axis.title = element_text(size=16),
        axis.text = element_text(size=14)) 

MAB <-
  BE |> 
  mutate(Fx = births / exposure,
         age2 = age + .5) |> 
  summarize(MAB = sum(age * Fx) / sum(Fx),
            TFR = sum(Fx)) 
MAB
BE |> 
  mutate(Fx = births / exposure) |> 
  ggplot(aes(x = age, y = Fx)) +
  geom_line(linewidth = 2) +
  theme_minimal() +
  labs(y = "birth rates") +
  theme(axis.title = element_text(size=16),
        axis.text = element_text(size=14)) +
  geom_vline(xintercept = MAB$MAB, color="red")
