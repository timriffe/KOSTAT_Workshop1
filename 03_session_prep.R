
#library(remotes)
#install_github("PPgp/wpp2022")
library(wpp2022)

data(mx1dt)
head(mx1dt)
pop <- read_csv("Data/pop1dt.csv.gz")
head(pop)
write_csv(mx1dt, "Data/mx1dt.csv.gz")
pop |> pull(age) |> unique()
mx1dt |> pull(age) |> unique()
