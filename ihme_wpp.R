library(wpp2022)
library(DemoTools)
data(mx1dt)
mx <- 
  mx1dt %>% 
  filter(name %in% c("Republic of Korea", "Kenya")) %>% 
  select(location = name, year, age, mxM, mxF) %>% 
  filter(between(year, 1990, 2019)) %>%
  pivot_longer(starts_with("mx"),
               names_to = "sex",
               values_to = "mx") %>% 
  mutate(sex = ifelse(str_detect(sex, "M$"), "Male", "Female"))

zz <- 
  read_csv("Data/IHME.csv",
           show_col_types = FALSE) %>% 
  select(-ends_with("_id")) %>% 
  select(-measure_name) %>% 
  filter(sex_name != "Both")


repeat_uniform <- function(data){
  x <- graduate_uniform(data$prev, data$age, data$AgeInt)
  tibble(age = as.integer(names(x)),
         prev = x)
}
z1 <- zz %>% 
  set_names(str_remove(names(.), "_name")) |> 
  filter(metric == "Percent") |>  
  select(location, cause, year, sex, age, prev = val) |> 
  mutate(age = str_remove(age, " years$") ,
         age = ifelse(age == "<1 year", "0", age),
         age = parse_number(age),
         AgeInt = case_when(age == 0 ~ 1,
                            age == 1 ~ 4,
                            TRUE ~ 5)) |> 
  group_by(location, cause, year, sex) |> 
  group_modify(~ repeat_uniform(data = .x)) |> 
  ungroup() |> 
  filter(age <= 100)



mx |> 
  inner_join(z1, by = c("location","year","sex","age")) |> 
  write_csv("Data/prev_mx.csv")
