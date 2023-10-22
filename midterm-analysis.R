
x <- read.csv("~/Downloads/2023-10-16T1140_Grades-BST_260.csv") |> janitor::clean_names() |> dplyr::slice(-1)


y <- readr::parse_number(x$midterm_1_2023_757880) 
x <- readr::parse_number(x$diagnostic_assessment_726041)
x[x==0] <- NA

library(ggplot2)
library(dplyr)

data.frame(x=as.character(x), y=y) |> 
  filter(x!=15 | is.na(x)) |>
  mutate(x = ifelse(x<18, "<18", x)) |>
  mutate(y=y/16*100, x = ifelse(is.na(x), "NA", x)) |>
  group_by(x) |>
  summarize(avg = mean(y, na.rm=TRUE), min = min(y, na.rm = TRUE), n = n()) |>
  filter(!is.na(avg)) |> 
  mutate(x = factor(x, levels = c("NA", "20", "19", "18", "<18")), avg = round(avg), min = round(min)) |>
  arrange(x) |>
  setNames(c("Diagnostic", "Midterm 1 avg", "Minimum", "N")) 
