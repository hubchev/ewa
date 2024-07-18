if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, janitor, psych, tinytable, ggstats,
               modelsummary, knitr, kableExtra, labelled)
rm(list = ls())

setwd("~/Dropbox/hsf/github/ewa/ss_24/read_in_71")

df_raw <- read.delim("Dataset 71.txt")

df_cosmetic <- df_raw |>
  clean_names() |>
  as_tibble() |>
  mutate(across(everything(), ~ if_else(is.nan(.), NA, .))) |>
  rowwise() |> 
  filter(!all(across(starts_with("item_"), ~ is.na(.)))) |> 
  ungroup()

df <- df_cosmetic |>
  rowwise() |>
  mutate(outlier = max(abs(c_across(starts_with("item_"))), na.rm = TRUE)) |>
  mutate(has_outlier = if_else(outlier > 5 | outlier == 0, TRUE, FALSE)) |>
  mutate(count_larger_5 = 
           sum( c_across(starts_with("item_")) > 5 | 
                  c_across(starts_with("item_")) == 0, na.rm = TRUE)) |> 
  mutate(count_typos = sum(c_across(starts_with("item_")) %in% 
                             c(11, 22, 33, 44, 55), na.rm = TRUE)) |> 
  mutate(has_larger_5_notypos = (count_typos < count_larger_5)) |> 
  mutate(has_typos = count_typos > 0 ) |>
  mutate(has_nas = if_else(anyNA(pick(starts_with("item_"))), TRUE, FALSE)) |>
  mutate(complete = (has_outlier == FALSE & has_nas == FALSE)) |> 
  ungroup()

likert_levels <- c(
  "Stimme überhaupt nicht zu",
  "Stimme nicht zu",
  "Neutral",
  "Stimme zu",
  "Stimme voll und ganz zu"
)

df_chr <- df |> 
  mutate(across(starts_with("item_"), 
                ~ case_when(
                  . == 1 ~ "Stimme überhaupt nicht zu",
                  . == 2 ~ "Stimme nicht zu",
                  . == 3 ~ "Neutral",
                  . == 4 ~ "Stimme zu",
                  . == 5 ~ "Stimme voll und ganz zu",
                  TRUE ~ as.character(.)
                ))) |> 
  mutate(across(starts_with("item_"), ~ factor(.x, levels = likert_levels)))

df_complete <- df_chr |> 
  filter(complete == TRUE) 

df_cleaned <- df |> 
  mutate(across(starts_with("item_"), ~ case_when(
    . == 11 ~ 1,
    . == 22 ~ 2,
    . == 33 ~ 3,
    . == 44 ~ 4,
    . == 55 ~ 5,
    TRUE ~ .
  ))) |> 
  mutate(across(starts_with("item_"), ~ if_else(. > 5 | . == 0, NA, .))) |> 
  mutate(across(starts_with("item_"), 
                ~ case_when(
                  . == 1 ~ "Stimme überhaupt nicht zu",
                  . == 2 ~ "Stimme nicht zu",
                  . == 3 ~ "Neutral",
                  . == 4 ~ "Stimme zu",
                  . == 5 ~ "Stimme voll und ganz zu",
                  TRUE ~ as.character(.)
                ))) |> 
  mutate(across(starts_with("item_"), ~ factor(.x, levels = likert_levels)))

rm(list = setdiff(ls(), c("df_complete", "df_raw", "df_cleaned", "df")))
save.image("data_71.RData")
