## ----echo=TRUE, message=FALSE---------------------------------------------------------------------------------------
if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, janitor, psych, tinytable, ggstats,
               modelsummary, knitr, kableExtra, labelled)
rm(list = ls())
setwd("~/Dropbox/hsf/24-ss/ewa/ewa_papers/read_in_71/")


## ----echo=TRUE, message=FALSE, output = FALSE, warning=FALSE--------------------------------------------------------
df_raw <- read.delim("Dataset 71.txt")


## ----echo=TRUE, message=FALSE, warning=FALSE------------------------------------------------------------------------
#| label: tbl-df_raw_glim
#| echo: false
#| tbl-cap: "Ausschnitt des Rohdatensatz"

head_df_raw <- head(df_raw)
tt(head_df_raw[,1:11])



## ----echo=TRUE, message=FALSE, warning=FALSE------------------------------------------------------------------------
#| label: tbl-df_raw_unique
#| echo: false
#| tbl-cap: "Unterschiedliche Werte in den Variablen"
#| fig-align: left


uv <- df_raw |> 
  select(-ID) |> 
  as.matrix() |> 
  as.vector() |> 
  unique()

uv_item <- df_raw |> 
  select(-ID) |> 
  map(~ list(Values = unique(.) |> sort())) |> 
  enframe(name = "Attribute", value = "Values") |>  
  tibble()
  

# Create and display the table using kable
uv_item |> 
   kable("latex", booktabs = TRUE, longtable = TRUE) 


## ----echo=FALSE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
#| label: tbl-df_long
#| echo: false
#| tbl-cap: "Häufigkeitstabelle der unterschiedlichen Werte (pro item)"
#| apa-note: "Die Tabelle zeigt an, in wie vielen Items die jeweiligen Werte vorkommen."

long <- df_raw  |> 
  pivot_longer(!ID, names_to = "item", values_to = "count") |> 
  distinct(item, count) |> 
  arrange(item, count) 

long$count |> 
  tabyl() |> 
  kable("latex", booktabs = TRUE) 



## ----echo=FALSE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
#| label: tbl-df_raw_skim
#| echo: false
#| tbl-cap: "Deskriptive Statistiken zum Rohdatensatz"

datasummary_skim(df_raw, output = "latex")


## ----echo=TRUE, message=FALSE---------------------------------------------------------------------------------------
df_cosmetic <- df_raw |>
  clean_names() |>
  as_tibble() |>
  # Ersetzen von NaN-Werten durch NA
  mutate(across(everything(), ~ if_else(is.nan(.), NA, .))) |>
  #Entfernen von Zeilen, bei denen alle "item_"-Spalten NA sind
  rowwise() |> 
  filter(!all(across(starts_with("item_"), ~ is.na(.)))) |> 
  ungroup()


## ----echo=TRUE, message=FALSE---------------------------------------------------------------------------------------
df <- df_cosmetic |>
  rowwise() |>
  # Berechnung des größten absoluten Werts in "item_"-Spalten für jede Zeile
  mutate(outlier = max(abs(c_across(starts_with("item_"))), na.rm = TRUE)) |>
  # Markieren, ob ein Ausreißer (> 5 oder gleich 0) vorhanden ist
  mutate(has_outlier = if_else(outlier > 5 | outlier == 0, TRUE, FALSE)) |>
  # Zählen der Werte, die größer als 5 sind, für jede Zeile
  mutate(count_larger_5 = 
           sum( c_across(starts_with("item_")) > 5 | 
                c_across(starts_with("item_")) == 0, na.rm = TRUE)) |> 
  # Zählen der Tippfehler (11, 22, 33, 44, 55) für jede Zeile
  mutate(count_typos = sum(c_across(starts_with("item_")) %in% 
                             c(11, 22, 33, 44, 55), na.rm = TRUE)) |> 
  # Markieren, ob mehr Werte größer als 5 sind als Tippfehler
  mutate(has_larger_5_notypos = (count_typos < count_larger_5)) |> 
  # Markieren, ob Tippfehler vorhanden sind
  mutate(has_typos = count_typos > 0 ) |>
  # Markieren, ob NA-Werte in "item_"-Spalten vorhanden sind
  mutate(has_nas = if_else(anyNA(pick(starts_with("item_"))), TRUE, FALSE)) |>
  # Markieren, ob eine Zeile vollständig ist (keine Ausreißer und keine NAs)
  mutate(complete = (has_outlier == FALSE & has_nas == FALSE)) |> 
  ungroup()


## ----echo=TRUE, message=FALSE---------------------------------------------------------------------------------------
# Labels definieren
likert_levels <- c(
  "Stimme überhaupt nicht zu",
  "Stimme nicht zu",
  "Neutral",
  "Stimme zu",
  "Stimme voll und ganz zu"
)

# Faktorisierung der Items und hinzufügen eines Labels
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
  


## ----echo=TRUE, message=FALSE---------------------------------------------------------------------------------------
df_cleaned <- df |> 
  # Ersetzen von bestimmten Werten (11, 22, 33, 44, 55) in "item_"-Spalten
  mutate(across(starts_with("item_"), ~ case_when(
    . == 11 ~ 1,
    . == 22 ~ 2,
    . == 33 ~ 3,
    . == 44 ~ 4,
    . == 55 ~ 5,
    TRUE ~ .
  ))) |> 
  # Ersetzen von Werten größer als 5 durch NA in "item_"-Spalten
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



## ----echo=TRUE, message=FALSE---------------------------------------------------------------------------------------
save.image("data_71.RData")


## ----echo=FALSE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
#| label: fig-df_com_gglik
#| echo: false
#| fig-cap: "Antwortverteilung zu den gestellten Fragen (df_complete)"

gglikert(df_complete, include = starts_with("item_"))


## ----echo=FALSE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
#| label: fig-df_cle_gglik
#| echo: false
#| fig-cap: "Antwortverteilung zu den gestellten Fragen (df_cleaned)"

gglikert(df_cleaned, include = starts_with("item_"))


## ----echo=TRUE, message=FALSE, warning=FALSE------------------------------------------------------------------------
#| label: tbl-df_com_unique
#| echo: false
#| tbl-cap: "Unterschiedliche Werte in den Variablen (df_complete)"

uv_item <- df_complete |> 
  select(-id) |> 
  select(group, starts_with("item_")) |>  
  map(~ list(Values = unique(.) |> sort())) |> 
  enframe(name = "Attribute", value = "Values") |>  
  tibble()
  

# Create and display the table using kable
uv_item |> 
   kable("latex", booktabs = TRUE, longtable = TRUE) 


## ----echo=TRUE, message=FALSE, warning=FALSE------------------------------------------------------------------------
#| label: tbl-df_cle_unique
#| echo: false
#| tbl-cap: "Unterschiedliche Werte in den Variablen (df_cleaned)"

uv_item <- df_cleaned |> 
  select(-id) |> 
  select(group, starts_with("item_")) |>  
  map(~ list(Values = unique(.) |> sort())) |> 
  enframe(name = "Attribute", value = "Values") |>  
  tibble()
  

# Create and display the table using kable
uv_item |> 
   kable("latex", booktabs = TRUE, longtable = TRUE) 

