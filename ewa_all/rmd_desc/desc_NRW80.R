## ----analysis-preferences---------------------------------------------------------------
# Seed for random number generation
set.seed(42)
knitr::opts_chunk$set(cache.extra = knitr::rand_seed)
# rm(list = ls())


## ----echo=TRUE, eval=TRUE---------------------------------------------------------------
# (Install and) load pacman package 
if (!require(pacman)) install.packages("pacman")

# load packages that are already installed and install packages that are not 
# installed yet and then load them:
pacman::p_load(tinylabels, 
               papaja,
               haven, 
               labelled, 
               janitor,
               skimr, 
               rstatix, 
               HH, 
               likert, 
               expss,
               tidyr, 
               ggstats,
               psych,
               sjlabelled,
               sjmisc,
               tidyverse, 
               MASS,
               dplyr)

sessionInfo()


## ----echo=TRUE--------------------------------------------------------------------------
getwd()
load("../data/gesis.RData")
df <- dfdta |>
  select(starts_with("alter"), 
         ALT_agegroup, 
         ALT_sex, 
         famst1, famst7, 
         demtectcorr, 
         kogstat, 
         final, 
         geschlecht)

# Remove the common prefix from all variables
df <- df |> 
  mutate_all(~ set_label(., gsub("^Alternserleben: ", "", get_label(.))))


## ----echo=TRUE--------------------------------------------------------------------------
df_alterl <- df |> 
  select(alterl1, 
         alterl2, 
         alterl3, 
         alterl4, 
         alterl5, 
         alterl6, 
         alterl7, 
         alterl8, 
         alterl9, 
         alterl10) |> 
  drop_unused_labels() 

# to remove unused labels you can use drop_unused_labels():
df_alterl_un <- df_alterl |>
  drop_unused_labels()

summary(df_alterl)


## ----echo=TRUE--------------------------------------------------------------------------
table(df_alterl$alterl1)


## ----echo=TRUE--------------------------------------------------------------------------
df_alterl |> 
  map(~ table(.))


## ----echo=TRUE--------------------------------------------------------------------------
df_alterl |> 
  map(~ proportions(table(.)))


## ----echo=TRUE--------------------------------------------------------------------------
df_alterl |> 
  tabyl(alterl1) 

df_alterl |> 
  map(~ tabyl(.))


## ----echo=TRUE--------------------------------------------------------------------------
df_alterl |> 
  map(~ frq(. , show.na = T))



## ----echo=TRUE--------------------------------------------------------------------------
summary(df)

sumstat_alter <- df |> 
  get_summary_stats(
    alterl1, 
    alterl2, 
    alterl3, 
    alterl4, 
    alterl5, 
    alterl6, 
    alterl7, 
    alterl8, 
    alterl9, 
    alterl10,  
    type = "five_number")  

sumstat_alter


## ----echo=TRUE--------------------------------------------------------------------------
sumstat_alter_psych <- df |>
  select(starts_with("alterl")) |> 
  psych::describe() |> 
  as_tibble(rownames="Question")  |> 
  select(-skew, -kurtosis, -range, -vars) 

sumstat_alter_psych


## ----echo=TRUE--------------------------------------------------------------------------
descriptives <- dfdta |>  
  # filter(alterl1 > 0) |> 
  group_by(geschlecht)  |> 
  summarize(
    Mean = mean(alterl1)
    , Count = n()
    , SD = sd(alterl1)
    , Min = min(alterl1)
    , Max = max(alterl1)
  )

descriptives


## ----tabrstatix, echo=TRUE--------------------------------------------------------------
apa_table(
  sumstat_alter
  , caption = "Summary Statistics: Experience of Ageing."
  , note = "This table contains all variables of `alterl*`."
  , escape = TRUE
  )


## ----tabsumstatalterpsych, echo = TRUE--------------------------------------------------
apa_table(
  sumstat_alter_psych
  , caption = "Summary Statistics: Experience of Ageing (psych)"
  , note = "This table contains all variables of `alterl*`."
  , escape = TRUE
)


## ----tabdescriptives, echo=TRUE---------------------------------------------------------
apa_table(
  descriptives
  , caption = "Experience of Ageing: Valuing Relationships and Other People 
  More (By Gender)"
  , escape = TRUE
)


## ----echo=TRUE--------------------------------------------------------------------------
df_alterl_balance <- df_alterl %>%
  rowwise() %>%
  mutate(has_negative = ifelse(any(c(across(alterl1:alterl10)) < 0), 1, 0)) |> 
  filter(has_negative == 0) |> 
  select(starts_with("alter")) |> 
  as_tibble()


## ----likertalterl1,  fig.cap="Experience of Ageing: Proportions of Answers (df_alterl)", echo=TRUE----
gglikert(df_alterl, 
         exclude_fill_values = c("Weiß nicht", "Verweigert"),
         sort = "ascending"
         )


## ----likertalterl2,  fig.cap="Experience of Ageing: Proportions of Answers (df_alterl_balance)", echo=TRUE----
gglikert(df_alterl_balance,
         sort = "ascending"
         )


## ----likertalterl3,  fig.cap="Experience of Ageing: Proportions of Answers - Stacked (df_alter)", echo=TRUE----
gglikert_stacked(df_alterl,
                 sort = "ascending",
                 sort_method = "mean"
                 )


## ----likertalterl4,  fig.cap="Experience of Ageing: Proportions of Answers - Stacked (df_alterl_balance)", echo=TRUE----
gglikert_stacked(df_alterl_balance,
                 sort = "ascending",
                 sort_method = "mean"
                 )


## ----tabsumstatalterpsychbal, echo=TRUE-------------------------------------------------
sumstat_alter_psych_bal <- df_alterl_balance |>
  psych::describe() |> 
  as_tibble(rownames="Question")  |> 
  dplyr::select(-skew, -kurtosis, -range, -vars) 

apa_table(
  sumstat_alter_psych_bal
  , caption = "Summary Statistics: Experience of Ageing - balanced (psych)"
  , note = "This table contains all variables of `alterl*` and only observations where all questions had been answered."
  , escape = TRUE
)


## ----echo = TRUE, eval = FALSE----------------------------------------------------------
## 
## ```{r tabrstatix, echo=TRUE}
## apa_table(
##   sumstat_alter
##   , caption = "Summary Statistics: Experience of Ageing."
##   , note = "This table contains all variables of `alterl*`."
##   , escape = TRUE
##   )
## ```
## 


## ----echo=T, eval=F---------------------------------------------------------------------
## # Remove the common prefix from all variables
## df <- df |>
##   mutate_all(~ set_label(., gsub("^Alternserleben: ", "", get_label(.))))

