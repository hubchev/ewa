## ----analysis-preferences-------------------------------------------------------------------------------
# Seed for random number generation
set.seed(42)
knitr::opts_chunk$set(cache.extra = knitr::rand_seed)
# rm(list = ls())


## ----echo=TRUE, eval=TRUE-------------------------------------------------------------------------------
if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, janitor, psych, magick, 
               car, knitr, papaja, kableExtra, stargazer)
rm(list = ls())

ModKogDat <- read.csv("../data/ModKogDat.csv", header=TRUE, sep=",") 
# Read in data
df <- ModKogDat |> 
  mutate(
    modus = as.factor(modus),
    kognition = as.factor(kognition)
    ) |> 
  group_by(modus, kognition) |> 
  mutate(
    id_num = cur_group_id(),
    m_str = substr(modus, 1, 2),
    k_str = substr(kognition, 1, 2),
    id = paste(m_str, k_str, sep = "_")
  ) |> 
  select(-m_str, -k_str) |> 
  tibble() |> 
  ungroup() 


## ----tabinspect, echo=FALSE-----------------------------------------------------------------------------

apa_table(df,  caption = "Full Dataset")



## ----tabsumstat, echo=FALSE-----------------------------------------------------------------------------
tabsumstat <- df |>
  psych::describe()   |> 
  as_tibble(rownames="Variables")  |> 
  select(-skew, -kurtosis, -range, -vars, -trimmed, -mad) 

apa_table(
  tabsumstat
  , caption = "Summary Statistics"
  , note = "This table contains all variables."
  , escape = TRUE
)


## ----tabsummary, echo=FALSE-----------------------------------------------------------------------------
tab_summary <- df |> 
  group_by(id) |> 
  summarize(
    count = n(),
    mean = mean(dauer),
    sd = sd(dauer),
    "COV (sd/mean)" = sd(dauer) / mean(dauer),
    min = min(dauer),
    q25 = quantile(dauer, 0.25),
    median = median(dauer),
    q75 = quantile(dauer, 0.75),
    max = max(dauer)
  ) |> 
  ungroup()


apa_table(
  tab_summary
  , caption = "Summary Statistics for the Variable `dauer`"
  , note = "This table contains summary statistics for each combination of `modus` and `kognition`"
  , escape = TRUE
)


## ----aboxplot,  fig.cap="Boxplots of all combinations of `modus` and `kognition`", echo=FALSE-----------
ggplot(df, aes(x=kognition, y=dauer)) +
  geom_boxplot() + 
  facet_wrap(~modus) 


## ----iplotdauerkog,  fig.cap="Interaction Plot: `dauer` and `modus`", echo=FALSE------------------------
interaction.plot(df$kognition, df$modus, response = df$dauer)


## ----iplotdauermod,  fig.cap="Interaction Plot: `dauer` and `kognition`", echo=FALSE--------------------
interaction.plot( df$modus, df$kognition, response = df$dauer)


## ----tabanova, echo=FALSE-------------------------------------------------------------------------------
anova2_model <- aov(dauer ~ modus + kognition + modus:kognition, data = df)

# summary(anova2_model)

apa_anova <- apa_print(anova2_model)

apa_table(
  apa_anova$table
  , caption = "A beautiful ANOVA table."
  , note = "Bli bla blubb."
)


## ----echo=TRUE------------------------------------------------------------------------------------------
contrasts(df$kognition) <- cbind(c(2, -1, -1), c(0, 1,-1)) 


## ----tabanovaext, echo=FALSE----------------------------------------------------------------------------
anova2_model2 <- summary.aov(anova2_model, 
            split=list(kognition=
                         list("altersadäquat vs beeinträchtigt"=1,
                              "LKB vs beginnende Demenz"=2)
                       )
            )
apa_anova2 <- apa_print(anova2_model2)

apa_table(
  apa_anova2
  , caption = "A beautiful ANOVA table."
  , note = "Bli bla blubb."
)



## ----echo=TRUE------------------------------------------------------------------------------------------
df3 <- read.csv("../data/ModKogDat3F.csv", header=TRUE, sep=",") |> 
  mutate(
    modus = as.factor(modus),
    kognition = as.factor(kognition),
    interviewer = as.factor(interviewer)
  ) |> 
    group_by(modus, kognition, interviewer) |> 
  mutate(
    id_num = cur_group_id(),
    m_str = substr(modus, 1, 2),
    k_str = substr(kognition, 1, 2),
    i_str = substr(interviewer, 1, 2),
    id = paste(m_str, k_str, i_str, sep = "_")
  ) |> 
  select(-m_str, -k_str, -i_str) |> 
  tibble() 


## ----tabinspect3, echo=FALSE----------------------------------------------------------------------------

apa_table(df3,  caption = "Full Dataset: `ModKogDat3F.csv`")



## ----tabsumstat3, echo=FALSE----------------------------------------------------------------------------
tabsumstat3 <- df |>
  psych::describe()   |> 
  as_tibble(rownames="Variables")  |> 
  select(-skew, -kurtosis, -range, -vars, -trimmed, -mad) 

apa_table(
  tabsumstat3
  , caption = "Summary Statistics: `ModKogDat3F.csv`"
  , note = "This table contains all variables."
  , escape = TRUE
)


## ----tabsummary3, echo=FALSE----------------------------------------------------------------------------
tab_summary <- df3 |> 
  group_by(id) |> 
  summarize(
    count = n(),
    mean = mean(dauer),
    sd = sd(dauer),
    "COV (sd/mean)" = sd(dauer) / mean(dauer),
    min = min(dauer),
    q25 = quantile(dauer, 0.25),
    median = median(dauer),
    q75 = quantile(dauer, 0.75),
    max = max(dauer)
  ) |> 
  ungroup()


apa_table(
  tab_summary
  , caption = "Summary Statistics for the Variable `dauer`:  `ModKogDat3F.csv`"
  , note = "This table contains summary statistics for each combination of `modus`,  `kognition`, and `interviewer`"
  , escape = TRUE
)


## ----df3boxplot,  fig.cap="Boxplots of all combinations of `modus`, `kognition`, and `interviewer`", echo=FALSE----
ggplot(df3, aes(x=kognition, y=dauer)) +
  geom_boxplot()+
  facet_wrap(~modus*interviewer) +
  stat_summary(fun=mean, colour="blue",
               geom="point", shape=18, size=3)


## ----tabanova3, echo=FALSE------------------------------------------------------------------------------
anova3_model <- aov(dauer ~ modus*kognition*interviewer, data = df3)
# summary.aov(anova3.Model)

anova3 <- summary.aov(anova3_model)
apa_anova3 <- apa_print(anova3)

apa_table(
  apa_anova3
  , caption = "A beautiful ANOVA table with `interviewer`."
  , note = "Bli bla blubb."
)


## ----tabsumstat3correct, echo=TRUE----------------------------------------------------------------------
tabsumstat3 <- df3 |>
  psych::describe()   |> 
  as_tibble(rownames="Variables")  |> 
  select(-skew, -kurtosis, -range, -vars, -trimmed, -mad) 

apa_table(
  tabsumstat3
  , caption = "Summary Statistics: `ModKogDat3F.csv`"
  , note = "This table contains all variables."
  , escape = TRUE
)


## ----tabinspect3split, echo=TRUE------------------------------------------------------------------------
df3p <- df3 |> 
  filter(interviewer == "profi")
df3e <- df3 |> 
  filter(interviewer == "ehrenamt")


## ----tabinspect3p, echo=TRUE----------------------------------------------------------------------------
apa_table(df3p,  caption = "Interviews by Professionals")


## ----tabinspect3e, echo=TRUE----------------------------------------------------------------------------
apa_table(df3e,  caption = "Interviews by Volunteers (Ehrenamt)")

