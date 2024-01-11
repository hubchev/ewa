setwd("~/Dropbox/hsf/23-ws/ewa/ewa_all/")

# Clear environment
# rm(list = ls())

# (Install and) load pacman package 
if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, haven, psych, sjlabelled)

# load data
load("../data/gesis.RData")

dfmin <- dfsav |>
  select(alterl_m1,                        # subset data 
         alterl_m2, 
         geschlecht,
         ALT_agegroup, 
         pFAKT
        ) |> 
  rename(m1 = alterl_m1,                   # simplify the variable names
         m2 = alterl_m2, 
         sex = geschlecht, 
         ag = ALT_agegroup,
         w = pFAKT) |> 
  mutate(sex = ifelse(sex == 1 | sex ==2,  # make gender binary
                      sex, 
                      0))  |> 
  arrange(sex, ag) |>                      # sort dataset
  mutate(id = row_number())              # add row number (id)


head(dfmin)

# make a summary table
d_min <- dfmin |>
  psych::describe() |> 
  as.data.frame() |> 
  select(-skew, -kurtosis, -range, -vars) 


d_min

# Let us make a summary statistics for each sub-group of age and gender
# using the same function, i.e., psych::describe()
# Therefore, we need to transform our data 'from long to wide'
# see https://r4ds.hadley.nz/data-tidy#widening-data
dfwide <- dfmin |> 
  pivot_wider(
    names_from = c(sex, ag),
    values_from = c(m1, m2)
  )
head(dfwide)

d_wide <- dfwide |>
  psych::describe() |> 
  as.data.frame() |>
  select(-skew, -kurtosis, -range, -vars) 

d_wide

# Let us make some boxplots to see the differences in age perception across
# age groups and gender in the two variables m1 and m2

# Therefore, we need to transform our data 'from wide to ling'
# see https://r4ds.hadley.nz/data-tidy#sec-pivoting
dflong <- dfmin %>%
  pivot_longer(cols = c(m1, m2), names_to = "m") |> 
  group_by(ag, sex, m) |> 
  mutate(id_group = cur_group_id()) |> 
  ungroup() |> 
  group_by(ag, sex) |> 
  mutate(id_group_ag_sex = cur_group_id()) |> 
  as_tibble()

head(dflong)

ggplot(dflong) +
  geom_boxplot(aes(x=sjlabelled::as_factor(id_group), y=value) )

ggplot(dflong) +
  geom_boxplot(aes(x=sjlabelled::as_factor(id_group), y=value, color=sex, fill= sjlabelled::as_factor(ag))) 

dsq <- dflong |> 
  filter(m=="m1") |> 
  ggplot() +
  geom_boxplot(aes(x=id_group, y=value, color=sjlabelled::as_factor(sex), fill= sjlabelled::as_factor(ag)))
dsq


# =======================================
# OK now lets make the graph a bit nicer:
# =======================================
dflong$id_group <- factor(dflong$id_group,
                          labels=c('m-jung-m1','m-jung-m2','w-jung-m1','w-jung-m2',
                                   'm-mittel-m1','m-mittel-m2','w-mittel-m1','w-mittel-m2',
                                   'm-alt-m1','m-alt-m2','w-alt-m1','w-alt-m2'))

bp_a <- dflong |> 
  ggplot() +
  geom_boxplot(aes(x=id_group, y=value)) +
  xlab(NULL) + 
  ylab(NULL) +
  labs(color = "Gender",
       fill = "Age Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
bp_a


dflong$sex <- factor(dflong$sex,
                     labels=c('men', 'female'))
dflong$ag <- sjlabelled::as_factor(dflong$ag)  
dflong$m <- factor(dflong$m, 
                      levels=c("m1", "m2"), 
                      labels=c("Age perception: positive", "Age perception: negative"))  

bp_b <- dflong |> 
  # filter(m=="m1") |>
  ggplot() +
  geom_boxplot(aes(x=id_group_ag_sex, y=value, color=sex, fill= ag)) +
  xlab(NULL) + 
  ylab(NULL) +
  labs(color = "Gender",
       fill = "Age Group") + 
  theme_minimal() + 
  scale_colour_colorblind() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_x_discrete(labels = NULL, breaks = NULL) +
  facet_wrap( ~ m)
bp_b


# knitr::spin("desc_age_perception.R")


