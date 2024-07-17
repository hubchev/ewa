## ----echo=TRUE, message=FALSE-----------------------------------------------
if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, janitor, psych, tinytable, ggstats,
               modelsummary, knitr, kableExtra, ggpubr, rstatix, rempsyc)
rm(list = ls())

load("~/Dropbox/hsf/github/ewa/ss_24/read_in_71/data_71.RData")

# ##########################
# One Sample t-test
# ##########################

t.test(df$item_1, mu = 1)
t.test(df$item_1, mu = 3)

# by default t.test runs a two sided test:
t.test(df$item_1, mu = 3, alternative = "two.sided")

# you can test 
t.test(df$item_1, 
       mu = 3, 
       alternative = "two.sided",
       subset(df, group == 0))

t.test(df$item_1, 
       mu = 3,
       alternative = "greater")

t.test(df$item_1, 
       mu = 3,
       alternative = "less")

# ##########################
# Two sided t-test in R
# ##########################

# Welch test
t.test(df$item_1, df$item_2, data = df)

# Student's t-test
t.test(df$item_1, df$item_2, data = df, var.equal = TRUE)

# Welch test
t.test(df$item_1, df$item_2, data = df, 
       paired = TRUE, 
       var.equal = TRUE)

## Die Antworten kommen von der selben Person, daher paired = TRUE


# ##########################
# check normality assumption
# ##########################

## With shapiro
df |> 
  group_by(group) |> 
  shapiro_test(item_1)

## with qq plots
ggqqplot(df, x = "item_1", facet.by = "group")

# if the p-value is small, we must use a test that does not assume normality 
# --> Mann-Whitney-U-Test (aka Wilcoxon-Rangsummentest) vergleicht die Mediane 
# von zwei unabhängigen Gruppen und ist robust gegenüber 
# Nicht-Normalverteilung und unterschiedlichen Varianzen.


# ##########################
# check homogeneity of variances
# ##########################

## with levene
leveneTest(item_1 ~ as.character(group), data = df)

## with bartlett.test
bartlett.test(item_1 ~ group, data = df)

## with Fligner-Kelleen
fligner.test(item_1 ~ group, data = df)

# if the p-value is small the variance is different between the groups and 
# we must use a test that does not assume normality 
# --> Welch t-test (or Mann-Whitney)

# ##########################
# Test 
# ##########################

## if normality and variance assumptions are given:
t.test(item_1 ~ group, data = df, var.equal = FALSE)

## if normality is given and normality not:
t.test(item_1 ~ group, data = df, var.equal = FALSE)

## if normality is not given:
wilcox.test(item_1 ~ group, data = df)



df |> 
  select(id, group, item_1, item_2) |> 
  head()

