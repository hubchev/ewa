## ----echo=TRUE, message=FALSE-----------------------------------------------
if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, janitor, psych, tinytable, ggstats, car, ggstatsplot, 
               modelsummary, knitr, kableExtra, ggpubr, rstatix, rempsyc)
rm(list = ls())

load("~/Dropbox/hsf/github/ewa/ss_24/read_in_71/data_71.RData")

# Zuerst wandle ich die Daten etwas um, so dass ich keine Faktorvariable mehr habe:
df_test <- df_cleaned |> 
  mutate_at(vars(starts_with("item_")), as.numeric)

# ##########################
# Ein-Stichproben t-Test
# ##########################

t.test(df_test$item_1, mu = 1)
t.test(df_test$item_1, mu = 3)

# Standardmäßig führt t.test einen zweiseitigen Test durch:
t.test(df_test$item_1, mu = 3, alternative = "two.sided")

# Sie können einen Test für einen Teil der Daten durchführen, indem Sie ein Argument der t.test-Funktion verwenden:
t.test(df_test$item_1, 
       mu = 3, 
       alternative = "two.sided",
       subset(df_test, group == 0))

t.test(df_test$item_1, 
       mu = 3,
       alternative = "greater")

t.test(df_test$item_1, 
       mu = 3,
       alternative = "less")

# ##########################
# Zweiseitiger t-Test in R
# ##########################

# ----------------------------
# Für Daten im Breitformat:
# ----------------------------

# Welch-Test
t.test(df_test$item_1, df_test$item_2, data = df_test)

# Student's t-Test
t.test(df_test$item_1, df_test$item_2, data = df_test, var.equal = TRUE)

# Welch-Test
t.test(df_test$item_1, df_test$item_2, data = df_test, 
       paired = TRUE, 
       var.equal = TRUE)

## Die Antworten kommen von der selben Person, daher paired = TRUE

# ----------------------------
# Für Daten im Langformat:
# ----------------------------

# Test, ob das item_1 in beiden Gruppen gleich ist

# Erstelle ein Boxplot von item_1 über Gruppen mit ggplot2
ggplot(df_test, aes(x = factor(group), y = item_1)) +
  geom_boxplot() +
  labs(x = "Group", y = "Item 1") +
  ggtitle("Boxplot of Item 1 Across Groups")

ggbetweenstats(
  data = df_test,
  x = group,
  y = item_1
)

## Wenn Normalitäts- und Varianzannahmen erfüllt sind:
t.test(item_1 ~ group, data = df_test, var.equal = FALSE)

# OK, aber wie testet man nun,
# ob die Antworten aller items in den jeweiligen Gruppen gleich sind? 

# Zuerst empfiehlt sich eine Umwandlung in das Langformat:
df_test_long <- df_test |> 
  pivot_longer(cols = starts_with("item_"), # Zu pivotierende Spalten
               names_to = "item",           # Neue Spalte für die Namen der Items
               values_to = "value")  |>     # Neue Spalte für die Werte der Items
  select(id, group, complete, item, value)

# Erstelle ein Boxplot von value über Gruppen mit ggplot2
ggplot(df_test_long, aes(x = factor(group), y = value)) +
  geom_boxplot() +
  labs(x = "Group", y = "Item 1") +
  ggtitle("Boxplot of all items Across Groups")

ggbetweenstats(
  data = df_test_long,
  x = group,
  y = value
)


# ##########################
# Prüfung der Normalitätsannahme
# ##########################

## Mit Shapiro
df_test |> 
  group_by(group) |> 
  shapiro_test(item_1)

## Mit QQ-Plots
ggqqplot(df_test, x = "item_1", facet.by = "group")

# Wenn der p-Wert klein ist, müssen wir einen Test verwenden, der keine Normalverteilung annimmt 
# --> Mann-Whitney-U-Test (auch Wilcoxon-Rangsummentest genannt) vergleicht die Mediane 
# von zwei unabhängigen Gruppen und ist robust gegenüber 
# Nicht-Normalverteilung und unterschiedlichen Varianzen.

# ##########################
# Überprüfung der Varianzhomogenität
# ##########################

## Mit Levene
leveneTest(item_1 ~ as.character(group), data = df_test)

## Mit bartlett.test
bartlett.test(item_1 ~ group, data = df_test)

## Mit Fligner-Kelleen
fligner.test(item_1 ~ group, data = df_test)

# Wenn der p-Wert klein ist, ist die Varianz zwischen den Gruppen unterschiedlich und 
# wir müssen einen Test verwenden, der keine Normalverteilung annimmt 
# --> Welch-t-Test (oder Mann-Whitney)

# ##########################
# Test 
# ##########################

## Wenn Normalitäts- und Varianzannahmen erfüllt sind:
t.test(item_1 ~ group, data = df_test, var.equal = TRUE)

## Wenn Normalität erfüllt ist und Varianz nicht:
t.test(item_1 ~ group, data = df_test, var.equal = FALSE)

## Wenn Normalität nicht erfüllt ist:
wilcox.test(item_1 ~ group, data = df_test)
