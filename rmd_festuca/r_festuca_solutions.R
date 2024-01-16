## ----echo=TRUE---------------------------------------------------------------------------------------------------------------------------------
setwd("~/Dropbox/hsf/courses/ewa/ewa_all")

rm(list = ls())

if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, rstatix, ggpubr, agricolae)


## ----echo=TRUE---------------------------------------------------------------------------------------------------------------------------------
data_present <- data.frame(
  Condition = rep(c("Calluna Present"), each = 5),
  `pH 3.5` = c(2.76, 2.39, 3.54, 3.71, 2.49),
  `pH 5.5` = c(3.21, 4.10, 3.04, 4.13, 5.21),
  check.names = FALSE
)
data_present

data_absent <- data.frame(
  Condition = rep(c("Calluna Absent"), each = 5),
  `pH 3.5` = c(4.10, 2.72, 2.28, 4.43, 3.31),
  `pH 5.5` = c(5.92, 7.31, 6.10, 5.25, 7.45),
  check.names = FALSE
)
data_absent


## ----echo=FALSE--------------------------------------------------------------------------------------------------------------------------------
?bind_rows
data <- bind_rows(data_present, data_absent)
data


## ----echo=FALSE--------------------------------------------------------------------------------------------------------------------------------
festuca <- data %>%
  pivot_longer(cols = starts_with("pH"), names_to = "pH", values_to = "Weight") |> 
  rename(Calluna = Condition) 


## ----echo=TRUE---------------------------------------------------------------------------------------------------------------------------------
festuca


## ----echo=FALSE, message=FALSE-----------------------------------------------------------------------------------------------------------------
summary_stats <- festuca |> 
  group_by(Calluna, pH) |> 
  summarize(
    mean = mean(Weight),
    var = var(Weight)
  ) |> 
  ungroup()


## ----echo=TRUE---------------------------------------------------------------------------------------------------------------------------------
summary_stats


## ----echo=FALSE--------------------------------------------------------------------------------------------------------------------------------
ggplot(data = festuca, aes(x = Calluna, y = Weight, colour = pH)) + 
  geom_boxplot()


## ----echo=FALSE--------------------------------------------------------------------------------------------------------------------------------
festuca_model <- aov(Weight ~ pH + Calluna + pH : Calluna, data = festuca)


## ----echo=FALSE--------------------------------------------------------------------------------------------------------------------------------
anova(festuca_model)


## ----echo=TRUE---------------------------------------------------------------------------------------------------------------------------------
plot(festuca_model, which = 2, add.smooth = FALSE)


## ----echo=TRUE---------------------------------------------------------------------------------------------------------------------------------
plot(festuca_model, which = 3, add.smooth = FALSE)


## ----echo=FALSE--------------------------------------------------------------------------------------------------------------------------------
interaction.plot(festuca$pH, festuca$Calluna, response = festuca$Weight)


## ----echo=TRUE, message=FALSE------------------------------------------------------------------------------------------------------------------
# step 1. calculate means for each treatment combination
festuca_means <- 
  festuca %>% 
  group_by(Calluna, pH) %>% # <- remember to group by *both* factors
  summarise(Means = mean(Weight))


## ----echo=TRUE---------------------------------------------------------------------------------------------------------------------------------
# step 2. plot these as an interaction plot
ggplot(festuca_means, 
       aes(x = Calluna, y = Means, colour = pH, group = pH)) +
  geom_point(size = 4) + geom_line()


## ----pressure, echo=FALSE, fig.cap="Understanding the model graphically", out.width = '100%'---------------------------------------------------
knitr::include_graphics("https://dzchilds.github.io/stats-for-bio/images/interaction-diagrams.png")


## ----echo=TRUE---------------------------------------------------------------------------------------------------------------------------------
TukeyHSD(festuca_model, which = 'pH:Calluna')


## ----echo=TRUE---------------------------------------------------------------------------------------------------------------------------------
HSD.test(festuca_model, trt = c("pH", "Calluna"), console = TRUE)


## ----echo=TRUE---------------------------------------------------------------------------------------------------------------------------------
# step 1. calculate means for each treatment combination
festuca_stats <- 
  festuca %>% 
  group_by(Calluna, pH) %>% # <- remember to group by the two factors
  summarise(Means = mean(Weight), SEs = sd(Weight)/sqrt(n()))


## ----echo=TRUE, message = FALSE----------------------------------------------------------------------------------------------------------------
# step 1. calculate means for each treatment combination
festuca_stats <- 
  festuca %>% 
  group_by(Calluna, pH) %>% # <- remember to group by the two factors
  summarise(Means = mean(Weight), SEs = sd(Weight)/sqrt(n()))


## ----echo=TRUE, message = FALSE----------------------------------------------------------------------------------------------------------------
# step 2. plot these as an interaction plot
ggplot(festuca_stats, 
       aes(x = Calluna, y = Means, colour = pH,
           ymin = Means - SEs, ymax = Means + SEs)) +
  # this adds the mean
  geom_point(size = 3) +
  # this adds the error bars
  geom_errorbar(width = 0.1) +
  # controlling the appearance
  scale_y_continuous(limits = c(2, 7)) + 
  xlab("Calluna") + ylab("Festuca yield (g dry weight)") + 
  # use a more professional theme
  theme_bw()


## ----echo=TRUE---------------------------------------------------------------------------------------------------------------------------------
# define a position adjustment 
pos <- position_dodge(0.15)
# make the plot
ggplot(festuca_stats, 
       aes(x = Calluna, y = Means, colour = pH,
           ymin = Means - SEs, ymax = Means + SEs)) +
  # this adds the mean (shift positions with 'position =')
  geom_point(size = 3, position = pos) +
  # this adds the error bars (shift positions with 'position =')
  geom_errorbar(width = 0.1, position = pos) +
  # controlling the appearance
  scale_y_continuous(limits = c(2, 7)) + 
  xlab("Calluna") + ylab("Festuca yield (g dry weight)") + 
  # use a more professional theme
  theme_bw()


## ----echo=TRUE---------------------------------------------------------------------------------------------------------------------------------
ggplot(festuca_stats, 
       aes(x = Calluna, y = Means, fill = pH,
           ymin = Means - SEs, ymax = Means + SEs)) +
  # this adds the mean
  geom_col(position = position_dodge()) +
  # this adds the error bars
  geom_errorbar(position = position_dodge(0.9), width=.2) +
  # controlling the appearance
  xlab("Calluna") + ylab("Festuca yield (g dry weight)")

