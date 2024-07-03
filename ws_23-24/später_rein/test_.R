
stop()
load("data/gesis.RData")


test <- aov(alterl_m1 ~ ALT_agegroup*final, data=dfdta, weights=pFAKT)
summary.aov(test)

test <- aov(alterl_m1 ~ ALT_agegroup*final, data=dfdta)
summary.aov(test)
