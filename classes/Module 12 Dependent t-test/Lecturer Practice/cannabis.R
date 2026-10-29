



install.packages("educate")

library(tidyverse)
library(lessR)
library(readxl)
library(BSDA)
library(educate)

cannabis <- read_excel("cannabis.xlsx") 
head(cannabis)

var.test(iq_change ~ cannabis_dep, data=cannabis)


# Two-sample t-test
t.test(iq_change ~ cannabis_dep, 
       data = cannabis,
       alternative = c("two.sided"),
       mu = 0, 
       paired = FALSE, 
       var.equal = TRUE,
       conf.level = 0.95)

t.test(
  x = cannabis$iq_change[cannabis$cannabis_dep == "Adult"],
  y = cannabis$iq_change[cannabis$cannabis_dep == "Teen"],
  mu = 0,
  paired = FALSE,
  alternative = "two.sided",
  var.equal = TRUE,
  conf.level = 0.95
)

# View t-distribution
plot_t_dist(my_t)

