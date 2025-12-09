
install.packages("ggstatsplot")
library(dplyr)
library(ggplot2)
library(ggpubr)
library(readxl)

library(ggpmisc)
library(ggbeeswarm)
library(broom)
library(ggstatsplot)
theme_set(theme_bw(16)) #

# https://stackoverflow.com/questions/64040206/qplot-ggplot-r-data-visualization-group-by
# https://www.r4photobiology.info/pages/linear-models-intro.html
# https://rpubs.com/Muo/947076

# Load the dataset
msleep <- read.csv("data/msleep.csv")  # msleep dataset contains information about sleep in different mammals

# Basic scatterplot with regression line colored by diet type (vore)
ggplot(msleep, aes(x = log(bodywt),  # log-transform body weight to reduce skew
                   y = sleep_total,  # total sleep time
                   color = vore)) +  # color points by diet type
  geom_point() +  # add points
  geom_smooth(method = "lm", se = FALSE)  # add linear regression line, no confidence interval


# add color manually ------------------------------------------------------
ggplot(msleep, aes(x = log(bodywt),  # log-transform body weight to reduce skew
                   y = sleep_total,  # total sleep time
                   color = vore
                   )) +  # color points by diet type
  geom_point() +  # add points
  geom_smooth(method = "lm", se = FALSE) +  # add linear regression line, no confidence interval
  scale_color_manual(
    values = c(
      "herbi" = "green",
      "carni" = "red",
      "omni"  = "blue",
      "insecti" = "orange",
      "unknown" = "gray"
    )
  )


# Define a formula for the regression equation
my.formula = y ~ x  # basic linear formula for stat_poly_eq
# my.formula = y ~ poly(x, 2)  # second-degree polynomial
# my.formula = y ~ poly(x, 3)  # cubic regression

# More advanced plot with regression equation and faceting
ggplot(msleep, aes(x = log(bodywt), y = sleep_total)) +
  geom_point() +  # add points
  geom_smooth(method = "lm", se = FALSE) +  # add linear regression line
  stat_poly_eq(formula = my.formula,       # add regression equation, R^2, and p-value
               parse = TRUE,              # parse the equation to show as expression
               label.y = "top",           # place equation at top of plot
               label.x = "left",          # place equation at left of plot
               use_label(c("eq", "R2", "P"))) +  # specify which components to show
  facet_wrap(. ~ vore,                   # create separate panels by diet type
             ncol = 2,                   # two columns of panels
             scales = "free") +           # allow scales to vary across panels
  theme_bw(16)                           # use black-and-white theme with base font size 16



# Strips ------------------------------------------------------------------
# Define a formula for the regression equation
my.formula = y ~ x  # basic linear formula for stat_poly_eq
# my.formula = y ~ poly(x, 2)  # second-degree polynomial
# my.formula = y ~ poly(x, 3)  # cubic regression

# More advanced plot with regression equation and faceting
ggplot(msleep, aes(x = log(bodywt), y = sleep_total)) +
  geom_point() +  # add points
  geom_smooth(method = "lm", se = FALSE) +  # add linear regression line
  stat_poly_eq(formula = my.formula,       # add regression equation, R^2, and p-value
               parse = TRUE,              # parse the equation to show as expression
               label.y = "top",           # place equation at top of plot
               label.x = "left",          # place equation at left of plot
               use_label(c("eq", "R2", "P"))) +  # specify which components to show
  facet_wrap(. ~ vore,                   # create separate panels by diet type
             ncol = 2,                   # two columns of panels
             scales = "free") +           # allow scales to vary across panels
  theme_bw(16) +                          # use black-and-white theme with base font size 16
  theme(
    strip.background = element_rect(fill = "steelblue", color = "black", size = 1),
    strip.text = element_text(color = "white", face = "bold", size = 14)
  )



# bacterias ----------------------------------------------------------------
df <- read_excel("data/41467_2021_27857_MOESM6_ESM.xlsx", 
                 sheet="Fig. 1b and Suppl. Fig. 1")

# Ensure columns are clean
df <- df %>%
  rename(rrn = `rrn copy number`,
         Location = dataset,
         Group = category)

# Check the unique values in the category column
unique(df$Group)
# [1] "Rare" "Intermediate" "Abund."   # notice the abbreviation

# If you want to rename it to match previous code
df$Group <- recode(df$Group, "Abund." = "Abundant")

# Order groups so they plot Abundant → Intermediate → Rare
df$Group <- factor(df$Group, 
                   levels = c("Abundant", 
                              "Intermediate", 
                              "Rare"))

# Compute mean + SE for plotting
summary_df <- df %>%
  group_by(Location, Group) %>%
  summarize(
    mean_rrn = mean(rrn, na.rm = TRUE),
    se_rrn   = sd(rrn, na.rm = TRUE) / sqrt(n())
  )

# Plot
p <- ggplot(summary_df, aes(x = Group, y = mean_rrn, fill = Group)) +
  geom_col(color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = mean_rrn - se_rrn, ymax = mean_rrn + se_rrn),
                width = 0.2) +
  facet_wrap(~ Location, scales = "free_y", ncol = 4) +
  scale_fill_manual(values = c("Abundant" = "#4DAF4A",   # green
                               "Intermediate" = "#FF7F00", # orange
                               "Rare" = "#6A51A3")) +      # purple
  labs(y = "rrn Copy Number", x = "") +
  theme_bw() +
  theme(strip.background = element_rect(fill = "grey90"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

p


