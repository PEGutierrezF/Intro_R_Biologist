# ===============================================================
# Basic commands for data visualization and statistical analysis
# Class: Tuesday, November 30, 2025
# 
# These are the examples we worked on in class. 
# You can add additional notes or comments to remind yourselves
# of what each function does and how to interpret the results.
# ===============================================================


# ---------------------------------------------------------------
# Load necessary libraries
# ---------------------------------------------------------------
library(dplyr)     # data manipulation (not used much here, but good practice)
library(ggplot2)   # data visualization
library(car)       # used for Levene’s test (homogeneity of variances)


# ---------------------------------------------------------------
# Load dataset
# ---------------------------------------------------------------
# The dataset "tyre.csv" should be inside the "data" folder
tyre <- read.csv("data/tyre.csv")

# Take a quick look at the structure of the data
head(tyre)
str(tyre)


# ---------------------------------------------------------------
# BASIC VISUALIZATION
# ---------------------------------------------------------------

# Boxplot using color outline by brand
ggplot(tyre, aes(Brands, Mileage)) +
  geom_boxplot(aes(colour = Brands))   # color only affects the outline

# Boxplot using fill color by brand
ggplot(tyre, aes(Brands, Mileage)) +
  geom_boxplot(aes(fill = Brands))     # fill gives a more distinct visual grouping

# Add title and axis labels
ggplot(tyre, aes(Brands, Mileage)) +
  geom_boxplot(aes(fill = Brands)) +
  labs(title = "Mileage by Brand",
       x = "Brand",
       y = "Mileage")                  # good practice to label plots clearly


# ---------------------------------------------------------------
# CUSTOMIZE THEME AND TEXT SIZE
# ---------------------------------------------------------------

# Customize axis titles, text, and main title
ggplot(tyre, aes(Brands, Mileage)) +
  geom_boxplot(aes(fill = Brands)) +
  labs(title = "Mileage by Brand",
       x = "Brand",
       y = "Mileage") +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5), # centers and enlarges title
    axis.title.x = element_text(size = 14, face = "bold"),            # x-axis title
    axis.title.y = element_text(size = 14, face = "bold"),            # y-axis title
    axis.text.x = element_text(size = 12),                            # x-axis tick labels
    axis.text.y = element_text(size = 12)                             # y-axis tick labels
  )

# Hide legend completely (if it’s redundant)
ggplot(tyre, aes(Brands, Mileage)) +
  geom_boxplot(aes(fill = Brands)) +
  theme(legend.position = 'none')

# Show and customize the legend (title and text sizes)
ggplot(tyre, aes(Brands, Mileage)) +
  geom_boxplot(aes(fill = Brands)) +
  labs(fill = "Car Brands") +   # custom legend title
  theme(
    legend.title = element_text(size = 14, face = "bold"),  # legend title font
    legend.text  = element_text(size = 12)                  # legend labels font
  )


# ---------------------------------------------------------------
# STATISTICAL ANALYSIS SECTION
# ---------------------------------------------------------------

# One-way ANOVA: tests if mean Mileage differs among Brands
anova_mod <- aov(Mileage ~ Brands, data = tyre)

# Display the ANOVA summary table
summary(anova_mod)     # Look for the p-value under "Pr(>F)"

# Check ANOVA assumptions

# (a) Extract residuals to test normality
resid_anova <- residuals(anova_mod)

# Shapiro–Wilk test for normality of residuals
shapiro.test(resid_anova)
# If p > 0.05 → residuals are approximately normal

# (b) Test for homogeneity of variances (Levene’s test)
leveneTest(Mileage ~ Brands, data = tyre)
# If p > 0.05 → equal variances assumption holds

# If ANOVA is significant, run post-hoc test (Tukey HSD)
TukeyHSD(anova_mod)
# Shows which specific brand pairs differ significantly


# ---------------------------------------------------------------
# FINAL POLISHED PLOT
# ---------------------------------------------------------------

# Combine visualization with customized theme
p <- ggplot(tyre, aes(Brands, Mileage)) +
  geom_boxplot(aes(fill = Brands)) +
  labs(
    title = "Mileage by Brand",
    x = "Brand",
    y = "Mileage",
    fill = "Car Brands"
  ) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    legend.position = "bottom"
  ) +
  # theme_bw()
  # theme_minimal()
  # theme_classic()
  # theme_light()
p

ggsave(
  filename = "Mileage_by_Brand.png",
  plot = p,
  width = 8, height = 6, dpi = 300
)

# ---------------------------------------------------------------
# NOTE TO STUDENTS:
# Try modifying the plot appearance (font size, legend position, 
# colors, etc.) and re-run the ANOVA with your own dataset. 
# Experimentation is the best way to learn ggplot2 and stats in R!
# ---------------------------------------------------------------


# https://datascienceplus.com/one-way-anova-in-r/