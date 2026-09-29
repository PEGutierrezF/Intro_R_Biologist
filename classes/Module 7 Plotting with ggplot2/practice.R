



# ===============================================================
# Basic commands for data visualization and statistical analysis
# Class from Tuesday, November 27, 2025
#
# These are the examples we worked on in class.
# You can add additional notes or comments to remember steps,
# interpretations, or results.
# ===============================================================


# https://bookdown.org/jgscott/DSGI/plots.html

# Load necessary libraries
library(dplyr)
library(ggplot2)


# Load the dataset from the 'data' folder
tvshows <- read.csv("data/tvshows.csv")

# Display the first few rows of the dataset to inspect its structure
head(tvshows)

# 1. Create a basic ggplot canvas (empty plot)
ggplot(data = tvshows)

# Create a ggplot canvas specifying the x variable (GRP)
ggplot(data = tvshows,
       mapping = aes(x = GRP))

# Create a ggplot canvas specifying both x and y variables (GRP vs PE)
ggplot(data = tvshows,
       mapping = aes(x = GRP, y = PE))

# Scatterplot with a smooth fitted line (default method = loess)
ggplot(data = tvshows,
       mapping = aes(x = GRP, y = PE)) +
  geom_point() +       # add points
  geom_smooth()        # add smooth curve

# Equivalent plot (mapping can be written without 'mapping=' for brevity)
ggplot(data = tvshows,
       aes(x = GRP, y = PE)) +
  geom_point() +
  geom_smooth()

# Scatterplot with a linear model fit (method = "lm")
ggplot(data = tvshows,
       aes(x = GRP, y = PE)) +
  geom_point() +
  geom_smooth(method = lm)

# Fit a linear regression model: PE predicted by GRP
mod <- lm(PE ~ GRP, data = tvshows)

# Extract residuals from the model
resid <- resid(mod)

# Test if residuals follow a normal distribution (Shapiro–Wilk test)
shapiro.test(resid)

# Apply log transformation to both variables and fit a new model
mod <- lm(log(PE) ~ log(GRP), data = tvshows)

# Extract residuals from the log-transformed model
resid <- resid(mod)

# Test normality of the new residuals
shapiro.test(resid)

# Plot the log-transformed variables with a smooth fitted line
ggplot(data = tvshows,
       aes(x = log(GRP), y = log(PE))) +
  geom_point() +       # add log-transformed scatter points
  geom_smooth(method= lm)        # add fitted smooth line




# Univariate  -------------------------------------------------------------
ggplot(data = tvshows, 
       mapping = aes(x = GRP)) +
  geom_histogram(binwidth = 500) 

ggplot(data = tvshows, 
       mapping = aes(x = GRP)) +
  geom_histogram(binwidth = 250, 
                 fill = "skyblue", color = "white") 


# Bivriate ----------------------------------------------------------------
ggplot(data = tvshows, 
       mapping = aes(x = GRP, y=PE)) +
  geom_point(size = 5)


# Local mappings ----------------------------------------------------------
ggplot(data = tvshows) +
  geom_point(mapping = aes(x = GRP, y=PE)) 

ggplot(data = tvshows) +
  geom_point(mapping = aes(x = GRP, y=PE,
                           color=Genre)) 

ggplot(tvshows) + 
  geom_point(aes(x=GRP, y=PE, shape=Genre))

ggplot(tvshows) + 
  geom_point(aes(x=GRP, y=PE, 
                 shape=Genre, color=Genre))


# Beyond two variables: faceting ------------------------------------------
ggplot(tvshows) + 
  geom_point(aes(x=GRP, y=PE)) + 
  facet_wrap(~Genre)




