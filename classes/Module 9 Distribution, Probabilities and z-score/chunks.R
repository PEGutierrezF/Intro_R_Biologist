## ----echo=FALSE, message=FALSE, warning=FALSE,fig.width=2, fig.height=2, out.width="50%"---------
# Load the necessary library
library(ggplot2)
library(dplyr)
# Parameters for the normal distribution
mean <- 0  # Mean 
sd <- 1    # Standard deviation 

# Generate a sequence of x values
x <- seq(-4, 4, length = 100)

# Calculate the density of the normal distribution for each x value
y <- dnorm(x, mean, sd)

# Create a data frame for ggplot
data <- data.frame(x = x, y = y)

# Plot the normal distribution
ggplot(data, aes(x = x, y = y)) +
  geom_line(color = "blue", size = 1) +
  geom_vline(xintercept = mean, linetype = "dashed", color = "red", size = 1) +
  labs(title = "", subtitle = expression(paste("")),x = "", y = "") +
  theme(axis.text.y = element_blank(), # Remove y-axis numbers
      #  axis.ticks.y = element_blank(), # Remove y-axis ticks
        axis.title.y = element_blank()) +# Remove y-axis title
  theme_minimal()



## ----echo=FALSE, message=FALSE, warning=FALSE,fig.width=2, fig.height=2, fig.path="figures/"-----
# Load necessary libraries
library(ggplot2)
library(cowplot)

# Define standard deviation and mean
mu <- 2
sd <- 1

# Create a data frame for the first plot (normal distribution centered at mu)
x_vals <- seq(mu - 4 * sd, mu + 4 * sd, length.out = 100)
dist1 <- dnorm(x_vals, mean = mu, sd = sd)
data1 <- data.frame(x = x_vals, y = dist1)

# Create a data frame for the second plot (normal distribution centered at 0)
x_vals2 <- seq(-4 * sd, 4 * sd, length.out = 100)
dist2 <- dnorm(x_vals2, mean = 0, sd = sd)
data2 <- data.frame(x = x_vals2, y = dist2)

# First plot: Normal distribution with mean mu
plot1 <- ggplot(data1, aes(x = x, y = y)) +
  geom_line(color = "blue", size = 1) +             # Plot the line
  geom_ribbon(data = subset(data1, x >= mu & x <= mu + sd), aes(ymin = 0, ymax = y), fill = "lightblue", alpha = 0.3) + # Fill light blue from mu to 1 SD
  labs(x = "", y = "") +
  scale_x_continuous(breaks = c(mu, mu + sd), labels = c(expression(mu), "x")) + # Label x-axis with mu and x at mu + 1 SD
  theme_minimal() +
  theme(axis.title.x = element_text(size = 14),axis.title.y = element_text(size = 14)
  )

# Second plot: Normal distribution with mean 0
plot2 <- ggplot(data2, aes(x = x, y = y)) +
  geom_line(color = "red", size = 1) +              # Plot the line
  geom_ribbon(data = subset(data2, x >= 0 & x <= 0 + sd), aes(ymin = 0, ymax = y), fill = "lightblue", alpha = 0.3) + # Fill light blue from 0 to 1 SD
  labs(x = "", y = "") +
  scale_x_continuous(breaks = c(0, 0 + sd), labels = c("0", "z")) + # Label x-axis with 0 and z at 1 SD
  theme_minimal() +
  theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14)
  )

# Combine the two plots side by side
combined_plot <- plot_grid(plot1, plot2, labels = c("A", "B"), label_size = 8)

# Display the combined plot
print(combined_plot)



## ----echo=FALSE, results='asis'------------------------------------------------------------------
cat("$$t = \\frac{\\bar{X_1} - \\bar{X_2}}{\\sqrt{\\frac{s_1^2}{n_1} + \\frac{s_2^2}{n_2}}}$$")



## ----echo=FALSE, results='asis'------------------------------------------------------------------
cat("$$t = \\frac{\\bar{D}}{\\frac{s_D}{\\sqrt{n}}}$$")


## ----echo=FALSE, results='asis'------------------------------------------------------------------
cat("$$z = \\frac{\\bar{X} - \\mu}{\\frac{\\sigma}{\\sqrt{n}}}$$")


## ----echo=FALSE, results='asis'------------------------------------------------------------------
cat("$$F = \\frac{MST}{MSE}$$")


## ----echo=FALSE, results='asis'------------------------------------------------------------------
cat("$$r = \\frac{\\sum (x_i - \\bar{x})(y_i - \\bar{y})}{\\sqrt{\\sum (x_i - \\bar{x})^2 \\sum (y_i - \\bar{y})^2}}$$")


## ----echo=FALSE, message=FALSE, warning=FALSE, fig.width=3.5, fig.height=2-----------------------
library(ggplot2)

# Define mean, standard deviation, and critical value for p < 0.05 (two-tailed)
mean <- 0  # Mean under the null hypothesis
sd <- 1    # Standard deviation for the standard normal distribution
test_stat <- 1.5  # Example of an observed test statistic

# Critical values for p < 0.05 in a two-tailed test (1.96 standard deviations)
critical_value <- qnorm(0.975, mean, sd)

# Create a sequence of x values for the plot
x_vals <- seq(mean - 4 * sd, mean + 4 * sd, length.out = 1000)

# Calculate the density (y-values) for the normal distribution
y_vals <- dnorm(x_vals, mean = mean, sd = sd)

# Create a data frame for plotting
data <- data.frame(x = x_vals, y = y_vals)

# Plot the normal distribution
ggplot(data, aes(x = x, y = y)) +
  geom_line(color = "blue", size = 1) +  # Plot the normal distribution curve
  geom_ribbon(data = subset(data, x <= -critical_value | x >= critical_value), 
              aes(ymin = 0, ymax = y), fill = "lightblue", alpha = 0.5) +  # Shade area beyond critical values
  geom_vline(xintercept = c(mean, mean + sd, mean - sd, mean + 2 * sd, mean - 2 * sd), 
             linetype = "dashed", color = "red", size = 0.3) +
  geom_vline(xintercept = c(-critical_value, critical_value), 
             color = "green", linetype = "solid", size = 0.3) +  # Lines for critical values
  scale_x_continuous(breaks = c(mean, mean - sd, mean + sd, mean - 2 * sd, mean + 2 * sd),
                     labels = c(expression(mu), expression(mu - sigma), expression(mu + sigma),
                                expression(mu - 2 * sigma), expression(mu + 2 * sigma))) +  # Label x-axis with mean and SDs
  labs(title = "", 
       x = "Test Statistic", 
       y = "Density") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title = element_text(hjust = 0.5))


## ----echo=FALSE, message=FALSE, warning=FALSE, fig.width=3, fig.height=2-------------------------
# Load necessary libraries
library(ggplot2)

# Define mean, standard deviation, and observed test statistic
mean <- 0  # Mean of the standard normal distribution (under null hypothesis)
sd <- 1    # Standard deviation of the standard normal distribution
test_stat <- 1.96  # Example test statistic for a 95% confidence level (for significance level of 0.05)

# Create a sequence of x values for the plot
x_vals <- seq(mean - 4 * sd, mean + 4 * sd, length.out = 1000)

# Calculate the density (y-values) for the normal distribution
y_vals <- dnorm(x_vals, mean = mean, sd = sd)

# Create a data frame for plotting
data <- data.frame(x = x_vals, y = y_vals)

# Plot the normal distribution
ggplot(data, aes(x = x, y = y)) +
  geom_line(color = "blue", size = 1) +  # Plot the normal distribution curve
  geom_vline(xintercept = test_stat, color = "red", linetype = "dashed", size = 1) +  # Mark the test statistic (1.96)
  geom_ribbon(data = subset(data, x >= test_stat), aes(ymin = 0, ymax = y), fill = "lightblue", alpha = 0.5) +  # Shade area beyond test statistic (p-value)
  labs(title = "", x = "Test Statistic (z)", y = "Density") +
  annotate("text", x = 2.5, y = 0.25, label = "Test Statistic (z = 1.96)", color = "red", angle = 90, size=2) +
  annotate("text", x = test_stat + 1.2, y = 0.05, label = "p-value < 0.05", color = "black", size = 2) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 6),
    axis.title.y = element_text(size = 6),
    plot.title = element_text(hjust = 0.5)
  )



## ----echo=FALSE, fig.cap="", out.width = '80%', fig.align='center'-------------------------------
knitr::include_graphics("figs/plot1.png")


## ----echo=FALSE, fig.cap="", out.width = '80%'---------------------------------------------------
knitr::include_graphics("figs/fisher.png")


## ----echo=FALSE, fig.cap="", out.width = '40%', fig.align='center'-------------------------------
knitr::include_graphics("figs/criticalregion.png")


## ----echo=FALSE, fig.cap="", out.width = '100%', fig.align='center'------------------------------
knitr::include_graphics("figs/plot2.png")


## ----echo=FALSE, fig.cap="", out.width = '70%', fig.align='center'-------------------------------
knitr::include_graphics("figs/significance.png")


## ----echo=FALSE, fig.cap="", out.width = '80%', fig.align='center'-------------------------------
knitr::include_graphics("figs/pregnant.jpg")


## ----echo=FALSE, message=FALSE, warning=FALSE, fig.width=2.2, fig.height=3.5---------------------
# Create a data frame for the standard normal distribution
df <- data.frame(x = seq(-4, 4, length.out = 1000))
df$y <- dnorm(df$x)

# Plot 1: P(X < z)
z1 <- 1  # Example Z-score
p1 <- ggplot(df, aes(x = x, y = y)) +
  geom_line() +
  geom_area(data = subset(df, x < z1), aes(x = x, y = y), fill = "blue", alpha = 0.3) +
  geom_vline(xintercept = z1, linetype = "dashed", color = "red") +
  labs(title = "P(X < z)", x = "Z-score", y = "") +
  theme_minimal()+
  theme(plot.title = element_text(size = 5),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 8),
        axis.text.y = element_text(size = 4),
        axis.text.x = element_text(size = 4))  # Adjust title size here

# Plot 2: P(X > z)
z2 <- 1  # Example Z-score
p2 <- ggplot(df, aes(x = x, y = y)) +
  geom_line() +
  geom_area(data = subset(df, x > z2), aes(x = x, y = y), fill = "blue", alpha = 0.3) +
  geom_vline(xintercept = z2, linetype = "dashed", color = "red") +
  labs(title = "P(X > z)", x = "Z-score", y = "") +
  theme_minimal()+
  theme(plot.title = element_text(size = 5),
        axis.title.y = element_text(size = 8),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 4),
        axis.text.x = element_text(size = 4))  # Adjust title size here

# Plot 3: P(z1 < X < z2)
z1 <- -1  # Example Z-score 1
z2 <- 1    # Example Z-score 2
p3 <- ggplot(df, aes(x = x, y = y)) +
  geom_line() +
  geom_area(data = subset(df, x > z1 & x < z2), aes(x = x, y = y), fill = "blue", alpha = 0.3) +
  geom_vline(xintercept = c(z1, z2), linetype = "dashed", color = "red") +
  labs(title = "P(z1 < X < z2)", x = "Z-score", y = "") +
  theme_minimal()+
  theme(plot.title = element_text(size = 5),
        axis.title.y = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        axis.text.y = element_text(size = 4),
        axis.text.x = element_text(size = 4))  # Adjust title size here

# Arrange the plots in a grid
library(gridExtra)
grid.arrange(p1, p2, p3, ncol = 1)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------
# Define mean and standard deviation
mean_value <- 0.6
sd_value <- 0.2

# Define threshold value
threshold_value <- 1

# Create a sequence of x values for the plot
x_vals <- seq(mean_value - 4*sd_value, mean_value + 4*sd_value, length.out = 1000)

# Calculate normal density for the x values
y_vals <- dnorm(x_vals, mean = mean_value, sd = sd_value)

# Create data frame for plotting
data <- data.frame(x = x_vals, y = y_vals)

# Right Tail Plot (larger values)
right_tail_plot <- ggplot(data, aes(x, y)) +
  geom_line(size = 1, color = "blue") +  # Plot the normal distribution curve
  geom_area(data = subset(data, x >= threshold_value), aes(y = y), fill = "skyblue", alpha = 0.5) +  # Shade the right area
  geom_vline(xintercept = threshold_value, linetype = "dashed", color = "black", size = 1) +  # Mark the threshold value
  labs(title = "Right Tail: Proportion Above Threshold",
       subtitle = paste("Proportion of Values Above", threshold_value, ":", round(1 - pnorm(threshold_value, mean = mean_value, sd = sd_value), 4)),
       x = "Value",
       y = "Density") +
  theme_minimal()


# Left Tail Plot (smaller values)
left_tail_plot <- ggplot(data, aes(x, y)) +
  geom_line(size = 1, color = "blue") +  # Plot the normal distribution curve
  geom_area(data = subset(data, x <= threshold_value), aes(y = y), fill = "skyblue", alpha = 0.5) +  # Shade the left area
  geom_vline(xintercept = threshold_value, linetype = "dashed", color = "black", size = 1) +  # Mark the threshold value
  labs(title = "Left Tail: Proportion Below Threshold",
       subtitle = paste("Proportion of Values Below", threshold_value, ":", round(pnorm(threshold_value, mean = mean_value, sd = sd_value), 4)),
       x = "Value",
       y = "Density") +
  theme_minimal()

# Print the plots
print(right_tail_plot)
print(left_tail_plot)



## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------
# Load necessary library
library(ggplot2)

# Define mean and standard deviation
mean_value <- 0.6
sd_value <- 0.2

# Define cumulative probabilities
p_left <- 0.25  # For the left tail
p_right <- 0.75  # For the right tail

# Calculate threshold values using qnorm
threshold_left <- qnorm(p_left, mean = mean_value, sd = sd_value)
threshold_right <- qnorm(1 - p_right, mean = mean_value, sd = sd_value)

# Create a sequence of x values for the plot
x_vals <- seq(mean_value - 4*sd_value, mean_value + 4*sd_value, length.out = 1000)

# Calculate normal density for the x values
y_vals <- dnorm(x_vals, mean = mean_value, sd = sd_value)

# Create data frame for plotting
data <- data.frame(x = x_vals, y = y_vals)

# Right Tail Plot (larger values)
right_tail_plot <- ggplot(data, aes(x, y)) +
  geom_line(size = 1, color = "blue") +  # Plot the normal distribution curve
  geom_area(data = subset(data, x >= threshold_right), aes(y = y), fill = "skyblue", alpha = 0.5) +  # Shade the right area
  geom_vline(xintercept = threshold_right, linetype = "dashed", color = "black", size = 1) +  # Mark the threshold value
  labs(title = "Right Tail: Cumulative Probability",
       subtitle = paste("Threshold Value (75th Percentile):", round(threshold_right, 2)),
       x = "Value",
       y = "Density") +
  theme_minimal()


# Left Tail Plot (smaller values)
left_tail_plot <- ggplot(data, aes(x, y)) +
  geom_line(size = 1, color = "blue") +  # Plot the normal distribution curve
  geom_area(data = subset(data, x <= threshold_left), aes(y = y), fill = "skyblue", alpha = 0.5) +  # Shade the left area
  geom_vline(xintercept = threshold_left, linetype = "dashed", color = "black", size = 1) +  # Mark the threshold value
  labs(title = "Left Tail: Cumulative Probability",
       subtitle = paste("Threshold Value (25th Percentile):", round(threshold_left, 2)),
       x = "Value",
       y = "Density") +
  theme_minimal()


# Print the plots
print(right_tail_plot)
print(left_tail_plot)


## ----echo=FALSE, message=FALSE, fig.cap="", fig.width=3, fig.height=3.5--------------------------
# Create data for the normal distribution
z_values <- seq(-3, 3, length.out = 100)
y_values <- dnorm(z_values)

# Create a data frame
data <- data.frame(z = z_values, density = y_values)

# Plot the normal distribution and shade the area below z = 0.76
ggplot(data, aes(x = z, y = density)) +
  geom_line() +
  geom_area(data = filter(data, z <= 0.76), fill = "blue", alpha = 0.5) +
  geom_vline(xintercept = 0.76, linetype = "dashed", color = "red") +
  annotate("text", x = 2, y = 0.05, label = "P(z < 0.76)", color = "blue", size = 4) +
  labs(title = "", x = "Z-score", y = "Density") + theme_minimal()



## ------------------------------------------------------------------------------------------------
# Calculate the cumulative 
# probability for z = 0.76
z_score <- 0.76
p_value <- pnorm(z_score)
# Convert to percentage
percent_area <- p_value * 100
percent_area


## ----echo=FALSE, message=FALSE, fig.cap="", fig.width=3, fig.height=3.5--------------------------
# Create data for the normal distribution
z_values <- seq(-3, 3, length.out = 100)
y_values <- dnorm(z_values)

# Create a data frame
data <- data.frame(z = z_values, density = y_values)

# Plot the normal distribution and shade the area below z = 0.76
ggplot(data, aes(x = z, y = density)) +
  geom_line() +
  geom_area(data = filter(data, z <= 0.76), fill = "blue", alpha = 0.5) +
  geom_vline(xintercept = 0.76, linetype = "dashed", color = "red") +
  annotate("text", x = 2, y = 0.05, label = "P(z < 0.76)", color = "blue", size = 4) +
  labs(title = "",
       x = "Z-score",
       y = "Density") +
  theme_minimal()



## ----echo=FALSE, message=FALSE, fig.cap="", fig.width=3, fig.height=3.5--------------------------
# Calculate the z-score for the upper 5%
p_value <- 0.95
z_score <- qnorm(p_value)

# Create data for the normal distribution
z_values <- seq(-3, 3, length.out = 100)
y_values <- dnorm(z_values)

# Create a data frame
data <- data.frame(z = z_values, density = y_values)

# Plot the normal distribution and shade the area above the z-score
ggplot(data, aes(x = z, y = density)) +
  geom_line() +
  geom_area(data = filter(data, z >= z_score), fill = "blue", alpha = 0.5) +
  geom_vline(xintercept = z_score, linetype = "dashed", color = "red") +
  annotate("text", x = z_score + 0, y = 0.05, label = "z where:\n P>z = 0.05", color = "red", size = 4) +
  labs(title = "", x = "Z-score", y = "Density") + theme_minimal()



## ------------------------------------------------------------------------------------------------
# Calculate the z-score for 
# the upper 5% 
p_value <- 0.05
z_score <- qnorm(1-p_value)
# Print the result
z_score


## ----echo=FALSE, message=FALSE, fig.cap="", fig.width=3, fig.height=3.5--------------------------
# Calculate the z-score for the upper 5%
p_value <- 0.95
z_score <- qnorm(p_value)

# Create data for the normal distribution
z_values <- seq(-3, 3, length.out = 100)
y_values <- dnorm(z_values)

# Create a data frame
data <- data.frame(z = z_values, density = y_values)

# Plot the normal distribution and shade the area above the z-score
ggplot(data, aes(x = z, y = density)) +
  geom_line() +
  geom_area(data = filter(data, z >= z_score), fill = "blue", alpha = 0.5) +
  geom_vline(xintercept = z_score, linetype = "dashed", color = "red") +
  annotate("text", x = z_score + 0, y = 0.05, label = "z where:\n P>z = 0.05", color = "red", size = 4) +
  labs(title = "", x = "Z-score", y = "Density") + theme_minimal()



## ----echo=FALSE, message=FALSE, fig.cap="", fig.width=3, fig.height=3.5--------------------------
# Parameters for the normal distribution
mean <- 100
sd <- 15

# Values to find the probability between
low_value <- 100
high_value <- 115

# Calculate the z-scores
z_low <- (low_value - mean) / sd
z_high <- (high_value - mean) / sd

# Calculate the probabilities using pnorm
p_low <- pnorm(z_low)
p_high <- pnorm(z_high)

# Probability of falling between 100 and 115
probability <- p_high - p_low

# Print the probability
print(paste("Probability of falling between 100 and 115:", round(probability, 4)))

# Create data for the normal distribution
x_values <- seq(mean - 4*sd, mean + 4*sd, length.out = 100)
y_values <- dnorm(x_values, mean, sd)

# Create a data frame
data <- data.frame(x = x_values, density = y_values)

# Plot the normal distribution and shade the area between 100 and 115
ggplot(data, aes(x = x, y = density)) +
  geom_line() +
  geom_area(data = filter(data, x >= low_value & x <= high_value), fill = "blue", alpha = 0.5) +
  geom_vline(xintercept = low_value, linetype = "dashed", color = "red") +
  geom_vline(xintercept = high_value, linetype = "dashed", color = "red") +
  labs(title = "", x = "Score", y = "Density") + theme_minimal()


## ------------------------------------------------------------------------------------------------
# Parameters
mean <- 100
sd <- 15
# Values
low_value <- 100
high_value <- 115

# Calculate the z-scores
z_low <- (low_value-mean)/sd
z_high <- (high_value-mean)/sd


## ----echo=FALSE, message=FALSE, fig.cap="", fig.width=3, fig.height=3.5--------------------------
# Parameters for the normal distribution
mean <- 100
sd <- 15

# Values to find the probability between
low_value <- 100
high_value <- 115

# Calculate the z-scores
z_low <- (low_value - mean) / sd
z_high <- (high_value - mean) / sd

# Calculate the probabilities using pnorm
p_low <- pnorm(z_low)
p_high <- pnorm(z_high)

# Probability of falling between 100 and 115
probability <- p_high - p_low

# Print the probability
print(paste("Probability of falling between 100 and 115:", round(probability, 4)))

# Create data for the normal distribution
x_values <- seq(mean - 4*sd, mean + 4*sd, length.out = 100)
y_values <- dnorm(x_values, mean, sd)

# Create a data frame
data <- data.frame(x = x_values, density = y_values)

# Plot the normal distribution and shade the area between 100 and 115
ggplot(data, aes(x = x, y = density)) +
  geom_line() +
  geom_area(data = filter(data, x >= low_value & x <= high_value), fill = "blue", alpha = 0.5) +
  geom_vline(xintercept = low_value, linetype = "dashed", color = "red") +
  geom_vline(xintercept = high_value, linetype = "dashed", color = "red") +
  labs(title = "", x = "Score", y = "Density") + theme_minimal()


## ------------------------------------------------------------------------------------------------
# Find the probabilities
p_low <- pnorm(z_low)
p_high <- pnorm(z_high)

# Probability of falling 
# between 100 and 115
probability <- p_high - p_low
probability


## ----echo=FALSE, message=FALSE, fig.cap="", fig.width=3, fig.height=3.5--------------------------
# Parameters for the normal distribution
mean <- 100
sd <- 15

# Values to find the probability between
low_value <- 100
high_value <- 115

# Calculate the z-scores
z_low <- (low_value - mean) / sd
z_high <- (high_value - mean) / sd

# Calculate the probabilities using pnorm
p_low <- pnorm(z_low)
p_high <- pnorm(z_high)

# Probability of falling between 100 and 115
probability <- p_high - p_low

# Print the probability
print(paste("Probability of falling between 100 and 115:", round(probability, 4)))

# Create data for the normal distribution
x_values <- seq(mean - 4*sd, mean + 4*sd, length.out = 100)
y_values <- dnorm(x_values, mean, sd)

# Create a data frame
data <- data.frame(x = x_values, density = y_values)

# Plot the normal distribution and shade the area between 100 and 115
ggplot(data, aes(x = x, y = density)) +
  geom_line() +
  geom_area(data = filter(data, x >= low_value & x <= high_value), fill = "blue", alpha = 0.5) +
  geom_vline(xintercept = low_value, linetype = "dashed", color = "red") +
  geom_vline(xintercept = high_value, linetype = "dashed", color = "red") +
  labs(title = "", x = "Score", y = "Density") + theme_minimal()


## ----echo=FALSE, message=FALSE, warning=FALSE, fig.cap="", fig.width=3, fig.height=3.5-----------
# Load necessary libraries
library(ggplot2)

# Given values
mean_lifespan <- 15.7
sd_lifespan <- 1.6
age_allison_cat <- 18

# Define the Z-score
z_score <- (age_allison_cat - mean_lifespan) / sd_lifespan

# Generate a sequence of x-values (ages) covering the normal distribution
x_values <- seq(mean_lifespan - 4 * sd_lifespan, mean_lifespan + 4 * sd_lifespan, length = 1000)

# Calculate the density for each x-value
density <- dnorm(x_values, mean = mean_lifespan, sd = sd_lifespan)

# Create a data frame for ggplot
df <- data.frame(x = x_values, density = density)

# Define the critical Z-score for shading
shade_df <- df[df$x >= age_allison_cat, ]

# Plot the normal distribution with the shaded area for P(X > 18)
ggplot(df, aes(x = x, y = density)) +
  geom_line(color = "blue", size = 1) +  # Plot the normal distribution
  geom_area(data = shade_df, aes(x = x, y = density), fill = "red", alpha = 0.4) +  # Shade the area P(X > 18)
  geom_vline(xintercept = age_allison_cat, linetype = "dashed", color = "black") +  # Add a dashed line at Allison's cat's age
  labs(title = "Probability of a Cat Living to be as Old as Allison's Cat",
       subtitle = paste0("Mean = 15.7, SD = 1.6, Age = ", age_allison_cat),
       x = "Cat's Age",
       y = "Density") +
  theme(plot.title = element_text(size = 4)) + 
  theme_minimal()


## ------------------------------------------------------------------------------------------------
# Given values
mean_lifespan <- 15.7
sd_lifespan <- 1.6
age_allison_cat <- 18
# Calculate Z-score
z_score <- (age_allison_cat - mean_lifespan) / sd_lifespan
# Find the probability that a cat lives longer than 18 years
probability <- 1 - pnorm(z_score)
probability


## ------------------------------------------------------------------------------------------------
# Given values
mean_lifespan <- 15.7
sd_lifespan <- 1.6
age_allison_cat <- 18
# Calculate Z-score
z_score <- (age_allison_cat - mean_lifespan) / sd_lifespan
# Find the probability that a cat lives longer than 18 years
probability <- 1 - pnorm(z_score)
probability


## ------------------------------------------------------------------------------------------------
# Parameters
mean <- 0.6
sd <- 0.2
# Threshold 4 lead remediation
threshold <- 1
# Calculate the z-score for the threshold
z_score<-(threshold-mean)/sd
# Calculate the proportion of 
#buildings above the threshold
threshold<-1-pnorm(z_score)
threshold

