# -----------------------------
# Load packages
# -----------------------------
library(ggplot2)
library(dplyr)

# -----------------------------
# Simulate data
# -----------------------------
set.seed(123)

# Factors
light_levels <- c("0% light", "50% light", "100% light", "400% light")
time_days <- c(0, 21, 36, 56, 67)

# Create dataframe
sim_data <- expand.grid(
  time = time_days,
  light = light_levels,
  rep = 1:6  # replicates per treatment
)

# Simulate yield depending on light and time
sim_data <- sim_data %>%
  mutate(
    yield = case_when(
      light == "0% light" ~ 0.5 - 0.002 * time + rnorm(n(), 0, 0.02),
      light == "50% light" ~ 0.5 + 0.001 * time + rnorm(n(), 0, 0.02),
      light == "100% light" ~ 0.5 + 0.0008 * time + rnorm(n(), 0, 0.02),
      light == "400% light" ~ 0.5 + 0.0005 * time + rnorm(n(), 0, 0.02)
    )
  )

sim_data %>%
  filter(time==0)
# -----------------------------
# Plot in ggplot2
# -----------------------------
ggplot(sim_data, aes(x = factor(time), y = yield, fill = light)) +
  geom_boxplot(outlier.shape = 18, outlier.size = 2, color = "black") +
  scale_fill_manual(values = c("#4F81BD", "#F79646", "#9BBB59", "#C0504D")) +
  labs(x = "time in days", y = "yield", fill = "") +
  theme_bw(base_size = 14) +
  theme(
    legend.position = c(0.9, 0.85),
    legend.background = element_rect(fill = "white", color = "gray"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )
