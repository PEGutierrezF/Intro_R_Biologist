





library(broom)
library(kableExtra)
library(dplyr)
library(emmeans)

sessile <- read.csv("sessile.csv")
head(sessile)

interaction.plot(sessile$Copper, sessile$Orientation, sessile$Richness)

# Consider an example where a researcher is testing the effects of 
# metal contamination on the number of species found in sessile 
# marine invertebrates (sponges, bryozoans and sea squirts etc.). 
# They would like to know whether copper reduces species richness, 
# but also know that the richness of invertebrates can depend on 
# whether the substrate is vertical or horizontal. Consequently, 
# they ran an experiment where species richness was recorded in 
# replicate samples in each of the six combinations of copper 
# enrichment (“None”,“Low”,“High”) and orientation (“Vertical”,
# “Horizontal”). The experimental design in termed factorial 
# because all levels of one treatment are represented in all 
#levels of the other treatment (also termed orthogonal).


mod1 <- aov(Richness ~ Copper * Orientation, 
            data = sessile)

summary(mod1)

# R² parcial (partial R-squared)
effectsize::eta_squared(mod1, partial = TRUE)

shapiro.test(residuals(mod1))

library(car)

leveneTest(Richness ~ Copper * Orientation,
           data = sessile)

par(mfrow = c(2, 2))
plot(mod1)


# Log ---------------------------------------------------------------------
sessile$Richness_log <- log(sessile$Richness)
mod2 <- aov(Richness_log ~ Copper * Orientation, 
            data = sessile)

summary(mod2)



shapiro.test(residuals(mod2))

leveneTest(Richness_log ~ Copper * Orientation,
           data = sessile)

par(mfrow = c(2, 2))
plot(mod2)



emmeans <- emmeans(mod2, pairwise ~ Copper | Orientation,
        adjust = "tukey")
emmeans::contrast(emmeans, 
                  method = "pairwise") %>%
  tidy() %>%
  mutate_at(c("estimate", "std.error", "statistic", "adj.p.value"),
            round, digits = 4) %>%
  kbl()


# Outliers ----------------------------------------------------------------
# Standardized residuals
rstandard(mod1)

# Identify observations with |standardized residual| > 2
which(abs(rstandard(mod1)) > 2)

sessile2 <- sessile[-c(13, 50), ]

mod3 <- aov(Richness ~ Copper * Orientation, 
            data = sessile2)

summary(mod3)
shapiro.test(residuals(mod3))

leveneTest(Richness ~ Copper * Orientation,
           data = sessile2)

par(mfrow = c(2, 2))
plot(mod1)
