



# https://environmentalcomputing.net/content/statistical-methods/anova




library(readxl)

sessile <- read.csv("sessile.csv")
head(sessile)


mod1 <- aov(Richness ~ Copper * Orientation, 
                   data = sessile)

summary(mod1)
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

library(emmeans)
emmeans(mod2, pairwise ~ Copper | Orientation,
        adjust = "tukey")

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

