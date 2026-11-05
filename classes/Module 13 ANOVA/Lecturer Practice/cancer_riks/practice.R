

library(ggplot2)
df_env_jdf_env_jdf_env_just <- read.csv('Environmental Justice.csv')
head(df_env_just)


mod1 <- aov(Cancer_risk_mil~ Income_class, data=df_env_just)
summary(mod1)


ggplot(df_env_just, aes(Ethnicity, Cancer_risk_mil)) +
  geom_boxplot()

mod2 <- aov(Cancer_risk_mil~ Ethnicity, data=df_env_just)
summary(mod2)
par(mfrow = c(2, 2))
plot(mod2)


shapiro.test(residuals(mod2))
bartlett.test(Cancer_risk_mil~ Ethnicity, data=df_env_just)

TukeyHSD(mod2)
plot(TukeyHSD(mod2))

summary.lm(mod2)$r.squared
