#Logistic regression modeling for pupae and adult

library(ggplot2)
library(ggtext)
library(tidyverse)
library(dplyr)
library(esquisse)
library(GWalkR)
library(gapminder)
library(patchwork)
library(janitor)
library(haven)
library(glmmTMB)
library(DHARMa)
library(mgcv)
library(gamm4)
library(performance) #provides measures to assess model quality like R-squared

df <- read_sav("A2CARES 4 SET DE DATOS INTEGRADOS L4 PUPAS ADULTOS AE.sav")
View(df)

str(df)

df1 <- df %>%
  filter(!(CV == "785" & Temporada == 2 & Año == 2022)) %>% 
  mutate(Year = as.factor(Año), 
         Season = as.factor(Temporada), 
         CV = as.factor(CV), 
         WaterIssues = as.factor(ProbAgua),
         Community = as.factor(Zona)
  )

esquisse::esquisser(df)

variables <- list(df$TotL4_AE, df$TotPupa_AE, df$Total_F_AE, df$Total_Adults_AE)
names(variables) <- c("Larvae4", "Pupae", "Females","Adults")

shapiro_results <- lapply(variables, shapiro.test)
shapiro_results

par(mfrow = c(1,4))

for (i in 1:4) {
  hist(variables[[i]], prob = TRUE, main = paste("Histogram for", names(variables)[i]))
  curve(dnorm(x, mean=mean(variables[[i]]), sd=sd(variables[[i]])), col="red", lwd=2, add=TRUE)
}

par(mfrow= c(1,4))

for (i in 1:4) {
  qqnorm(variables[[i]], main = paste("Q-Q Plot for", names(variables)[i]))
  qqline(variables[[i]], col = "red")
}

wilcox.test(Total_F_AE ~ Community, data = df)

model1 <- glm(Total_Adults_AE ~ Season + Year + Community, data = df, family = poisson())

testDispersion(model1)
simulationOutput <- simulateResiduals(fittedModel = model1, plot = F)
plot(simulationOutput)

model2 <- glm(Total_Adults_AE ~ Season + Year + Community, data = df, family = quasipoisson())

summary(model1)
summary(model2)

## Fit poisson model for count data
model3 <- glmmTMB(Total_Adults_AE ~ Season + Year + Community + (1|CV), data = df, family = "poisson")
summary(model3)

testDispersion(model3)
simulationOutput3 <- simulateResiduals(fittedModel = model3, plot = T)

## Fit negative binomial model with "constant" Zero Inflation
model4 <- glmmTMB(Total_Adults_AE ~ Season + Year + Community + (1|CV), data = df, family = nbinom1(), zi = ~1)
summary(model4)

testDispersion(model4)
simulationOutput4 <- simulateResiduals(fittedModel = model4, plot = T)

model5 <- glmmTMB(Total_Adults_AE ~ Season + Year + Community + (1|CV), data = df, family = nbinom1())
summary(model5)

testDispersion(model5)
simulationOutput5 <- simulateResiduals(fittedModel = model5, plot = T)

model6 <- glmmTMB(Total_Adults_AE ~ Season + Year + Community + (1|CV), data = df, family = nbinom2())
summary(model6)

testDispersion(model6)
simulationOutput5 <- simulateResiduals(fittedModel = model6, plot = T)




## TEST Gamm models
GammM1 <- gamm4(Total_Adults_AE ~ s(TotPupa_AE)+s(TotalRECAgua) + Season + Year + Community,
               family = poisson,
               random = ~(1|CV),
               data = df)
summary(GammM1$gam) ##summary of gam
summary(GammM1$mer) ## underlying mixed model
anova(GammM1$gam)

gam.check(GammM1$gam) #The results show that the K is too narrow for the smooth of TotPupa_AE, increasing it to try an capture more complexity

plot(GammM1$gam, pages = 1) #Shows that the TotalRECAgua smooth term is a straight line, it does not need a GAMM for the model
vis.gam(GammM1$gam, theta=35)

GammM2 <- gamm4(Total_Adults_AE ~ s(TotPupa_AE)+s(TotalRECAgua) + Season + Year + Community,
                family = negative.binomial(1), #The fit of the model for the GAMM transforms into a linear prediction for both smooths
                random = ~(1|CV),
                data = df)
summary(GammM2$gam) ##summary of gam
summary(GammM2$mer) ## underlying mixed model
anova(GammM2$gam)

plot(GammM2$gam, pages = 1)
vis.gam(GammM2$gam, theta=35)

GammM3 <- gamm4(Total_Adults_AE ~ s(TotPupa_AE, k= 15)+ TotalRECAgua + Season + Year + Community,
                family = poisson,
                random = ~(1|CV),
                data = df)
summary(GammM3$gam) ## summary of gam
summary(GammM3$mer) ## underlying mixed model
anova(GammM3$gam)

gam.check(GammM3$gam) 

plot(GammM3$gam, pages = 1) 

vis.gam(GammM3$gam, theta=35)

GammM4 <- gamm4(Total_Adults_AE ~ s(TotPupa_AE, k= 30)+ Season + Year + Community, #A K higher than 30 decreases the efficiency of the model
                family = poisson,
                random = ~(1|CV),
                data = df)
summary(GammM4$gam) ## summary of gam
summary(GammM4$mer) ## underlying mixed model
anova(GammM4$gam)

gam.check(GammM4$gam) 

plot(GammM4$gam, pages = 1) 

vis.gam(GammM4$gam, theta=35)

GammM5 <- gamm4(Total_Adults_AE ~ s(TotPupa_AE, k= 30)+ Season + Year + Community, #A K higher than 30 decreases the efficiency of the model
                family = negative.binomial(1),
                random = ~(1|CV),
                data = df)
summary(GammM5$gam) ## summary of gam
gam.check(GammM5$gam)

plot(GammM5$gam, pages = 1, scale = 0) 

##Testing GAM autocorrelation
df1 <- df %>%
  filter(!(CV == "785" & Temporada == "2" & Año == "2022"))

Gam <- gam(Total_Adults_AE ~ s(TotPupa_AE, k= 15)+ s(TotalRECAgua) + Season + Year + Community,data = df)

par(mfrow = c(1,2))
acf(resid(Gam),  main = "ACF")
pacf(resid(Gam),  main = "pACF")

summary(Gam)
# The data is highly autocorrelated for the 1st lag but none after, which would suggest that ideally an AR(1) model would be best for the data analysis.
# Therefore a low order AR model is likely needed.

Gam1 <- gamm(Total_Adults_AE ~ s(TotPupa_AE, k= 15) + s(TotalRECAgua) + Season + Year + Community,
             data = df1, method = "REML")

Gam2 <- gamm(Total_Adults_AE ~ s(TotPupa_AE, k= 15) + s(TotalRECAgua) +  Season + Year + Community,
             correlation = corARMA(form = ~ 1 | TotPupa_AE, p = 1), data = df1, method = "REML")

summary(Gam1$lme)
summary(Gam2$lme)

gam.check(Gam1$gam)
plot(Gam1$gam, pages = 1, scale = 0) 

gam.check(Gam2$gam)
plot(Gam2$gam, pages = 1, scale = 0) 

## Testing random intercepts for CV
Gam0 <- gamm(Total_Adults_AE ~ s(TotPupa_AE, k= 15) + s(TotalRECAgua) + s(CV, bs = "re") + Season + Year + Community,
             data = df1, method = "REML")

summary(Gam0$lme)
plot(Gam0$gam, pages = 1, scale = 0, select = 3) 

plot.gam(Gam0$gam)
