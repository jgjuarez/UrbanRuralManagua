#R code for Urban-Rural Manuscript

library(haven)
library(DHARMa)
library(MASS)
library(effects)
library(ggplot2)
library(ggtext)
library(tidyverse)
library(dplyr)
library(gamm4)
library(glmmTMB)
library(mgcv)
library(performance) #provides measures to assess model quality like R-squared
library(see) #needed to plot collinearity of variables in the model
library(emmeans) #used for post hoc comparisons "contrasts"
library(ggeffects)

df <- read_sav("A2CARES 4 SET DE DATOS INTEGRADOS L4 PUPAS ADULTOS AE.sav")
View(df)

str(df)

## Filtering and adjustment ------------------------- 

df <- df %>%
  filter(!(CV == "785" & Temporada == 2 & Año == 2022)) %>% 
  mutate(Year = as.factor(Año), 
         Season = as.factor(Temporada), 
         CV = as.factor(CV), 
         WaterIssues = as.factor(ProbAgua),
         Community = as.factor(Zona)
  )

## Normality of data set ------------------

variables <- list(df$TotL4_AE, df$TotPupa_AE, df$Total_F_AE, df$Total_Adults_AE)
names(variables) <- c("Larvae4", "Pupae", "Females","Adults")

shapiro_results <- lapply(variables, shapiro.test)
shapiro_results

#par(mfrow = c(1,4))

for (i in 1:4) {
  hist(variables[[i]], prob = TRUE, main = paste("Histogram for", names(variables)[i]))
  curve(dnorm(x, mean=mean(variables[[i]]), sd=sd(variables[[i]])), col="red", lwd=2, add=TRUE)
}

#par(mfrow= c(1,4))

for (i in 1:4) {
  qqnorm(variables[[i]], main = paste("Q-Q Plot for", names(variables)[i]))
  qqline(variables[[i]], col = "red")
}

wilcox.test(Total_F_AE ~ Community, data = df)
V <- wilcox.test(df$Total_F_AE)
check_symmetry(V)

## Testing Generalized Linear Models ---------

model1.1 <- glm(Total_Adults_AE ~ Season + Year + Community, data = df, family = poisson())
summary(model1.1)

testDispersion(model1.1)
simulationOutput1.1 <- simulateResiduals(fittedModel = model1.1, plot = F)
plot(simulationOutput1.1)

model1.2 <- glm.nb(Total_Adults_AE ~ Season + Year + Community, data = df)
summary(model1.2)

testDispersion(model1.2)
simulationOutput1.2 <- simulateResiduals(fittedModel = model1.2, plot = F)
plot(simulationOutput1.2)

model1.3 <- glm(Total_Adults_AE ~ Season + Year + Community, data = df, family = quasipoisson())
summary(model1.3)

#### Calculate dispersion statistic----
dispersion <- sum(residuals(model1.3, type = "pearson")^2) / df.residual(model1.3)
dispersion # shows a large effect of overdispersion and the NB model fits better.


model1.4 <- glmmTMB(Total_Adults_AE ~ Season + Year + Community, data = df, family = nbinom2())
summary(model1.4)

#### Testing Generalized Linear Mixed Models for random effects for Household -----

#### Fit poisson model for count data with a Random Effect----
model2.1 <- glmmTMB(Total_Adults_AE ~ Season + Year + Community + (1|CV), data = df, family = "poisson")
summary(model2.1)

testDispersion(model2.1)
simulationOutput2.1 <- simulateResiduals(fittedModel = model2.1, plot = T)

#### Fit negative binomial model with "constant" Zero Inflation-------
model2.2 <- glmmTMB(Total_Adults_AE ~ Season + Year + Community + (1|CV), data = df, family = nbinom1(), zi = ~1)
summary(model2.2)

testDispersion(model2.2)
simulationOutput2.2 <- simulateResiduals(fittedModel = model2.2, plot = T)

model2.3 <- glmmTMB(Total_Adults_AE ~ Season + Year + Community + (1|CV), data = df, family = nbinom1())
summary(model2.3)

testDispersion(model2.3)
simulationOutput2.3 <- simulateResiduals(fittedModel = model2.3, plot = T)

model2.4 <- glmmTMB(Total_Adults_AE ~ Season + Year + Community + (1|CV), data = df, family = nbinom2())
summary(model2.4)

testDispersion(model2.4)
simulationOutput2.4 <- simulateResiduals(fittedModel = model2.4, plot = T)

anova(model1.4, model2.4)

##BEST MODEL FIT NB2 or Quasi-Poisson --------

# Extract fitted values and actual values
fitted_values <- predict(model2.4, type = "response")
observed_values <- df$Total_Adults_AE

# Plot fitted vs. observed values
plot(observed_values, fitted_values, 
     xlab = "Observed Values", 
     ylab = "Fitted Values", 
     main = "Fitted vs. Observed",
     pch = 19, col = "blue")
abline(0, 1, col = "red")  # Add a 1:1 line for reference

# Extract residuals and fitted values
residuals <- residuals(model2.4, type = "pearson")

# Plot residuals vs. fitted values
plot(fitted_values, residuals, 
     xlab = "Fitted Values", 
     ylab = "Residuals", 
     main = "Residuals vs. Fitted",
     pch = 19, col = "blue")
abline(h = 0, col = "red")  # Add a horizontal line at 0

# Extract random effects for CV
ranef_data <- ranef(model2.4)$cond$CV[, 1]

# Plot the random effects using dotchart
dotchart(ranef_data, 
         main = "Random Effects for CV", 
         xlab = "Effect Size", 
         ylab = "CV")

plot(allEffects(model2.4))

confint(model2.4)

coef_df <- as.data.frame(summary(model2.4)$coefficients$cond)

ggplot(coef_df, aes(x = rownames(coef_df), y = Estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = Estimate - 1.96 * `Std. Error`, ymax = Estimate + 1.96 * `Std. Error`), width = 0.2) +
  labs(title = "Effect Sizes for Each Variable", x = "Variables", y = "Effect Size (Estimate)") +
  theme_minimal()

# Extract the coefficients and their standard errors
coef_table <- summary(model2.4)$coefficients$cond

####Calculate Odds Ratios (OR) by exponentiating the estimates------
odds_ratios <- exp(coef_table[, "Estimate"])

# Calculate 95% Confidence Intervals for the Odds Ratios
lower_ci <- exp(coef_table[, "Estimate"] - 1.96 * coef_table[, "Std. Error"])
upper_ci <- exp(coef_table[, "Estimate"] + 1.96 * coef_table[, "Std. Error"])

# Create a data frame with OR and CIs
odds_ratios_df <- data.frame(
  Variable = rownames(coef_table),
  OR = odds_ratios,
  Lower_CI = lower_ci,
  Upper_CI = upper_ci
)

# Print the odds ratios and confidence intervals
print(odds_ratios_df)

##Testing GAM models----

Gam <- gam(Total_Adults_AE ~ s(TotPupa_AE, k= 15)+ s(TotalRECAgua) + Season + Year + Community,data = df)

####Autocorrelation ----
par(mfrow = c(1,2))
acf(resid(Gam),  main = "ACF")
pacf(resid(Gam),  main = "pACF")

summary(Gam)
# The data is highly autocorrelated for the 1st lag but none after, which would suggest that ideally an AR(1) model would be best for the data analysis.
# Therefore a low order AR model is likely needed.

###Best fit for Total Pupae-----

Gam1 <- gamm(Total_Adults_AE ~ s(TotPupa_AE, k= 15) + s(TotalRECAgua) + Season + Year + Community,
             data = df, method = "REML")

Gam2 <- gamm(Total_Adults_AE ~ s(TotPupa_AE, k= 15) + s(TotalRECAgua) +  Season + Year + Community,
             correlation = corARMA(form = ~ 1 | TotPupa_AE, p = 1), data = df, method = "REML")

summary(Gam1$lme)
summary(Gam2$lme)

gam.check(Gam1$gam)
plot(Gam1$gam, pages = 1, scale = 0) 

gam.check(Gam2$gam)
plot(Gam2$gam, pages = 1, scale = 0) 

###Random effect modeling----
GammM1 <- gamm4(Total_Adults_AE ~ s(TotPupa_AE)+s(TotalRECAgua) + Season + Year + Community,
                family = poisson,
                random = ~(1|CV),
                data = df)

summary(GammM1$gam) ##summary of gam
summary(GammM1$mer) ## underlying mixed model
anova(GammM1$gam)

gam.check(GammM1$gam) #The results show that the K is too narrow for the smooth of TotPupa_AE, increasing it to try an capture more complexity

plot(GammM1$gam, pages = 1, scale = 0) #Shows that the TotalRECAgua smooth term is a straight line, it does not need a GAMM for the model
vis.gam(GammM1$gam, theta=35)

###Testing Larva 4 stage for GAMM-----
GammM2 <- gamm4(Total_Adults_AE ~ s(TotL4_AE)+s(TotalRECAgua) + Season + Year + Community,
                family = poisson,
                random = ~(1|CV),
                data = df)

summary(GammM2$gam) ## summary of gam
summary(Gamm2$mer) ## underlying mixed model
anova(GammM2$gam)

gam.check(GammM2$gam) #The results show that the K is too narrow for the smooth of TotPupa_AE, increasing it to try an capture more complexity

plot(GammM2$gam, pages = 1, scale = 0)

####A mixture of L4 and Pupae-----

df <- df %>%
  mutate(L4Pupae = TotL4_AE + TotPupa_AE)

GammM3 <- gamm4(Total_Adults_AE ~ s(L4Pupae)+s(TotalRECAgua) + Season + Year + Community,
                family = poisson,
                random = ~(1|CV),
                data = df)

summary(GammM3$gam) ## summary of gam
summary(GammM3$mer) ## underlying mixed model
anova(GammM3$gam)

gam.check(GammM3$gam) #The results show that the K is too narrow for the smooth of TotPupa_AE, increasing it to try an capture more complexity

plot(GammM3$gam, pages = 1, scale = 0)

##Modeling the presence of L4-Pupae and the presence of Females-----
df <- df %>%
  mutate(LP = ifelse(TotL4_AE > 0, 1, 0),
         PP = ifelse(TotPupa_AE > 0, 1, 0),
         LPP = ifelse(L4Pupae > 0, 1, 0),
         FP = ifelse(Total_Adults_AE > 0, 1, 0)
         )

model3.1 <- glmmTMB(FP ~ LP + Season + Year + Community + (1|CV), data = df, family = "binomial")

testDispersion(model3.1)
simulationOutput3.1 <- simulateResiduals(fittedModel = model3.1, plot = T)

summary(model3.1)

model3.2 <- glmmTMB(FP ~ PP + Season + Year + Community + (1|CV), data = df, family = "binomial")

testDispersion(model3.2)
simulationOutput3.2 <- simulateResiduals(fittedModel = model3.2, plot = T)

summary(model3.2)


model3.3 <- glmmTMB(FP ~ LPP + Season + Year + Community + (1|CV), data = df, family = "binomial")

testDispersion(model3.3)
simulationOutput3.3 <- simulateResiduals(fittedModel = model3.1, plot = T)

summary(model3.3)

#### Best model presence of L4------
confint(model3.1)

coef_df3 <- as.data.frame(summary(model3.1)$coefficients$cond)

ggplot(coef_df3, aes(x = rownames(coef_df3), y = Estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = Estimate - 1.96 * `Std. Error`, ymax = Estimate + 1.96 * `Std. Error`), width = 0.2) +
  labs(title = "Effect Sizes for Each Variable", x = "Variables", y = "Effect Size (Estimate)") +
  theme_minimal()

# Extract the coefficients and their standard errors
coef_table3 <- summary(model3.1)$coefficients$cond

####Calculate Odds Ratios (OR) by exponentiating the estimates------
odds_ratios3 <- exp(coef_table3[, "Estimate"])

# Calculate 95% Confidence Intervals for the Odds Ratios
lower_ci3 <- exp(coef_table3[, "Estimate"] - 1.96 * coef_table3[, "Std. Error"])
upper_ci3 <- exp(coef_table3[, "Estimate"] + 1.96 * coef_table3[, "Std. Error"])

# Create a data frame with OR and CIs
odds_ratios_df3 <- data.frame(
  Variable = rownames(coef_table3),
  OR = odds_ratios3,
  Lower_CI = lower_ci3,
  Upper_CI = upper_ci3
)

# Print the odds ratios and confidence intervals
print(odds_ratios_df3)

## Performing model checks using the performance package ------ 
check_autocorrelation(model3.1)

plot(check_collinearity(model3.1))

plot(check_dag(model3.1))

check_distribution(model3.1)

check_heterogeneity_bias(df, select = c("TotL4_AE", "TotPupa_AE"), by = "CV")

check_model(model3.1)

outliers_list <- check_outliers(df$TotPupa_AE, method = "zscore", threshold = 2)
as.numeric(outliers_list)

plot(outliers_list)

check_overdispersion(model3.1)

check_zeroinflation(model2.4)

COM <- compare_performance(model2.1, model2.2, model2.3, model2.4, metrics = "all") # provides full model comparison 
display(COM)

emmeans(model2.4, "Community")
emm.model2.4 <- emmeans(model2.4, ~ Community*Season*Year, type = "response")
pairs(emm.model2.4)

emm.model2.4 %>%
  confint()

emm.model2.4 %>%
  summary(infer = TRUE)

emm.model2.4.2 <- emmeans(model2.4, ~ Community|Season|Year, type = "response")

emm.model2.4.2 %>%
  summary(infer = TRUE)

EMM <- emmeans(model2.4, ~ Community|Season|Year)
EMM # show the means

contrast(emm.model2.4.2, "pairwise", simple = "Community")

#Predictions of GLMM model
test <- predict_response(model2.4, terms = c("Community", "Year", "Season"))
ggplot(test, aes(x, predicted, colour = group)) + geom_point() +
  geom_errorbar(aes(ymin = predicted - 1.96 * `std.error`, ymax = predicted + 1.96 * `std.error`), width = 0.2) + 
  facet_wrap(~facet)

test

#Predictions of GAMM model
test2 <- predict_response(GammM1, "TotPupa_AE")
ggplot(test2, aes(x, predicted)) + 
  geom_line()+
  geom_point()
