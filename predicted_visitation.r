# Install packages if needed
install.packages(c("lme4", "DHARMa", "emmeans"))

# Load packages
library(lme4)
library(Matrix)
library(DHARMa)
library(emmeans)

# Read data
data <- read_csv("C:/Users/cchamsj/Desktop/Amastra_intermedia_annotations-20260617T001042Z-3-001/X20251118_Amas_Utex2340_Concatenated_treatment_visitations.csv")

# Convert variables to factors
data$Individual <- as.factor(data$Individual)
data$Treatment <- as.factor(data$Treatment)

# Fit Poisson GLMM
model <- glmer(Visits ~ Treatment + (1 | Individual),
               family = poisson(link = "log"),
               data = data)

# Model summary
summary(model)

# Convert the estimates to incidence rate ratios

# Because you used a Poisson GLMM with a log link, exponentiating the coefficients gives the multiplicative effect on visit counts.

exp(fixef(model))

# Compare all treatments

# Your model output only compares each treatment to the reference. To compare every pair of treatments, run:

library(emmeans)

emmeans(model, pairwise ~ Treatment, type = "response")

# Check model fit

library(DHARMa)

sim <- simulateResiduals(model)
plot(sim)
testDispersion(sim)

# Bad fit use glmm

install.packages("glmmTMB")
library(glmmTMB)
library(emmeans)
library(DHARMa)

model_nb <- glmmTMB(
  Visits ~ Treatment + (1 | Individual),
  family = nbinom2,
  data = data
)

summary(model_nb)

sim_nb <- simulateResiduals(model_nb)

plot(sim_nb)

testDispersion(sim_nb)

emmeans(model_nb, pairwise ~ Treatment, type = "response")

drop1(model_nb, test = "Chisq")

# Plot

library(emmeans)
library(ggplot2)

emm <- emmeans(model_nb, ~ Treatment, type = "response")

emm_df <- as.data.frame(emm)

ggplot(emm_df,
       aes(x = Treatment, y = response)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = asymp.LCL,
                    ymax = asymp.UCL),
                width = 0.15,
                linewidth = 0.8) +
  ylab("Predicted number of visits") +
  xlab("Treatment") +
  theme_classic(base_size = 14)

library(ggplot2)
library(emmeans)

emm_df <- as.data.frame(emmeans(model_nb, ~ Treatment, type = "response"))

# Add significance letters manually
emm_df$group <- c("b", "ab", "a", "a")

ggplot(emm_df,
       aes(x = Treatment, y = response)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = asymp.LCL,
                    ymax = asymp.UCL),
                width = 0.15) +
  geom_text(aes(y = asymp.UCL + 0.5,
                label = group),
            size = 6) +
  ylab("Predicted number of visits") +
  xlab("Treatment") +
  theme_classic(base_size = 14)
