library(dplyr)
library(tidyr)
library(magrittr)
library(lme4)
library(ggplot2)

setwd("E:/MS4/00_code_clean")

#### GENERALIZED LINEAR MIXED MODELS #### 
## Probability of forest and shrub cover increase within 300m cells
## TL.DIST = distance in km from the treeline multipled by treeline position (TL)
## TL = -1 for below the treeline, +1 for above the treeline
## convert forest and shrub change classes to binary (increasing vs not increasing)
ptab = read.csv("01_transect_plots.csv") %>% 
  mutate(plot.id = 1:nrow(ptab), 
         TL.DIST = NEAR_DIST * TL,
         forest_inc = ifelse(C_Forest > 0, 1, 0),
         shrub_inc = ifelse(C_Shrub > 0, 1, 0))

## Reorder the transect numbers North to South
ptab$Transect = factor(ptab$Transect, levels = c("2", "3", "4", "1"), labels = 1:4)

## convert to long format for modeling 
plong = ptab %>% select(Transect, TL.DIST, forest_inc, shrub_inc) %>% 
  pivot_longer(!c(Transect, TL.DIST), names_to = "Veg.Type", values_to = "Increasing")

## Fit global effect of treeline distance on probability of increasing veg. cover
## then random slope and intercepts for each combination of transect and veg. type
m1 = glm(Increasing ~ TL.DIST, data = plong, family = "binomial")
m2 = glmer(Increasing ~ TL.DIST + (TL.DIST|Veg.Type:Transect), data = plong, family = "binomial")
coef(m2)

# compare the intercept only model with the random effects model
# LRT calculated using the loglik() function
# Compare model with and without random effects
G2 = -2 * logLik(m1) + 2 * logLik(m2)

# p-value for hypothesis test 
pchisq(as.numeric(G2), df=1, lower.tail=F)

## Plot the results 
## predict new data using random effects model (m2)
## data frame of covariates
newdata = data.frame(Transect = plong$Transect,
                     TL.DIST = plong$TL.DIST,
                     Veg.Type = plong$Veg.Type,
                     observed = plong$Increasing)
## predict 
fitted = predict(m2, newdata = newdata, type = "link", se.fit=T)
newdata = cbind(newdata, fitted)
  
# compute confidence intervals from the standard error
newdata = newdata %>%
  mutate(lower = plogis(fit - 1.96 * se.fit),
         upper = plogis(fit + 1.96 * se.fit),
         predicted = plogis(fit))

# transect names for the plot
transect.labels = c("T1:Coppermine (1969)","T2:GSU (1937)","T3:Whitefish West (1930)","T4:Whitefish East (1971)")
names(transect.labels) = 1:4
# colors for tree and shrub groups
cols = c("forest_inc" = "#556B2F", "shrub_inc" = "darkgoldenrod3")

#### FIGURE 4 ####
ggplot(aes(x = TL.DIST, y = predicted, color = Veg.Type, fill = Veg.Type), data = newdata) + 
  facet_wrap(~Transect, scales = "free_x", labeller = labeller(Transect = transect.labels)) +
  scale_color_manual(name = "", values = cols, labels = c("Forest", "Shrub"))+
  scale_fill_manual(name = "", values = cols, labels = c("Forest", "Shrub")) +
  geom_point(aes(x = TL.DIST, y = observed), alpha = 0.1, size = 2, show.legend = FALSE) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA, show.legend = FALSE)+
  geom_line(size = 1) +
  ylab("Probability of Increasing Cover")+
  xlab("Distance to 2021 Treeline (km)")+
  geom_vline(xintercept = 0, color = "darkgrey", linetype = "dashed")+
  theme_bw()+
  theme(legend.position = "bottom", legend.text = element_text(size = 12),
        strip.text = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))