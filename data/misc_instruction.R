# Date: 5 April 2023
# AJ Smit
# An exercise in cutting

library(tidyverse)

dat <- faithful

dat$eruptions
range(dat$ eruptions)
?cut

dat_break <- data.frame(eruption = dat$eruptions,
                        breaks = cut(x = dat$eruptions, breaks = 15))


# One-sample t-test -------------------------------------------------------

sample_A <- rnorm(n = 30, mean = 10, sd = 13)
mean(sample_A)
sd(sample_A)

t.test(sample_A, mu = 30,
       alternative = "less")


# Two sample t-tests ------------------------------------------------------

chicks <- ChickWeight

# age
# diets 1 and 3

# Ho: There is no difference in the mean mass in adult chicken in Diets 1 and 3.
# Ha: There is difference in the mean mass in adult chicken in Diets 1 and 3.

adult_chicks <- chicks |>
  filter(Time == 21,
         Diet == 1 | Diet == 3)

t.test(weight ~ Diet, data = adult_chicks)

# A Student's t-test applied to the chicken dataset indicates that there is a statistic ally significant differeence in the final mass the chicken attained at 21 days as a result of having been fed Diets 1 and 2 (t = -3.43, df = 16.41, p < 0.05).


# Beach birds ANOVA -------------------------------------------------------

birds <- read.csv("data/BeachBirds.csv")

# H0: The species of beach birds will not affect their flushing distance.
# Ha: The species of beach birds will affect their flushing distance.

birds.aov <- aov(flush.dist ~ Species, data = birds)
class(birds.aov)
str(birds.aov)

plot(TukeyHSD(birds.aov, ordered = TRUE))

birds.aov
summary(birds.aov)

# An ANOVA indicates that the flushing distance of beach birds if not affected by the species of bird in question, in other workds, all birds behave the same with respect to approaching cars (F-ratio = 0.25, df = 3, P = > 0.05).

# H0: There is no difference in the flushing distance shown by beach birds among sites along the beach
# Ha: There is a difference in the flushing distance shown by beach birds among sites along the beach

summary(aov(flush.dist ~ Site, data = birds))

#H0: There is no difference in the flushing distance shown by beach birds among among sexes
#Ha: There is no difference in the flushing distance shown by beach birds among among sexes

t.test(flush.dist ~ Sex, data = birds)


# The ToothGrowth data ----------------------------------------------------

tandjies <- ToothGrowth
summary(aov(len ~ supp + dose, data = tandjies))

# H0(1): There is no diffference in guinnea pig tooth growth among supplement treament
# H0(2): There is no diffference in guinnea pig tooth growth among supplement dose

summary(aov(len ~ supp * dose, data = tandjies))

# H0(1): There is no diffference in guinnea pig tooth growth among supplement treament
# H0(2): There is no diffference in guinnea pig tooth growth among supplement dose
# H0(3): There is no diffference in guinnea pig tooth growth among supplement dose or among dose, and that is no interaction between dose and supplement


# Tukey HSD test ----------------------------------------------------------

tandjies$dose <- as.factor(tandjies$dose)

teeth_dose.aov <- aov(len ~ dose, data = tandjies)
summary(teeth_dose.aov)

teeth_dose.tukey <- TukeyHSD(teeth_dose.aov, ordered = TRUE)
plot(teeth_dose.tukey)


# A graph -----------------------------------------------------------------

ggplot(data = tandjies, aes(x = as.factor(dose), y = len)) +
  geom_boxplot(aes(colour = supp))


# LINEAR REGRESSIONS ------------------------------------------------------

cars <- cars

# H0:

ggplot(cars, aes(x = speed, y = dist)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("MPH") + ylab("Feet")

cars.lm <- lm(dist ~ speed, data = cars)
class(cars.lm)
summary(cars.lm)

# Results: The linear model fitted to the data representing the relationship between speed and the distance over which the car stops indicates that a greater distance is required to stop for cars drivving at a higer speed (R2 = 0.64, F = 89.57, p < 0.05). The slope of the relationship is 3.9 +/- 6.76 feet.MPH-1 (t = -2.6, p < 0.05)

str(cars.lm)
mean(cars.lm$residuals)
cars.lm$coefficients[1]


# The orange data ---------------------------------------------------------

orange <- Orange |>
  mutate(Tree = as.numeric(Tree))

ggplot(data = orange, aes(x = age, y = circumference)) +
  geom_point(aes(colour = as.factor(Tree))) +
  geom_smooth(method = "lm", aes(colour = as.factor(Tree)))

orange.lm <- lm(circumference ~ age + as.factor(Tree), data = orange)
summary(orange.lm)

# We see that `Tree` does influence the

orange.aov <- aov(circumference ~ as.factor(Tree), data = orange)
summary(orange.aov)


# An ANCOVA
orange.aov1 <- aov(circumference ~ age + as.factor(Tree), data = orange)
summary(orange.aov1)

orange.aov2 <- aov(circumference ~ age * as.factor(Tree), data = orange)
summary(orange.aov2)




# Pearson correlation -----------------------------------------------------

eck <- read.csv("data/ecklonia.csv")

cor.test(x = eck$stipe_length, eck$frond_length,method = "pearson")

ggplot(data = eck, aes(x = stipe_length, y = frond_length)) +
  geom_point(aes(colour = site))

lam <- read.csv("data/laminaria.csv")

lam_pearson <- round(cor(lam[, 4:12]), 2)
corrplot(lam_pearson, method = "circle")


# Beach birds -------------------------------------------------------------

birds <- read.csv("data/BeachBirds.csv") |>
  mutate(Species = as.factor(Species))
mod1.aov <- aov(land.dist ~ Species, data = birds)
summary(mod1.aov)

# Note, by assigning Species as a factor, the factor levels will become
# alphabetised unless otherwise specified
levels(birds$Species)

# Assign contrasts
contrasts(birds$Species) <- contr.sum(4)
contrasts(birds$Species)
