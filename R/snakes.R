snakes <- read.csv("data/snakes.csv")
snakes$day = as.factor(snakes$day)

snakes.aov <- aov(openings ~ as.factor(day) + snake, data = snakes)
summary(snakes.aov)

# Checking assumptions...
# make a histogram of the residuals;
# they must be normal
snakes.res <- residuals(snakes.aov)
hist(snakes.res)

# make a plot of residuals and the fitted values;
# # they must be normal and homoscedastic
plot(fitted(snakes.aov), residuals(snakes.aov))
