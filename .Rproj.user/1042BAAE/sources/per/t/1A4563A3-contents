# 1. Install package: plot3D
# 2. Load language_diversity dataset
# 3. Explore variables, tidy (long to wide)
# 4. Check normality, transform, plot
# 5. Fit model (MRC, 3 params)
# 6. Write up results
# 7. Convert to html presentation (xaringan)
# 8. Create github repo, make website


install.packages("plot3D")


library("tidyverse")
library("ds4ling")
library("untidydata")
library("plot3D")

glimpse(language_diversity)
head(language_diversity)

language_diversity$Measurement |> unique() # one printout of every unique factor

ld <- language_diversity |> 
  filter(Continent == "Africa") |> 
  pivot_wider(names_from = "Measurement", values_from = "Value")

ld

ld |> 
ggplot() + 
  aes(x = Population, y = Langs) + # if you put it here it will be the global aesthetics 
  geom_text(aes(color = Area, label = Country)) + #these aesthetics are only going to apply to geomtext
  geom_smooth(method = lm, formula = "y ~ x")


my_mod <- lm(Langs ~ Area + Population, data = ld)

summary(my_mod) #intercept is the estimate of languages when the area is 0 (predictor)
# area slope, a one unit increase in area is associated with at -2.807e06 decrease in languages, holding population constant at 0 
# population slope, a one unit increase in population is associated with a 2.715e03 increase in languages, holding area constant at 0 
# R squared is the variance explained by the model, adjusted R squared adjusting R squared so it doesn't inflate erroneously - if you are doing multiple regression you probably want to report adjusted R squared 
# p value on F statistic is an omnibus test on the whole model and if it is significant it tells us whether at least one predictor in the model is significant 

ds4ling::diagnosis(my_mod)
# first plot does not look good - homoskedasticity
# QQ plot doesn't look good
# residuals do not look normally distributed

ld <- ld |> 
  mutate(
    logPop = log(Population), 
    logArea = log(Area)
  )

hist(ld$Population)
hist(ld$logPop)
hist(ld$Area)
hist(ld$logArea)

# log transforming is NOT the same as standardizing 

ld |> #same plot but using the log population and log area as predictors
ggplot() + 
  aes(x = logPop, y = Langs) +
  geom_text(aes(color = logArea, label = Country)) + 
  geom_smooth(method = lm, formula = "y ~ x")


# Fit an additive model (number of languages as a function of lopPop and 
# logArea)

my_log_mod <- lm(Langs ~ logArea + logPop, data = ld)

summary(my_log_mod)

# Fit a multiplicative model (number of languages as a function of lopPop and 
# logArea)

my_log_mod_mult <- lm(Langs ~ logArea*logPop, data = ld)

summary(my_log_mod_mult)



# For fun
x <- ld$logPop
y <- ld$logArea
z <- ld$Langs

plot3D::scatter3D(x, y, z, 
    pch = 21, cex = 1, expand = 0.75, colkey = F,
    theta = 45, phi = 20, ticktype = "detailed",
    xlab = "logPop", ylab = "logArea", zlab = "Langs")


