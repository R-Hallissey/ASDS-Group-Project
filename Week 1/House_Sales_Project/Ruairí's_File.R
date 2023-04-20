# Hallissey's Housing Price Project

## Data
d <- read.table ("https://raw.githubusercontent.com/gedeck/practical-statistics-for-data-scientists/master/data/house_sales.csv"
)


install.packages("plotly")
install.packages("dplR")
install.packages("stargazer")
library(stargazer)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)
library(car)

# Landvalue and bathrooms multivariate regression
lm(dat$AdjSalePrice ~ dat$Bathrooms + dat$LandVal)
brlv.lm <- lm(dat$AdjSalePrice ~ dat$Bathrooms + dat$LandVal)
summary(brlv.lm)

## Bathroom regression and plot
lm(dat$AdjSalePrice ~ dat$Bathrooms)
br.lm <- lm(dat$AdjSalePrice ~ dat$Bathrooms)
summary (br.lm)
plot(dat$Bathrooms, dat$AdjSalePrice)
abline(br.lm)

#Landvalue regression and plot r .65
lm(dat$AdjSalePrice ~ dat$LandVal)
lv.lm <- lm(dat$AdjSalePrice ~ dat$LandVal)
summary(lv.lm)
plot(dat$LandVal, dat$AdjSalePrice)
abline(lv.lm)

# Sqr foot to living regression and plot r .48
summary(lm(dat$AdjSalePrice ~ dat$SqFtTotLiving))
sqftol.lm <- lm(dat$AdjSalePrice ~ dat$SqFtTotLiving)
summary(sqftol.lm)
plot(dat$SqFtTotLiving, dat$AdjSalePrice)
abline(sqftol.lm)

# BldgGrade r.45
summary(lm(dat$AdjSalePrice ~ dat$BldgGrade))
bldg.lm <- lm(dat$AdjSalePrice ~ dat$BldgGrade)
summary(bldg.lm)
plot(dat$BldgGrade, dat$AdjSalePrice)
abline(bldg.lm)

# Impsval regression r. 69
summary(lm(dat$AdjSalePrice ~ dat$ImpsVal))
impval.lm <- lm(dat$AdjSalePrice ~ dat$ImpsVa)
plot(dat$AdjSalePrice ~ dat$ImpsVal)
abline(impval.lm)

# Multivariates
# r .53
summary(
  lm(dat$AdjSalePrice ~ dat$Bathrooms + dat$BldgGrade + dat$SqFtTotLiving))

# r .70
summary(
  lm(dat$AdjSalePrice ~ dat$Bathrooms + dat$BldgGrade + dat$SqFtTotLiving
     + dat$ImpsVal))

# r .86 (https://medias.spotern.com/spots/w640/65/65297-1533296705.jpg)
summary(
  lm(dat$AdjSalePrice ~ dat$Bathrooms + dat$BldgGrade + dat$SqFtTotLiving
     + dat$ImpsVal + dat$LandVal))

# r .86
summary(
  lm(dat$AdjSalePrice ~ dat$ImpsVal + dat$LandVal))

### Bi and multivarte regressions for bathrooms, bldg grade, and sq ft living
## Bivariates
# Bathroom regression and plot r .28
lm(dat$AdjSalePrice ~ dat$Bathrooms)
br.lm <- lm(dat$AdjSalePrice ~ dat$Bathrooms)
summary (br.lm)
plot(dat$Bathrooms, dat$AdjSalePrice)
abline(br.lm)
# Sqr foot to living regression and plot r .48
summary(lm(dat$AdjSalePrice ~ dat$SqFtTotLiving))
sqftol.lm <- lm(dat$AdjSalePrice ~ dat$SqFtTotLiving)
summary(sqftol.lm)
plot(dat$SqFtTotLiving, dat$AdjSalePrice)
abline(sqftol.lm)
# BldgGrade r.45
summary(lm(dat$AdjSalePrice ~ dat$BldgGrade))
bldg.lm <- lm(dat$AdjSalePrice ~ dat$BldgGrade)
summary(bldg.lm)
plot(dat$BldgGrade, dat$AdjSalePrice)
abline(bldg.lm)

# Multivariate
# r .53
  lm(dat$AdjSalePrice ~ dat$BldgGrade + dat$SqFtTotLiving)
lm.1 <- lm(dat$AdjSalePrice ~ dat$BldgGrade
                + dat$SqFtTotLiving)
lm.1.res <- resid(lm.1)


## Code for model with zip_group
# data = dat

c(dat, lm.1.res)

zip_group <- dat %>%
  group_by(ZipCode) %>%
  summarise(resids = median(lm.1.res),
            count = n()) %>%
  arrange(resids) %>%
  mutate(cumul_count = cumsum(count),
         ZipGroup = ntile(cumul_count, 5))

dat2 <- dat %>%
  left_join(select(zip_group, ZipCode, ZipGroup), by = "ZipCode")

lm.2 <- lm(AdjSalePrice ~ SqFtTotLiving + BldgGrade + ZipGroup, data
           = dat2)
summary(lm.2)

#Transformaion Sqaure foot to Living 

mod2 <- lm(AdjSalePrice ~ SqFtTotLiving, data = dat) # bivariate regression model

scatter.smooth(dat$SqFtTotLiving, resid(mod3), # plot a smooth line on the scatter plot
               lpars = list(col = "blue", lwd = 3, lty = 3), 
               main = "Residual Plot (Sale Price ~ Size)",
               xlab = "Total Living Area (sq.ft.)",
               ylab = "Residuals")
abline(h = 0, col = "red")

# BLDG Residuals
mod3 <- lm(AdjSalePrice ~ BldgGrade, data = dat) # bivariate regression model

scatter.smooth(dat$BldgGrade, resid(mod3), # plot a smooth line on the scatter plot
               lpars = list(col = "blue", lwd = 3, lty = 3), 
               main = "Residual Plot (Sale Price ~ Size)",
               xlab = "Building Grade",
               ylab = "Residuals")
abline(h = 0, col = "red")

## BLDG 2 
mod2 <- lm(AdjSalePrice ~ SqFtTotLiving +  
             BldgGrade + ZipGroup, data = dat2)

par(mfrow = c(1,1))
avPlot(mod2, variable = "BldgGrade")

terms <- predict(mod2, type = "terms") # extract the individual regression terms from our model for each observation

partial_resid <- resid(mod2) + terms # add the individual regression terms to the residual for each observation

df <- data.frame(BldgGrade = dat2[, "BldgGrade"], # create a new data.frame of these vals
                 Terms = terms[, "BldgGrade"],
                 PartialResid = partial_resid[, "BldgGrade"])

ggplot(df, aes(BldgGrade, PartialResid)) +
  geom_point(alpha = 0.2) +
  geom_smooth() +
  geom_line(aes(BldgGrade, Terms), colour = "red")

# Squaring BLDG
mod4 <- lm(AdjSalePrice ~ I(BldgGrade^2) + SqFtTotLiving + 
             BldgGrade + ZipGroup, data = dat2)

terms_poly <- predict(mod4, type = "terms") # extract the individual regression terms from our model for each observation

partial_resid_poly <- resid(mod4) + terms_poly # add the individual regression terms to the residual for each observation

df_poly <- data.frame(BldgGrade = dat[, "BldgGrade"], # create a new data.frame of these vals
                      Terms = terms_poly[, "I(BldgGrade^2)"],
                      PartialResid = partial_resid_poly[, "I(BldgGrade^2)"])

ggplot(df_poly, aes(BldgGrade, PartialResid)) +
  geom_point(alpha = 0.2) +
  geom_smooth() +
  geom_line(aes(BldgGrade, Terms), colour = "red")

# SqrftToLiving Residuals
mod3 <- lm(AdjSalePrice ~ SqFtTotLiving, data = dat) # bivariate regression model

scatter.smooth(dat$SqFtTotLiving, resid(mod3), # plot a smooth line on the scatter plot
               lpars = list(col = "blue", lwd = 3, lty = 3), 
               main = "Residual Plot (Sale Price ~ Size)",
               xlab = "Building Grade",
               ylab = "Residuals")
abline(h = 0, col = "red")

## BLDG 2 
mod2 <- lm(AdjSalePrice ~ SqFtTotLiving +  
             BldgGrade + ZipGroup, data = dat2)

par(mfrow = c(1,1))
avPlot(mod2, variable = "SqFtTotLiving")

terms <- predict(mod2, type = "terms") # extract the individual regression terms from our model for each observation

partial_resid <- resid(mod2) + terms # add the individual regression terms to the residual for each observation

df <- data.frame(SqFtTotLiving = dat2[, "SqFtTotLiving"], # create a new data.frame of these vals
                 Terms = terms[, "SqFtTotLiving"],
                 PartialResid = partial_resid[, "SqFtTotLiving"])

ggplot(df, aes(SqFtTotLiving, PartialResid)) +
  geom_point(alpha = 0.2) +
  geom_smooth() +
  geom_line(aes(SqFtTotLiving, Terms), colour = "red")

# Squaring SqrFtL
mod4 <- lm(AdjSalePrice ~ I(SqFtTotLiving^2) + SqFtTotLiving + 
             BldgGrade + ZipGroup, data = dat2)

terms_poly <- predict(mod4, type = "terms") # extract the individual regression terms from our model for each observation

partial_resid_poly <- resid(mod4) + terms_poly # add the individual regression terms to the residual for each observation

df_poly <- data.frame(SqFtTotLiving = dat2[, "SqFtTotLiving"], # create a new data.frame of these vals
                      Terms = terms_poly[, "I(SqFtTotLiving^2)"],
                      PartialResid = partial_resid_poly[, "I(SqFtTotLiving^2)"])

ggplot(df_poly, aes(SqFtTotLiving, PartialResid)) +
  geom_point(alpha = 0.2) +
  geom_smooth() +
  geom_line(aes(SqFtTotLiving, Terms), colour = "red")

## Model Comparison
summary(lm.1)
summary(mod3)
summary(mod6)
summary(mod7)
summary(mod8)

stargazer(lm.1, mod8)


# Various squared models
mod5 <- lm(AdjSalePrice ~ I(Bathrooms^2) + Bathrooms + I(SqFtTotLiving^2) +
          SqFtTotLiving + BldgGrade + ZipGroup, data = dat2)

mod6 <- lm(AdjSalePrice ~ Bathrooms + I(SqFtTotLiving^2) +
              SqFtTotLiving + BldgGrade + ZipGroup, data = dat2)

mod7 <- lm(AdjSalePrice ~ Bathrooms + I(SqFtTotLiving^2) +
            SqFtTotLiving + ZipGroup + BldgGrade +
            I(BldgGrade^2), data = dat2)

Final_Model <- lm(AdjSalePrice ~  I(SqFtTotLiving^2) +
             SqFtTotLiving + ZipGroup + BldgGrade +
             I(BldgGrade^2), data = dat2)

Initial_Model <- lm.1

stargazer(lm.1, lm.2, Final_Model)

plot (d$SqFtTotLiving, d$Bathrooms)
plot (d$SqFtTotLiving, d$Bedrooms)

vif(mod7)

lm.b <- lm(dat$AdjSalePrice ~ dat$BldgGrade
           + dat$SqFtTotLiving +dat$Bathrooms)

# Initial models
summary(lm.a)
summary(lm.b)

stargazer(lm.a)
stargazer(lm.a, lm.2)

mfrow = c(2,2)
plot(lm.b)

# Pres model
mod6 <- lm(AdjSalePrice ~ Bathrooms + I(SqFtTotLiving^2) +
             SqFtTotLiving + BldgGrade + ZipGroup, data = dat2)

summary(lm.2)
summary(Final_Model)
