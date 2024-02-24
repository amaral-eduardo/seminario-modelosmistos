# rm(list=ls())
# cat('\14')

class<-nlmeU::SIIdata
attach(class)
library(nlme)

# Model 4.1.
model4.1.fit <- lme(mathgain ~ 1, random = ~ 1 | schoolid/classid,
                    class, method = "REML") # Wilkinson–Rogers notation
# ou
model <- lme(mathgain ~ 1, random =list( ~ 1 | schoolid, ~ 1 | classid),
             class, method = "REML")


VarCorr(model4.1.fit)
VarCorr(model)


ef <- random.effects(model4.1.fit)
head(ef[[1]])


ef2 <- random.effects(model)
head(ef2[[1]])

head(ef[[2]])

head(ef2[[2]])

#########################

# Model 4.1A.
model4.1A.fit <- lme(mathgain ~ 1, random = ~1 | schoolid,
                     data = class, method = "REML")

VarCorr(model4.1.fit)

VarCorr(model4.1A.fit)

(ob<- anova(model4.1.fit, model4.1A.fit))

0.5*pchisq(ob[2,8], df = 0, lower.tail = F) + 0.5*pchisq(ob[2,8], df = 1, lower.tail = F)

###########################

# Model 4.2.
model4.2.fit <- lme(mathgain ~ mathkind + sex + minority + ses,
                    random = ~1 | schoolid/classid, class,
                    na.action = "na.omit", method = "REML")
summary(model4.2.fit)$tTable


# Model 4.1: ML estimation with lme().
model4.1.ml.fit <- lme(mathgain ~ 1, random = ~1 | schoolid/classid,
                       class, method = "ML")
# Model 4.2: ML estimation with lme().
model4.2.ml.fit <- lme(mathgain ~ mathkind + sex + minority + ses,
                       random = ~1 | schoolid/classid, class,
                       na.action = "na.omit", method = "ML")
(ob<- anova(model4.1.ml.fit, model4.2.ml.fit))

###################

# Model 4.3.
model4.3.fit <- update(model4.2.fit, fixed =
                         ~ mathkind + sex + minority + ses + 
                         yearstea + mathprep + mathknow)

summary(model4.3.fit)$tTable

#####################

# Model 4.4.
model4.4.fit <- update(model4.2.fit, fixed = ~ mathkind + 
                         sex + minority+ ses + housepov)

summary(model4.4.fit)$tTable
anova(model4.2.fit, model4.4.fit )


model4.4.ml.fit <- update(model4.2.fit, fixed = ~ mathkind + sex + minority+ ses +
                            housepov, method = "ML")
summary(model4.4.fit)$tTable
anova(model4.2.ml.fit, model4.4.ml.fit )

 
