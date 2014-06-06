# Reanalysis of Jung, K.; Shavitt, S.; Viswanathan, M. & Hilbe, J. M. (2014) Female hurricanes are deadlier than male hurricanes. PNAS, in press.
#
# By Florian Hartig, http://florianhartig.wordpress.com/
#
# Heavily based on previous work by Bob O'Hara, from whom forked this repository
# see Bob's and GrrlScientist's conclusions at http://www.theguardian.com/science/grrlscientist/2014/jun/04/hurricane-gender-name-bias-sexism-statistics

rm(list=ls(all=TRUE))

library(gdata)
library(mgcv)
library(MASS)
library(MuMIn)

# Read in the data
Data=read.xls("http://www.pnas.org/content/suppl/2014/05/30/1402786111.DCSupplemental/pnas.1402786111.sd01.xlsx", nrows=92, as.is=TRUE)
Data$Category=factor(Data$Category)
Data$Gender_MF=factor(Data$Gender_MF)


# Fit and model selection of the full model used in paper with mgcv
originalModelGAM=gam(alldeaths~scale(MasFem)*(scale(Minpressure_Updated.2014)+scale(NDAM)), data=Data, family=negbin(theta=c(0.2,10)), na.action = "na.fail")
summary(originalModelGAM)
#gam.check(originalModelGAM)
dredge(originalModelGAM)[1:5]

# As a test of the new negative binomial in mgcv, I repeated the analysis with glm.mb in MASS
originalModelGLMNB <- glm.nb(alldeaths~scale(MasFem)*(scale(Minpressure_Updated.2014)+scale(NDAM)), data=Data, na.action = "na.fail")
summary(originalModelGLMNB)
#gam.check(originalModelGLMNB)
dredge(originalModelGLMNB)[1:5]

# Comment: parameters are VERY reproducible between the two packes, but AICc aren't ... anyone know why?
# The parameters are quite different from the parameters provided in the SI of the paper though, why?

# In summary, the model selection consistently ranks the full model as best, so one might be tempted to add more parameters, and ass Bob points out the NDAM is an obvious candidate

# Fit and model selection of the model in the paper with added quadratic and sqrt effects 

LargerModel=gam(alldeaths~scale(MasFem)*(scale(Minpressure_Updated.2014)+ scale(sqrt(NDAM))+ scale(NDAM) + scale(NDAM^2)), data=Data, family=negbin(theta=c(0.2,10)), na.action = "na.fail")
#gam.check(LargerModel)
summary(LargerModel)
dredge(LargerModel)[1:10]

# Result : the two models with highest AICc have MasFem removed, but within a Delta AICc of 2 there are quite a few models that have the MasFem still in. 

# Interestingly, the interaction that was carrying the effect always goes away, and there is always a nonlinear term for NDAM. 
# I would conclude that Bob is right in that a part of the effect was partly a fluke, that resulted from a strong interaction that was actually compensating a missing nonlinearity in NDAM.

# However, accoutning for that, there might still be a small effect of MasFem. The AICc is in the possible range, and the estimates are consistently around 0.05

# It was also pointed out by some commentators that a few hurricanes with very large damage may bias the analysis. I therefore removed everything with sqrt(NDAM) > 200

LargerModelOutlier=gam(alldeaths~scale(MasFem)*(scale(Minpressure_Updated.2014)+ scale(sqrt(NDAM))+ scale(NDAM) + scale(NDAM^2)), data=Data[sqrt(Data$NDAM) < 200,], family=negbin(theta=c(0.2,10)), na.action = "na.fail")
summary(LargerModelOutlier)
dredge(LargerModelOutlier)[1:10]

# The results are similar, but put the models with MasFem included yet closer to the best model (Delta AICc of 0.24, which is basically as good as the best model).

# To be conservative, I'm taking this model with the outlier-removed data, and plot the effects. 

possibleModel=gam(alldeaths~ scale(MasFem)*scale(NDAM) + scale(sqrt(NDAM)) , data=Data[sqrt(Data$NDAM) < 200,], family=negbin(theta=c(0.2,10)), na.action = "na.fail")
summary(possibleModel)
#gam.check(possibleModel)
AICc(possibleModel)

newdata = data.frame(MasFem = seq(1,11, length.out = 100), NDAM = rep(max(Data$NDAM), 100))
predictions = predict(possibleModel,newdata = newdata, type="response")
plot(newdata$MasFem, predictions, type = "l", col = 1)
newdata = data.frame(MasFem = seq(1,11, length.out = 100), NDAM = rep(quantile(Data$NDAM, 0.75), 100))
predictions = predict(possibleModel,newdata = newdata, type="response")
lines(predictions, lty = 2, col = 2)
newdata = data.frame(MasFem = seq(1,11, length.out = 100), NDAM = rep(median(Data$NDAM), 100))
predictions = predict(possibleModel,newdata = newdata, type="response")
lines(predictions, lty = 3, col = 3)
newdata = data.frame(MasFem = seq(1,11, length.out = 100), NDAM = rep(min(Data$NDAM), 100))
predictions = predict(possibleModel,newdata = newdata, type="response")
lines(predictions, lty = 4, col = 4)
legend("topleft", c("max", "75%", "median", "min"), lty = 1:4, col = 1:4 )

vis.gam(possibleModel, view=c("MasFem","NDAM"),plot.type="contour",color="terrain", type = "response")

# So yes, IF you believe that this is a plausible model, and IF you believe that this model can also be trusted in the high NDAM area where we have VERY FEW data points, then we would see an effect of MasFem that is of practical relevance. 

# If you believe the model with MasFem is plausible, i.e. one possibility among many, but cannot be trusted for high NDAM (that would be my opinion), the effect of MasFem is relatively small.

# So my conclusion of the analysis would be that it's possible that there is a small effect of MasFem, but there could as well be none. 


