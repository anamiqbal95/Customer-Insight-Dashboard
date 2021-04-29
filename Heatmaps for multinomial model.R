rm(list = ls())
cat("\014")

## model code ##
{
load("datTL.rda", verbose=T)

head(datTL)
colnames(datTL)

datTL$Resp.new <- relevel(datTL$Resp.new, ref="Pr.p.only")
datTL$Telicity <- relevel(datTL$Telicity, ref = "telic")
datTL$Aktionsart <- relevel(datTL$Aktionsart, ref = "achievement")

datTL$Relevance.type <- relevel(datTL$Relevance.type,
                                ref="Continuative")

levels(datTL$Resp.new)

table(datTL$Resp.new, exclude=TRUE)

#####################################################################
## Now we need to fit a multinomial model (as there are more than 2
## levels of the response variable).
#####################################################################

library(nnet)

#### response ~ predictors
datTL.mnm1 <- multinom(Resp.new ~ as.vector(Proficiency.c) +
                         Relevance.type +
                         Telicity,
                       data = datTL,
                       Hess = TRUE)
## Hessian saves info to facilitate the calcuation of standard errors

summary(datTL.mnm1, Wald=TRUE)

datTL.mnm2 <- multinom(Resp.new ~ as.vector(Proficiency.c) +
                         Relevance.type*Telicity +
                         Telicity,
                       data = datTL,
                       Hess = TRUE)

summary(datTL.mnm2, Wald=TRUE)

anova(datTL.mnm1, datTL.mnm2)

## create the z values
(z <- summary(datTL.mnm1)$coefficients/summary(datTL.mnm1)$standard.errors)

## obtain the p values associated with the coefficients
(p <- (1 - pnorm(abs(z), 0, 1)) * 2)
}

## new code ##
# Two separate plots for model datTL.mnm1 & datTL.mnm2, to see to which verb type of Resp.new 
# the participants used interacting with the other variables : Telicity (telic vs. atelic), 
# Relevance.type (continuative vs. Recent.past), and proficiency.c.
{
# install.packages("data.table")
# install.packages("ggplot2")
# install.packages("plot.matrix")
library(data.table)
library(ggplot2)
library('plot.matrix') 
library(dplyr)
  
exp(coef(datTL.mnm1))

head(pp <- fitted(datTL.mnm1))

data_filtered <- data.frame(Resp.new = datTL$Resp.new,
                            Proficiency.c = as.numeric(datTL$Proficiency.c), 
                            Relevance.type = datTL$Relevance.type, 
                            Telicity = datTL$Telicity
                            )

summary(data_filtered)

## plot for model 1 ##
{
## store the predicted probabilities for each value of predictor variables
pp.write <- cbind(data_filtered, predict(datTL.mnm1, newdata = data_filtered, type = "probs", se = TRUE))

Model1_Resp.new_Relevance.type <- do.call("rbind",by(pp.write[, 5:10], pp.write$Relevance.type, colMeans))
par(mar=c(5.1, 4.1, 4.1, 4.1)) # adapt margins
plot(Model1_Resp.new_Relevance.type, breaks = 7, ylab="Relevance.type", xlab="Resp.new")

Model1_Resp.new_Telicity <- do.call("rbind",by(pp.write[, 5:10], pp.write$Telicity, colMeans))
par(mar=c(5.1, 4.1, 4.1, 4.1)) # adapt margins
plot(Model1_Resp.new_Telicity, breaks = 7, ylab="Telicity", xlab="Resp.new")

Model1_Resp.new_Proficiency.c <- do.call("rbind",by(pp.write[, 5:10], round(pp.write$Proficiency.c, digits=2), colMeans))
par(mar=c(5.1, 4.1, 4.1, 4.1)) # adapt margins
plot(Model1_Resp.new_Proficiency.c, breaks = 4, ylab="Proficiency.c", xlab="Resp.new")

}

## plot for model 2 ##
{
## store the predicted probabilities for each value of predictor variables
pp.write2 <- cbind(data_filtered, predict(datTL.mnm2, newdata = data_filtered, type = "probs", se = TRUE))

Model2_Resp.new_Relevance.type <- do.call("rbind",by(pp.write2[, 5:10], pp.write2$Relevance.type, colMeans))
par(mar=c(5.1, 4.1, 4.1, 4.1)) # adapt margins
plot(Model2_Resp.new_Relevance.type, col=topo.colors,breaks = 7, ylab="Relevance.type", xlab="Resp.new")

Model2_Resp.new_Telicity <- do.call("rbind",by(pp.write2[, 5:10], pp.write2$Telicity, colMeans))
par(mar=c(5.1, 4.1, 4.1, 4.1)) # adapt margins
plot(Model2_Resp.new_Telicity, breaks = 7, col=topo.colors,ylab="Telicity", xlab="Resp.new")

Model2_Resp.new_Proficiency.c <- do.call("rbind",by(pp.write2[, 5:10], round(pp.write2$Proficiency.c, digits=2), colMeans))
par(mar=c(5.1, 4.1, 4.1, 4.1)) # adapt margins
plot(Model2_Resp.new_Proficiency.c, breaks = 4, col=topo.colors, ylab="Proficiency.c", xlab="Resp.new")

ggplot(pp.write2, aes(x = Telicity, y = Relevance.type, fill = Resp.new)) +
  geom_tile()
}


}
