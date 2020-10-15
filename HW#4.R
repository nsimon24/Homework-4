load("acs2017_ny_data.Rdata")
attach(acs2017_ny)
use_varb <- (AGE >= 25) & (AGE <= 55) & (LABFORCE == 2) & (WKSWORK2 > 4) & (UHRSWORK >= 35)
dat_use <- subset(acs2017_ny,use_varb) # 
summary (dat_use)
detach()
attach(dat_use)

model_temp1 <- lm(INCWAGE ~ AGE + female + AfAm + Asian + Amindian + race_oth + Hispanic + educ_hs + educ_somecoll + educ_college + educ_advdeg)
summary(model_temp1)

par(mfrow=c(2,2))
plot(model_temp1,col="red",pch=16,cex=1,lwd=1,lty=2)

require(stargazer)
stargazer(model_temp1, type = "text")

nAmindian<-as.numeric(as.character(dat_use$INCWAGE))
par(mfrow=c(2,2))
Wage_Amindian<-lm(INCWAGE~Amindian)
plot(Wage_Amindian,col="green",pch=14,cex=1,lwd=1,lty=2)
summary(Wage_Amindian)

nHispanic<-as.numeric(as.character(dat_use$INCWAGE))
par(mfrow=c(2,2))
Wage_Hispanic<-lm(INCWAGE~Hispanic)
plot(Wage_Hispanic,col="purple",pch=14,cex=1,lwd=1,lty=2)
summary(Wage_Hispanic)


require(AER)
NNobs <- length(INCWAGE)
set.seed(12345) 
graph_obs <- (runif(NNobs) < 0.1)
dat_graph <-subset(dat_use,graph_obs) 

plot(INCWAGE ~ jitter(AGE, factor = 2), pch = 16, col = rgb(0.5, 0.5, 0.5, alpha = 0.2), data = dat_graph)

plot(INCWAGE ~ jitter(AGE, factor = 2), pch = 16, col = rgb(0.5, 0.5, 0.5, alpha = 0.2), ylim = c(0,150000), data = dat_graph)

to_be_predicted2 <- data.frame(AGE = 25:55, female = 1, AfAm = 0, Asian = 0, Amindian = 1, race_oth = 1, Hispanic = 1, educ_hs = 0, educ_somecoll = 0, educ_college = 1, educ_advdeg = 0)
to_be_predicted2$yhat <- predict(model_temp1, newdata = to_be_predicted2)

lines(yhat ~ AGE, data = to_be_predicted2)


NNobs <- length(INCWAGE)
set.seed(12345)
graph_obs <- (runif(NNobs) < 0.1)
dat_graph <-subset(dat_use,graph_obs)
plot(INCWAGE ~ jitter(AGE, factor = 2), pch = 16, col = rgb(0.5, 0.5, 0.5, alpha = 0.2), ylim = c(0,150000), data = dat_graph)
to_be_predicted2 <- data.frame(AGE = 25:55, female = 1, AfAm = 0, Asian = 0, Amindian = 1, race_oth = 1, Hispanic = 1, educ_hs = 0, educ_somecoll = 0, educ_college = 1, educ_advdeg = 0)
to_be_predicted2$yhat <- predict(model_temp1, newdata = to_be_predicted2)
lines(yhat ~ AGE, data = to_be_predicted2)


NNNobs <- length(INCTOT)
set.seed(12345)
graph_obs <- (runif(NNobs) < 0.1)
dat_graph <-subset(dat_use,graph_obs)
plot(INCTOT ~ jitter(AGE, factor = 2), pch = 16, col = rgb(0.5, 0.5, 0.5, alpha = 0.2), ylim = c(0,150000), data = dat_graph)
to_be_predicted4 <- data.frame(AGE = 25:55, educ_hs = 0, educ_somecoll = 0)
to_be_predicted4$yhat <- predict(model_temp1, newdata = to_be_predicted2)
lines(yhat ~ AGE, data = to_be_predicted2)







NNobs <- length(INCTOT)
set.seed(12345)
graph_obs <- (runif(NNobs) < 0.1)
dat_graph <-subset(dat_use,graph_obs)
plot(INCTOT ~ jitter(AGE, factor = 2), pch = 16, col = rgb(0.5, 0.5, 0.5, alpha = 0.2), data = dat_graph)

plot(INCTOT ~ jitter(AGE, factor = 2), pch = 16, col = rgb(0.5, 0.5, 0.5, alpha = 0.2), ylim = c(0,150000), data = dat_graph)

to_be_predicted2 <- data.frame(AGE = 25:55, female = 1, AfAm = 0, Asian = 0, Amindian = 1, race_oth = 1, Hispanic = 1, educ_hs = 0, educ_somecoll = 0, educ_college = 1, educ_advdeg = 0)
to_be_predicted2$yhat <- predict(model_temp1, newdata = to_be_predicted2)

lines(yhat ~ AGE, data = to_be_predicted2)
summary(to_be_predicted2)
summary(INCTOT)
summary(NNobs)