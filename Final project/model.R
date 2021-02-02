# https://archive.ics.uci.edu/ml/datasets/wine+quality
loc = '~/Google Drive/OnlineCourses/Bayesian Statistics/Techniques and Models/wine/'
red_file = 'winequality-red.csv'
white_file = 'winequality-white.csv'

red = read.csv(paste(loc, red_file, sep=""), header=TRUE, sep= ",")
white = read.csv(paste(loc, white_file, sep=""), header=TRUE, sep= ";")
head(red)
head(white)

dat = red

sum(is.na(dat))

# pairs(dat)

library("corrplot")
Cor = cor(dat)

corrplot(Cor, method='ellipse')
corrplot(Cor, type='lower', method='number', col='black', add=TRUE,
         diag=FALSE, tl.pos='n', cl.pos="n")

full_model = lm(quality ~ ., data=dat)
summary(full_model)

library(MASS)
step_model <- stepAIC(full_model, direction = "both", 
                      trace = FALSE)
summary(step_model)

plot(step_model$fitted.values, step_model$residuals)
plot(step_model$residuals)

library("rjags")



model0_string = " model {
  for (i in 1:length(quality)) {
    quality[i] ~ dnorm(mu[i], prec)
    mu[i] = b[1] + b[2]*volatile.acidity[i] + b[3]*chlorides[i] + 
      b[4]*free.sulfur.dioxide[i] + b[5]*total.sulfur.dioxide[i] + b[6]*pH[i] + b[7]*sulphates[i] +
      b[8]*alcohol[i]
  }
  
  for(j in 1:8) {
    b[j] ~ dnorm(0, 1/1e6)
  }
  
  prec ~ dgamma(1/2, 1/2)
  sig2 = 1/prec
  sig = sqrt(sig2)
} "

data_jags = as.list(dat)

params = c("b", "sig")

mod0 = jags.model(textConnection(model0_string), data=data_jags, n.chains=3)
update(mod0, 1e3)

mod0_sim = coda.samples(mod0, variable.names = params, n.iter=5e5, thin=50)
mod0_csim = as.mcmc(do.call(rbind, mod0_sim))
summary(mod0_sim)


plot(mod0_sim)
gelman.diag(mod0_sim)
autocorr.diag(mod0_sim)
effectiveSize(mod0_sim)
dic0 = dic.samples(mod0, n.iter=1e3)

pm_params = colMeans(mod0_csim)

rel_data = dat[c("volatile.acidity", "chlorides", "free.sulfur.dioxide", "total.sulfur.dioxide",
      "pH", "sulphates", "alcohol")]

yhat = pm_params[1] + data.matrix(rel_data) %*% pm_params[2:8]

resid0 = dat$quality - yhat
plot(resid0)

qqnorm(resid0)
good_red = c(1, 0.35, 0.073, 18, 38, 3.4, 0.87, 13.6)
vodka = c(1, 0, 0.01, 0, 0, 6, 0, 40)
post = mod0_csim[, 1:8] %*% good_red
post_v = mod0_csim[, 1:8] %*% vodka
plot(density(post))
plot(density(post_v))




dic0 = dic.samples(mod0, n.iter=1e3)







model1_string = " model {
  for (i in 1:length(quality)) {
    quality[i] ~ dnorm(mu[i], prec)
    mu[i] = b[1] + b[2]*volatile.acidity[i] + b[3]*alcohol[i]
  }
  
  for(j in 1:3) {
    b[j] ~ dnorm(0, 1/1e6)
  }
  
  prec ~ dgamma(1/2, 1/2)
  sig2 = 1/prec
  sig = sqrt(sig2)
} "

mod1 = jags.model(textConnection(model1_string), data=data_jags, n.chains=3)
update(mod1, 1e3)

mod1_sim = coda.samples(mod1, variable.names = params, n.iter=1e5, thin=50)
mod1_csim = as.mcmc(do.call(rbind, mod1_sim))
summary(mod1_sim)
gelman.diag(mod1_sim)
autocorr.diag(mod1_sim)

dic1 = dic.samples(mod1, n.iter=1e3)




