library(extraDistr)
library(metRology)

#_____________ normal fordeling ________________
x_ene <- seq(0, 1000, 10)
x_ene_icdf <- seq(0, 1, 0.1)
mu <- 500
sigma <- 150

y_ene_pdf <- dnorm(x_ene, mu, sigma) #pdf
y_ene_CDF <- pnorm(x_ene, mu, sigma) #CDF
y_ene_iCDF <- qnorm(x_ene_icdf, mu, sigma) #iCDF

plot(x_ene, y_ene_pdf, type = 'l')
plot(x_ene, y_ene_CDF, type = 'l')
plot(x_ene_icdf, y_ene_iCDF, type = 'l')


#_____________ student t fordeling _____________
x_ene <- seq(0, 1000, 10)
x_ene_icdf <- seq(0, 1, 0.1)
nu <- 10
mu <- 500
sigma <- 200
p <- 12

y_ene_pdf <- dt.scaled(x_ene, nu, mu, sigma) #pdf
y_ene_CDF <- pt.scaled(x_ene, nu, mu, sigma) #CDF
y_ene_iCDF <- qt.scaled(p, nu, mu, sigma) #iCDF

plot(x_ene, y_ene_pdf, type = 'l')
plot(x_ene, y_ene_CDF, type = 'l')
plot(x_ene_icdf, y_ene_iCDF, type = 'l')


#_____________ gamma fordeling _________________
x_ene <- seq(0, 1500, 10)
x_ene_icdf <- seq(0, 1, 0.1)
k <- 10
lambda <- 0.017

y_ene_pdf <- dgamma(x_ene, k, lambda) #pdf
y_ene_CDF <- pgamma(x_ene, k, lambda) #CDF
y_ene_iCDF <- qgamma(p, k, lambda) #iCDF


plot(x_ene, y_ene_pdf, type = 'l')
plot(x_ene, y_ene_CDF, type = 'l')
plot(x_ene_icdf, y_ene_iCDF, type = 'l')


#_____________ beta fordeling __________________
x_ene <- seq(-1, 1, 0.001)
x_ene_icdf <- seq(0, 1, 0.1)
a <- 1
b <- 5

y_ene_pdf <- dbeta(x_ene, a, b)
y_ene_CDF <- pbeta(x_ene, a, b)
y_ene_iCDF <- qbeta(x_ene_icdf, a, b)

plot(x_ene, y_ene_pdf, type = 'l')
plot(x_ene, y_ene_CDF, type = 'l')
plot(x_ene_icdf, y_ene_iCDF, type = 'l')


#_____________ binomisk fordeling _____________
x_ene <- seq(0, 15, 1)
x_ene_icdf <- seq(0, 1, 0.1)
n <- 15
p <- 0.2

y_ene_pdf <- dbinom(x_ene, n, p)
y_ene_CDF <- pbinom(x_ene, n, p)
y_ene_iCDF <- qbinom(x_ene_icdf, n, p)

plot(x_ene, y_ene_pdf, type = 'h')
plot(x_ene, y_ene_CDF, type = 's')
plot(x_ene_icdf, y_ene_iCDF, type = 's')


#____________ poisson fordeling__________________

lambda = 3.6
x = 0:10
x_iCDF = seq(0, 1, 0.1)


yVals_pdf=dpois(x, lambda)
yVals_CDF=ppois(x, lambda)
yVals_iCDF = qpois(x_iCDF, lambda)

plot(x,yVals_pdf,col="maroon",type="h")
plot(x,yVals_CDF,col="maroon",type="s")
plot(x_iCDF,yVals_iCDF,col="maroon",type="s")


# $ X \thicksim pois_{\lamda}(x) $
# $ X \thicksim POIS_{\lamda}(x) $
# $ X \thicksim POIS_{\lamda}^{-1}(x) $


#____________ negativ binomisk fordeling _____________

k = 3
p = 0.4
x = 0:20
x_iCDF = seq(0, 1, 0.1)

yVals_pdf = dnbinom(x, k, p)
yVals_CDF = pnbinom(x, k, p)
yVals_iCDF = qnbinom(x_iCDF, k, p)

plot(x, yVals_pdf, col="maroon", type="h")
plot(x, yVals_CDF, col="maroon", type="s")
plot(x_iCDF, yVals_iCDF, col="maroon", type="s")

# $ X \thicksim nb_{(k, p)}(x) $
# $ X \thicksim NB_{(k, p)}(x) $
# $ X \thicksim NB_{(k, p)}^{-1}(x) $


#____________beta binomisk fordeling ___________

a = 3
b = 5
n = 8
x = 0:9
p = seq(0, 1, 0.1) # iCDF

yVals_pdf = dbbinom(x, n, a, b)
yVals_CDF = pbbinom(x, n, a, b)
yVals_iCDF = qbetabinom(p, n, a, b)

plot(x, yVals_pdf, col="maroon", type="h")
plot(x, yVals_CDF, col="maroon", type="s")
plot(p, yVals_iCDF, col="maroon", type="s")

# $ X \thicksim \beta b_{(a, b, n)}(x)$
# $ X \thicksim BB_{(a, b, n)}(x)$
# $ X \thicksim BB_{(a,b,b)}^{-1} (x)$  

#________________ beta negativ binomisk fordeling_____________

a = 3
b = 5
k = 8
x = 0:9

yVals_pdf = dbnbinom(x, k, a, b)
yVals_CDF = pbnbinom(x, k, a, b)

plot(x, yVals_pdf, col="maroon", type="h")
plot(x, yVals_CDF, col="maroon", type="s")


# $ X \thicksim \beta nb_{(a, b, n)}(x)$
# $ X \thicksim BNB_{(a, b, n)}(x)$
# $ X \thicksim BNB_{(a,b,b)}^{-1} (x)$ 

#_____________ Fischer-Snedecor (F) fordeling _____________
x_ene = seq(0, 2.5, 0.001)

alpha <- 150
beta <- 45

expected <- beta/(beta - 2) # mu T
mode <- (beta * (alpha - 2)) / (alpha * (beta + 2)) # T_max
median <- qf(0.5, alpha, beta) # ~T
variance <- (2 * beta^2 * (alpha + beta -2)) / (alpha * (beta - 4) * (beta - 2)^2) # sigma_T^2

y_ene_pdf = df(x_ene, alpha, beta)
y_ene_cdf = pf(x_ene, alpha, beta)
y_ene_icdf = qf(x_ene, alpha, beta)

plot(x_ene, y_ene_pdf, col="maroon", type="l")
plot(x_ene, y_ene_cdf, type="l", col="blue")
plot(x_ene*(x_ene<=1), y_ene_icdf, type="l", col="purple")

# Rmd / LaTeX
# PDF $T \thicksim f_{(k, \kappa, \tau)}(t)$
# CDF $T \thicksim F_{(k, \kappa, \tau)}(t)$
# iCDF $T \thicksim F^{-1}_{(k, \kappa, \tau)}(t)$


#_____________ Gamma-gamma _____________
x_ene = seq(0, 16, 0.01)

k <- 10
kappa <- 140
tau <- 50

expected <- (k * tau) / (kappa - 1) # mu T
mode <- ((k - 1) * tau) / (kappa + 1) # T_max
median <- qbetapr(0.5, k, kappa, tau) # ~T
variance <- (k * tau^2 * (kappa + k -1)) / ((kappa - 2)*(kappa - 1)^2) # sigma_T^2

y_ene_pdf = dbetapr(x_ene, k, kappa, tau)
y_ene_cdf = pbetapr(x_ene, k, kappa, tau)
y_ene_icdf = qbetapr(x_ene*(x_ene<=1), k, kappa, tau)

plot(x_ene, y_ene_pdf, col="maroon", type="l")
plot(x_ene, y_ene_cdf, type="l", col="blue")
plot(x_ene*(x_ene<=1), y_ene_icdf, type="l", col="purple")

# Rmd / LaTeX
# PDF $T \thicksim g\gamma_{(k, \kappa, \tau)}(t)$
# CDF $T \thicksim G\Gamma_{(k, \kappa, \tau)}(t)$
# iCDF $T \thicksim G\Gamma^{-1}_{(k, \kappa, \tau)}(t)$
