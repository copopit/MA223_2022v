
# Misc -------------------------------------------------------------------

# * Libraries ----
library("datasets")  # For datasets
library("readxl")    # Lese excel filer
library(metRology)   # t-fordeling, beta binom, negativ beta binom
library(extraDistr)  # gamma-gamma-fordeling
library(matlib)
library(car)

# Dummydata show how to use examples
data_x = c(0); data_y = c(0); n = 1

# List of OK colors to use in case more than a few are needed ----
colors = c("#400043","#B03060","#0005a9","#8fffb4","#a94bfc","#ffd56e","#938aff","#005524","#d40032","#01bca1","#b3005f","#4dd4ff","#713700","#ffb6f2","#00333c","#ffb39b")


# * Highlight interval under graph ----
yVals=dbetapr(xVals,2,6,19.3)
plot(xVals,yVals,type="l",col="maroon",lwd=2)
lines(xVals,yVals*(xVals<5),type="h",col="yellow")

# Draw the graph again as a line to fix outline
lines(xVals,yVals,type="l",col="maroon",lwd=2)

# * Adding legend graph to show what each line is. https://r-coder.com/add-legend-r/ ----
legend(x = "topright", 
       title = "Title",
       legend = c("graph", "interval"), 
       # 1 = full, 2 = lines, 3 = dotted 
       lty = c(1, 1), 
       col = c("maroon", "yellow"), 
       lwd = 4,
       # Change legend scale
       cex = 2)


# * Interval estimates ----
xVals = seq(2.5, 3.7, 0.01)

tau = 100
kappa = 310

# List of percentiles to show the interval for
percentiles = c(0.5, 0.7)

yValsCDFlong=abs(2*pgamma(xVals, kappa, tau) - 1)
yValsPDF= dgamma(xVals, kappa, tau)
plot(xVals, yValsCDFlong, type="l", ylab = "Intervall", xlab="Oppdagelser")

# Loop through percentiles and add a segment for each interval
for (i in 1:length(percentiles)) {
  bottom = (1 - percentiles[i])/2
  top = 1 - bottom
  
  L = qgamma(bottom, k_[101],tau_[101])
  R = qgamma(top, k_[101],tau_[101])
  
  segments(L, percentiles[i], R, percentiles[i], col=colors[i],lwd=3)
}



# 1.1 Varians -----------------------------------------------------------

# SSx
SSx = sum(data^2) - (sum(data)^2 / length(data))

# SSxy
SSxy = sum(data * data_y) - (sum(data) * sum(data_y) / length(data))

# Populasjons varians
sigma_x2 = SSx / n       #  sigma^2_x

# Populasjons avvik
sigma_x = sqrt(pop_var)  #  sigma_x

# Utvalgs varians
s_x2 = SSx / (n - 1)     #  s^2_x

# Utvalgs avvik
s_x = sqrt(utv_var)      # s_x

# Populasjons kovarians for x, y
sigma_xy = SSxy / n

# Utvalgs kovarians
s_xy = SSxy / (n - 1)

# Korrellasjon
rho_xy = sima_xy / sigma_x * sigma_y  #  sigma_y = sigma_x for y verdier

r_xy = s_xy / s_x * s_y               #  s_y = s_x for y verdier

