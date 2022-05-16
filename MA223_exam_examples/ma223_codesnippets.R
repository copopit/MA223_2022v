
# Misc -------------------------------------------------------------------

# Dummydata show how to use examples
data_x = c(0); data_y = c(0); n = 1

# List of OK colors to use in case more than a few are needed
colors = c("#400043","#B03060","#0005a9","#8fffb4","#a94bfc","#ffd56e","#938aff","#005524","#d40032","#01bca1","#b3005f","#4dd4ff","#713700","#ffb6f2","#00333c","#ffb39b")


# Libraries
library("datasets")  # For datasets
library("readxl")    # Lese excel filer
library(metRology)   # t-fordeling, beta binom, negativ beta binom
library(extraDistr)  # gamma-gamma-fordeling
library(matlib)
library(car)

# 1 Weighted measures / vekttall ------------------------------------------

test = dbbinom(1, 2, 3, 4)

# * 1.1 Varians -----------------------------------------------------------

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

# Utvalgss avvik
s_x = sqrt(utv_var)      # s_x

# Populasjons kovarians for x, y
sigma_xy = SSxy / n

# Utvalgs kovarians
s_xy = SSxy / (n - 1)

# Korrellasjon
rho_xy = sima_xy / sigma_x * sigma_y  #  sigma_y = sigma_x for y verdier

r_xy = s_xy / s_x * s_y               #  s_y = s_x for y verdier
  



# * 1.2 Annet -------------------------------------------------------------


