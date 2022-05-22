
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

# Utvalgs avvik
s_x = sqrt(utv_var)      # s_x

# Populasjons kovarians for x, y
sigma_xy = SSxy / n

# Utvalgs kovarians
s_xy = SSxy / (n - 1)

# Korrellasjon
rho_xy = sima_xy / sigma_x * sigma_y  #  sigma_y = sigma_x for y verdier

r_xy = s_xy / s_x * s_y               #  s_y = s_x for y verdier
  



# Processes -------------------------------------------------------------

# * Bernoulli Process -- Eksamen desember 2021 --------------------------


# ** a) Ligninggenerator ----
# Nyberg har laget en generator for tredjegradsligninger.
# Sannsynligheten for at Sebastian klarer å løse en tilfeldig generert ligning derfra
# er 3/8. Sebastian henter 5 tredjegradsligninger fra generatoren.

# Sannsynligheten for at Sebastian klarer å løse en 3. grads ligning
p = 3/8

# Antall 3. grads ligninger som Sebastian trekker
k = 5

# *** i.  Sannsynlighet for å presis løse 2 av 3. grads ligningene ----
K_n = dbinom(2, k, p)

# *** ii. Sannsynlighet for å løse 2 eller flere av ligningene ----
to_eller_flere = 1 - pbinom(1, k, p)

# *** iii. Ber om nye ligninger helt til han klarer å løse 2 av dem ----
# Y ~nb(2, 3/8)(x) Y = dbnbinom(, 2, p) 

  
# ** b) Avløpsvann ----
# Du har fått i sommerjobb å passe på rensing av avløpsvann i en storby. I dag
# ser du spesielt etter gummiprodukter, og erfaringen fra tidligere år tilsier at
# det i snitt er 0.017 gummiprodukter per kubikkmeter rensevann.

lambda = 0.017

# *** i. Hvilken prosess er den riktige til denne problemstillingen? ----
# Poisson

# *** ii. Sannsynlighetsfordelingen for mengden rensevann for å få 10 gummiprodukter ----
# T ~gamma(10, 0.017)(t)

# *** iii. Skisser forderling i punnk ii. og marker punktestimatene ----
k = 10
xs = seq(0, 1500, 1)
expectedVal = k/lambda
medianVann = qgamma(0.5, k, lambda)
modeVann = (k - 1) / lambda

ti_gummiprodukter = dgamma(xs, k, lambda)
plot(xs, ti_gummiprodukter, type="l")
abline(v = expectedVal, col ="maroon")
abline(v = medianVann, col="blue")
abline(v = modeVann, col="purple")


# *** iv. Sannsynlighetsfordelingen for antall gummiprodukter i 100 kubikkmeter med  ----
# pois ~(0.017, 100)(x)

# * Poisson Process --  ----------------------------

# * Gaussian Process --  ---------------------------
