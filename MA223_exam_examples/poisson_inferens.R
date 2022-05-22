library("datasets")
  
data(discoveries)
?discoveries

tid <- seq(1860, 1959, 1)
tid

plot(tid, discoveries, type = "h")

# Posterior poisson prosess

# Setter opp 2 lister med N antall 0'er
kappa_ = rep(0, (length(tid) + 1))
tau_ = rep(0, (length(tid) + 1))

# Velger nøytrale priorer 0, og 0
kappa_[1] = 0
tau_[1] = 0

# Setter opp en loop for å finne alle posterior for all dataen
for (i in 1:length(tid)) {
  kappa_[i] = kappa_[i] + discoveries[i]
  tau_[i] = tau_[i] + 1
}

# Definerer de forskjellige indeksene vi vil se på, legger til +1 fordi lengden på dataen er én lenger pga priorene
obs_1 = 3 +1
obs_2 = obs_1 + 22 +1
obs_3 = obs_2 + 75 +1

xVals = seq(0.2, 1, 0.001)


# ---- Etter 3 observasjoner ----

lambda_1 = dgamma(xVals, kappa_[obs_1], tau_[obs_1])
plot(xVals, lambda_1, type = "l", main="3 Observasjoner")
sprintf("n: %i  t: %i  k: %i  tau: %i",
        discoveries[obs_1], obs_1, kappa_[obs_1], tau_[obs_1])

# ---- Etter 25 observasjoner ----

lambda_2 = dgamma(xVals, kappa_[obs_2], tau_[obs_2])
plot(xVals, lambda_2, type = "l", main="25 Observasjoner")
sprintf("n: %i  t: %i  k: %i  tau: %i",
        discoveries[obs_2], obs_2, kappa_[obs_2], tau_[obs_2])

# ---- Etter 100 observasjoner ----

lambda_3 = dgamma(xVals, kappa_[obs_3], tau_[obs_3])
plot(xVals, lambda_3, type = "l", main="100 Observasjoner")
sprintf("n: %i  t: %i  k: %i  tau: %i",
        discoveries[obs_3], obs_3, kappa_[obs_3], tau_[obs_3])


plot(xVals, lambda_3, col ="blue", type = "l")
lines(xVals, lambda_2, col ="orange", type = "l")
lines(xVals, lambda_1, col ="green", type = "l")


# ---- Finner  P(X = 5) der X = N_5 ----
library("extraDistr")
# Legger til +2 til siste observasjon i tau for å finne for de neste 2 årene, 5 er antallet observasjoner etter de 2 årene
PT_2 = dnbinom(5, kappa_[obs_3], tau_[obs_3] / (tau_[obs_3] + 2))
print(PT_2)
