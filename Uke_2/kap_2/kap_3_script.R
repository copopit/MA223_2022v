#-----------------------------------------------------------#
#                   ---- Libraries ----
#-----------------------------------------------------------#

#-----------------------------------------------------------#
#                   ---- OPPGAVE 1 ----
#-----------------------------------------------------------#
{
#---- (a ----
x_k = c(-1, 0, 3, 5)

y_k = c(3, 5, 9, 7)
data <- c(c(-1, 3), c(0, 5), c(3,9), c(5,7))
dtable = cbind(x_data, y_data)
dtable

n = 4

#---- (b ----
plot(dtable)

#---- (c ----
sum_xy_k = 0
for (i in 1:4) {
  sum_xy_k = sum_xy_k + (x_k[i] * y_k[i])
}
sum_xy_k

SSxy_1 = 0
mean_x = (-1 + 0 + 3 + 5)/n
mean_y = (3 + 5 + 9 + 7)/n
for (i in 1:4) {
  print(i)
  SSxy_1 = SSxy_1 + (x_k[i] - mean_x)*(y_k[i] - mean_y)
}

SSxy_1

# Populasjonsvarians
Oxy_1 = SSxy_1 / n
Oxy_1

# Utvalgssvarians
Sxy_1 = SSxy_1 / (n-1)
Sxy_1


#---- (d ----
RHOxy_1 = cor.test(x_k, y_k)
RHOxy_1

SSx_1 = 0
SSy_1 = 0
for (i in 1:4) {
  SSx_1 = SSx_1 + (x_k[i] - mean_x)^2
  SSy_1 = SSy_1 + (y_k[i] - mean_y)^2
}
Ox_1 = sqrt(SSx_1 / n) 
Oy_1 = sqrt(SSy_1 / n)

RHOxy_1 = Oxy_1 / (Ox_1 * Oy_1)
} # Oppgave 1 slutt
#-----------------------------------------------------------#
#                   ---- OPPGAVE 2 ----
#-----------------------------------------------------------#
{
# Gidder ikke lol
}
#-----------------------------------------------------------#
#                   ---- OPPGAVE 3 ----
#-----------------------------------------------------------#
{
#---- (a ----
{
  
# ----  __Data ----
x_ = c(85, 103, 98, 94, 90, 80, 75, 102, 107, 102)
y_ = c(221.5, 146.1, 262.6, 139.8, 189, 126, 146.5, 221.4, 252.9, 121.4)

n = length(x_)
SSx = 0
SSy = 0
SSxy = 0

m_x = mean(x_)
m_y = mean(y_)
for (k in 1:n) {
  SSxy = SSxy+ ( (x_[k] - m_x) * (y_[k] - m_y) )
  
  SSx = SSx+ ((x_[k] - mean(x_))^2)
  SSy = SSy+ ((y_[k] - mean(y_))^2)
}

# ---- __Utvalg sigma ----
Oxy = SSxy / n

# ---- __Populasjon s ----
Sxy = SSxy / (n - 1)


# ---- __Korrellasjon ----
Ox = sqrt(SSx / n)
Oy = sqrt(SSy / n)

Rhoxy = Oxy / (Ox * Oy)

# ---- __Linær regresjonsline ---- 
X <- cbind(rep(1, n), x_)

# Beta = (X^T * X)^{-1} * X^T * y
Beta = inv(t(X) %*% X) %*% t(X) %*% y_
Beta

# Matrisene
X
t(X)
t(X) %*% X
inv(t(X) %*% X)
t(X) %*% y_
inv(t(X) %*% X) %*% t(X) %*% y_

plot(x_, y_, type="p", col="royalblue")
abline(Beta[1], Beta[2], col="red")

a = Beta[1]
b = Beta[2]

for (i in 1:n) {
  segments(x_[i], y_[i], x_[i], a+b*x_[i], col="orange")  # For hver x-verdi tegner vi en oransj linje fra punktet til linja. Residualene.
}

print("y = 19.0917 + 1.74817x")

# ---- __Standardfeil ----

# y og B er vektorer
# SSe = y^Ty - B^TX^TXB

SSe = (t(y_) %*% y_) - (t(Beta) %*% (t(X) %*% X) %*% Beta)
SSe
S2e = SSe / (n-2)
S2e

} # (a slutt




#---- (b ----
{
# ---- __Data ----
x_ = c(5.65, 15.62, -2.96, 1.29, 3.84)
y_ = c(2.64, 9.03, -1.19, 2.02, -1.82)

n = length(x_)
SSx = 0
SSy = 0
SSxy = 0

m_x = mean(x_)
m_y = mean(y_)
for (k in 1:n) {
  SSxy = SSxy+ ( (x_[k] - m_x) * (y_[k] - m_y) )
  
  SSx = SSx+ ((x_[k] - mean(x_))^2)
  SSy = SSy+ ((y_[k] - mean(y_))^2)
}

# ---- __Utvalg sigma ----
Oxy = SSxy / n

# ---- __Populasjon s ----
Sxy = SSxy / (n - 1)


# ---- __Korrellasjon ----
Ox = sqrt(SSx / n)
Oy = sqrt(SSy / n)

Rhoxy = Oxy / (Ox * Oy)

# ---- __Linær regresjonsline ---- 
X <- cbind(rep(1, n), x_)

# Beta = (X^T * X)^{-1} * X^T * y
Beta = inv(t(X) %*% X) %*% t(X) %*% y_
Beta

# Matrisene
X
t(X)
t(X) %*% X
inv(t(X) %*% X)
t(X) %*% y_
inv(t(X) %*% X) %*% t(X) %*% y_

plot(x_, y_, type="p", col="royalblue")
abline(Beta[1], Beta[2], col="red")

a = Beta[1]
b = Beta[2]

for (i in 1:n) {
  segments(x_[i], y_[i], x_[i], a+b*x_[i], col="orange")  # For hver x-verdi tegner vi en oransj linje fra punktet til linja. Residualene.
}

print("y = 19.0917 + 1.74817x")

# ---- __Standardfeil ----

# y og B er vektorer
# SSe = y^Ty - B^TX^TXB

SSe = (t(y_) %*% y_) - (t(Beta) %*% (t(X) %*% X) %*% Beta)
SSe
S2e = SSe / (n-2)
S2e

} # (b slutt



#---- (c ----
{
# ---- __Data ----
x_ = c(28, 66, 44, 39, 9, 1, 73, 41)
y_ = c(24, 69, 48, 44, 9, 15, 64, 44)

n = length(x_)
SSx = 0
SSy = 0
SSxy = 0

m_x = mean(x_)
m_y = mean(y_)
for (k in 1:n) {
  SSxy = SSxy+ ( (x_[k] - m_x) * (y_[k] - m_y) )
  
  SSx = SSx+ ((x_[k] - mean(x_))^2)
  SSy = SSy+ ((y_[k] - mean(y_))^2)
}

# ---- __Utvalg sigma ----
Oxy = SSxy / n

# ---- __Populasjon s ----
Sxy = SSxy / (n - 1)


# ---- __Korrellasjon ----
Ox = sqrt(SSx / n)
Oy = sqrt(SSy / n)

Rhoxy = Oxy / (Ox * Oy)

# ---- __Linær regresjonsline ---- 
X <- cbind(rep(1, n), x_)

# Beta = (X^T * X)^{-1} * X^T * y
Beta = inv(t(X) %*% X) %*% t(X) %*% y_
Beta

# Matrisene
X
t(X)
t(X) %*% X
inv(t(X) %*% X)
t(X) %*% y_
inv(t(X) %*% X) %*% t(X) %*% y_


plot(x_, y_, type="p", col="royalblue")
abline(Beta[1], Beta[2], col="red")

a = Beta[1]
b = Beta[2]

for (i in 1:n) {
  segments(x_[i], y_[i], x_[i], a+b*x_[i], col="orange")  # For hver x-verdi tegner vi en oransj linje fra punktet til linja. Residualene.
}

print("y = 19.0917 + 1.74817x")

# ---- __Standardfeil ----

# y og B er vektorer
# SSe = y^Ty - B^TX^TXB

SSe = (t(y_) %*% y_) - (t(Beta) %*% (t(X) %*% X) %*% Beta)
SSe
S2e = SSe / (n-2)
S2e

} # (c slutt
} # Oppgave 3 slutt