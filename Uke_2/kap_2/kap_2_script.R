# ----------------------- OPPGAVE 1 ------------------------- 

# 1a
postNr = c(2443, 6020, 7374, 8092)
Fantall = c(24, 6, 19, 1)
FFFF = cbind(postNr, Fantall)
FFFF

print("Typetall: 2443 -> Engerdal")
rep(FFFF)
meanA = mean(rep(postNr, Fantall))
meanA
medianA = median(rep(postNr, Fantall))
medianA
print("Typetall passer best")



# 1b
inntekt = c(1:50)

for (x in 1:50) {
  if (x <= 5) {
    inntekt[x] = 0
  } else if (x > 5) {
    inntekt[x] = (340000 + ((x - 6) * 1000))
  }
}

inntekt[47] = 613000
inntekt[48] = 727000
inntekt[49] = 958000
inntekt[50] = 70000000

inntekt

print("Typetall: 0")
meanI1 = mean(inntekt)
medianI1 = median(inntekt)
meanI1
medianI1
print("Median passer best")


# 1c
inntekt2 = c(1:100)

for (x in 1:100) {
  if (x <= 51) {
    inntekt2[x] = 0
  } else if (x > 51 && x <= 75 ) {
    inntekt2[x] = 312000
  } else {
    inntekt2[x] = 478000
  }
}

inntekt2
print("Typetall: 0")
medianC = median(inntekt2)
meanC = mean(inntekt2)
medianC
meanC
print("Gjennomsnitt passer best")


# ----------------------- OPPGAVE 2 ------------------------- 
vokaler = c('a', 'e', 'i', 'o', 'u', 'y', 'æ', 'ø', 'å')
vokVerdier = c(1, 5, 9, 15, 21, 25, 27, 28, 29)

meanVok = mean(vokVerdier)
meanVok

medianVok = median(vokVerdier)
medianVok

n = length(vokVerdier)
Sx = 0

for (x in vokVerdier) {
  Sx = Sx + (x^2)
}

Sx

SSx <- Sx - n * meanVok^2
popStdAvvik_a = sqrt(SSx / (n))
popStdAvvik_a


# ----------------------- OPPGAVE 3 ------------------------- 

upper = 0.75
lower = 0.25

# _________ 3a 

aData = c(-1, -3, 4)
aData

sort(aData)
median(aData)
upper_a = quantile(aData, upper, type=6)
lower_a = quantile(aData, lower, type=6)
upper_a - lower_a

mean(aData)

n_a = length(aData)
Sa = 0

for (x in aData) {
  Sa = Sa + (x^2)
}

SSa = Sa - n_a * mean(aData)^2
sqrt(SSa/(n_a - 1))


# _________ 3b

bData = c(-0.2, 9.6, -0.1, 11.1, 1.3, -0.2, 11.1, -0.8, 0.4)
bData

sort(bData)
median(bData)
upper_b = quantile(bData, upper, type=6)
lower_b = quantile(bData, lower, type=6)
upper_b - lower_b

mean(bData)

n_b = length(bData)
Sb = 0

for (x in bData) {
  Sb = Sb + (x^2)
}

SSb = Sb - n_b * mean(bData)^2
sqrt(SSb/(n_b - 1))


# _________ 3c

cData = c(60, 66, 70, 103, 138, 34)
cData

sort(cData)
median(cData)
upper_c = quantile(cData, upper, type=6)
lower_c = quantile(cData, lower, type=6)
upper_c - lower_c

mean(cData)

n_c = length(cData)
Sc = 0

for (x in cData) {
  Sc = Sc + (x^2)
}

SSc = Sc - n_c * mean(cData)^2
sqrt(SSc/(n_c - 1))


# _________ 3d

dData = c(0.97149, 0.65964, 0.34581, 0.51590, 0.92881)
dData

sort(dData)
median(dData)
upper_d = quantile(dData, upper, type=6)
lower_d = quantile(dData, lower, type=6)
upper_d - lower_d

mean(dData)

n_d = length(dData)
Sd = 0

for (x in dData) {
  Sd = Sd + (x^2)
}

SSd = Sd - n_d * mean(dData)^2
sqrt(SSd/(n_d - 1))

# ----------------------- OPPGAVE 4

