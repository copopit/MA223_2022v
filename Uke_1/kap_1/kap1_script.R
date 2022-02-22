# ------------------ Oppgave 1 ------------------------ #

kronmynt <- data.frame(
  '1' = c('M', 'M', 'M', 'K', 'K', 'K', 'K', 'M'),
  '2' = c('M', 'M', 'K', 'K', 'K', 'M', 'M', 'K'),
  '3' = c('M', 'K', 'K', 'K', 'M', 'M', 'K', 'M')
)

kronmynt

counter = 0;

for (i in 1:8) {
  mcounter = 0;
  kcounter = 0;
  
  for (col in 1:3) {
      if (kronmynt[i, col] == 'M') {
        mcounter = mcounter + 1;
      } else if (kronmynt[i, col] == 'K') {
        kcounter = kcounter + 1;
      }
  }
  
  if (mcounter == 2 && kcounter == 1) {
    counter = counter + 1;
  }
}

counter


# ------------------ Oppgave 2 ------------------------ #

threes = 0;
sevens = 0;

for (i in 1:6) {
  for (j in 1:6) {
    if ((i + j) == 3) {
      threes = threes + 1;
    }
    if ((i + j) == 7) {
      sevens = sevens + 1;
    }
  }
}

threes
sevens

# Sjansen for å slå 1 på første terning, og 2 på andre er 1/6 * 1/6
# Derfor blir den totale sjansen 1/6^2 + 1/6^2 siden det er totalt 2 muligheter for en sum på 3

threes_chance = (1/6)^2 * threes
threes_chance # 1/18


# Samme scenario her, bare her så har antallet muligheter for å få summen økt fra 2 til 6
sevens_chance = (1/6)^2 * sevens
sevens_chance # 1/6

