#funkcja porownania
interpretuj_wynik <- function(p_value) {
  if (p_value >= 0.05) {
    return("Nie odrzucamy na poziomie istotności 5%: Nie mamy dowodów przeciwko H_0.")
  } else if (p_value >= 0.01) {
    return("Odrzucamy na poziomie istotności 5%: Mamy dowody przeciwko H_0.")
  } else if (p_value >= 0.001) {
    return("Odrzucamy na poziomie istotności 1%: Mamy mocne dowody przeciwko H_0.")
  } else {
    return("Odrzucamy na poziomie istotności 0,1%: Mamy bardzo mocne dowody przeciwko H_0.")
  }
}
#----------------#
#       1        #
#----------------#

# i - prof 
# liczba prób
k<-3
# próby
c1<-c(68,80,74,62)
c2<-c(85,67,79,73)
c3<-c(60,66,57)
cw<-c(c1,c2,c3)
# liczności
ng<-c(length(c1),length(c2),length(c3))
# liczba wszystkich obserwacji
n<-length(cw)
#średnie z prób
sr<-c(mean(c1),mean(c2),mean(c3))
sw<-mean(cw)
# całkowita suma kwadratów
(csk<-sum((cw-sw)^2))
# suma kwadratów pomiędzy grupami
(msk<-sum((sr-sw)^2*ng))
# suma kwadratow wewnatrz grup
c1w<-sum((c1-sr[1])^2)
c2w<-sum((c2-sr[2])^2)
c3w<-sum((c3-sr[3])^2)
(wsk<-c1w+c2w+c3w)
# średnia suma kwadratów pomiędzy grupami
(smsk<-msk/(k-1))
# średnia suma kwadratów wewnątrz grup
(swsk<-wsk/(n-k))
# realizacja statystyki testowej
#b---------------
(f<-smsk/swsk)
#----------------
# wartość p (jest to p'stwo prawostronne)
(p<-1-pf(f,k-1,n-k))

#interpretation
(interpretuj_wynik(p))
# ii (c,d) - aov ---------------------------
(data <- data.frame(
  value = cw,
  group = factor(rep(1:k, times = ng))
))

(wyn <-summary(aov(value ~ group, data = data)))
(wyn[[1]][["Pr(>F)"]][1])
#interpretation
(interpretuj_wynik(wyn[[1]][["Pr(>F)"]][1]))

# a tabela ----------------------------------------

(tabela_wynikow <- data.frame(
  `Suma kwadratów` = c(msk, wsk, csk),
  `Stopnie swobody` = c(k - 1, n - k, n - 1),
  `Średnia kwadratów` = c(smsk, swsk, NA),
  row.names = c("MG ", "WG", "C")
))

#----------------#
#       2        #
#----------------#

#a 
(aparts <- read.table("mieszkania.csv", header = TRUE, sep = ";", dec = ","))

aparts_aov <-(aov(Metraz ~ Dzielnica, data = aparts))
(aparts_aov_smry <- summary(aov(Metraz ~ Dzielnica, data = aparts)))
(interpretuj_wynik(aparts_aov_smry[[1]][["Pr(>F)"]][1]))

#b parami

(aparts_tukey <- TukeyHSD(aparts_aov, "Dzielnica"))

interpretuj_wynik_tukey <- function(tukey_result) {
  results <- as.data.frame(aparts_tukey$Dzielnica)
  interpretacje <- apply(results, 1, function(row) {
    comparison <- rownames(results)[which(results$diff == row["diff"])]
    if (row["p adj"] < 0.05) {
      return(paste(comparison, ": Różnica jest statystycznie istotna (p =", round(row["p adj"], 4), ")."))
    } else {
      return(paste(comparison, ": Różnica nie jest statystycznie istotna (p =", round(row["p adj"], 4), ")."))
    }
  })
  return(interpretacje)
}

(interpretacje_tukey <- interpretuj_wynik_tukey(aparts_tukey))

#----------------#
#       3        #
#----------------#

#a
aparts$Kategorie_Pokoje <- cut(aparts$Pokoje,
                               breaks = c(-Inf, 1, 2, 3, Inf),
                               labels = c("1-pokojowe", "2-pokojowe", "3-pokojowe", "wielopokojowe"))

(aparts)

#B
aparts$Cena_m2 <- aparts$Cena / aparts$Metraz

aparts3_aov <- aov(Cena_m2 ~ Kategorie_Pokoje, data = aparts)
(aparts3_aov_smr<-summary(anova_result))

(interpretuj_wynik(aparts3_aov_smr[[1]][["Pr(>F)"]][1]))

#c

(aparts3_tuk <- TukeyHSD(aparts3_aov, "Kategorie_Pokoje"))
(interpretacje_tukey <- interpretuj_wynik_tukey(aparts3_tuk))

