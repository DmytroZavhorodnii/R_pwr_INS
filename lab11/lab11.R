#funkcja porownania
interpretuj_wynik <- function(p_value, alpha = 0.05) {
  if (p_value >= alpha) {
    return("Nie odrzucamy na poziomie istotności 5%: Nie mamy dowodów przeciwko H_0.")
  } else if (p_value >= 0.01) {
    return("Odrzucamy na poziomie istotności 5%: Mamy dowody przeciwko H_0.")
  } else if (p_value >= 0.001) {
    return("Odrzucamy na poziomie istotności 1%: Mamy mocne dowody przeciwko H_0.")
  } else {
    return("Odrzucamy na poziomie istotności 0,1%: Mamy bardzo mocne dowody przeciwko H_0.")
  }
}



library("stats")


#1------------------------------------------------
# liczba kategorii
k<-6
# wektor z p'stwem każdej kategorii
p<-1/k*rep(1,times=k)
# wektor z frekwencjami
m<-c(171,200,168,213,226,222)
# liczba obserwacji
n<-sum(m)
# wektor z oczekiwanymi frekwencjami
(me<-n*p)
# realizacja statystyki testowej
(t<-sum((m-me)^2/me))
# wartość p
(wp<-1-pchisq(t,k-1))
#///////////////////////////////////////
#b
(t <- sum((m - me)^2/me))
#c
(p1 <- 1 - pchisq(t, k - 1))
#d
(interpretuj_wynik(p1))
#e
(test_chi <- chisq.test(m, p = p))

#2-----------------------------------------------

# liczba wierszy i kolumn
r<-2
k<-2
# macierz frekwencji
m<-matrix(c(200,150,300,350),nrow=r)
# liczba obserwacji w wierszach oraz kolumnach
sumr<-array(0,dim=r)
sumk<-array(0,dim=k)
# liczba obserwacji
n<-sum(m)
sumr<-rowSums(m)
sumk<-colSums(m)
# macierz oczekiwanych freq oraz realizacja statystyki testowej
# na początku me jest macierzą o odpowiednich wymiarach, który #zawiera same zera
me<-matrix(0,nrow=r,ncol=k)
t<-0
for (i in 1:r){
  for (j in 1:k){
    me[i,j]<-sumr[i]*sumk[j]/n
    t<-t+(m[i,j]-me[i,j])^2/me[i,j]
  }
}

# Oczekiwane frekwencje
me

#b podać realizację statystyki testowej
t
#c --podać wartość p
df<-(r-1)*(k-1)
(p2<-1-pchisq(t,df))


#d 
(interpretuj_wynik(p2))

#e
(test_chi <- chisq.test(m))

#f
(test_fisher <- fisher.test(m))

#3------------------------------------------------------------------

# Tworzenie przykładowych danych
dzielnica <- c("A", "B", "C", "A", "B", "C", "A", "B", "C", "A")
liczba_pokoi <- c(2, 3, 2, 4, 3, 1, 2, 4, 2, 3)
dane <- data.frame(dzielnica, liczba_pokoi)

# a) Tabela rozdzielcza opisująca rozkład liczby pokoi w zależności od dzielnicy
(tabela_a <- table(dane$dzielnica, dane$liczba_pokoi))
rownames(tabela_a) <- c("Dzielnica A", "Dzielnica B", "Dzielnica C")
colnames(tabela_a) <- c("1 pokój", "2 pokoje", "3 pokoje", "4 pokoje")


# b) Nowa zmienna przyjmująca wartość 4 gdy liczba pokoi jest co najmniej 4, w przeciwnym razie równa się liczbie pokoi
dane$nowa_zmienna <- ifelse(dane$liczba_pokoi >= 4, 4, dane$liczba_pokoi)

# c) Testowanie hipotezy, że liczba pokoi jest niezależna od dzielnicy
# Do tego celu można użyć testu chi-kwadrat
(test_chi_kwadrat <- chisq.test(table(dane$dzielnica, dane$nowa_zmienna)))

#4 --------------------------------------------------------------------

dzielnica <- c("A", "B", "C", "A", "B", "C", "A", "B", "C", "A")
cena_m2 <- c(5500, 7000, 6200, 5800, 6500, 6300, 6100, 5900, 6800, 6000)
dane <- data.frame(dzielnica, cena_m2)

dane$cena_wieksza_niz_6000 <- ifelse(dane$cena_m2 > 6000, "Tak", "Nie")
(test_chi_kwadrat <- chisq.test(table(dane$dzielnica, dane$cena_wieksza_niz_6000)))



#5------------------------

cena_m2 <- c(5500, 7000, 6200, 5800, 6500, 6300, 6100, 5900, 6800, 6000)
metraz_srodmiescie <- c(80, 90, 85, 75, 100, 95, 85, 80, 95, 90)
dzielnica <- c("A", "B", "C", "A", "B", "C", "A", "B", "C", "A")
dane <- data.frame(cena_m2, metraz_srodmiescie, dzielnica)

(test_shapiro_cena <- shapiro.test(dane$cena_m2))



par(mar = c(5, 5, 2, 2))

hist(dane$cena_m2, breaks = 5, main = "Estymator gęstości dla ceny za m2", xlab = "Cena za m2 (zł)", freq = FALSE)
lines(density(dane$cena_m2), col = "red")


(test_shapiro_srodmiescie <- shapiro.test(dane$metraz_srodmiescie))

hist(dane$metraz_srodmiescie, breaks = 5, main = "Estymator gęstości dla metrażu mieszkań na Śródmieściu", xlab = "Metraż (m2)", freq = FALSE)
lines(density(dane$metraz_srodmiescie), col = "blue")




#^6---------------------
library(MASS)
set.seed(123)  # Ustawienie ziarna losowości dla reprodukowalności

# a) Generowanie próby 1000 realizacji z rozkładu wykładniczego o wartości oczekiwanej 1
próba_exp <- rexp(1000, rate = 1)

# b) Przetestowanie hipotez
# i) Testowanie hipotezy, czy próba pochodzi z rozkładu normalnego o średniej mu=1 i odchyleniu sigma=1
(test_norm <- shapiro.test(próba_exp))


# ii) Testowanie hipotezy, czy próba pochodzi z rozkładu wykładniczego z parametrem lambda=1
(test_exp <- shapiro.test(próba_exp)
)

# c) Generowanie próby 1000 realizacji z rozkładu Gamma o parametrze kształtu 100 i parametrze skali 1
próba_gamma <- rgamma(1000, shape = 100, rate = 1)

# d) Przetestowanie hipotez
# i) Testowanie hipotezy, czy próba pochodzi z rozkładu normalnego o średniej mu=100 i odchyleniu sigma=10
(test_norm_gamma <- shapiro.test(próba_gamma)
)

# ii) Testowanie hipotezy, czy próba pochodzi z rozkładu Gamma z parametrami 100 i 1
(parametry_gamma <- fitdistr(próba_gamma, densfun = "gamma")$estimate

)


















