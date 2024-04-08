#1.	Rzucono monetą 6 razy. Niech X będzie liczbą reszek. Wyznaczyć


#i)	P(X = 5)
dbinom(5, 6, 0.5)

#ii)	P(X ≥ 3)
1-pbinom(2, 6, 0.5) # 1 - P(X <= 2)
#dbinom(0, 6, 0.5)+dbinom(1, 6, 0.5)+dbinom(2, 6, 0.5)+dbinom(3, 6, 0.5)

#iii)	P(2 ≤ X ≤ 4) 
#P(k_1≤X≤k_2 )=P(X≤k_2 )-P(X≤k_1-1)

pbinom(4, 6, 0.5) - pbinom(2-1, 6, 0.5) 

#wykres-----------------------------------------------------
# Rzucono monetą 6 razy
n <- 6
p <- 0.5

# Liczby reszek od 0 do 6
x_values <- 0:n

# Prawdopodobieństwa dla poszczególnych ilości reszek
probabilities <- dbinom(x_values, n, p)

# Narysowanie wykresu
plot(x_values, probabilities, type="h", lwd=5, col="blue",
     main="Rozkład zmiennej X (Liczba reszek)",
     xlab="Liczba reszek", ylab="Prawdopodobieństwo")






#2.	Pewien salon średnio sprzedaje trzy samochody tygodniowo. Niech X będzie liczbą samochodów sprzedanych w ciągu 2 tygodni. Zakładając, iż liczba samochodów sprzedanych przez firmę w dowolnym przedziale czasu ma rozkład Poissona, wyznaczyć

srednia_tygodniowa <- 3
liczba_tygodni <- 2
# Parametr rozkładu Poissona
lambda <- srednia_tygodniowa * liczba_tygodni
#i)	P(X = 5)
dpois(5, lambda)
#ii)	P(X ≥ 4)
1-ppois(3, lambda)
#iii)	P(3 ≤ X ≤ 5)
#P(k_1≤X≤k_2 )=P(X≤k_2 )-P(X≤k_1-1)
ppois(5, lambda) - ppois(3-1, lambda)

ppois(2, lambda)
#wykres-----------------------------------------------------
# Liczby reszek od 0 do 6
xx_values <- 0:30

# Prawdopodobieństwa dla poszczególnych wartości x
probabilities_2 <- dpois(xx_values, lambda)

# Narysowanie wykresu
plot(xx_values, probabilities_2, type="h", lwd=5, col="blue",
     main="Rozkład zmiennej X (Liczba samochodów sprzedanych w ciągu 2 tygodni)",
     xlab="Liczba samochodów", ylab="Prawdopodobieństwo")



#3 ------------------
# Prawdopodobieństwa
px <- c(0.2, 0.4, 0.3, 0.1)

# Wartości x
x_values <- 1:length(px)

# Obliczamy E(X) korzystając z iloczynu wektorowego
E_X <- sum(x_values * px)
E_X
# Obliczamy E(X^2) korzystając z iloczynu wektorowego
E_X_squared <- sum((x_values^2) * px)
E_X_squared
# Obliczamy wariancję Var(X)
Var_X <- E_X_squared - (E_X)^2
Var_X

#4.	Rzucono kostką 180 razy. Niech X będzie liczbą jedynek. Wyznaczyć
#i)	P(X = 27)
dbinom(27, 180, 1/6)
#ii)	P(X ≥ 32)
1 - pbinom(31, 180, 1/6)
#iii)	P(X < 29)
pbinom(28, 180, 1/6)
#	P(25 ≤ X ≤ 33)
#P(k_1≤X≤k_2 )=P(X≤k_2 )-P(X≤k_1-1)
pbinom(33, 180, 1/6)  - pbinom(25-1, 180, 1/6)


#5.	Telefony przychodzą do pewnej centrali losowo z stałą intensywnością 3,5 na minutę. Niech X będzie liczbą telefonów w ciągu 5 minut. Wyznaczyć

srednia <- 3.5
minut <- 5
# Parametr rozkładu Poissona
lambda2 <- srednia * minut
#i)	P(X = 16)
dpois(16, lambda2)
#ii)	P(X ≥ 20)
1 - ppois(19, lambda2)
#iii)	P(X < 12)
ppois(11, lambda2)
#iv)	P(14 ≤ X < 22)
#P(k_1≤X≤k_2 )=P(X≤k_2 )-P(X≤k_1-1)
ppois(21, lambda2) - ppois(13, lambda2)

#	a) Niech X~Binom(100;0,02). Narysować wykres rozkładu zmiennej X.
# b) Niech Y~Poisson(2). Nałożyć wykres rozkładu zmiennej Y na wykres rozkładu zmiennej X (funkcja: lines, col=”red”)

# Część a) Wykres rozkładu zmiennej X ~ Binom(100, 0.02)
x <- 0:100  # Możliwe wartości zmiennej losowej X
px <- dbinom(x, size = 100, prob = 0.02)  # Prawdopodobieństwa dla poszczególnych wartości X

# Utworzenie wykresu
plot(x, px, type = "h", main = "Rozkład zmiennej X ~ Binom(100, 0.02)", xlab = "Wartość X", ylab = "Prawdopodobieństwo", col = "blue")

# Część b) Wykres rozkładu zmiennej Y ~ Poisson(2)
y <- 0:100  # Możliwe wartości zmiennej losowej Y
py <- dpois(y, lambda = 2)  # Prawdopodobieństwa dla poszczególnych wartości Y

# Nałożenie wykresu zmiennej Y na wykres zmiennej X
lines(y, py, col = "red")

# Legenda
legend("topright", legend = c("X ~ Binom(100, 0.02)", "Y ~ Poisson(2)"), col = c("blue", "red"), lty = 1)






