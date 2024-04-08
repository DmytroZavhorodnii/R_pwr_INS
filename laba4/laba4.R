# Dane
a <- 4
b <- 12

# i) P(X<7)
p_X_less_than_7 <- punif(7, min = a, max = b)
p_X_less_than_7

# ii) P(5<X<11)
p_X_between_5_and_11 <- punif(11, min = a, max = b) - punif(5, min = a, max = b)
p_X_between_5_and_11

# iii) P(X>10)
p_X_greater_than_10 <- 1 - punif(10, min = a, max = b)
p_X_greater_than_10

# iv) Wyznaczyć x takie, że P(X>x)=0.6
x_for_0.6_probability <- qunif(0.6, min = a, max = b)
x_for_0.6_probability


#2--------------------------
lambda <- 4  # Intensywność przychodzenia telefonów (4 na minutę)

# i) Prawdopodobieństwo, że czas między telefonami jest większy niż 30s.
p_greater_than_30s <- 1 - pexp(0.5, rate = lambda/60)
p_greater_than_30s

# ii) Prawdopodobieństwo, że czas między telefonami jest mniejszy niż 20s.
p_less_than_20s <- pexp(20/60, rate = lambda/60)
p_less_than_20s

# iii) Prawdopodobieństwo, że czas między telefonami mieści się w przedziale od 40 do 80s.
p_between_40_and_80s <- pexp(80/60, rate = lambda/60) - pexp(40/60, rate = lambda/60)
p_between_40_and_80s

# iv) Wyznaczyć czas t taki, że prawdopodobieństwo, iż czas między telefonami jest większy niż t wynosi 0,2.
t_for_0.2_probability <- qexp(0.2, rate = lambda/60)
t_for_0.2_probability

# Narysować wykres gęstości zmiennej T na przedziale 0≤t≤3
t <- seq(0, 3, by = 0.01)
density <- dexp(t, rate = lambda/60)
plot(t, density, type = "l", xlab = "Czas (min)", ylab = "Gęstość", main = "Gęstość zmiennej T")

#3-----------------------------
# Dane
lambda <- 1/3

# i) Większy niż 2 lata
p_greater_than_2 <- 1 - pexp(2, rate = lambda)
p_greater_than_2

# ii) Mniejszy niż 4 lata
p_less_than_4 <- pexp(4, rate = lambda)
p_less_than_4
# iii) Między 3 a 5 lat
p_between_3_and_5 <- pexp(5, rate = lambda) - pexp(3, rate = lambda)
p_between_3_and_5

# iv) Wyznaczyć czas t taki, że p’stwo, iż czas do usterki jest mniejszy niż t wynosi 0,4
t_for_0.4_probability <- qexp(0.4, rate = lambda)
t_for_0.4_probability

#4 ------------------------------
mu <- 170
sigma <- 12

# i) P(X > 180)
(p_X_greater_than_180 <- 1 - pnorm(180, mean = mu, sd = sigma))

# ii) P(X < 165)
p_X_less_than_165 <- pnorm(165, mean = mu, sd = sigma)
p_X_less_than_165

# iii) P(155 < X < 190)
p_between_155_and_190 <- pnorm(190, mean = mu, sd = sigma) - pnorm(155, mean = mu, sd = sigma)
p_between_155_and_190

# Narysować wykres gęstości zmiennej X na przedziale 130≤x≤210
x <- seq(130, 210, by = 0.1)
density <- dnorm(x, mean = mu, sd = sigma)
plot(x, density, type = "l", xlab = "Wzrost [cm]", ylab = "Gęstość prawdopodobieństwa", main = "Wykres gęstości zmiennej X")
