# 	Wzrost X ma rozkład normalny z średnią 170cm a wariancją 144cm2. 
m <- 170
var <- 144
#i) p(X<164)
odstan <- sqrt(var)
x <- 164
# Znormalizowana odległość od średniej
z = (x-m)/odstan
# Prawdopodobieństwo P(X < 164)
(pnorm(z))
(1-pnorm((188-m)/odstan))
(ul <- (pnorm((185-m)/odstan) - pnorm((158-m)/odstan)) )

#b 
(k <- qnorm(1 - 0.15, mean = m, sd = odstan))


#2
# Utwórz pusty wektor do przechowywania wyników
Z_values <- numeric(10000)
Y_values <- numeric(10000)
# Wygeneruj 10 000 realizacji
for (i in 1:10000) {
  # Wygeneruj losowe wartości U1 i U2
  U1 <- runif(1)
  U2 <- runif(1)
  
  # Oblicz wartość Z
  Z <- cos(2 * pi * U1) * sqrt(-2 * log(U2))
  
  # Zapisz wynik
  Z_values[i] <- Z
  
  
  #iii) --------------------------------
  Y <- 100 + 15 * Z
  Y_values[i] <- Y
}
(Y_values[i])
# Wyświetl histogram wyników
hist(Z_values, breaks = 30, col = "skyblue", main = "Histogram wartości Z", xlab = "Wartość Z")

# ii) Estymator jądrowy gęstości --------------------------------------------------------
kde <- density(Z_values)

# Gęstość rozkładu normalnego standardowego
x <- seq(min(Z_values), max(Z_values), length.out = 1000)
normal_density <- dnorm(x)



plot(kde, main = "Porównanie estymatora  i  normalnej", xlab = "Wartość Z", ylab = "Gęstość", xlim = c(-5, 5), ylim = c(0, 0.5))
lines(x, normal_density, col = "red", lwd = 2)


#iii) ---------------------

# Estymator jądrowy gęstości dla zmiennej Y
kde_Y <- density(Y_values)

x_Y <- seq(min(Y_values), max(Y_values), length.out = 1000)
normal_density_Y <- dnorm(x_Y, mean = 100, sd = sqrt(15))

# Wykres porównawczy
plot(kde_Y, main = "Porównanie  Y", 
     xlab = "Wartość Y", ylab = "Gęstość", xlim = c(min(Y_values), max(Y_values)), ylim = c(0, 0.05))
lines(x_Y, normal_density_Y, col = "green", lwd = 2)




#3 Rozkład normalny standardowy X ma rozkład normalny z średnią 170cm a odchyleniem standardowym 12cm.

#i
n <- 2000
meanX <- 170
sdX <- 12
(X_values <- rnorm(n, mean = meanX, sd = sdX))


#ii 	Niech Z={X-170}/{12}. Sporządzić estymator jądrowy gęstości dla zmiennej Z. Porównać to z gęstością rozkładu normalnego standardowego.

# Obliczenie wartości Z
Z_values <- (X_values - 170) / 12

# Estymator jądrowy gęstości dla zmiennej Z
kde_Z <- density(Z_values)

# Gęstość rozkładu normalnego standardowego
x_Z <- seq(min(Z_values), max(Z_values), length.out = 1000)
normal_density_Z <- dnorm(x_Z, mean = 0, sd = 1)

# Wykres porównawczy
plot(kde_Z, main = "Porównanie dla Z", 
     xlab = "Wartość Z", ylab = "Gęstość", xlim = c(min(Z_values), max(Z_values)), ylim = c(0, 0.5))
lines(x_Z, normal_density_Z, col = "orange", lwd = 2)

#500------------------------------------------------------------
#i
n <- 500
mean_X <- 170
sd_X <- 12
(X_values <- rnorm(n, mean = mean_X, sd = sd_X))


#ii 	Niech Z={X-170}/{12}. Sporządzić estymator jądrowy gęstości dla zmiennej Z. Porównać to z gęstością rozkładu normalnego standardowego.

# Obliczenie wartości Z
Z_values <- (X_values - 170) / 12

# Estymator jądrowy gęstości dla zmiennej Z
kde_Z <- density(Z_values)

# Gęstość rozkładu normalnego standardowego
x_Z <- seq(min(Z_values), max(Z_values), length.out = 1000)
normal_density_Z <- dnorm(x_Z, mean = 0, sd = 1)

# Wykres porównawczy
plot(kde_Z, main = "Porównanie dla Z", 
     xlab = "Wartość Z", ylab = "Gęstość", xlim = c(min(Z_values), max(Z_values)), ylim = c(0, 0.5))
lines(x_Z, normal_density_Z, col = "orange", lwd = 2)



#100 ----------------------------------
#i
n <- 2000
mean_X <- 170
sd_X <- 12
(X_values <- rnorm(n, mean = mean_X, sd = sd_X))


#ii 	Niech Z={X-170}/{12}. Sporządzić estymator jądrowy gęstości dla zmiennej Z. Porównać to z gęstością rozkładu normalnego standardowego.

# Obliczenie wartości Z
Z_values <- (X_values - 170) / 12

# Estymator jądrowy gęstości dla zmiennej Z
kde_Z <- density(Z_values)

# Gęstość rozkładu normalnego standardowego
x_Z <- seq(min(Z_values), max(Z_values), length.out = 1000)
normal_density_Z <- dnorm(x_Z, mean = 0, sd = 1)

# Wykres porównawczy
plot(kde_Z, main = "Porównanie dla Z", 
     xlab = "Wartość Z", ylab = "Gęstość", xlim = c(min(Z_values), max(Z_values)), ylim = c(0, 0.5))
lines(x_Z, normal_density_Z, col = "orange", lwd = 2)


#4-----------------
# korzystam z faktu że suma n zmiennych o 
# rozkładzie Exp(a) ma rozkład Gamma(n,a)
n<-1
s<-rgamma(1000,n,0.5)
# E(S_n)=2n, Var(S_n)=4n, SD(S_n)=2n^0.5
mu<-2*n
sd<-2*n^0.5
z<-(s-mu)/sd
wyk<-density(z)
plot(wyk)
x<-(-400:400)*0.01
lines(x,dnorm(x,0,1),col="red")

n<-20
s<-rgamma(1000,n,0.5)
# E(S_n)=2n, Var(S_n)=4n, SD(S_n)=2n^0.5
mu<-2*n
sd<-2*n^0.5
z<-(s-mu)/sd
wyk<-density(z)
plot(wyk)
x<-(-400:400)*0.01
lines(x,dnorm(x,0,1),col="red")

n<-200
s<-rgamma(1000,n,0.5)
# E(S_n)=2n, Var(S_n)=4n, SD(S_n)=2n^0.5
mu<-2*n
sd<-2*n^0.5
z<-(s-mu)/sd
wyk<-density(z)
plot(wyk)
x<-(-400:400)*0.01
lines(x,dnorm(x,0,1),col="red")

#5 ---------------------

# Dla n=30, p=0.05
n<-30
x<-0:n
p<-0.5
# wartość oczekiwana oraz odchylenie
mu<-n*p
war<-mu*(1-p)
sd<-war^0.5
s<-rbinom(10000,n,p)
(rfrek<-table(s)/length(s))
plot(rfrek)
lines(x,dnorm(x,mu,sd),col="red")