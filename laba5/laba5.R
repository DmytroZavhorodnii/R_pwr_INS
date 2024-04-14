#1.	Korzystając z generatora liczb pseudolosowych z odpowiedniego rozkładu w R, wylosować
set.seed(123)
#a)	5 000 realizacji z rozkładu jednostajnego na przedziale [0,1]
(uniform_data <- runif(5000, 0, 1))

#b)	3 000 realizacji z rozkładu normalnego o średniej 100 oraz odchyleniu standardowym 15.
(normal_data <- rnorm(3000, mean = 100, sd = 15))
#c)	[Nowe struktury danych – obiekt] W obu przypadkach wyznaczyć estymator gęstości rozkładu za pomocą i) histogramu (polecenie „hist”), ii) estymatora jądrowego (polecenie „density” – tworzy odpowiedni obiekt). 

hist(uniform_data, main = "Histogram - Rozkład jednostajny", xlab = "Wartości", ylab = "Częstość", prob = TRUE)
hist(normal_data, main = "Histogram - Rozkład normalny", xlab = "Wartości", ylab = "Częstość", prob = TRUE)


(density(uniform_data))
(density(normal_data))

plot(density(uniform_data))
plot(density(normal_data))

#2 Zasymulować 600 rzutów kostką (odpowiednio przekształcić realizacje z rozkładu jednostajnego na [0,1]). 
(dice_rolls <- ceiling(runif(600, 0, 1) * 6))
#ii)	Wyznaczyć średni wynik oraz wariancję z próby [porównać z wartością oczekiwaną 3,5 oraz wariancją 35/12]. 
(mean(dice_rolls))
(var(dice_rolls))
#iii)	Wyznaczyć rozkład częstości dla tych wyników, polecenie - table [porównać z dyskretnym rozkładem jednostajnym]. 
(dice_table <- table(dice_rolls))
(table(rpois(600, 3.5)))
#iv)	Przekształcić tablicę w ramkę danych za pomocą polecenia „as.data.frame”. Wyświetlić tę ramkę oraz wyznaczyć wariancję tych częstości.
(as.data.frame(dice_table))
#b)	Zasymulować 600 rzutów kostką za pomocą procedury wyboru elementu ze zbioru z zwracaniem (polecenie „sample”). 
(table(sample(1:6, 600, replace = TRUE)))

#3
# Podane prawdopodobieństwa
prawdopodobienstwa <- c(0.3, 0.4, 0.2, 0.1)  # P(X=k) dla k = 0, 1, 2, 3
# Możliwe wartości k
k <- 0:3
# Wygenerowanie 1000 liczb z rozkładu dyskretnego
(table(sample(k, 1000, replace = TRUE, prob = prawdopodobienstwa)))

#4 
#i)	Wygenerować 100 realizacji z rozkładu Bin(10; 0,3)
# Ustawienie parametrów
n <- 10  # liczba prób (rzutów monetą)
p <- 0.3 # prawdopodobieństwo sukcesu (wypadnięcia orła)

# Funkcja do symulowania rzutów monetą i zliczania liczby orłów
generuj_realizacje_bin <- function(n, p) {
  # Symulacja rzutów monetą
  rzuty <- rbinom(n, 1, p)
  # Zliczanie liczby orłów
  liczba_orlow <- sum(rzuty)
  return(liczba_orlow)
}

# Wygenerowanie 100 realizacji z rozkładu dwumianowego
(table(replicate(100, generuj_realizacje_bin(n, p))))

#	Wygenerować 50 realizacji z rozkładu Geom(0,4) [Niech X będzie liczbą prób do pierwszego sukcesu gdy p’stwo sukcesu w dowolnej próbie wynosi p, wtedy X~Geom(p)].
(rgeom(50, 0.4))

#5 Funkcja generująca realizacje z rozkładu Poissona(λ) na podstawie generatora liczb pseudolosowych z rozkładu jednostajnego
generate_poisson <- function(lambda) {
  U <- runif(1)  # Generujemy liczbę pseudolosową z rozkładu jednostajnego na [0, 1]
  k <- 0
  p <- exp(-lambda)
  F <- p
  # Pętla typu "while" do generowania realizacji
  while (U >= F) {
    k <- k + 1
    p <- p * lambda / k
    F <- F + p
  }
  return(k)  # Zwracamy liczbę realizacji
}
# Parametr λ dla rozkładu Poissona
lambda <- 3
# Liczba realizacji do wygenerowania
n <- 50
# Generowanie 50 realizacji z rozkładu Poissona(3)
(realizacje_poisson <- replicate(n, generate_poisson(lambda)))

#6--------------------------



wyn<-array(0,dim=200)
for (i in 1:200){
  stop=0
  while (stop==0){
    x<-2*runif(1) 
    u<-runif(1)
    if (u<0.5*x) {stop<-1}
  }
  wyn[i]<-x
}
(sr<-mean(wyn))



