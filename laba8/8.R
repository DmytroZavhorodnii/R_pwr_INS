
# a) Przedział ufności dla proporcji studentów mających IQ wyższy niż 115

# Poziom ufności 95%
prop <- 30/100
n <- 100
SE <- sqrt(prop*(1-prop)/n)
z <- qnorm(0.975) # Z dla 95% poziomu ufności
margin_of_error <- z * SE
confidence_interval_95 <- c(prop - margin_of_error, prop + margin_of_error)
confidence_interval_95

# Poziom ufności 99%
z <- qnorm(0.995) # Z dla 99% poziomu ufności
margin_of_error <- z * SE
confidence_interval_99 <- c(prop - margin_of_error, prop + margin_of_error)
confidence_interval_99

# b) Przedział ufności dla średniego IQ

# Poziom ufności 95%
mean_IQ <- 109
sigma <- sqrt(225)
n <- 100
SE <- sigma / sqrt(n)
z <- qnorm(0.975) # Z dla 95% poziomu ufności
margin_of_error <- z * SE
confidence_interval_mean_95 <- c(mean_IQ - margin_of_error, mean_IQ + margin_of_error)
confidence_interval_mean_95

# Poziom ufności 99%
z <- qnorm(0.995) # Z dla 99% poziomu ufności
margin_of_error <- z * SE
confidence_interval_mean_99 <- c(mean_IQ - margin_of_error, mean_IQ + margin_of_error)
confidence_interval_mean_99

#2) ----------------------
# Wczytanie danych z pliku CSV z użyciem średnika jako separatora
data <- read.csv("waga1(1).csv", sep = ";")

# Wyświetlenie danych, aby upewnić się, że zostały poprawnie wczytane
head(data)

# Przypisanie kolumny "Wzrost" do zmiennej heights
heights <- data$Wzrost

# Obliczenie średniego wzrostu
mean_height <- mean(heights)

# Obliczenie odchylenia standardowego
sd_height <- sd(heights)

# Liczność próby
n <- length(heights)

# a) Przedział ufności dla średniego wzrostu na poziomie ufności 90% przy użyciu wartości krytycznej dla rozkładu normalnego
z <- qnorm(0.95)
SE <- sd_height / sqrt(n)
margin_of_error <- z * SE
confidence_interval_normal <- c(mean_height - margin_of_error, mean_height + margin_of_error)
confidence_interval_normal

# b) Przedział ufności dla średniego wzrostu na poziomie ufności 90% przy użyciu wartości krytycznej dla rozkładu Studenta
t <- qt(0.95, df = n - 1)
margin_of_error <- t * SE
confidence_interval_student <- c(mean_height - margin_of_error, mean_height + margin_of_error)
confidence_interval_student

# c) Przedział ufności dla średniego wzrostu na poziomie ufności 90% przy użyciu metody „bootstrap”
bootstrap_ci <- function(data, alpha, n_bootstraps = 1000) {
  boots <- replicate(n_bootstraps, mean(sample(data, replace = TRUE)))
  quantiles <- quantile(boots, c((1-alpha)/2, 1-(1-alpha)/2), na.rm = TRUE)
  return(quantiles)
}

bootstrap_ci(heights, 0.1)


#3 ----------------------
# Wczytanie danych z pliku CSV z użyciem średnika jako separatora
data <- read.csv("waga1(1).csv", sep = ";")

# Przefiltrowanie danych dla studentek (zakładając, że kolumna "plec" zawiera informację o płci)
female_heights <- data$Wzrost[data$plec == 1]

# Obliczenie średniego wzrostu studentek
mean_height_female <- mean(female_heights)

# Obliczenie odchylenia standardowego
sd_height_female <- sd(female_heights)

# Liczność próby
n_female <- length(female_heights)

# Wartość krytyczna dla 98% poziomu ufności
z <- qnorm(0.99)

# Standardowy błąd średniej
SE <- sd_height_female / sqrt(n_female)

# Obliczenie przedziału ufności
margin_of_error <- z * SE
confidence_interval_normal <- c(mean_height_female - margin_of_error, mean_height_female + margin_of_error)
confidence_interval_normal

#b--------------------
# Wartość krytyczna dla rozkładu Studenta
t <- qt(0.99, df = n_female - 1)

# Obliczenie przedziału ufności
margin_of_error <- t * SE
confidence_interval_student <- c(mean_height_female - margin_of_error, mean_height_female + margin_of_error)
confidence_interval_student

#c----------------
# Funkcja do obliczania bootstrapu
bootstrap_ci <- function(data, alpha, n_bootstraps = 1000) {
  female_heights <- data[data$plec == 1, "Wzrost"]
  boots <- replicate(n_bootstraps, mean(sample(female_heights, replace = TRUE)))
  quantiles <- quantile(boots, c((1-alpha)/2, 1-(1-alpha)/2))
  return(quantiles)
}

# Wywołanie funkcji bootstrap
bootstrap_ci(data, 0.02)

#4-----------------------------
# Wczytanie danych z pliku CSV z użyciem średnika jako separatora
data <- read.csv("waga1(1).csv", sep = ";")

# Przypisanie kolumny "Wzrost" do zmiennej heights
heights <- data$Wzrost

# Obliczenie liczby studentów o wzroście przekraczającym 168cm
students_above_168 <- sum(heights > 168)

# Obliczenie proporcji studentów o wzroście przekraczającym 168cm
prop_above_168 <- students_above_168 / length(heights)

# Obliczenie standardowego błędu proporcji
SE_prop <- sqrt(prop_above_168 * (1 - prop_above_168) / length(heights))

# Wartość krytyczna dla 94% poziomu ufności
z <- qnorm(0.97)

# Obliczenie przedziału ufności
margin_of_error <- z * SE_prop
confidence_interval_normal <- c(prop_above_168 - margin_of_error, prop_above_168 + margin_of_error)
confidence_interval_normal
#b-----------
# Funkcja do obliczania bootstrapu
bootstrap_ci <- function(data, alpha, n_bootstraps = 1000) {
  prop_above_168 <- sum(data > 168) / length(data)
  boots <- replicate(n_bootstraps, sum(sample(data, replace = TRUE) > 168) / length(data))
  quantiles <- quantile(boots, c((1-alpha)/2, 1-(1-alpha)/2))
  return(quantiles)
}

# Wywołanie funkcji bootstrap
bootstrap_ci(heights, 0.06)

#5-----------------------
# Wczytanie danych z pliku CSV z użyciem średnika jako separatora
data <- read.csv("waga1(1).csv", sep = ";")

# Przypisanie kolumny "Wzrost" dla studentek do zmiennej heights_female
heights_female <- data$Wzrost[data$plec == 1]

# Obliczenie liczby studentek o wzroście przekraczającym 168cm
students_above_168_female <- sum(heights_female > 168)

# Obliczenie proporcji studentek o wzroście przekraczającym 168cm
prop_above_168_female <- students_above_168_female / length(heights_female)

# Obliczenie standardowego błędu proporcji
SE_prop_female <- sqrt(prop_above_168_female * (1 - prop_above_168_female) / length(heights_female))

# Wartość krytyczna dla 96% poziomu ufności
z <- qnorm(0.98)

# Obliczenie przedziału ufności
margin_of_error <- z * SE_prop_female
confidence_interval_normal <- c(prop_above_168_female - margin_of_error, prop_above_168_female + margin_of_error)
confidence_interval_normal


#b ----------------
# Funkcja do obliczania bootstrapu
bootstrap_ci <- function(data, alpha, n_bootstraps = 1000) {
  heights_female <- data[data$plec == 1, "Wzrost"]
  prop_above_168_female <- sum(heights_female > 168) / length(heights_female)
  boots <- replicate(n_bootstraps, sum(sample(heights_female, replace = TRUE) > 168) / length(heights_female))
  quantiles <- quantile(boots, c((1-alpha)/2, 1-(1-alpha)/2))
  return(quantiles)
}

# Wywołanie funkcji bootstrap
bootstrap_ci(data, 0.04)















