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


#---------------------------------------------------------------------------
#a -i---
#prop proby
n = 1000
p = 385/n
p_0 = 0.4
#odchylenie standartowe
(se_p = sqrt(p_0*(1-p_0)/n))
(test_z1 =(p-p_0)/se_p)
(p_z1 <- 2 * (1 - pnorm(abs(test_z1))))
(interpretuj_wynik(p_z1))

#b -i-------------------------------------------------------
(p_z1_prop <- prop.test(385,1000,p=p_0,conf.level=0.95))
(interpretuj_wynik(p_z1_prop))
#b -i---
p_kobiety <- 220 / 520
p_mezczyzni <- 165 / 480
p <- 385 / 1000

# Odchylenie standardowe
SE <- sqrt(p * (1 - p) * (1/520 + 1/480))

# Test Z
(Z <- (p_kobiety - p_mezczyzni) / SE)
(p_value_Z <- 2 * (1 - pnorm(abs(Z))))

# Interpretacja wyniku za pomocą funkcji
(interpretuj_wynik(p_value_Z))


#-ii---
# Dane
x <- c(220, 165)
n <- c(520, 480)

# Test proporcji
prop_test <- prop.test(x, n)

# Wartość p
(p_value_prop <- prop_test$p.value)

# Interpretacja wyniku za pomocą funkcji
(interpretuj_wynik(p_value_prop))






















test <- function(z) {
  # Calculate critical values for the z-test
  critical_value_z_5 <- qnorm(0.975)  # Critical value for 5% significance level
  critical_value_z_1 <- qnorm(0.995)  # Critical value for 1% significance level
  critical_value_z_01 <- qnorm(0.9995)  # Critical value for 0.1% significance level
  
  
  # Evaluate based on critical values for both z-test and t-test
  if (abs(z) > critical_value_z_5 && abs(z) > critical_value_t_5) {
    if (abs(z) > critical_value_z_1 && abs(z) > critical_value_t_1) {
      if (abs(z) > critical_value_z_01 && abs(z) > critical_value_t_01) {
        cat("Mamy bardzo mocne dowody przeciwko H0.\n")
      } else {
        cat("Mamy mocne dowody przeciwko H0.\n")
      }
    } else {
      cat("Mamy dowody przeciwko H0.\n")
    }
  } else {
    cat("Nie mamy dowodów przeciwko H0.\n")
  }
}

# c-------------------------------------------

# Dane
n1 <- 520
mean1 <- 166
var1 <- 100
sd1 <- sqrt(var1)

n2 <- 480
mean2 <- 174
var2 <- 121
sd2 <- sqrt(var2)

# Odchylenie standardowe różnicy średnich
SE <- sqrt((sd1^2 / n1) + (sd2^2 / n2))

Z <- (mean1 - mean2) / SE

p_value_Z <- 2 * (1 - pnorm(abs(Z)))
(interpretacja <- interpretuj_wynik(p_value_Z))

#2------------------------------------------------------

(waga <- read.table("waga1(1).csv", header = TRUE, sep = ";", dec = ","))
liczba_kobiet <- sum(waga$płeć == 1)

liczba_studentów <- nrow(waga)

proporcja_kobiet <- liczba_kobiet / liczba_studentów


p_0 <- 0.5

hat_p <- proporcja_kobiet

n <- liczba_studentów

# Odchylenie standardowe
SE <- sqrt(p_0 * (1 - p_0) / n)

Z <- (hat_p - p_0) / SE

p_value_Z2 <- 2 * (1 - pnorm(abs(Z)))

(interpretacja <- interpretuj_wynik(p_value_Z2))

prop_test2 <- prop.test(liczba_kobiet, liczba_studentów, p = 0.5)

(interpretacja <- interpretuj_wynik(prop_test2$p.value))


#3 ------------------------------

waga_mezczyzni <- waga$Waga_po[waga$plec == 0]
waga_kobiety <- waga$Waga_po[waga$plec == 1]

# Liczba mężczyzn i kobiet
n_mezczyzni <- length(waga_mezczyzni)
n_kobiety <- length(waga_kobiety)

# Średnia i odchylenie standardowe dla mężczyzn i kobiet
mean_mezczyzni <- mean(waga_mezczyzni)
mean_kobiety <- mean(waga_kobiety)

sd_mezczyzni <- sd(waga_mezczyzni)
sd_kobiety <- sd(waga_kobiety)


SE <- sqrt((sd_mezczyzni^2 / n_mezczyzni) + (sd_kobiety^2 / n_kobiety))


Z3 <- (mean_mezczyzni - mean_kobiety) / SE

# p-value
p_value_Z3 <- 2 * (1 - pnorm(abs(Z3)))
(interpretacja <- interpretuj_wynik(p_value_Z3))
# Wykonanie testu t
t_test <- t.test(waga$Waga_po ~ waga$plec)

(interpretacja <- interpretuj_wynik(t_test))

#4---------------------------
kobiety <- waga[waga$plec == 1, ]
mezczyzni <- waga[waga$plec == 0, ]

# Proporcje
p_kobiety <- sum(kobiety$Waga_po > 70) / nrow(kobiety)
p_mezczyzni <- sum(mezczyzni$Waga_po > 70) / nrow(mezczyzni)

# Liczby
n_kobiety <- nrow(kobiety)
n_mezczyzni <- nrow(mezczyzni)

# Łączna proporcja
p_hat <- (sum(kobiety$Waga_po > 70) + sum(mezczyzni$Waga_po > 70)) / (n_kobiety + n_mezczyzni)

# Odchylenie standardowe różnicy proporcji
SE4 <- sqrt(p_hat * (1 - p_hat) * (1/n_kobiety + 1/n_mezczyzni))

# Statystyka Z
Z4 <- (p_kobiety - p_mezczyzni) / SE4

# p-value
p_value_Z4 <- 2 * (1 - pnorm(abs(Z4)))
(interpretacja <- interpretuj_wynik(p_value_Z4))

#b -
waga$waga_70 <- ifelse(waga$Waga_po > 70, 1, 0)

# Test t dla proporcji (założenie homogenicznych wariancji)
t_test <- t.test(waga$waga_70 ~ waga$plec, var.equal = TRUE)
(interpretacja <- interpretuj_wynik(t.test$p.value))

#c ---------------
library(boot)
# Funkcja obliczająca różnicę proporcji
diff_prop <- function(data, indices) {
  d <- data[indices, ]
  p_kobiety <- sum(d[d$plec == 1, "Waga_po"] > 70) / nrow(d[d$plec == 1, ])
  p_mezczyzni <- sum(d[d$plec == 0, "Waga_po"] > 70) / nrow(d[d$plec == 0, ])
  return(p_kobiety - p_mezczyzni)
}

# Bootstrap
set.seed(123)  # Ustawienie ziarna dla powtarzalności wyników
results <- boot(data = waga, statistic = diff_prop, R = 1000)

# Przedział ufności 95%
boot.ci(results, type = "perc")

#5---------------------------------------


sredni_wzrost_mezczyzni <- mean(waga$Wzrost[waga$plec == 0])
sredni_wzrost_kobiety <- mean(waga$Wzrost[waga$plec == 1])

# Test t dla dwóch niezależnych prób
t_test5 <- t.test(waga$Wzrost ~ waga$plec, mu = 5, alternative = "two.sided")
(interpretacja <- interpretuj_wynik(t.test5$p.value))


#6 --------------------------------------------

waga$Roznica_wagi <- waga$Waga_po - waga$Waga_przed

# Obliczenie ile studentów przybiera na wadze
przybierajacy_na_wadze <- sum(waga$Roznica_wagi > 0)
# Proporcja studentów przybierających na wadze
proporcja_przybierajacych <- przybierajacy_na_wadze / nrow(waga)
# Test proporcji
prop_test6 <- prop.test(przybierajacy_na_wadze, nrow(waga), p = 0.8, alternative = "two.sided")
(interpretacja <- interpretuj_wynik(prop_test6$p.value))

#7 -----------------------------------------------------

prop_studentek <- sum(waga$Wzrost[waga$plec == 1] > 170) / sum(waga$plec == 1)
prop_studentow <- sum(waga$Wzrost[waga$plec == 0] > 170) / sum(waga$plec == 0)
# Test proporcji
prop_test7 <- prop.test(c(sum(waga$Wzrost[waga$plec == 1] > 170), sum(waga$Wzrost[waga$plec == 0] > 170)),
                       c(sum(waga$plec == 1), sum(waga$plec == 0)), alternative = "two.sided")

(interpretacja <- interpretuj_wynik(prop_test7$p.value))

#b

bootstrap_diff_prop <- function(data, indices) {
  sampled_data <- data[indices, ]
  prop_studentek_sample <- sum(sampled_data$Wzrost[sampled_data$plec == 1] > 170) / sum(sampled_data$plec == 1)
  prop_studentow_sample <- sum(sampled_data$Wzrost[sampled_data$plec == 0] > 170) / sum(sampled_data$plec == 0)
  return(prop_studentek_sample - prop_studentow_sample)
}

set.seed(123)  # Ustawienie ziarna dla powtarzalności wyników
bootstrap_results <- boot(data = waga, statistic = bootstrap_diff_prop, R = 1000)

# Przedział ufności 98%
boot.ci(bootstrap_results, type = "perc", conf = 0.98)













