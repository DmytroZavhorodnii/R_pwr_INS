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

test_df <- function(z, df) {
  # Calculate critical values for the t-test
  critical_value_t_5 <- qt(0.975, df)  # Critical value for 5% significance level
  critical_value_t_1 <- qt(0.995, df)  # Critical value for 1% significance level
  critical_value_t_01 <- qt(0.9995, df)  # Critical value for 0.1% significance level
  
  # Evaluate based on critical values for the t-test
  if (abs(z) > critical_value_t_5) {
    if (abs(z) > critical_value_t_1) {
      if (abs(z) > critical_value_t_01) {
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




#a)	testu Z i) wyznaczając odpowiednią wartość p, ii) porównując z odpowiednimi wartościami krytycznymi.
x_ser <- 109
n_obs = 100
odch <- sqrt(225)
m_ser = 105

(test_z = sqrt(n_obs)*(x_ser - m_ser)/odch)

#i wartosc p-------------------------------------------------------------
#p ≈ 2P(Tz > |t|)
(p1 <- 2 * pnorm(-abs(test_z)))

#porownanie-------------------------------


(interpretacja <- interpretuj_wynik(p1))
(test1 <- test(abs(test_z)))

#b testu Studenta i) wyznaczając odpowiednią wartość p, ii) porównując z odpowiednimi wartościami krytycznymi


df <- n_obs - 1  # Stopnie swobody
(p_st <- 2 * (1 - pt(abs(test_z), df))) #p dwustronnie

(interpretacja <- interpretuj_wynik(p_st))
(test2 <- test_df(abs(test_z), df))

#zadanie2----------------------------------------------------------------------------------------

(waga <- read.table("waga1(1).csv", header = TRUE, sep = ";",dec = ","))

(wzrost <- mean(waga$Wzrost))
(n <- length(waga$Wzrost))
mu <- 168
(s <- sd(waga$Wzrost))


#a)	testu Z i) wyznaczając odpowiednią wartość p, ii) porównując z odpowiednimi wartościami krytycznymi.

(test_z2 <- sqrt(n)*(wzrost - mu)/s)
(p_z2 <- 2 * (1 - pnorm(abs(test_z2))))



(interpretacja <- interpretuj_wynik(p_z2))
(test3 <- test(abs(test_z2)))


#student
df <- n - 1  # Stopnie swobody
(p_st2 <- 2 * (1 - pt(abs(test_z2), df)))



(interpretacja <- interpretuj_wynik(p_st2))
(test4 <- test_df(abs(test_z2), df))


#t.tets
(t_result <- t.test(waga$Wzrost, alternative = "two.sided", mu = mu))
t_result$p.value


(interpretacja <- interpretuj_wynik(t_result$p.value))



#zadanie 3 ----------------------------


wzrost <- mean(waga$Wzrost[waga$plec == 0])
mu3 <- 172
n3 <- length(waga$Wzrost[waga$plec == 0])
s3 <- sd(waga$Wzrost[waga$plec == 0])
(test_z3 <- sqrt(n3)*(wzrost - mu3)/s3)
(p_value_z3 <- 2 * (1 - pnorm(abs(test_z3))))
(interpretacja <- interpretuj_wynik(p_value_z3))
(test5 <- test(abs(test_z3)))
#student

df3 <- n3 - 1
p_value_t3 <- 2 * (1 - pt(abs(test_z3), df3))
(interpretacja <- interpretuj_wynik(p_value_t3))
(test2 <- test_df(abs(test_z3), df3))
#ttest

(t_test3 <- t.test(waga$Wzrost, alternative = "two.sided", mu = mu3))


(interpretacja <- interpretuj_wynik(t_test3$p.value))

#zadanie 4-----------------------------------------
#p_value_t3 <- 0.03
#interpretacja <- interpretuj_wynik(p_value_t3)
#print(interpretacja)

(fat <- mean(waga$Waga_po - waga$Waga_przed))

mu4 <- 2
n4 <- length(waga$Waga_po)
s4 <- sd(waga$Waga_po - waga$Waga_przed)

(test_z4 <- sqrt(n4)*(fat - mu4)/s4)
(p_value_z4 <- 2 * (1 - pnorm(abs(test_z4))))


(interpretacja <- interpretuj_wynik(p_value_z4))
(test6 <- test(abs(test_z4)))
#student
df4 <- n4 - 1  # Stopnie swobody
(p_value_t4 <- 2 * (1 - pt(abs(test_z4), df4)))
(interpretacja <- interpretuj_wynik(p_value_t4))
(test7 <- test_df(abs(test_z4), df4))
#ttest
(p_ttest4 <- t.test((waga$Waga_po - waga$Waga_przed), alternative = "two.sided", mu = mu4))
(interpretacja <- interpretuj_wynik(p_ttest4$p.value))




#zadanie 5 -----------------------------

fatman <- mean(waga$Waga_przed[waga$plec == 0] - waga$Waga_po[waga$plec == 0])
mu5 <- 4
n5 <- length(waga$Waga_po[waga$plec == 0])
s5 <- sd(waga$Waga_po[waga$plec == 0] - waga$Waga_przed[waga$plec == 0])

(test_z5 <- sqrt(n5)*(fatman - mu5)/s5)
(p_value_z5 <- 2 * (1 - pnorm(abs(test_z5))))
(interpretacja <- interpretuj_wynik(p_value_z5))
(test7 <- test(abs(test_z5)))

#студ

df5 <- n5 - 1 
(p_value_t5 <- 2 * (1 - pt(abs(test_z5), df5)))

(interpretacja <- interpretuj_wynik(p_value_t5))
(test2 <- test_df(abs(test_z5), df5))

#testt

t_test5<-  t.test((waga$Waga_przed[waga$plec == 0] - waga$Waga_po[waga$plec == 0]), alternative = "two.sided", mu = mu5)
(interpretacja <- interpretuj_wynik(t_test5$p.value))
