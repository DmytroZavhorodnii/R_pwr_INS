#Stworzyć następującą ramę danych o nazwie „oceny”

oceny<-data.frame(Imie, Plec, Analiza, Algebra)
oceny
#b)	Wyświetlić pierwsze dwa wiersze tej ramy (nie licząc nazw kolumn).
head(oceny, 2)
#c)	Za pomocą polecenia „str”, opisać strukturę tej ramy (np. liczbę osobników, zmiennych).
str(oceny)
#d)	Wyznaczyć średnią ocenę z analizy.
srednia_algebra <- mean(oceny$Algebra)
srednia_algebra
#e)	Stworzyć nową kolumnę w ramie zawierającą średnią ocenę z obu przedmiotów dla każdego studenta
oceny$Srednia <- rowMeans(oceny[c("Analiza", "Algebra")])
oceny
#f)	Stworzyć ramę, która zawiera wyniki wszystkich kobiet
subset_oceny <- oceny[oceny$Plec == "k", ]
subset_oceny
#g)	Stworzyć ramę, który zawiera wyniki każdego studenta, który dostał co najmniej 4.5 z jednego z tych przedmiotów.
oceny_wysokie <- oceny[rowSums(oceny[, c("Analiza", "Algebra")] >= 4.5, na.rm = TRUE) > 0, ]
oceny_wysokie
#h)	Podać ile osób dostało co najmniej 4.5 z analizy
liczba_osob_z_wysoka_ocena <- sum(oceny$Analiza >= 4.5)
liczba_osob_z_wysoka_ocena





#Wczytać dane z pliku waga1.csv 
waga<-read.csv("waga1.csv", header = TRUE, sep = ";", dec = ",")
#
waga$plec  <- ifelse(waga$plec  == 0, "mężczyzna", "kobieta")
#b)	Wyświetlić pierwsze pięć wierszy tej ramy (nie licząc nazw kolumn).
head(waga, 5)
#c)	Za pomocą polecenia „str”, opisać strukturę tej ramy (np. liczbę osobników, zmiennych).
str(waga)
#d)	Wyznaczyć średni wzrost oraz średnią wagę przed studiami
waga$SredniWzrost <- mean(waga$Wzrost)
waga$SredniaWagaPrzed <-mean(waga$Waga_przed)
#e)	Stworzyć nową kolumnę ramy zawierającą wskaźnik wagi (przed studiami) określony wzorem
waga$wskaznik <- waga$Waga_przed/((waga$Wzrost/100)**2)
waga

#f)	Stworzyć ramę, który zawiera dane dotyczące wszystkich kobiet o wskaźniku wagi powyżej 25.
kobiety_wysoka_waga <- waga[waga$plec == "kobieta" & waga$wskaznik > 25, ]
kobiety_wysoka_waga
#g)	Stworzyć ramę, która zawiera dane dotyczące wszystkich mężczyzn.
men <- waga[waga$plec == "mężczyzna", ]
men
#h)	Podać ile osób jest wyższe od 175cm.
osoby_wyzsze_od_175 <- waga[waga$Wzrost > 175, ]
osoby_wyzsze_od_175

liczba_osob_wyzszych_od_175 <- nrow(osoby_wyzsze_od_175)
liczba_osob_wyzszych_od_175

#Wczytać dane z pliku mieszkania.csv jako ramę danych. Plik zawiera dane dotyczące ofert nieruchomości we Wrocławiu (zmienne: dzielnica, metraż, liczba pokoi, piętro, cena) 
kwart<-read.csv("mieszkania.csv", header = TRUE, sep = ";", dec = ",")
kwart
#b)	Wyświetlić pierwsze sześć wierszy tej ramy (nie licząc nazw kolumn).
head(kwart, 6)
#c)	Za pomocą polecenia „str”, opisać strukturę tej ramy (np. liczbę osobników, zmiennych).
str(kwart)
#d)	Wyznaczyć średni metraż oraz średnią cenę
kwart$MeanMetra <- mean(kwart$Metraz)
kwart$MeanCena <- mean(kwart$Cena)

#e)	Dopisać cenę za m2 do oryginalnej ramki jako nową kolumnę
kwart$MC <- kwart$Cena/kwart$Metraz
#f)	Stworzyć ramę, który zawiera dane dotyczące wszystkich ofert na Psim Polu o cenie poniżej 400 000PLN.
Pp400 <- kwart[kwart$Dzielnica  == "Psie Pole" & kwart$Cena > 400000, ]
Pp400
#g)	Stworzyć ramę, która zawiera dane dotyczące wszystkich ofert w Śródmieściu o metrażu powyżej 60m2.
S60 <- kwart[kwart$Dzielnica  == "Srodmiescie" & kwart$Metraz > 60, ]
S60

#h)	Podać ile jest mieszkań o metrażu większym niż 60m2 oraz o cenie poniżej 350 000PLN.
mieszkania_warunki <- kwart[kwart$Metraz > 60 & kwart$Cena < 350000, ]


liczba<- nrow(mieszkania_warunki)
liczba

