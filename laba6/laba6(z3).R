



Y<-c(2, 3, 3.5, 4, 4.5, 5)
X<-c(2, 3, 3.5, 4, 4.5, 5)

macierz <- matrix(c(0.05, 0.05, 0.03, 0.01, 0, 0,
                    0.03, 0.07, 0.05, 0.04, 0.02, 0,
                    0.02, 0.05, 0.06, 0.06, 0.05, 0.01,
                    0, 0.03, 0.04, 0.06, 0.08, 0.02,
                    0, 0, 0.02, 0.06, 0.04, 0.03,
                    0, 0, 0, 0.01, 0.01, 0.03),
                  nrow = 6, byrow = TRUE,
                  dimnames = list(X,
                                  Y))

macierz
# Rozklad brzegowy
pXX <- rowSums(macierz)
pXX

pYY <- margin.table(macierz, margin = 2)


M<-length(X)
N<-length(Y)

#iii) rozklad warunkowy------------------


warum <- macierz/pXX
warum
#ii) 	Wyznaczyć współczynnik korelacji -------------------

# wartości oczekiwane
eX<-sum(X*pX)
eY<-sum(Y*pY)
eX2<-sum(X*X*pX)
eY2<-sum(Y*Y*pY)
varX<-eX2-eX^2
varY<-eY2-eY^2
# rozkład oraz wartości x,y w postaci wektorowej
(Xv<-rep(X,times=N))
(Yv<-rep(Y,each=M))
(pv<-c(macierz))
eXY<-sum(pv*Xv*Yv)
cov<-eXY-eX*eY
# współczynnik korelacji
(rhoxy<-covxy/(varx*vary)^0.5)
for (i in 1:M){
  pYC[i,]<-macierz[i,]/pX[i]
}
pYC


#iv)	Korzystając z metody generowania najpierw z rozkładu brzegowego zmiennej X a potem z rozkładu warunkowego zmiennej Y, wylosować 1000 par realizacji z tego rozkładu. 
MN<-M*N

# p'stwa skumulowane
S<-array(dim=MN)
pv
S[1]<-pv[1]
for (i in 2:MN){
  S[i]=S[i-1]+pv[i]
}
# realizacja
X_rea<-array(dim=1000)
Y_rea<-array(dim=1000)
# 1000 realizacji z U[0,1]
r<-runif(1000)
for (j in 1:1000){
  i<-1
  while (r[j]>S[i]){
    i<-i+1
  }
  X_rea[j]<-Xv[i]
  Y_rea[j]<-Yv[i]
}

mat_rea<-cbind(X_rea,Y_rea)
tab<-table(X_rea,Y_rea)
#v)Sporządzić tablice rozdzielcze opisującą relatywne frekwencje każdej pary \left(X,Y\right).
(tab_r<-tab/length(X_rea))
#vi)vi)	Za pomocą polecenia cor, w oparciu o te dane oszacować współczynnik korelacji metodami Pearsona, Spearmana oraz Kendalla
(r_p<-cor(X_rea,Y_rea))
(r_s<-cor(X_rea,Y_rea,method="spearman"))
(r_k<-cor(X_rea,Y_rea,method="kendall"))

