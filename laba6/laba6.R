# nośniki zmiennych
x<-c(0,1)
y<-c(0,1,2)
#rozmiary macierzy
m<-length(x)
n<-length(y)
pxy<-matrix(c(1/8,1/6,1/4,1/6,1/8,1/6),nrow=m)
# rozkłady brzegowe
(px<-margin.table(pxy, margin = 1))
(py<-margin.table(pxy, margin = 2))
# rozkłady warunkowe y -pierwszy wiersz P(Y=y|X=0)
# drugi P(Y=y|X=1)
pyc<-array(0,dim=c(m,n))
for (i in 1:m){
  px[i]<-sum(pxy[i,1:n])
}
for (j in 1:n){
  py[j]<-sum(pxy[1:m,j])
}
px
py
# wartości oczekiwane
ex<-sum(x*px)
ey<-sum(y*py)
ex2<-sum(x*x*px)
ey2<-sum(y*y*py)
varx<-ex2-ex^2
vary<-ey2-ey^2
# rozkład oraz wartości x,y w postaci wektorowej
(xv<-rep(x,times=n))
(yv<-rep(y,each=m))
(pv<-c(pxy))
exy<-sum(pv*xv*yv)
covxy<-exy-ex*ey
# współczynnik korelacji
(rhoxy<-covxy/(varx*vary)^0.5)
for (i in 1:m){
  pyc[i,]<-pxy[i,]/px[i]
}
pyc

# Zadanie 2 ----------------------
x<-c(0,1)
y<-c(0,1,2)
#rozmiary macierzy
m<-length(x)
n<-length(y)
(pxy<-matrix(c(1/8,1/6,1/4,1/6,1/8,1/6),nrow=m))

#I generowanie pary (X,Y) za pomocą rozkładu łącznego
# p'stwa dla rozkładu łącznego, pv 
# liczba elementów w rozkładzie łącznym 
mn<-m*n

# p'stwa skumulowane
s<-array(dim=mn)

s[1]<-pv[1]
for (i in 2:mn){
  s[i]=s[i-1]+pv[i]
}
# realizacja
x_rea<-array(dim=1000)
y_rea<-array(dim=1000)
# 1000 realizacji z U[0,1]
r<-runif(1000)
for (j in 1:1000){
  i<-1
  while (r[j]>s[i]){
    i<-i+1
  }
  x_rea[j]<-xv[i]
  y_rea[j]<-yv[i]
}
mat_rea<-cbind(x_rea,y_rea)
tab<-table(x_rea,y_rea)
(tab_r<-tab/length(x_rea))
(r_p<-cor(x_rea,y_rea))
(r_s<-cor(x_rea,y_rea,method="spearman"))
(r_k<-cor(x_rea,y_rea,method="kendall"))


#iii
# generowanie pary (X,Y) za pomocą rozkładów brzegowych
# p'stwa skumulowane dla zmiennej X
sx<-array(0,dim=m)
sx[1]<-px[1]
for (i in 2:m){
  sx[i]<-sx[i-1]+px[i]
}
# p'stwaskumulowane dla zmiennej Y przy danej X
sy<-array(0,dim=c(m,n))
sy[,1]<-pyc[,1]
for (j in 2:n){
  sy[,j]<-sy[,j-1]+pyc[,j]
}
sx
sy
N<-1000
x_rea<-array(0,dim=N)
y_rea<-array(0,dim=N)
for (k in 1:N){
  # generowanie x
  u<-runif(1)
  ii<-1
  while (u>sx[ii]){
    ii<-ii+1    
  }
  x_rea[k]<-x[ii]
  # generowanie y z rozkładu warunkowego
  u<-runif(1)
  jj<-1
  while (u>sy[ii,jj]){
    jj<-jj+1
  }
  y_rea[k]<-y[jj]
}
mat_rea<-cbind(x_rea,y_rea)
(tab<-table(x_rea,y_rea))
(tab_r<-tab/length(x_rea))
(r_p<-cor(x_rea,y_rea))
(r_s<-cor(x_rea,y_rea,method="spearman"))
(r_k<-cor(x_rea,y_rea,method="kendall"))




