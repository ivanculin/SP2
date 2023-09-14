library(tseries)
electric<-read.table('electric.txt', header=FALSE, sep='')
electric_vector=as.numeric(electric[,1])
electric_series=ts(electric_vector,start=1996, frequency=12)
plot(electric_series,col='red', cex=0.5)

#procjena sezonalnosti metodom pomicnih zareza

d=12
q=6
m_t<-filter(electric_vector, filter=c(1/24, 1/12, 1/12, 1/12,1/12,
                                      1/12,1/12,1/12,1/12,1/12,1/12,1/12,1/24))
m_t

wk=numeric(0)
for(k in 1:d)
{
  if (k<=q) j=1
  if (k>q) j=0
  wk[k]=mean(electric_vector[seq(from=k+j*d,to=length(electric_vector)-q,by=d)]-
               m_t[seq(from=k+j*d,to=length(electric_vector)-q,by=d)])
}

sk=wk-mean(wk)
sk


x<-stl(electric_series, "periodic")
komponente<-x$time.series
komponente
electric_seasonal<-komponente[,"seasonal"]
electric_seasonal
electric_s=numeric(12)
for(i in 1:12)
{electric_s[i]=electric_seasonal[i]}
electric_s
plot(sk,pch=19,col='red')
points(electric_s,pch=19,col='blue',cex=0.8)

##Vidimo da su naša procjena i procjena stl metodom gotovo identične

#a dio
##Rješavamo se sezonalnosti
electric_ds=electric_series-electric_seasonal
electric_ds

plot(electric_ds,col='blue', cex=0.5,lwd=2)
vrijeme<-time(electric_ds)
vrijeme=vrijeme-1996
vrijeme
#Trend moramo procjeniti nekim polinomom visokog stupnja jer je graf dosta varijabilan
electric_lm<-lm(electric_ds~vrijeme+I(vrijeme^2)+I(vrijeme^3)+I(vrijeme^4)
                +I(vrijeme^5)+I(vrijeme^6)+I(vrijeme^7)+I(vrijeme^8)+I(vrijeme^9))

plot(vrijeme+1996, electric_ds, type="l",col='red')
lines(as.numeric(vrijeme+1996), electric_lm$fitted.values, col="blue",lwd=2)

ostaci=electric_lm$residuals

ts.plot(ostaci)
acf(ostaci)

##Vidimo da većina ostataka prelazi preko plave linije tako da ne možemo govoriti
#o slučajnom šumu, odnosno korelirani su.

arma_AIC=function(x,n)
{
  a=0
  b=0
  p=0
  q=0
  t=arima(x,order=c(0,0,0),include.mean=FALSE)
  ai=t$aic
  for (p in 0:n)
    for (q in 0:n)
      if ((p+q)<=n)
      {
        c=arima(x,order=c(p,0,q),include.mean=FALSE)
        aic=c$aic
        if (aic<ai) { ai=aic; a=p; b=q; }
      }
  print(a)
  print(b)
  print(ai)
}

arma_AIC(ostaci,5)  
arma_AIC(ostaci,6)  
arma_AIC(ostaci,7)  

#Za sva 3 n-a dobijemo da ostatke mozemo modelirat ARMA(4,1)

model_ostaci=arima(ostaci,order=c(4,0,1))
ostaci.ostataka=model_ostaci$residuals
acf(ostaci.ostataka)
# samo jedan stupić izlazi izvan pruge => acf sugerira da se radi o bijelom šumu (ostaci su nekorelirani)

# još provjeravamo normalnost:
library(nortest)
lillie.test(ostaci.ostataka) #ne odbacujemo H_0,ostaci normalni -> uistinu se radi o bijelom šumu

## b dio

# Analiza podataka (bez procjene trenda) i  pogodan ARIMA model za njih.

# promatramo desezonalizirani niz:
adf.test(electric_ds)  # testira stacionarnost niza
#velika p-vrijednost => ne možemo odbaciti H0 koja pretpostavlja da niz nije stacionaran

dif.electric_ds=diff(electric_ds,differences=1)
adf.test(dif.electric_ds)
# => p-vrijednost ispada 0.01427 => odbacujemo H0 u korist H1
# sada imamo stacionarnost -> diferenciran niz desezonaliziranog originalnog niza je stacionaran

arma_AIC(dif.electric_ds,6)
arma_AIC(dif.electric_ds,5)
arma_AIC(dif.electric_ds,4)
#najbolji je MA(6) proces

#originalan proces (desezonalizirani niz) je ARIMA(0,1,6) proces


#### Ispitaj podobnost dobivenog modela u oba slučaja.
# goodness of fit:
# procjena koeficijenata modela:
arma_ostaci=arima(ostaci,order=c(4,0,1))
arma_ostaci$coef
#          ar1          ar2          ar3          ar4          ma1    intercept 
#         1.28         -0.14        0.17          -0.38      -0.99    0.003 
model_arima=arima(electric_ds,order=c(0,1,6))
model_arima$coef
#         ma1         ma2         ma3         ma4         ma5         ma6 
#        -0.41        0.047       0.31        -0.13       0.13        0.22
#  vidimo da su svi dobiveni koeficijenti značajno različiti od 0 => to je u redu

# provjera reziduala: 
# za proces iz a) smo već provjerili reziduale i dobili da su oni iz klase bijelog šuma
# provjeravamo za model iz b):
acf(model_arima$residuals)
# => iz dobivenog grafa vidimo da su svi stupići unutar 95% pouzdane pruge
# => reziduali dolaze iz klase bijelog šuma

# provjera stacionarnosti
adf.test(dif.electric_ds)
# p-vrijednost ispada 0.014 => odbacujemoH0 => niz zbilja jest stacionaran
adf.test(ostaci)
# => p-vrijednost ispada 0.02 => odbacujemoH0 => niz zbilja jest stacionaran



##procjena proizvodnje

# predviđamo proizvodnju za idućih 10 mjeseci - PRVI SLUČAJ - sa trendom:
tmp=ts(rep(1,10), start=c(2011,2),frequency=12)
t=time(tmp)
t_new1=t-1996
t_new=as.numeric(t_new1)
novi_trend=electric_lm$coef[1]+electric_lm$coef[2]*t_new+electric_lm$coef[3]*t_new^2+electric_lm$coef[4]*t_new^3+electric_lm$coef[5]*t_new^4+electric_lm$coef[6]*t_new^5+electric_lm$coef[7]*t_new^6+electric_lm$coef[8]*t_new^7+electric_lm$coef[9]*t_new^8 + electric_lm$coefficients[10]*t_new^9

novi_ostaci=predict(arma_ostaci,n.ahead=10)
proc.ostaci=novi_ostaci$pred
nova_sezona=sk[3:12]

novi_podaci=novi_trend+nova_sezona+proc.ostaci
stvarni_podaci=read.table("electric_new.txt",header=FALSE)

# grafička usporedba:
error=novi_ostaci$se
error
donja=novi_podaci-2*error
gornja=novi_podaci+2*error
plot(t_new+1996,as.numeric(novi_podaci),xlab="mjeseci",ylab="procjene",ylim=c(0,200),pch=19)
lines(t_new+1996,as.numeric(donja),col="red")
lines(t_new+1996,as.numeric(gornja),col="red")
points(t_new+1996,stvarni_podaci[,1],col="blue",pch=19)



# predviđamo proizvodnju za idućih 10 mjeseci - 
#DRUGI SLUČAJ (dakle, provodimo isti postupak ko i gore, ali za drugi model bez trenda - kojeg smo procijenili s ARIMA):
novi_ostaci_arima=predict(model_arima, n.ahead=10)
proc.ostaci_arima=novi_ostaci_arima$pred
nova_sezona_arima=sk[3:12]

novi_podaci_arima=nova_sezona_arima+proc.ostaci_arima

error_arima=novi_ostaci_arima$se
donja_arima=novi_podaci_arima-2*error_arima
gornja_arima=novi_podaci_arima+2*error_arima
plot(t_new+1996,as.numeric(novi_podaci_arima),xlab="mjeseci",ylab="procjene",ylim=c(50,150))
lines(t_new+1996,as.numeric(donja_arima),col="red")
lines(t_new+1996,as.numeric(gornja_arima),col="red")
points(t_new+1996,stvarni_podaci[,1],col="blue")




# usporedba obje procjene:
# Prvi model ne procjenjuje podatke tako dobro kao drugi (stvarni podaci su ispod procijenjenih i izvan
# 95% pouzdane pruge). ARIMA model pak dobro procjenjuje podatke (većina ih leži unutar pruge).
