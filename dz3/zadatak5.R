slasticarnica = read.csv("slasticarnica.csv", header=T)
library(forecast)
#1. dio

model = lm(slasticarnica$x~slasticarnica$X)
summary(model) #dobar model
plot(slasticarnica$X, slasticarnica$x, col="blue",cex=0.5,pch=19)
lines(model$fitted.values, col="red",lwd=2)

#podaci eksponencijalno distribuirani
y=log(slasticarnica$x)
  y.ts = ts(y, frequency=365)
vrijeme = time(y.ts)

#  provjera pretpostavki linearnog modela
#linearnost podataka
res = model$res
plot(model$fitted.values, res,col='red',cex=0.5,pch=19) 
abline(a=0,b=0)
#raspršenje oko apcise ukazuje na nelinearnost
plot(slasticarnica$x, predict(model),col='green',pch=19,cex=0.5)
abline(a=0,b=1, col="red")
#raspršenje oko pravca y=x ukazuje na nelinearnost


plot(slasticarnica$X, slasticarnica$x,col='green',pch=19,cex=0.5)
plot(slasticarnica$X, log(slasticarnica$x),col='green',pch=19,cex=0.5) #vidimo da je ovo dobra transformacija

model = lm(log(slasticarnica$x)~slasticarnica$X)
summary(model)

#nezavisnost grešaka

library(lmtest)
#dodaj qqnorm
dwtest(model)
#null hipoteza je da ne postoji korelacija
#velika p vrijednost, ne odbacujemo null hipotezu 
#možemo pretpostaviti da na svakoj standardnoj raz značajnosti nisu korelirani
#nekoreliranost povlaci nezavisnost

# homogenost greška
library(tseries)
jarque.bera.test(model$res)
#mala p value, odbacujemo H_0, 
#null hipoteza je podaci dolaze iz normalne distribucije
#ovi reziduali nisu normalni


# normalnost grešaka
shapiro.test(model$res)
#mala p vrijednost, odbacujemo H_0
#ne mogu pretp normalnost

#procjena mjesecne prodaje za sijecanj 2018.
#95% pi za opazenu vrijednost

y <- log(slasticarnica$x)
x <- slasticarnica$X
model = lm(y ~ x)
m2 = lm(y~vrijeme)



#2. dio
#gls regresija
library(nlme)
reziduali=model$residuals
acf(reziduali)
acf(y)
corr_value<-acf(y, plot=FALSE)$acf[2]
model_gls=gls(y~x,correlation=corAR1(value=corr_value))
summary(model_gls)
ts.plot(y, main="GLS procjena zarade (nakon logaritmiranja)",col='red')
lines(model_gls$fitted, col="dodgerblue",lwd=3)


#3.dio Arima

plot(y,pch=19,cex=0.5)
y.ts = ts(y,frequency=365)
# S obzirom da nemamo jasnu sezonalnu komponentu, stavit ćemo da je period na godišnjoj razini
y.ts
stl_model = stl(y.ts,"periodic")

komp = stl_model$time.series
komp
plot(x,komp[,"trend"], main="Vremenski trend")
plot(x,komp[,"seasonal"], main="Sezonalna komponenta")
plot(x,komp[,"remainder"], main="Stacionarna komponenta")
y.ds=y.ts-komp[,'seasonal']
y.stac=komp[,'remainder']
acf(y.stac) #niz nije stacionaran trebamo koristiti arimu
arima = auto.arima(y.stac)
summary(arima)



#d)
stvarne = read.csv("slasticarnica_sijecanj.csv", header = TRUE)
# Linearni model
n = length(x)
vrijeme_novo = data.frame(vrijeme = 4 + (1:31)/365)
xx = data.frame(x=n:(n+31))
p1 = predict(m2, vrijeme_novo, i = 'p')
vrijeme_novo
p1
par(mfrow=c(1,1))
plot(vrijeme_novo$vrijeme, p1[,"fit"], ylim = c(2,5))
lines(vrijeme_novo$vrijeme, p1[,"lwr"], col="dodgerblue")
lines(vrijeme_novo$vrijeme, p1[,"upr"], col="dodgerblue")
points(vrijeme_novo$vrijeme, log(stvarne$x), col="salmon")


#GLS MODEL
xx = data.frame(x=n:(n+30))
p2 = predict(model_gls,xx, i='confidence')


#arima
p3_rez = predict(arima,n.ahead=31)
p3_sez = stl_model$time.series[,"seasonal"][1:31]
trend = stl_model$time.series[,"trend"]
plot(trend)
vrijeme = time(y.ts)
tr_mod = lm(trend ~ vrijeme)

p3_tr = predict(tr_mod, vrijeme_novo)
p3 = as.numeric(p3_rez[[1]]) + p3_sez + p3_tr
p3
er = p3_rez$se
er
p3_dg = p3 - 2*er
p3_gg = p3 + 2*er

plot(vrijeme_novo$vrijeme,as.numeric(p3),ylim=c(0,5), main="Usporedba stvarnosti i procjene buduće vrijednosti - ARIMA")
lines(vrijeme_novo$vrijeme, as.numeric(p3_dg), col="dodgerblue")
lines(vrijeme_novo$vrijeme, as.numeric(p3_gg), col="dodgerblue")
points(vrijeme_novo$vrijeme, log(stvarne$x), col="salmon")


