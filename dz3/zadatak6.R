korona_days<-as.numeric(korona_hrv2022$...1)
korona_days
plot(korona_days,col='red',type='l')


##Postoji jedan outlier sa 0 zaraženih i njega procjenjujemo sa brojem zarazenih u tjednu
#prije i poslije

ind=which(korona_days==0)
korona_days[ind]=(korona_days[ind-7]+korona_days[ind+7])/2
korona_transform<-log(korona_days)
plot(korona_transform,col='blue',type='l')
#ne možemo reći da su podaci homogeni jer se distribucija broja zaraženih u siječnju
#dosta razlikuje od distribucije u ostalim mjesecima
korona.ts=ts(korona_transform,start=1,frequency=7)
plot(korona.ts,col='red')
korona.ts

##razdvajamo na senzonalnost, trend i sum
x<-stl(korona.ts, "periodic")
komponente<-x$time.series
komponente
korona_seasonal<-komponente[,"seasonal"]
korona_seasonal
korona.ds=korona.ts- korona_seasonal
plot(korona.ds,col='blue')


vrijeme=time(korona.ts)

model <- lm(korona.ds ~ vrijeme + I(vrijeme^2) + I(vrijeme^3) + I(vrijeme^4) + I(vrijeme^5)  + I(vrijeme^6))
summary(model)
lines(as.numeric(vrijeme),model$fitted.values,col='red',lwd=2)
remainder=model$residuals
ts.plot(remainder)
acf(remainder)

###Vidimo da sum nije stacionaran tako da moramo koristiti ARIMA modele


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

arma_AIC(remainder,5)  
arma_AIC(remainder,6)  
arma_AIC(remainder,7) 
#remainder~AR(7)
model_ostaci = auto.arima(remainder, seasonal = FALSE)
model_ostaci
ostaci.ostataka=model_ostaci$residuals
acf(ostaci.ostataka)

#c
novi_ostaci=predict(model_ostaci,n.ahead=17)
proc.ostaci=novi_ostaci$pred
nova_sezona=korona_seasonal[4:20]
nova_sezona
tmp=1:321
tmp2=ts(tmp,start=1,frequency=7)
v=time(tmp2)
t1=v[305:321]
t=as.numeric(t1)
t
novi_trend=model$coef[1]+model$coef[2]*t+model$coef[3]*t^2+model$coef[4]*t^3+model$coef[5]*t^4+model$coef[6]*t^5+model$coef[7]*t^6
novi_trend
procjena=novi_trend+nova_sezona+proc.ostaci
procjena_back=exp(procjena)
procjena_back
pravi_pod=korona_hrv2022_studeni$...1
error=novi_ostaci$se
error
donja=exp(procjena - 2*error)
gornja=exp(procjena +2*error)
plot(t,as.numeric(procjena_back),xlab="tjedni",ylab="procjene",pch=19,ylim=c(0,1600))
lines(t,as.numeric(donja),col="red")
lines(t,as.numeric(gornja),col="blue")
points(t,pravi_pod,col="blue",pch=19)

####Vidimo da procjena i nije bas točnija što je i očekivano s obzirom na varijabilnost podataka
