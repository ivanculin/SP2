#a)#procjenitelj za theta je max(x(i))
theta=0.5
n=50
uzorak<-runif(50,0,theta)

#Ftheta_kapa=? i ftheta_kapa=?
##Ftheta_kapa(x)=P(theta_kapa<=x)=P(X_max<=x)=P(X<=x)^n=(x/theta)^n
#ftheta_kapa(x)=Ftheta_kapa(x)'=n*x^(n-1)/theta^n za xe[0,theta]
###b) Distr od theta_kap procjeni s param i neparam bootstrapom za B=1000
### i usporedi s pravom distr, koja je procjena bolja?

### Neparametarski bootstrap
B=1000
stat=function(podaci, k){
  return(max(podaci[k]))
}

library(boot)
boot_stat = boot(uzorak, statistic = stat, R = B)
boot_stat
boot_stat$t #vrijednosti od theta_kap

### Usporedba funkcija distribucije
### Uoči, i fja distr i fja gustoće ovise i o theta i o n!!

x=seq(0, 0.5, 0.01)
n_x=length(x) #51
plot(ecdf(boot_stat$t),lwd=2)

curve((x/theta)^(n_x), type='l', ylab='Vrijednosti', col="red",add=T)







### Usporedba funkcija gustoće
h=hist(boot_stat$t, probability = TRUE, xlim=c(0, 0.5))
curve(n_x*x^(n_x-1)/theta^n_x, add = TRUE, col="red" )
#Sličan zaključak kao i s fjama distribucije





### Parametarski pristup
sim_uzorci = replicate(B, runif(n, 0, max(uzorak))) 

stat2 = function(podaci){
  return(max(podaci))
}

sim_stat = apply(X = sim_uzorci, MARGIN = 2, FUN = stat2 )
sim_stat



#Usporedba fje distribucije
plot(ecdf(sim_stat),lwd=2)
curve((x/theta)^(n_x), type='l', ylab='Vrijednosti', col="red",add=T)

### U parametarskom pristupu simuliramo uzorke koristeci poznatu informaciju o
### distribuciji uzorka pa su procjene za theta_kap bolje.

###Zaključak: Param. metoda daje bolju procjenu za distribuciju od theta_kap






### c) Usporedi proc ocek i var od theta_kap s pravim vrijednostima.

### Neparametarski
mean(boot_stat$t)   #0.4894
var(boot_stat$t)    #4.181016e-05

### Parametarski
mean(sim_stat)      #0.4899
var(sim_stat)       #9.081673e-05

### Za funkcija gustoće f(x)=n*x^(n-1)/theta^n, x na [0, theta]
### EX= n*theta/(n+1)
### EX^2= n*theta^2/(n+2)
### VarX=EX^2-(EX)^2

EX=n*theta/(n+1)
EX #0.4901961

EX2=n*theta^2/(n+2)
VarX=EX2-(EX)^2
VarX #9.242008e-05

###Parametarski pristup je opet bolje procjenio



### d) Ponoviti sve ovo za n=200, theta=50
n=200
theta=50
uzorak.d=runif(n, 0, theta)

### neparametarski
boot_stat.d = boot(uzorak.d, statistic = stat, R = B)
h=hist(boot_stat.d$t, probability = TRUE, xlim=c(0, theta))

x=seq(0, 0.5, 0.01)
plot(ecdf(boot_stat.d$t),lwd=2)

curve((x/theta)^(200), type='l', ylab='Vrijednosti', col="red",add=T)
### Ne poklapaju se bas.


### parametarski
sim_uzorci.d = replicate(B, runif(n, 0, theta) ) 
sim_stat.d = apply(X = sim_uzorci.d, MARGIN = 2, FUN = stat2 )

plot(ecdf(sim_stat.d),lwd=2)

curve((x/theta)^(200), type='l', ylab='Vrijednosti', col="red",add=T)

### Bolje se poklapaju.


#usporedba ocekivanja i varijanci

mean(boot_stat.d$t)   #49.93
var(boot_stat.d$t)    #0.028

### Parametarski
mean(sim_stat.d)      #49.75511
var(sim_stat.d)       #0.06045598

EX=n*theta/(n+1)
EX #49.75124

EX2=n*theta^2/(n+2)
VarX=EX2-(EX)^2
VarX #0.06126699


#Zakljucak je opet isti -> parametarski model je bolji.

