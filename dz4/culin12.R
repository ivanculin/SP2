# a)
glm = read.csv("GLM (1).csv")
n = length(glm[,1])

library(ggplot2)
ggplot(glm) +
  aes(x = as.factor(br.cl), y = udio.rate) +
  geom_boxplot() +
  ggtitle("Odnos broja uzdrzavanih clanova i udjela rate") +
  xlab("Br. clanova") +
  ylab("Udio rate") 

# Iz grafičkog prikaza naslućujemo da korelacija postoji - veći broj uzdržavanih članova
# će (u prosjeku) značiti veći udio rate



# b)
# Parametarski test - ANOVA
anova(lm(glm$udio.rate ~ as.factor(glm$br.cl)))
par(mfrow=c(1,1))
par(mfrow=c(4,2))
for(i in 0:6){
  qqnorm(glm$udio.rate[glm$br.cl==i])
}
# Za dosta grafova, pogotovo one za br. clanova 5 ili 6, imamo naznake odstupanja od normalnosti

# c)
# Randomizacijski test
# Promotrit ćemo 1000 permutacija podataka i na temelju njih procijeniti p-vrijednost
n = length(glm$udio.rate)
kor = cor(glm$udio.rate,glm$br.cl)
kor
# Korelacija 0.46452, pa ćemo za p-vrijednost gledati udio svih permutacija u kojima je korelacija veća od toga
count = 0
for(b in 1:1000){
  perm = sample(1:n,n,prob=rep(1/n,n))
  kor_temp = cor(glm$udio.rate[perm],glm$br.cl)
  if(kor < kor_temp){
    count = count + 1
  }
}
pv = count/n
pv
# p-vrijednost 0 - Dakle, jer je nul hipoteza da je korelacija 0 (H_1: korelacija > 0), odbacujemo nultu

