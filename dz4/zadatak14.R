library('nortest')
skole<-read.table("skole.txt",header=T)
skole_skupa<-as.vector(skole)
skole_skupa<-unlist(skole_skupa)
format(pbinom(0:24,24,1/2),scientific=FALSE)
pbinom(8,24,1/2)


y = sort(skole_skupa)
dg1 = y[7]
gg1 = y[16]
dg1
gg1
# 90% p.i. dobiven ovom metodom je <12.6,14.6>

# Bootstrap
B = 500
boot = numeric(B)
for( b in 1:B ){
  temp = sample(skole_skupa,replace = T, prob=rep(1/24,24))
  boot[b] = median(temp)  
}
boot.gg = 2*mean(boot) - quantile(boot,0.05)
boot.dg = 2*mean(boot) - quantile(boot,0.95)
boot.gg
boot.dg

# 90% p.i. za medijan dobiven bootstrap metodom je <12.886, 14.686>





# Kako je 14.4 u oba pouzdana intervala, zaključujemo da ne bismo odbacili hipotezu o jednakosti očekivanog
# vremena da se istrči 100m za naše i europske učenike

# Parametarska metoda - t test
t.test(skole_skupa, mu = 14.4,alternative="less")$p.value
# Ovim testom zaključujemo isto (p-vrijednost 16%)
# Pretpostavke testa - normalnost
lillie.test(skole_skupa)
qqnorm(skole_skupa)
abline(mean(skole_skupa),sd(skole_skupa))
# Nemamo puno podataka, ali cini se ok

# b)
# Grafička usporedba
boxplot(skole[[1]], skole[[2]], skole[[3]], skole[[4]])
which(skole==min(skole))
# Škola pobjednika je A
# ANOVA metodom provjeravamo slabiju pretpostavku - ima li općenito razlike u vremenima
# među školama
new_skole = data.frame(skola = c(rep('A',6), rep('B',6),rep('C',6),rep('D',6)),vrijeme = skole_skupa)
anova(lm(pom$vrijem ~ new_skole$skola))
# p-vrijednost ANOVA testa je 36% - zaključujemo da nema razlike u vremenima trčanja za škole
# (Pa onda ni da pobjednička škola nije značajno različita)
# Pretpostavke - normalnost svakog poduzorka, nezavnisnost poduzoraka

# Neparametarska metoda
kruskal.test(vrijeme~skola,data=new_skole)
# P-vrijednost 38% - nikako ne odbacujemo nul hipotezu da je škola pobjednika jednako dobra kao ostale u trčanju
