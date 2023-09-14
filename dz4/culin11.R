data1 = read.csv("GLM (1).csv")
data2 = read.csv("telefon.csv")

x = data1$Default
y = data2$x

boxplot(y[x==0],y[x==1])
# Ne vidimo neku razliku


# PSM
# Procjena vrijednosti
model = glm(data=data1, data1$Default ~ as.factor(data1$zupanija) + data1$br.cl + data1$spolovi, family=binomial(link="logit"))
model$fitted.values
p=predict(model, type="response", se.fit = TRUE)
p = p$fit
p
t = cbind(data1$Default, p)
n1 = length(data1$Default[data1$Default==1])
n0 = length(data1$Default[data1$Default==0])
n1
n0
par = matrix(0,nrow=min(n0,n1),ncol=2)
ind0 = which(data1$Default==0)
ind1 = which(data1$Default==1)
for(i in 1:n0){
  tren = ind0[i]
  temp = p[ind1]
  pom = abs(temp-p[tren])
  m = min(pom)
  indeks = which(pom==m)
  indeks = as.numeric(indeks[1])
  par[i,] = c(ind0[i],ind1[indeks])
}
p0 = par[,1]
p1 = par[,2]
y0 = y[p0]
y1 = y[p1]
y0
y1
wilcox.test(y1,y0, alternative="two.sided",paired=T)
# p-vrijednost 2.92e-08 -> Odbacujemo nul-hipotezu da su iznosi računa za obje grupe Defaulta jednaki
