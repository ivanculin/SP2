# Ucitavanje

glm = read.csv("GLM.csv")
n = length(glm[,1])

# a)
table(glm$Default, glm$spolovi)/n
table(glm$Default, glm$zupanija)/n


# b)
chisq.test(table(glm$Default, glm$spolovi))
chisq.test(table(glm$Default, glm$zupanija))
# Prvi test daje p-vrijednost 0.5%, drugi 5%
# U oba slučaja ćemo smatrati da postoji značajna ovisnost defaulta o spolu, odnosno županiji

# Mjerimo koja varijabla ima jači utjecaj koristeći Cramerov V koeficijent, kako smo rekli
# na nekim od prvih vježbi
library("lsr")
cramersV(table(glm$Default, glm$spolovi))
cramersV(table(glm$Default, glm$zupanija))
# Male vrijednosti koeficijenta, 0.036, odnosno 0.072 (veća za županije)

# c)
# Grafička usporedba
par(mfrow=c(1,1))
boxplot( glm$traj.rad.odn[glm$Default==0],  glm$traj.rad.odn[glm$Default==1])


# Parametarski test - ANOVA
anova(lm(data =glm, traj.rad.odn ~ Default))
# p-vrijednost 87%, pa ne bismo zaključili da postoji razlika u trajanju radnog odnosa
# za 2 različite grupe Defaulta

# Neparametarski test - Wilcox
wilcox.test(glm$traj.rad.odn[glm$Default==0],  glm$traj.rad.odn[glm$Default==1])
# p-vrijednost 77%, isti zaključak kao ranije




# d)
model = glm(data=glm, Default ~ as.factor(spolovi)+as.factor(zupanija)+br.cl+traj.rad.odn+izn.kre+udio.rate, family=binomial(link="logit"))
summary(model)
model$deviance

# e)

mod_e = glm(data=glm, Default ~ br.cl, family=binomial(link="logit"))
summary(mod_e)
mod_e$deviance

niz = min(glm$br.cl):max(glm$br.cl)
plot(niz, 1/(1+exp(-mod_e$coefficients["br.cl"]*niz)), type="l", ylim=c(0,1))
ind = seq(from = 1, to = 6000, by = 100)
points(glm$br.cl[ind],glm$Default[ind])

# f)

mod_f = glm(data=glm, Default ~ izn.kre, family=binomial(link="logit"))
summary(mod_f)
niz = seq(from = min(glm$izn.kre), to = max(glm$izn.kre), length = 100)
plot(niz, 1/(1+exp(-mod_f$coefficients["izn.kre"]*niz)),type="l",ylim=c(0,1))
points(glm$izn.kre[ind],glm$Default[ind])

# Uključujemo i interakciju uz županije
mod_f = glm(data= glm, Default ~ izn.kre + izn.kre*as.factor(zupanija) - as.factor(zupanija), family = binomial(link="logit"))
summary(mod_f)
mod_f1 = glm(data=glm, Default ~ izn.kre + izn.kre*zupanija - zupanija, family = binomial(link="logit"))
summary(mod_f1)


# h)
# Za utjecajnost točaka gledamo Cookove udaljenosti (reducirani model)

plot(cooks.distance(reducirani))
# Ima nekih koje su veće od ostalih, ali uzmemo li u obzir skalu (sve je manje od 0.05), ne
# bismo rekli da ima utjedcajnih točaka
