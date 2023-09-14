
library('moments')
library("boot")
lambda = 2
n = 100
B = 500

# a)
x = rexp(n,lambda)

# b)

# Neparametarski
boot.np <- numeric(B)
for( b in 1:B ){
  sample_ = sample(x, replace = T, prob = rep(1/n,n))
  sigma = sd(sample_)
  mu = mean(sample_)
skew = mean(((sample_-mu)/sigma)^3)
  boot.np[b] = skew
}

oc.np = mean(boot.np)
var.np = var(boot.np)
oc.np
var.np
# Procjena očekivane vrijednosti 2.1645, procjena varijance 0.353

# Parametarski
boot.p <- numeric(B)
for( b in 1:B ){
  sample_ = rexp(n,mean(x))
  sigma = sd(sample_)
  mu = mean(sample_)
  skew = mean(((sample_-mu)/sigma)^3)
  boot.p[b] = skew
}
oc.p = mean(boot.p)
var.p = var(boot.p)
oc.p
var.p
# Procjena očekivane vrijednosti 1.756, procjena varijance 0.292


# c) Pouzdani intervali
koef_zakr <- function(d,i){
  sample_ = d[i]
  sigma = sd(sample_)
  mu = mean(sample_)
  skew = mean(((sample_-mu)/sigma)^3)
  return(skew)
}



skew.b = boot(x, koef_zakr, R = 500)
boot.ci(skew.b, type="all")
# Normalni <0.2494,2.3520>
boot.ci(skew.b, type="norm")
# Percentilni <-1.0733,1.0422>
boot.ci(skew.b, type="perc")
# Osnovni <0.5870,2.7025>
boot.ci(skew.b, type="basic")
# BC  <-0.2459,1.0733>
boot.ci(skew.b, type="bca")


# d) Ponovljeni a-c

norm.count = 0
perc.count = 0
basic.count = 0
bc.count = 0
for( k in 1:1000){
  x = rexp(n,lambda)
  skew.b = boot(x, koef_zakr, R = 500)
  a = boot.ci(skew.b,type="norm")
  b = boot.ci(skew.b,type="perc")
  c = boot.ci(skew.b,type="basic")
  d = boot.ci(skew.b,type="bca")
  if(a$normal[2] <= 2 & a$normal[3] >= 2){
    norm.count = norm.count + 1
  }
  if(b$percent[4] <= 2 & b$percent[5] >= 2){
    perc.count = perc.count + 1
  }
  if(c$basic[4] <= 2 & c$basic[5] >= 2){
    basic.count = basic.count + 1
  }
  if(d$bca[4] <= 2 & d$bca[5] >= 2){
    bc.count = bc.count + 1
  }
}
norm.count = norm.count/1000
perc.count = perc.count/1000
basic.count = basic.count/1000
bc.count = bc.count/1000
norm.count
perc.count
basic.count
bc.count
# Normalni - pogađa 65.3%, percentilni 63.9%, osnovni 62.3%, BC 69.2% slučajeva


# e)
# Ručno normalni p.i.
# Neparametarski
dg.np = oc.np - 1.96*sqrt(var.np)
gg.np = oc.np + 1.96*sqrt(var.np)
dg.np
gg.np
# 95% p.i. je <1.061,2.848>

# Parametarski
dg.p = oc.p - 1.96*sqrt(var.p)
gg.p = oc.p + 1.96*sqrt(var.p)
dg.p
gg.p
# 95% p.i. je <0.7504,2.7618>


# Ručno percentilni p.i.
# Neparametarski
pdg.np = 2*oc.np-quantile(boot.np,0.95)
pgg.np = 2*oc.np-quantile(boot.np,0.05)
pdg.np
pgg.np
# 95% p.i. je <1.305,2.855>

# Parametarski
pdg.p = 2*oc.p-quantile(boot.p,0.95)
pgg.p = 2*oc.p-quantile(boot.p,0.05)
pdg.p
pgg.p
# 95% p.i. je <0.819,2.411>

