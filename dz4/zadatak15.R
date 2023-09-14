data = read.table("stres.txt",header = T)
data

# Grafički prikaz utjecaja glazbe na stres
boxplot(data$stres[data$glazba=="Drama"],data$stres[data$glazba=="Horror"],data$stres[data$glazba=="Disney"])
# Ne bismo zaključili da posstoji razlika u razini stresa koju uzrokuje pojedina vrsta glazbe

# MANOVA
mod = aov(data$stres ~ data$glazba + Error(data$ID))
summary(mod)

# Nema razlike u stresu za različite vrste glazbe


# Dodajemo spol
n1 = length(data$ID[data$ID<=20])
n1
n2 = length(data$ID) - n1
n2
spol = c(rep(0,n1),rep(1,n2))
data$spol = spol


# Dodajemo spol u model
mod2 = aov(data$stres ~ data$glazba + data$spol + Error(data$ID/data$spol))
summary(mod2)
# Test govori da ni spol ne utječe na razinu stresa

# Grafički prikaz
boxplot(data$stres[data$spol==0], data$stres[data$spol==1])
# I iz grafičkog prikaza bismo rekli da razlike nema
