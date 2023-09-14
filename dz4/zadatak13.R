# Ucitavanje

data = read.table("pismenost.txt", header = T)

plot(ecdf(data$prije), col="red", xlim=c(0,10))
lines(ecdf(data$poslije), col="blue")

# cini se da je poslije veća

wilcox.test(data$prije, data$poslije, alternative = "less", paired = TRUE)

# P vrijednost 1% - zaključujemo da na razini značajnosti 5% (ili 10%) odbacujemo H0
# u korist alternative da je varijabla Poslije stohastički veća od varijable Prije

