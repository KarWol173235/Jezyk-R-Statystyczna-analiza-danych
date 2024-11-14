library(moments) #wsk+wsp asymterii

dane <- read.csv("C://Users//Karolina//Desktop//SAD-projekt//diabetes.csv")
dane$Glucose[dane$Glucose == 0] <- NA
dane$BloodPressure[dane$BloodPressure == 0] <- NA
dane$SkinThickness[dane$SkinThickness == 0] <- NA
dane$Insulin[dane$Insulin == 0] <- NA
dane$BMI[dane$BMI == 0.0] <- NA

osoby_z_cukrzyca <- dane[dane$Outcome == 1, ]
osoby_bez_cukrzycy <- dane[dane$Outcome == 0, ]

#Parametry
#min
min_gluk_cukrzyk <- min(osoby_z_cukrzyca$Glucose, na.rm = T)
min_gluk_niecukrzyk <- min(osoby_bez_cukrzycy$Glucose, na.rm = T)
min_cisn_cukrzyk <- min(osoby_z_cukrzyca$BloodPressure, na.rm = T)
min_cisn_niecukrzyk <- min(osoby_bez_cukrzycy$BloodPressure, na.rm = T)

paste("Minimalna glukoza we krwi kobiety z cukrzycą:", min_gluk_cukrzyk)
paste("Minimalna glukoza we krwi kobiety bez cukrzycy:", min_gluk_niecukrzyk)
paste("Minimalne ciśnienie kobiety z cukrzycą:", min_cisn_cukrzyk)
paste("Minimalne ciśnienie kobiety bez cukrzycy:",min_cisn_niecukrzyk)





#max
max_gluk_cukrzyk <- max(osoby_z_cukrzyca$Glucose, na.rm = T)
max_gluk_niecukrzyk <- max(osoby_bez_cukrzycy$Glucose, na.rm = T)
max_cisn_cukrzyk <- max(osoby_z_cukrzyca$BloodPressure, na.rm = T)
max_cisn_niecukrzyk <- max(osoby_bez_cukrzycy$BloodPressure, na.rm = T)

paste("Maksymalna glukoza we krwi kobiety z cukrzycą:", max_gluk_cukrzyk)
paste("Maksymalna glukoza we krwi kobiety bez cukrzycy:", max_gluk_niecukrzyk)
paste("Maksymalne ciśnienie kobiety z cukrzycą:", max_cisn_cukrzyk)
paste("Maksymalne ciśnienie kobiety bez cukrzycy:",max_cisn_niecukrzyk)

#rozstep
rozstep_gluk_cukrzyk <- max_gluk_cukrzyk - min_gluk_cukrzyk
rozstep_gluk_niecukrzyk <- max_gluk_niecukrzyk - min_gluk_niecukrzyk
rozstep_cisn_cukrzyk <- max_cisn_cukrzyk - min_cisn_cukrzyk
rozstep_cisn_niecukrzyk <- max_cisn_niecukrzyk - min_cisn_niecukrzyk

paste("Rozstep glukozy u kobiet z cukrzycą:", rozstep_gluk_cukrzyk)
paste("Rozstep glukozy u kobiet bez cukrzycy:", rozstep_gluk_niecukrzyk)
paste("Rozstep ciśnienia u kobiet z cukrzycą:", rozstep_cisn_cukrzyk)
paste("Rozstep ciśnienia u kobiet bez cukrzycy:", rozstep_cisn_niecukrzyk)

#średnia
sred_gluk_cukrzyk <- mean(osoby_z_cukrzyca$Glucose, na.rm =T)
sred_gluk_niecukrzyk <- mean(osoby_bez_cukrzycy$Glucose, na.rm =T)
sred_cisn_cukrzyk <- mean(osoby_z_cukrzyca$BloodPressure, na.rm =T)
sred_cisn_niecukrzyk <- mean(osoby_bez_cukrzycy$BloodPressure, na.rm =T)

paste("Średnia glukoza u kobiet z cukrzycą:", round(sred_gluk_cukrzyk,2))
paste("Średnia glukoza u kobiet bez cukrzycy:",   round(sred_gluk_niecukrzyk,2))
paste("Średnie ciśnienie u kobiet z cukrzycą:", round(sred_cisn_cukrzyk,2))
paste("Średnie ciśnienie u kobiet bez cukrzycy:", round(sred_cisn_niecukrzyk,2))


#mediana
med_gluk_cukrzyk <- median(osoby_z_cukrzyca$Glucose, na.rm =T)
med_gluk_niecukrzyk <- median(osoby_bez_cukrzycy$Glucose, na.rm = T)
med_cisn_cukrzyk <- median(osoby_z_cukrzyca$BloodPressure, na.rm =T)
med_cisn_niecukrzyk <- median(osoby_bez_cukrzycy$BloodPressure, na.rm = T)

paste("Mediana glukozy u kobiet z cukrzycą:", med_gluk_cukrzyk)
paste("Mediana glukozy u kobiet bez cukrzycy:", med_gluk_niecukrzyk)
paste("Mediana ciśnienia u kobiet z cukrzycą:", med_cisn_cukrzyk)
paste("Mediana ciśnienia kobiet bez cukrzycy:", med_cisn_niecukrzyk)

#kwantyle
quantile(osoby_z_cukrzyca$Glucose, c(0.25, 0.5, 0.75), na.rm =T)
quantile(osoby_bez_cukrzycy$Glucose, c(0.25, 0.5, 0.75), na.rm =T)
quantile(osoby_z_cukrzyca$BloodPressure, c(0.25, 0.5, 0.75), na.rm =T)
quantile(osoby_bez_cukrzycy$BloodPressure, c(0.25, 0.5, 0.75), na.rm =T)

#wariancja
wariancja_glukozy_cukrzyk <- var(osoby_z_cukrzyca$Glucose, na.rm = T)
wariancja_glukozy_niecukrzyk <- var(osoby_bez_cukrzycy$Glucose, na.rm = T)
wariancja_ciśnienia_cukrzyk <- var(osoby_z_cukrzyca$BloodPressure, na.rm = T)
wariancja_ciśnienia_niecukrzyk <- var(osoby_bez_cukrzycy$BloodPressure, na.rm = T)

paste("Wariancja glukozy u kobiet z cukrzycą:", round(wariancja_glukozy_cukrzyk,2))
paste("Wariancja glukozy u kobiet bez cukrzycy:", round(wariancja_glukozy_niecukrzyk,2))
paste("Wariancja ciśnienia u kobiet z cukrzycą:", round(wariancja_ciśnienia_cukrzyk,2))
paste("Wariancja ciśnienia kobiet bez cukrzycy:", round(wariancja_ciśnienia_niecukrzyk,2))


#odchylenie standardowe
os_gluk_cukrzyk <- sd(osoby_z_cukrzyca$Glucose, na.rm=T)
os_gluk_niecukrzyk <- sd(osoby_bez_cukrzycy$Glucose, na.rm=T)
os_cisn_cukrzyk <- sd(osoby_z_cukrzyca$BloodPressure, na.rm=T)
os_cisn_niecukrzyk <- sd(osoby_bez_cukrzycy$BloodPressure, na.rm=T)

paste("Odchylenie standardowe glukozy u kobiet z cukrzycą:", round(os_gluk_cukrzyk,2))
paste("Odchylenie standardowe glukozy u kobiet bez cukrzycy:", round(os_gluk_niecukrzyk,2))
paste("Odchylenie standardowe ciśnienia u kobiet z cukrzycą:", round(os_cisn_cukrzyk,2))
paste("Odchylenie standardowe ciśnienia kobiet bez cukrzycy:", round(os_cisn_niecukrzyk,2))

#współczynnik zmienności
wz_gluk_cukrzyk <- os_gluk_cukrzyk/sred_gluk_cukrzyk*100
wz_gluk_niecukrzyk <- os_gluk_niecukrzyk/sred_gluk_niecukrzyk*100
wz_cisn_cukrzyk <- os_cisn_cukrzyk/sred_cisn_cukrzyk*100
wz_cisn_niecukrzyk <- os_cisn_niecukrzyk/sred_cisn_niecukrzyk*100

paste("Współczynnik zmienności glukozy u kobiet z cukrzycą:", round(wz_gluk_cukrzyk,2), "%")
paste("Współczynnik zmienności glukozy u kobiet bez cukrzycy:", round(wz_gluk_niecukrzyk,2),"%")
paste("Współczynnik zmienności ciśnienia u kobiet z cukrzycą:", round(wz_cisn_cukrzyk,2),"%")
paste("Współczynnik zmienności ciśnienia kobiet bez cukrzycy:", round(wz_cisn_niecukrzyk,2),"%")

#Korelacja
cor(osoby_z_cukrzyca$Glucose,osoby_z_cukrzyca$BloodPressure, use = "complete.obs")
cor(osoby_z_cukrzyca$Glucose,osoby_z_cukrzyca$Pregnancies, use = "complete.obs")
cor(dane$Glucose,dane$Outcome, use = "complete.obs")


#wsp asymetrii
wspas_gluk_cukrzyk <- skewness(osoby_z_cukrzyca$Glucose, na.rm=T)
wspas_gluk_niecukrzyk <- skewness(osoby_bez_cukrzycy$Glucose, na.rm=T)

paste("Współczynnik asymterii glukozy u kobiet z cukrzycą:",round(wspas_gluk_cukrzyk,3) )
paste("Współczynnik asymetrii glukozy u kobiet bez cukrzycy:", round(wspas_gluk_niecukrzyk,3))

#wsk asymterii
wskas_gluk_cukrzyk <- kurtosis(osoby_z_cukrzyca$Glucose, na.rm=T)
wskaz_gluk_niecukrzyk <- kurtosis(osoby_bez_cukrzycy$Glucose, na.rm=T)

paste("Wskaźnik asymterii glukozy u kobiet z cukrzycą:",round(wskas_gluk_cukrzyk,3))
paste("Wskaźnik asymetrii glukozy u kobiet bez cukrzycy:", round(wskaz_gluk_niecukrzyk,3))

#podsumowanie
summary(osoby_z_cukrzyca$Glucose)
summary(osoby_bez_cukrzycy$Glucose)
summary(osoby_z_cukrzyca$BloodPressure)
summary(osoby_bez_cukrzycy$BloodPressure)

#GRAFICZNA PREZENTACJA DANYCH
#Histogramy
par(mfrow = c(2, 1))

hist(osoby_z_cukrzyca$Pregnancies,
     main= "Liczba kobiet z cukrzycą, a ilość ciąż", xlab="ilość ciąż", ylab="liczba kobiet",col = "blue")

hist(osoby_bez_cukrzycy$Pregnancies, 
     main= "Liczba kobiet bez cukrzycy, a ilość ciąż", xlab="ilość ciąż", ylab="liczba kobiet", col = "red")

hist(osoby_z_cukrzyca$Glucose,
     main= "Liczba kobiet z cukrzycą, a poziom glukozy", xlab="poziom glukozy", ylab="liczba kobiet",col = "blue")

hist(osoby_bez_cukrzycy$Glucose,
     main= "Liczba kobiet bez cukrzycy, a poziom glukozy", xlab="poziom glukozy", ylab="liczba kobiet", col = "red")

#Gęstość
#glukoza
par(mfrow = c(1, 1))
plot(density(na.omit(osoby_z_cukrzyca$Glucose)), 
     main = "Wykres gęstości glukozy u kobiet z cukrzycą", 
     xlab = "glukoza", 
     ylab = "Gęstość")

plot(density(na.omit(osoby_bez_cukrzycy$Glucose)), 
     main = "Wykres gęstości glukozy u kobiet bez cukrzycy", 
     xlab = "glukoza", 
     ylab = "Gęstość")



#Gęstość
#Insulina
plot(density(na.omit(osoby_z_cukrzyca$Insulin)), 
     main = "Wykres gęstości insuliny u kobiet z cukrzycą", 
     xlab = "Insulina", 
     ylab = "Gęstość")

plot(density(na.omit(osoby_bez_cukrzycy$Insulin)), 
     main = "Wykres gęstości insuliny u kobiet bez cukrzycy", 
     xlab = "Insulina", 
     ylab = "Gęstość")

#Dystrubuanta
#glukoza
plot(ecdf(na.omit(osoby_z_cukrzyca$Glucose)), 
     main = "Wykres dystrybuanty glukozy u kobiet z i bez cukrzycy",
     xlab = "glukoza", 
     ylab = "Częstość", col = "blue")
plot(ecdf(na.omit(osoby_bez_cukrzycy$Glucose)), add=T, col="red") 
# Dodanie legendy
legend("bottomright", 
       legend = c("z cukrzycą", "bez cukrzycy"), 
       col = c("blue", "red"), 
       lwd = 4, 
       cex = 0.8) 



#Dystrybuanta
#insulina
plot(ecdf(na.omit(osoby_z_cukrzyca$Insulin)), 
     main = "Wykres dystrybuanty insuliny u kobiet z i bez cukrzycy", 
     xlab = "Insulina", 
     ylab = "Częstość", col = "blue")
plot(ecdf(na.omit(osoby_bez_cukrzycy$Insulin)), add=T, col="red")
# Dodanie legendy
legend("bottomright", 
       legend = c("z cukrzycą", "bez cukrzycy"), 
       col = c("blue", "red"), 
       lwd = 4, 
       cex = 0.8)
 


#pudełkowe
#glukoza
par(mfrow = c(2, 1))
boxplot(ylab="glukoza", main = "Glukoza u osób z cukrzycą i bez", 
        osoby_z_cukrzyca$Glucose, osoby_bez_cukrzycy$Glucose, names=c("z cukrzycą", "bez cukrzycy"))

#BMI
boxplot(ylab="BMI", main = "BMI u osób z cukrzycą i bez", 
        osoby_z_cukrzyca$BMI, osoby_bez_cukrzycy$BMI, names=c("z cukrzycą", "bez cukrzycy"))

#Ciśnienie krwi
boxplot(ylab="Ciśnienie krwi", main = "Ciśnienie u osób z cukrzycą i bez", 
        osoby_z_cukrzyca$BloodPressure, osoby_bez_cukrzycy$BloodPressure, names=c("z cukrzycą", "bez cukrzycy"))

#Ciąże
boxplot(ylab="Ciąże", main = "Ciąże u osób z cukrzycą i bez", 
        osoby_z_cukrzyca$Pregnancies, osoby_bez_cukrzycy$Pregnancies, names=c("z cukrzycą", "bez cukrzycy"))



#HIPOTEZY
#1
#H0: srednia glukozy u kobiet z cukrzycą jest większa niż 120
#H1: średnia glukoza <120
t.test(osoby_z_cukrzyca$Glucose, mu=120, alternative="less")

#H0: srednia glukozy u kobiet z cukrzycą jest mniejsza niż 120
#H1: średnia glukoza > 120
t.test(osoby_z_cukrzyca$Glucose, mu=120, alternative="greater")

#H0: srednia glukozy u kobiet z cukrzycą jest równa niż 120
#H1: średnia glukoza jest różna od 120
t.test(osoby_z_cukrzyca$Glucose, mu=120, alternative="two.sided")


#2
#H0: wariancje obu próbek są równe
#H1: wariancje obu próbek nie są równe
var.test(osoby_z_cukrzyca$BMI, osoby_bez_cukrzycy$BMI)
var.test(osoby_z_cukrzyca$Glucose, osoby_bez_cukrzycy$Glucose)

