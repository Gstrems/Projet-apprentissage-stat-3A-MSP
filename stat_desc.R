################################################################################
# on fait ici quelques statistiques descriptives pour se donner une première 
# idée du lien entre notre variable d'intérêt et les autres  
################################################################################
#faire tourner avant tout traitement_NR.R
library(ggplot2)
# test d'indépendance du Chi2 

chisq.test(Y, escap$Q03) #X-squared = 199.12, df = 23, p-value < 2.2e-16
chisq.test(Y, escap$Q04) #X-squared = 75.3, df = 23, p-value = 1.8e-07
chisq.test(Y, escap$Q04A) #X-squared = 129.87, df = 69, p-value = 1.307e-05
chisq.test(Y, escap$Q04B) #X-squared = 149.82, df = 92, p-value = 0.0001328
chisq.test(Y, escap$Q05) #X-squared = 169.69, df = 23, p-value < 2.2e-16
chisq.test(Y, escap$Q06A) #X-squared = 82.219, df = 46, p-value = 0.0008195
chisq.test(Y, escap$Q06B) #X-squared = 105.15, df = 46, p-value = 1.585e-06
chisq.test(Y, escap$Q08) #X-squared = 104.89, df = 92, p-value = 0.169 !!!
chisq.test(Y, escap$Q08C) #X-squared = 86.947, df = 92, p-value = 0.6294 !!!
chisq.test(Y, escap$Q09A1) #X-squared = 157.98, df = 138, p-value = 0.1173!!!
chisq.test(Y, escap$Q09B1) #X-squared = 172.41, df = 138, p-value = 0.025!!!
chisq.test(Y, escap$Q10A1) #X-squared = 322.45, df = 184, p-value = 1.2e-09
chisq.test(Y, escap$Q10B1) #X-squared = 322.78, df = 176, p-value = 1.002e-10
chisq.test(Y, escap$B08A) #X-squared = 332.16, df = 92, p-value < 2.2e-16
chisq.test(Y, escap$B08B) #X-squared = 312.45, df = 92, p-value < 2.2e-16
#attention : veut pas forcément dire que c'est des mauvais prédicteurs si H0 non rejetée

#Barplot
escap$Y <- Y


#Q03 : sexe
tab <- table(escap[[2]], escap[[18]]) 
barplot(tab, 
        legend = TRUE,
        col = c("steelblue", "firebrick"),
        main = "Répartition de var2 selon var1",
        xlab = "Y", ylab = "Q03")

#Q04 : Situation
tab <- table(escap[[3]], escap[[18]]) 
barplot(tab, 
        legend = TRUE,
        col = c("steelblue", "firebrick"),
        main = "Répartition de var2 selon var1",
        xlab = "Y", ylab = "Q03")

#Q04A : Situation scolaire
tab <- table(escap[[4]], escap[[18]])
barplot(tab, 
        legend = TRUE,
        col = c("steelblue", "firebrick", "yellow", "purple"),
        main = "Répartition de var2 selon var1",
        xlab = "Y", ylab = "Q03")

#Q04B : Situation pro
tab <- table(escap[[5]], escap[[18]])
barplot(tab, 
        legend = TRUE,
        col = c("steelblue", "firebrick", "yellow", "purple", "green"),
        main = "Répartition de var2 selon var1",
        xlab = "Y", ylab = "Q03")

#Q05 : redoublement
tab <- table(escap[[6]], escap[[18]])
barplot(tab, 
        legend = TRUE,
        col = c("steelblue", "firebrick"),
        main = "Répartition de var2 selon var1",
        xlab = "Y", ylab = "Q03")

#Q06A
tab <- table(escap[[7]], escap[[18]])
barplot(tab, 
        legend = TRUE,
        col = c("steelblue", "firebrick", "yellow"),
        main = "Répartition de var2 selon var1",
        xlab = "Y", ylab = "Q03")

#Q06B
tab <- table(escap[[8]], escap[[18]])
barplot(tab, 
        legend = TRUE,
        col = c("steelblue", "firebrick", "yellow"),
        main = "Répartition de var2 selon var1",
        xlab = "Y", ylab = "Q03")

#Q08
tab <- table(escap[[9]], escap[[18]])
barplot(tab, 
        legend = TRUE,
        col = c("steelblue", "firebrick", "yellow", "purple", "black"),
        main = "Répartition de var2 selon var1",
        xlab = "Y", ylab = "Q03")

#Q08C
tab <- table(escap[[10]], escap[[18]])
barplot(tab, 
        legend = TRUE,
        col = c("steelblue", "firebrick", "yellow", "purple", "black"),
        main = "Répartition de var2 selon var1",
        xlab = "Y", ylab = "Q03")

#Q09A1
tab <- table(escap[[11]], escap[[18]])
barplot(tab, 
        legend = TRUE,
        col = c("steelblue", "firebrick", "yellow", "purple", "black", "green", "orange"),
        main = "Répartition de var2 selon var1",
        xlab = "Y", ylab = "Q03")

#Q09B1
tab <- table(escap[[12]], escap[[18]])
barplot(tab, 
        legend = TRUE,
        col = c("steelblue", "firebrick", "yellow", "purple", "black", "green", "orange"),
        main = "Répartition de var2 selon var1",
        xlab = "Y", ylab = "Q03")

#Q10A1 
tab <- table(escap[[13]], escap[[18]])
barplot(tab, 
        legend = TRUE,
        col = c("steelblue", "firebrick", "yellow", "purple", 
                "black", "green", "orange", "blue", "red",
                "gray"),
        main = "Répartition de var2 selon var1",
        xlab = "Y", ylab = "Q03")

#Q10B1 
tab <- table(escap[[14]], escap[[18]])
barplot(tab, 
        legend = TRUE,
        col = c("steelblue", "firebrick", "yellow", "purple", 
                "black", "green", "orange", "blue", "red", "gray"),
        main = "Répartition de var2 selon var1",
        xlab = "Y", ylab = "Q03")

#B08A 
tab <- table(escap[[15]], escap[[18]])
barplot(tab, 
        legend = TRUE,
        col = c("steelblue", "firebrick", "yellow", "purple", "black", "green"),
        main = "Répartition de var2 selon var1",
        xlab = "Y", ylab = "Q03")

#B08B
tab <- table(escap[[16]], escap[[18]])
barplot(tab, 
        legend = TRUE,
        col = c("steelblue", "firebrick", "yellow", "purple", "black", "green"),
        main = "Répartition de var2 selon var1",
        xlab = "Y", ylab = "Q03")


