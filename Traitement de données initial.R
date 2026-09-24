
library(mice)
library(tidyverse)

#############################################################

tab = read.csv("ESCAP.csv", sep = ';')
summary(tab) 
nrow(tab)
tab$pm17B <- as.numeric(gsub(",", ".", as.character(tab$pm17B)))
sum(tab$pm17B) #  = nrow


#############################################################

#DISCRETISATION  DE Y
q <- quantile(tab$Q19A, probs = c(0.20, 0.40, 0.60, 0.80), na.rm = TRUE)
q

tab$Q19A <- cut(tab$Q19A, breaks = c(-Inf, q, Inf), labels = c("13-", "14", "15", "16", "17+"),
  include.lowest = TRUE)

tab$Q19A <- as.character(tab$Q19A)
tab$Q19A[is.na(tab$Q19A)] <- "NC"
table(tab$Q19A)

#############################################################


# 20 individus  qui sont en situation scolaire + pro
nrow(tab[which(!is.na(tab$Q04A) & !is.na(tab$Q04B) ),])

#COMBINAISON  DE VARIABLE
# Combinaison des variables  situation  scolaire/pro (en favorisant le scolaire 
# donc effacement des 20 individus en double situation)
tab$Q04B = tab$Q04B+3   # -> modalités 4 à 7 = situation pro
tab[which(is.na(tab$Q04A) & !is.na(tab$Q04B) ),]$Q04A = tab[which(is.na(tab$Q04A) & !is.na(tab$Q04B) ),]$Q04B
nrow(tab[which(is.na(tab$Q04A)),]) # plus que 76  individus sans situation claire
tab = tab[,-5]

#Modification de Q04 (remplissage des trous rendus évidents par les réponses données dans notre variable combinée)
tab[which(is.na(tab$Q04) &  !is.na(tab$Q04A) & tab$Q04A <=  3),]$Q04 = 1 # situation scolaire
tab[which(is.na(tab$Q04) &  !is.na(tab$Q04A) & tab$Q04A  >  3),]$Q04 = 2 # situation professionnelle
# on passe de 1583 NA à 19

#############################################################

#ELIMINATION D'INDIVIDUS
#recherche  d'individu  avec  beaucoup  de  non  réponses parmi les variables où on a beaucoup de non réponses
# Attention !! -> si on retire des individus, il faudra renormaliser les poids (colonne pm17B)
# Notons que le caractère genré des questions sur les parents invisibilise les couples homosexuels
# Beaucoup de NA pour les PCS des parents et leur consommation de boisson
nrow(tab[rowSums(is.na(tab[, c("Q10A1", "Q10B1", "B08A", "B08B")])) >= 2, ]) #Dans ces 4 variables ça ne va pas

nrow(tab[rowSums(is.na(tab[, c("Q10A1", "Q10B1")])) == 2, ]) 
# 870 ne connaissent pas les PCS des parents -> on les élimine
nrow(tab[rowSums(is.na(tab[, c("B08A", "B08B")])) == 2, ]) 
# 774 ne renseignent rien quant à la consommation d'alcool des parents -> on les élimine
# insatisfaits (plus de 500 NA restantes par variable) par l'élimination de seulement 
# ceux-ci, on étend l'élimination

df_NA = tab[rowSums(is.na(tab[, c("Q10A1", "Q10B1","B08A", "B08B")])) >= 2, ]
df_NA$pm17B = as.numeric(gsub(",", ".", as.character(df_NA$pm17B)))
sum(df_NA$pm17B) #1796.04 pour 1794 individus

df = tab[rowSums(is.na(tab[, c("Q10A1", "Q10B1","B08A", "B08B")])) < 2, ]
df$pm17B <- as.numeric(gsub(",", ".", as.character(df$pm17B)))
#Permet de réduire à  moins de 500 NA par variable parmi les 4 variables problématiques
nrow(df) #perte de 1794 individus

nrow(df[rowSums(is.na(tab)) > 2, ]) #reste 593 individus avec plus de 2 Non-réponses

plot(df_NA$pm17B)
plot(df$pm17B)
summary(df_NA$pm17B)
summary(df$pm17B)
# graphiquement et par étude des quantiles, on voit que les poids des individus perdus sont bien répartis
#Au niveau des poids, on ne perd pas trop d'information en les enlevant
#on perd juste le seul poids à 3

#############################################################

#REPONDERATION
df$pm17B <- df$pm17B * nrow(df) / sum(df$pm17B, na.rm = TRUE)
sum(df$pm17B)  #super
nrow(df)

summary(df)
#############################################################

#IMPUTATION (je n'ai pas appris à faire ça donc c'est pas mal de l'IA)
#Attention ça prend 1h30 à compiler
df[, 2:16] <- lapply(df[, 2:16], factor)
str(df)
summary(df)

vars_imp <- names(df)[2:16]
df[vars_imp] <- lapply(df[vars_imp], factor)

# Initialisation
ini <- mice(df, maxit = 0, printFlag = FALSE)

meth <- ini$method
meth[] <- ""

# Choisir automatiquement la méthode (logreg ou polyreg) selon le nombre de modalités
for (v in vars_imp) {
  
  nlev <- nlevels(df[[v]])
  
  if (nlev == 2) {
    meth[v] <- "logreg"
  } else if (nlev > 2) {
    meth[v] <- "polyreg"
  }
}

pred <- ini$predictorMatrix

# jamais prédicteurs :
pred[, "A01"] <- 0
pred[, "pm17B"] <- 0

# Les questions peuvent être utilisées pour imputer les autres questions
pred[vars_imp, vars_imp] <- 1

# Imputation
imp <- mice(
  df,
  m = 5,
  maxit = 20,
  method = meth,
  predictorMatrix = pred,
  seed = 12345,
  printFlag = TRUE
)




imp
plot(imp)
df_imp1 <- complete(imp, 1)
df_imp2 <- complete(imp, 2)
df_imp3 <- complete(imp, 3)
df_imp4 <- complete(imp, 4)
df_imp5 <- complete(imp, 5)

anyNA(df_imp4) #good

write.csv2(df_imp1, file = "Imputé_1.csv", row.names = FALSE)
write.csv2(df_imp2, file = "Imputé_2.csv", row.names = FALSE)
write.csv2(df_imp3, file = "Imputé_3.csv", row.names = FALSE)
write.csv2(df_imp4, file = "Imputé_4.csv", row.names = FALSE)
write.csv2(df_imp5, file = "Imputé_5.csv", row.names = FALSE)


