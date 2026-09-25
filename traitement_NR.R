################################################################################
# Dans ce fichier on traite la non réponse dans les données
# Objectif : obtenir un jeu de données utilisable pour les modèles
################################################################################
#charger ESCAP depuis un fichier dédié et inscrit au .gitignore
library(dplyr)
library(FactoMineR)
library(missMDA)
# Etat de la non-réponse pour chaque variable et premiers ajustements
################################################################################
summary(escap)
summary(Y)
#pm17B = poids de sondages
any(is.na(escap$pm17B)) #FALSE : on a l'info sur toutes les variables
plot(escap$pm17B) 
summary(escap$pm17B)

#la variable d'intérêt Q19A (âge au premier alcool)
any(is.na(Y))#TRUE
indiv_na_Y <-escap[which(is.na(Y)), 1]
length(indiv_na_Y) #2393 non répondant


#Q03 : sexe
indiv_nr_Q03 <- escap[which(is.na(escap$Q03)),1]

#Q04
indiv_nr_Q04 <- escap[which(is.na(escap$Q04)),1]

#Q04 A et B: situation scolaire (A) et pro (B)
# Hypothèse : la situation scolaire et professionnelles s'excluent : NA = non concerné
# individus NR pour A et B
indiv_nr_Q04AB <- escap |> 
  filter(is.na(Q04A) & is.na(Q04B)) |> select(A01)
      #Q0A : individus répondants et non répondants exceptés individus NR pour A et B
      indiv_nr_Q04A <- escap |> 
        filter(!(A01 %in% indiv_nr_Q04AB$A01)) |> 
        filter(is.na(Q04A)) |> 
        dplyr::select(A01)
      indiv_r_Q04A <- escap |> 
        filter(!(is.na(Q04A))) |> 
        dplyr::select(A01)
      #Q0B : individus répondants et non répondants exceptés individus NR pour A et B
      indiv_nr_Q04B <- escap |> 
        filter(!(A01 %in% indiv_nr_Q04AB$A01)) |> 
        filter(is.na(Q04B)) |> 
        dplyr::select(A01)
      indiv_r_Q04B <- escap |> 
        filter(!(is.na(Q04B))) |> 
        dplyr::select(A01)
#individus hors intersection entre répondre à Q04B et ne pas répondre à Q04A
diff_r_Q0B_nr_Q0A <-setdiff(indiv_nr_Q04A, indiv_r_Q04B) #0
#individus hors intersection entre répondre à Q04A et ne pas répondre à Q04B
diff_r_Q0A_nr_Q0B <-setdiff(indiv_nr_Q04B, indiv_r_Q04A) #0
#on en conclut que ormis les 76 : tous non concernés
pds_indiv_nr_Q04AB <- escap |> filter(A01 %in% indiv_nr_Q04AB$A01) |> select(pm17B)
pds_indiv_nr_Q04AB <- pds_indiv_nr_Q04AB$pm17B
#On impute une nouvelle modalité : 0 = "Non concerné" pour Q04A et Q04B
#Q04A
escap <- escap |> 
  mutate(Q04A = ifelse(A01 %in% indiv_nr_Q04A$A01, 0, Q04A))
#Q04B
escap <- escap |> 
  mutate(Q04B = ifelse(A01 %in% indiv_nr_Q04B$A01, 0, Q04B))
indiv_nr_Q04AB <- indiv_nr_Q04AB$A01


#Q04
# est-ce que le volume de NR peut être réduit avec info de Q04A et Q04B ?
indiv_r_Q04AB <- escap |> filter(!(is.na(Q04A))&!(is.na(Q04B))) |> select(A01)
indiv_r_Q04AB <- indiv_r_Q04AB$A01
length(setdiff(indiv_nr_Q04, indiv_r_Q04AB))
# 19 indiv NR de Q04 ne sont pas parmi les indiv qui ont répondu à Q04A ou Q04B
# on impute la réponse en fonction des réponses cochées dans Q04A et Q04B
escap <- escap |>
  mutate(Q04 = ifelse(is.na(Q04A), NA, 1)) |> 
  mutate(Q04 = ifelse(!(Q04B %in% c(0,NA)), 2, Q04))
#ici on a une hypothèse : travailler (ou rechercher) = arrêter ses études
#néanmoins on a vu que les non réponses dans Q04A et Q04B étaient vraisemblablement des 
#réponses de non concerné : donc pour nos individus cette hypothèse est crédible.

#libérer environnement
rm(diff_r_Q0B_nr_Q0A)
rm(diff_r_Q0A_nr_Q0B)
rm(indiv_nr_Q04A)
rm(indiv_r_Q04A)
rm(indiv_nr_Q04B)
rm(indiv_r_Q04B)


#Q05 : redoublement 
any(is.na(escap$Q05))#TRUE
indiv_nr_Q05 <- escap |> filter(is.na(Q05)) |> select(A01)
indiv_nr_Q05 <- indiv_nr_Q05$A01
setdiff(indiv_nr_Q05, indiv_nr_Q04AB) #36 sur 40 sont pas parmi les non répondants de A et B


#Q06A et B
any(is.na(escap$Q06A))#TRUE
indiv_nr_Q06A <- escap |> filter(is.na(Q06A)) |> select(A01)
indiv_nr_Q06A <- indiv_nr_Q06A$A01
any(is.na(escap$Q06B))#TRUE
indiv_nr_Q06B <- escap |> filter(is.na(Q06B)) |> select(A01)
indiv_nr_Q06B <- indiv_nr_Q06B$A01
#intersections entre non répondants ?
setdiff(indiv_nr_Q06B, indiv_nr_Q06A)
setdiff(indiv_nr_Q06A, indiv_nr_Q06B)
#globalement, peu d'individus n'ont pas répondu aux deux questions 


#Q08
any(is.na(escap$Q08))#TRUE
indiv_nr_Q08 <- escap |> filter(is.na(Q08)) |> select(A01)
indiv_nr_Q08 <- indiv_nr_Q08$A01


#Q08C
indiv_nr_Q08C <- escap |> filter(is.na(Q08C)) |> select(A01)
indiv_nr_Q08C <- indiv_nr_Q08C$A01
#individus qui ont pas connus leur parent pour X raison => réduire NR des réponses sur parents
indiv_sans_parents <- escap |> filter(Q08C %in% c(3,4)) |> 
  select(A01)
indiv_sans_parents <- indiv_sans_parents$A01 #538 
indiv_sans_pere <- escap |> filter(Q09A1 == 7) |> 
  select(A01)
indiv_sans_pere <- indiv_sans_pere$A01 #554 individus sans père
indiv_sans_mere <- escap |> filter(Q09B1 == 7) |> 
  select(A01)
indiv_sans_mere <- indiv_sans_mere$A01 #157 individus sans mere


#Q09A1
indiv_nr_Q09A1 <- escap |> filter(is.na(Q09A1)) |> select(A01)
indiv_nr_Q09A1 <- indiv_nr_Q09A1$A01 #402 non répondants 
#indiv qui ne savent pas : 
indiv_nsp_pere <- escap |> filter(Q09A1 == 6) |> select(A01)
indiv_nsp_pere <- indiv_nsp_pere$A01

#Q09B1
indiv_nr_Q09B1 <- escap |> filter(is.na(Q09B1)) |> select(A01)
indiv_nr_Q09B1 <- indiv_nr_Q09B1$A01 
#indiv qui ne savent pas : 
indiv_nsp_mere <- escap |> filter(Q09B1 == 6) |> select(A01)
indiv_nsp_mere <- indiv_nsp_mere$A01

#Q10A1 
escap <- escap |> mutate(Q10A1 = ifelse(A01 %in% indiv_sans_pere, 9, Q10A1))
escap <- escap |> mutate(Q10A1 = ifelse(A01 %in% indiv_sans_parents, 9, Q10A1))
#nouvelle modalité : ne sait pas 
escap <- escap |> mutate(Q10A1 = ifelse(A01 %in% indiv_nsp_pere, 0, Q10A1))
indiv_nr_Q10A1 <- escap |> filter(is.na(Q10A1)) |> select(A01)
indiv_nr_Q10A1 <- indiv_nr_Q10A1$A01
escap$Q10A1 <- as.factor(escap$Q10A1 )



#Q10B1
escap <- escap |> mutate(Q10B1 = ifelse(A01 %in% indiv_sans_mere, 9, Q10B1))
escap <- escap |> mutate(Q10B1 = ifelse(A01 %in% indiv_sans_parents, 9, Q10B1))
#nouvelle modalité : ne sait pas 
escap <- escap |> mutate(Q10B1 = ifelse(A01 %in% indiv_nsp_mere, 0, Q10B1))
indiv_nr_Q10B1 <- escap |> filter(is.na(Q10B1)) |> select(A01)
indiv_nr_Q10B1 <- indiv_nr_Q10B1$A01
escap$Q10B1 <- as.factor(escap$Q10B1)


#B08A
any(is.na(escap$B08A))#TRUE
indiv_nr_B08A <- escap |> filter(is.na(B08A)) |> select(A01)
indiv_nr_B08A<- indiv_nr_B08A$A01 #1400 non répondants
#ajout d'une nouvelle modalité : 0 = non concerné
escap <- escap |> mutate(B08A = ifelse(A01 %in% indiv_sans_pere, 0, B08A))
indiv_nr_B08A <- escap |> filter(is.na(B08A)) |> select(A01)
indiv_nr_B08A<- indiv_nr_B08A$A01
escap <- escap <- escap |> mutate(B08A = ifelse(A01 %in% indiv_sans_parents, 0, B08A))
indiv_nr_B08A <- escap |> filter(is.na(B08A)) |> select(A01)
indiv_nr_B08A<- indiv_nr_B08A$A01 
escap$B08A <- as.factor(escap$B08A)


#B08B
any(is.na(escap$B08B))#TRUE
indiv_nr_B08B <- escap |> filter(is.na(B08B)) |> select(A01)
indiv_nr_B08B<- indiv_nr_B08B$A01 #1050 non répondants
#on ajoute une modalité: 0 = non concerné
escap <- escap |> mutate(B08B = ifelse(A01 %in% indiv_sans_mere, 0, B08B))
indiv_nr_B08B <- escap |> filter(is.na(B08B)) |> select(A01)
indiv_nr_B08B<- indiv_nr_B08B$A01
escap <- escap <- escap |> mutate(B08B = ifelse(A01 %in% indiv_sans_parents, 0, B08B))
indiv_nr_B08B <- escap |> filter(is.na(B08B)) |> select(A01)
indiv_nr_B08B<- indiv_nr_B08B$A01 
escap$B08B <- as.factor(escap$B08B)

summary(escap)


#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### 
#### Construction d'une table de non réponse : 
# 1 = réponse pour la variable concernée
# 0 = NR
table_NR <- data.frame( id = escap$A01,
                        Q03 = ifelse(!(is.na(escap$Q03)), 1, 0),
                        Q04 = ifelse(!(is.na(escap$Q04)), 1, 0),
                        Q04A = ifelse(!(is.na(escap$Q04A)), 1, 0),
                        Q04B = ifelse(!(is.na(escap$Q04B)), 1, 0),
                        Q05 =  ifelse(!(is.na(escap$Q05)), 1, 0),
                        Q06A = ifelse(!(is.na(escap$Q06A)), 1, 0),
                        Q06B = ifelse(!(is.na(escap$Q06B)), 1, 0),
                        Q08 = ifelse(!(is.na(escap$Q08)), 1, 0),
                        Q08C = ifelse(!(is.na(escap$Q08C)), 1, 0),
                        Q09A1 = ifelse(!(is.na(escap$Q09A1)), 1, 0),
                        Q09B1 = ifelse(!(is.na(escap$Q09B1)), 1, 0),
                        Q10A1 = ifelse(!(is.na(escap$Q10A1)), 1, 0),
                        Q10B1 = ifelse(!(is.na(escap$Q10B1)), 1, 0),
                        B08A = ifelse(!(is.na(escap$B08A)), 1, 0),
                        B08B = ifelse(!(is.na(escap$B08B)), 1, 0)
                        )
table_NR$nb_nr <-15 -rowSums(table_NR[,-1])
table_NR$pds <- escap$pm17B
#nombre d'individu selon le nombre de non réponse
table_NR |> 
  group_by(nb_nr) |> 
  summarise(nb_individus = n())
#variables les plus touchées par la non réponse
nr_var <- 13314 - colSums(table_NR |> select(-c("id", "pds", "nb_nr"))) 
nr_var


################################################################################
# Suppression de certains individus qui accumulent trop de non réponse pour 
# être convenablement traités
################################################################################
# je ne le fais pas pour voir comment on s'en sort avec uniquement imputation
################################################################################
# Gestion des NA dans les prédicteurs : imputation avec le package missMDA
# (plus simple et moins gourmand)
# limite de l'approche : on prend pas en compte la relation à Y pour imputer
################################################################################
df_MCA <- escap |> select(-c(pm17B,A01))
df_MCA <- df_MCA |> mutate(across(everything(), as.factor))
rownames(df_MCA) <- escap$A01


res <- MCA(escap)
#très long: 
#nb_comp <- estim_ncpMCA(df_MCA, ncp.max = 5, method.cv = "Kfold", nbsim = 20)
nb_comp <- 4
X_complete <- imputeMCA(df_MCA, nb_comp)
X_complete <- X_complete$completeObs
################################################################################
# DISCRETISATION  DE Y
################################################################################
escap$Y <- as.numeric(Y)

q <- quantile(escap$Y, probs = c(0.20, 0.40, 0.60, 0.80), na.rm = TRUE)
q

escap$Y <- cut(escap$Y, breaks = c(-Inf, q, Inf), labels = c("13-", "14", "15", "16", "17+"),
                include.lowest = TRUE)

escap$Y <- as.factor(escap$Y)
################################################################################
# Gestion des NA dans Y: on procède par repondération
################################################################################
# 
Y_rep <- ifelse(is.na(escap$Y), 0, 1)
as.factor(Y_rep)
#on écarte Q04B car il y a beaucoup de corrélations avec Q04A
X_complete$Y_rep <- Y_rep
X_complete <- X_complete |> select(-Q04B)
mod <- glm(Y_rep ~ ., data = X_complete, family = binomial(link = "logit"))
summary(mod)
#probabilités prédites pour chaque individu
escap$repond <- predict(mod, type = "response", newdata = X_complete)
escap$pds_rep <- escap$pm17B/escap$repond

escap_final <- escap |> filter(!is.na(Y)) |> select(-c(pm17B, repond, Y_rep))

