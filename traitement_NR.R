################################################################################
# Dans ce fichier on traite la non réponse dans les données
# Objectif : obtenir un jeu de données utilisable pour les modèles
################################################################################
#charger ESCAP depuis un fichier dédié et inscrit au .gitignore
library(dplyr)

# Etat de la non-réponse pour chaque variable et premiers ajustements
################################################################################
summary(escap)
#pm17B = poids de sondages
any(is.na(escap$pm17B)) #FALSE : on a l'info sur toutes les variables
plot(escap$pm17B) 
summary(escap$pm17B)

#la variable d'intérêt Q19A (âge au premier alcool)
Y <- escap$Q19A
any(is.na(Y))#TRUE
indiv_na_Y <-which(is.na(Y))
length(indiv_na_Y) #2393 non répondant


#Q03 : sexe
any(is.na(escap$Q03)) #TRUE
which(is.na(escap$Q03)) #23 non répondants
indiv_nr_Q03 <- escap[which(is.na(escap$Q03)),1]
summary(pds_indiv_nr_Q03)



#Q04 A et B: situation scolaire (A) et pro (B)
any(is.na(escap$Q04A)) #TRUE
any(is.na(escap$Q04B)) #TRUE
length(which(is.na(escap$Q04A))) #593 non répondants
length(which(is.na(escap$Q04B))) #12777 non répondants 
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
any(is.na(escap$Q04))#TRUE
indiv_nr_Q04 <- escap[which(is.na(escap$Q04)),1] #1583
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
any(is.na(escap$Q08C))#TRUE
indiv_nr_Q08C <- escap |> filter(is.na(Q08C)) |> select(A01)
indiv_nr_Q08C <- indiv_nr_Q08C$A01


#Q09A1
any(is.na(escap$Q09A1))#TRUE
indiv_nr_Q09A1 <- escap |> filter(is.na(Q09A1)) |> select(A01)
indiv_nr_Q09A1 <- indiv_nr_Q09A1$A01 #402 non répondants 

#Q09B1
any(is.na(escap$Q09B1))#TRUE
indiv_nr_Q09B1 <- escap |> filter(is.na(Q09B1)) |> select(A01)
indiv_nr_Q09B1 <- indiv_nr_Q09B1$A01 

#B08A
any(is.na(escap$B08A))#TRUE
indiv_nr_B08A <- escap |> filter(is.na(B08A)) |> select(A01)
indiv_nr_B08A<- indiv_nr_B08A$A01 #1400 non répondants

#B08B
any(is.na(escap$B08B))#TRUE
indiv_nr_B08B <- escap |> filter(is.na(B08B)) |> select(A01)
indiv_nr_B08B<- indiv_nr_B08B$A01 #1050 non répondants





x
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
                        B08B = ifelse(!(is.na(escap$B08B)), 1, 0),
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


