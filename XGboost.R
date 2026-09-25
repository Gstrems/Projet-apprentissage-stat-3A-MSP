################################################################################
# Implementatation de XGBoost
################################################################################

library(dplyr)
library(tidyr)
library(tidyverse)
library(xgboost)
library(plotROC)
library(yardstick)
library(forcats)
library(SHAPforxgboost)
library(ggplot2)
library(caret)
library(questionr)
#on récupère 
df <- escap_final
#model.matrix gère difficilement les NA dans les factors : on en fait une modalité à part
#séparation train/test
df <- df |> mutate(across(everything(), ~ifelse(is.na(.), "NR", .)))
df <- df |> mutate(across(!all_of(c("pds_rep", "A01")), as.factor))
df <- df 
set.seed(123)
split <- createDataPartition(df$Y, p = 0.8, list = FALSE)

train <- df[split, -1]
test  <- df[-split, -1]
# Nombre de classes distinctes
num_classes <- nlevels(df$Y)

#matrices pour xgboost (one-hot encoding)
dtrain <- model.matrix(Y ~ . - 1 - pds_rep, data = train, na.action = na.pass)
dtest  <- model.matrix(Y ~ . - 1 - pds_rep, data = test,  na.action = na.pass)

# XGBoost multiclasse attend un vecteur d'entiers allant de 0 à (num_classes - 1)
y_train <- as.numeric(train$Y) -1
y_test <- as.numeric(test$Y) -1

w_train <- train$pds_rep
w_test  <- test$pds_rep

# Création des objets xgb.DMatrix (recommandé pour gérer facilement les poids)
xgb_train <- xgb.DMatrix(data = dtrain, label = y_train, weight = w_train)
xgb_test  <- xgb.DMatrix(data = dtest, label = y_test, weight = w_test)

#définition des hyperparamètres (je reprends ceux de la prof)
param_grid <- expand.grid(max_depth = c(3, 5, 7), 
                          nrounds = c(50, 100, 150))                       
best_acc <- 0
best_params <- list()


for (i in 1:nrow(param_grid)) {
  depth  <- param_grid$max_depth[i]
  nround <- param_grid$nrounds[i]
  
  cv_params <- list(
    objective   = "multi:softprob",
    num_class   = num_classes,
    max_depth   = depth,
    eval_metric = "merror",
    verbosity   = 0   # Placé à l'intérieur
  )
  
  cv <- xgb.cv(
    params                = cv_params,
    data                  = xgb_train,
    nrounds               = nround,
    nfold                 = 5,
    early_stopping_rounds = 10
  )
  
  acc <- 1 - min(cv$evaluation_log$test_merror_mean)
  if (acc > best_acc) {
    best_acc    <- acc
    best_params <- list(nrounds = nround, max_depth = depth)
  }
}
# paramètres optimisés par cv
params <- list(
  objective   = "multi:softprob",
  num_class   = num_classes,
  max_depth   = best_params$max_depth,
  eval_metric = "merror",
  verbosity   = 0   
)
final_model <- xgb.train(
  params  = params,
  data    = xgb_train,
  nrounds = best_params$nrounds   # Traité séparément de params
)

#  Calcul des probabilités de prédiction sur l'objet xgb_test
# 'predict' renvoie un vecteur plat de taille (N_observations * num_classes)
probs_vector <- predict(final_model, newdata = xgb_test)

# 2. Transformation du vecteur plat en matrice [N_observations x num_classes]
probs_matrix <- matrix(
  probs_vector, 
  ncol = num_classes, 
  byrow = TRUE
)

# Optionnel : Nommer les colonnes avec les vrais noms de classes pour plus de clarté
colnames(probs_matrix) <- levels(train$Y)

# 3. Extraction de la classe ayant la probabilité maximale pour chaque ligne
# max.col() donne l'indice de la colonne maximale (1 à num_classes)
pred_class_index <- max.col(probs_matrix) 

# 4. Conversion des indices en facteurs correspondant aux modalités d'origine
pred_lab <- factor(
  levels(train$Y)[pred_class_index], 
  levels = levels(train$Y)
)

# 5. Récupération des vrais labels de test sous forme de facteurs
y_test_lab <- factor(
  levels(train$Y)[y_test + 1], 
  levels = levels(train$Y)
)

#matrice de confusion #en l'état : on arrive pas vraiment à bien prédire
conf_matrix_weighted <- xtabs(w_test ~ pred_lab + y_test_lab)
conf_matrix_weighted
#importance des variables
importance <- xgb.importance(model = final_model)
xgb.plot.importance(importance_matrix = importance[1:10,])

