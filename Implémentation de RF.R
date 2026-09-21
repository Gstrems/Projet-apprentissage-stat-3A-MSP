
library(ranger)
library(dplyr)
library(tidyr)
library(caret)
library(ggplot2)
library(readr)
library(purrr)
library(plotROC)

df_imp  =  read.csv2("Imputé_1.csv " ,   sep =  ";")
df_imp[, 2:16] <- lapply(df_imp[, 2:16], factor)

set.seed(42)

data_rf <- df_imp[, c(14, 2:13, 15:17)]
names(data_rf)[1] <- "Y"

train_index <- createDataPartition(
  data_rf$Y,
  p = 0.7,
  list = FALSE
)

train_data <- data_rf[train_index, ]
test_data <- data_rf[-train_index, ]

folds <- createFolds(
  train_data$Y,
  k = 5,
  list = TRUE,
  returnTrain = FALSE
)

#hyperparamètres testés
tuneGrid <- expand.grid(
  mtry = 1:7,
  num.trees = seq(50, 500, by = 50),
  min.node.size = c(1, 5, 10)
)

#Par VC, on teste le nb d'arbres, mtry et la taille de noeud minimale des arbres de la RF
# Attention c'est un peu lourd à compiler (ça prend 45 min)
results <- tuneGrid %>%
  mutate(
    cv_error = pmap_dbl(
      list(mtry, num.trees, min.node.size),
      function(m, ntree, node) {
        
        fold_errors <- map_dbl(
          folds,
          function(valid_index) {
            
            train_fold <- train_data[-valid_index, ]
            valid_fold <- train_data[valid_index, ]
            
            model <- ranger(
              Y ~ . - pm17B,
              data = train_fold,
              mtry = m,
              num.trees = ntree,
              min.node.size = node,
              importance = "permutation",
              classification = TRUE,
              probability = FALSE,
              case.weights = train_fold$pm17B
            )
            
            pred <- predict(
              model,
              data = valid_fold[, setdiff(names(valid_fold), c("Y", "pm17B"))]
            )$predictions
            
            mean(
              pred != valid_fold$Y
            )
          }
        )
        
        mean(fold_errors)
      }
    )
  )

best_row <- results %>%
  slice_min(cv_error, n = 1)

#autour de 300 arbres et 2 mtry
best_mtry <- best_row$mtry
best_ntree <- best_row$num.trees
best_node <- best_row$min.node.size
best_cv <- best_row$cv_error

cat(
  "Meilleur modèle : mtry =", best_mtry,
  ", ntree =", best_ntree,
  ", min.node.size =", best_node,
  ", erreur CV =", round(best_cv, 4), "\n"
)

# Meilleur modèle : mtry = 1 , ntree = 150 , min.node.size = 10 , erreur CV = 0.701 

ggplot(
  results %>%
    filter(min.node.size == best_node),
  aes(
    x = num.trees,
    y = cv_error,
    color = as.factor(mtry)
  )
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Erreur de validation croisée selon le nombre d'arbres et mtry",
    x = "Nombre d'arbres (num.trees)",
    y = "Erreur CV",
    color = "mtry"
  ) +
  theme_minimal()

final_model <- ranger(
  Y ~ . - pm17B,
  data = train_data,
  mtry = best_mtry,
  num.trees = best_ntree,
  min.node.size = best_node,
  importance = "permutation",
  classification = TRUE,
  probability = TRUE,
  case.weights = train_data$pm17B
)

cat(
  "Erreur OOB du modèle final =",
  round(final_model$prediction.error, 4),
  "\n"
)
#Erreur OOB du modèle final = 0.6909 

importance_df <- data.frame(
  Variable = names(final_model$variable.importance),
  Importance = final_model$variable.importance
) %>%
  arrange(desc(Importance))

ggplot(
  importance_df[1:min(10, nrow(importance_df)), ],
  aes(
    x = reorder(Variable, Importance),
    y = Importance
  )
) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Importance des variables (permutation)",
    x = "Variable",
    y = "Importance"
  ) +
  theme_minimal()
#plus importantes : la conso d'alcool par les parents

pred_probs <- predict(
  final_model,
  data = test_data[, setdiff(names(test_data), c("Y", "pm17B"))]
)$predictions

positive_class <- which(
  colnames(pred_probs) == levels(data_rf$Y)[2]
)

probs <- pred_probs[, positive_class]

pred_labels <- ifelse(
  probs > 0.5,
  levels(data_rf$Y)[2],
  levels(data_rf$Y)[1]
) %>%
  as.factor()

confusion <- confusionMatrix(
  pred_labels,
  test_data$Y
)

print(confusion)

roc_df <- data.frame(
  D = ifelse(
    test_data$Y == levels(data_rf$Y)[2],
    1,
    0
  ),
  M = probs
)

p <- ggplot(
  roc_df,
  aes(m = M, d = D)
) +
  geom_roc(
    color = "darkblue",
    size = 1.2,
    n.cuts = 0
  ) +
  style_roc() +
  labs(
    title = "Courbe ROC avec plotROC"
  )

roc_stats <- calc_auc(p)

print(p)
print(roc_stats)

# AUC reste autour de 0,65 même avec un algo  
# bien moins lourd (sans regarder les noeuds) 
#  même   sans  prendre  en  compte  les  poids
#  et peu importe le df imputé (1 à 5)

