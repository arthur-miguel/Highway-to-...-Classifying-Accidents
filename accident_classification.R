#' =============================================================================
#' @name 			   accident_classification
#' 
#' @brief        Performs classification on traffic accident data set.
#' 
#' @description
#' Loads, clean and encode data from the PRF data set. Apply SMOTE for data 
#' oversampling and RF-RFE for feature selection. Uses kNN, rpart, svm and nnet
#' for classification.
#' 
#' @author 			 Arthur Gabardo | arthur.miguel(at)grad.ufsc.br
#' =============================================================================

library(readr)
library(dplyr)
library(tidyr)
library(stringi)
library(reshape2)
library(doParallel)
library(smotefamily)

library(nnet)
library(rpart)
library(e1071)
library(caret)
library(MLeval)

set.seed(420)

theme_transparent <- function() {
  theme_minimal() +
    theme(
      panel.background = element_rect(color = NA),
      plot.background = element_rect(color = NA),
      legend.background = element_rect(color = NA),
      legend.box.background = element_rect(color = NA),
      legend.key = element_rect(color = NA)
    )
}

theme_set(theme_transparent())

# Function to use lapply to read all files to single dataframe
read_ <- function(file) {
  df <- suppressWarnings(read_delim(file, 
                                    locale = locale(encoding = "latin1", 
                                                    decimal_mark = ','), 
                                    delim = ";", 
                                    col_types = col_types
  ))
  return(df)
}

kfold_encode <- function(df, target, categorical_vars, k = 5) {
  class_col <- df[[target]]
  df <- df %>%
    mutate(across(all_of(target), as.numeric))
  folds <- createFolds(df[[target]], k = k)
  
  for (var in categorical_vars) {
    df[[paste0(var, "_encoded")]] <- NA  # initialize new column
    
    for (i in 1:k) {
      train_idx <- folds[[i]]
      fold_df <- df[train_idx, ]
      
      # Calculate mean target for each category in the training fold
      encoding_map <- fold_df %>%
        group_by_at(var) %>%
        summarise(mean_target = mean(get(target), na.rm = TRUE))
      
      # Join the mean target encoding back to the original dataframe
      df <- df %>%
        left_join(encoding_map, by = var) %>%
        mutate(!!paste0(var, "_encoded") := ifelse(is.na(mean_target), mean(df[[target]], na.rm = TRUE), mean_target)) %>%
        select(-mean_target)
    }
  }
  
  # remove non_encoded columns
  df <- df %>% select(-all_of(categorical_vars))
  colnames(df) <- gsub("_encoded", "", colnames(df))
  df[[target]] <- class_col
  return(df)
}

unregister_dopar <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}

# List all CSV files matching the pattern "acidentes20**"
# Glob files in source directory
files <- list.files(path = "../data/", 
                    pattern = "acidentes20\\d{2}.csv", 
                    full.names = TRUE)

# Set type of attributes in the dataset
col_types <- cols(
  id                     = col_skip(),
  pesid                  = col_skip(),
  data_inversa           = col_skip(),#col_date(format = "%Y-%m-%d"),
  dia_semana             = col_character(),
  horario                = col_skip(),#col_time(format = "%H:%M:%S"),
  uf                     = col_character(),
  br                     = col_number(),
  km                     = col_number(),
  municipio              = col_skip(),#col_character(),
  causa_acidente         = col_skip(),#col_character(),
  tipo_acidente          = col_character(),
  classificacao_acidente = col_factor(),
  fase_dia               = col_character(),
  sentido_via            = col_character(),
  condicao_metereologica = col_character(),
  tipo_pista             = col_character(),
  tracado_via            = col_skip(),#col_character(),
  uso_solo               = col_character(),
  id_veiculo             = col_skip(),#col_integer(),
  tipo_veiculo           = col_character(),
  marca                  = col_skip(),#col_character(),
  ano_fabricacao_veiculo = col_integer(),
  tipo_envolvido         = col_character(),
  estado_fisico          = col_skip(),#col_character(),
  idade                  = col_number(),
  sexo                   = col_character(),
  ilesos                 = col_integer(),
  feridos_leves          = col_integer(),
  feridos_graves         = col_integer(),
  mortos                 = col_integer(),
  latitude               = col_skip(),#col_double(),
  longitude              = col_skip(),#col_double(),
  regional               = col_skip(),#col_character(),
  delegacia              = col_skip(),#col_character(),
  uop                    = col_skip()#col_character()
)


# Reads all files from 2017 -- 2023
df <- files %>% lapply(read_) %>% bind_rows()

# Remove all entries with missing or misfiled data
df <- na.omit(df)
df <- df %>% filter(ano_fabricacao_veiculo != 0)
df <- df %>% filter(idade >= 1)
df <- df %>% filter(idade <= 120)
df <- df %>% filter(sexo == "Masculino" | sexo == "Feminino")
df <- df %>% filter(condicao_metereologica != "Ignorado")
df <- df %>% filter(sentido_via != "Não Informado")
df <- df %>% filter(tipo_veiculo != "Não Informado")

# Select a state
df <- df %>% filter(uf == "PR") 

# Creates column gravidade
df <- df %>%
  mutate(classificacao_acidente = as.character(classificacao_acidente)) %>%
  mutate(classificacao_acidente = ifelse(classificacao_acidente == "Com Vítimas Feridas", "Sem Vítimas", classificacao_acidente))
  
  
df <- df %>%
  mutate(
    gravidade = classificacao_acidente
  ) %>%
  select(-(ilesos:mortos),classificacao_acidente) %>%
  select(-classificacao_acidente) %>%
  mutate(gravidade = factor(gravidade, levels = c("Com Vítimas Fatais", "Sem Vítimas")))

df <- df %>%
  filter(gravidade != "Com Vítimas Feridas") %>%
  mutate(gravidade = droplevels(gravidade))

df <- df %>% mutate(km = round(km / 5) * 5)

df <- df %>%
  mutate(tipo_veiculo = as.character(tipo_veiculo)) %>%
  mutate(tipo_acidente = as.character(tipo_acidente))

df <- df %>%
  mutate(tipo_veiculo = ifelse(tipo_veiculo %in% c("Camioneta","Caminhonete","Caminhão-trator","Caminhão","Reboque","Semireboque","Trator de rodas"), "Caminhão", tipo_veiculo)) %>%
  mutate(tipo_veiculo = ifelse(tipo_veiculo %in% c("Ônibus","Micro-ônibus"), "Ônibus", tipo_veiculo)) %>%
  mutate(tipo_veiculo = ifelse(tipo_veiculo %in% c("Motocicleta","Motoneta"), "Motocicleta", tipo_veiculo)) %>%
  mutate(tipo_veiculo = ifelse(tipo_veiculo %in% c("Automóvel","Utilitário"), "Carro", tipo_veiculo))

df <- df %>%
  mutate(tipo_acidente = ifelse(tipo_acidente %in% c("Colisão traseira","Colisão com objeto","Colisão frontal"), "Colisão", tipo_acidente)) %>%
  mutate(tipo_acidente = ifelse(tipo_acidente %in% c("Colisão lateral sentido oposto","Colisão lateral mesmo sentido","Colisão lateral","Colisão transversal"), "Colisão lateral", tipo_acidente)) %>%
  mutate(tipo_acidente = ifelse(tipo_acidente %in% c("Atropelamento de Pedestre","Atropelamento de Animal"), "Atropelamento", tipo_acidente)) %>%
  mutate(tipo_acidente = ifelse(tipo_acidente %in% c("Eventos atípicos","Queda de ocupante de veículo","Derramamento de carga"), "Eventos atípicos", tipo_acidente))

# Convert tipo_veiculo back to a factor
df <- df %>%
  mutate(tipo_veiculo = as.factor(tipo_veiculo)) %>%
  mutate(tipo_acidente = as.factor(tipo_acidente))

## Encodes categorical variables in the dataset
features.categorical <- names(col_types$cols)[sapply(col_types$cols, function(x) inherits(x, "collector_character"))]
df <- kfold_encode(df, "gravidade", features.categorical)
# names_ <- colnames(df)
# df <- df %>% 
#   mutate(across(all_of(features.categorical), ~ as.numeric(as.factor(.))))
# colnames(df) <- names_

# Remove UF column
df <- df %>% select(-uf)

# Center and scales all numerical data
df <- df %>% mutate_if(is.numeric, ~ as.numeric(scale(.)))

# Performs upscaling using SMOTE
train.idx <- createDataPartition(df$gravidade, p = 0.7, list = FALSE)
df <- df[train.idx, ]
df.test <- df[-train.idx, ]

res.smote <- SMOTE(subset(df, select = -gravidade), df$gravidade, K = 3, dup_size = 5)
df <- res.smote$data
colnames(df)[ncol(df)] <- "gravidade"
df$gravidade <- as.factor(df$gravidade)

# Defines cluster parameters
numCores <- detectCores() - 1
cl <- makeCluster(numCores)
registerDoParallel(cl)

# Perform Recursive Feature Elimination (RFE)
rfe.ctrl <- rfeControl(functions = rfFuncs, # random forest
                       method = "repeatedcv", # cross-validation
                       repeats = 5,
                       number = 10, # number of folds
                       allowParallel = T)

rfe.res <- rfe(gravidade ~ ., data = df,
                  sizes = c(1:ncol(df)-1),
                  rfeControl = rfe.ctrl)

stopCluster(cl)
unregister_dopar()

features.best <- predictors(rfe.res)
features.best <- c(features.best,"gravidade")

df <- df %>% select(all_of(features.best)) 
df.test <- df.test %>% select(all_of(features.best)) 

### Train Suport Vector Machine, K-nn, Decision Tree and Single Hidden Layer Neural Network models

df$gravidade <- as.factor(make.names(df$gravidade))

# Defines 80/20 cross-validation partitioning 
models.ctrl <- trainControl(method = "LGOCV", 
                            number = 5, 
                            search = "grid", 
                            p = 0.8,
                            summaryFunction=twoClassSummary,
                            classProbs=T,
                            savePredictions = T)

# Models grid search parameters
svm.grid <- expand.grid(
  C = seq(0.1, 1.0, by = 0.1)
)

knn.grid <- expand.grid(
  k = seq(3, 11, by = 2)
)

tree.grid <- expand.grid(
  maxdepth = seq(2, 20, by = 2)
)

nnet.grid <- expand.grid(
  size = seq(11, 51, by = 4),
  decay = 10^seq(-4, -1, by = 1)
)

# Train

numCores <- detectCores() - 3
cl <- makeCluster(numCores)
registerDoParallel(cl)

svm.res  <- train(gravidade ~ ., data = df, method = "svmLinear", trControl = models.ctrl, tuneGrid = svm.grid, alowParallel = T)
svm.pred <- predict(svm.res, df)
svm.cmat <- confusionMatrix(svm.pred, df$gravidade)
print(svm.res)
print(svm.cmat)
print("/n===============================/n")

knn.res  <- train(gravidade ~ ., data = df, method = "knn", trControl = models.ctrl, tuneGrid = knn.grid)
knn.pred <- predict(knn.res, df)
knn.cmat <- confusionMatrix(knn.pred, df$gravidade)
print(knn.res)
print(knn.cmat)
print("/n===============================/n")

tree.res <- train(gravidade ~ ., data = df, method = "rpart2", trControl = models.ctrl, tuneGrid = tree.grid)
tree.pred <- predict(tree.res, df)
tree.cmat <- confusionMatrix(tree.pred, df$gravidade)
print(tree.res)
print(tree.cmat)
print("/n===============================/n")

nnet.res <- train(gravidade ~ ., data = df, method = "nnet", trControl = models.ctrl, tuneGrid = nnet.grid, alowParallel = T)
nnet.pred <- predict(nnet.res, df)
nnet.cmat <- confusionMatrix(nnet.pred, df$gravidade)
print(nnet.res)
print(nnet.cmat)
print("/n===============================/n")

stopCluster(cl)
unregister_dopar()
