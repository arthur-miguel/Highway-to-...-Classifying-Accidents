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
library(ranger)
library(MLeval)
library(kernlab)
library(randomForest)

set.seed(420)

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
    df[[paste0(var, "_encoded")]] <- NA 
    
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
files <- list.files(path = "data/", 
                    pattern = "acidentes20\\d{2}.csv", 
                    full.names = TRUE)

# Set type and select attributes in the dataset
col_types <- cols(
  id                     = col_skip(),
  pesid                  = col_skip(),
  data_inversa           = col_skip(),
  dia_semana             = col_character(),
  horario                = col_skip(),
  uf                     = col_character(),
  br                     = col_number(),
  km                     = col_number(),
  municipio              = col_skip(),
  causa_acidente         = col_skip(),
  tipo_acidente          = col_character(),
  classificacao_acidente = col_character(),
  fase_dia               = col_character(),
  sentido_via            = col_character(),
  condicao_metereologica = col_character(),
  tipo_pista             = col_character(),
  tracado_via            = col_skip(),
  uso_solo               = col_character(),
  id_veiculo             = col_skip(),
  tipo_veiculo           = col_character(),
  marca                  = col_skip(),
  ano_fabricacao_veiculo = col_integer(),
  tipo_envolvido         = col_skip(),
  estado_fisico          = col_skip(),
  idade                  = col_number(),
  sexo                   = col_character(),
  ilesos                 = col_integer(),
  feridos_leves          = col_integer(),
  feridos_graves         = col_integer(),
  mortos                 = col_integer(),
  latitude               = col_skip(),
  longitude              = col_skip(),
  regional               = col_skip(),
  delegacia              = col_skip(),
  uop                    = col_skip()
)


# Reads all files from 2021 -> 2024
df <- files %>% lapply(read_) %>% bind_rows()

# Select a region
df <- df %>% filter(uf %in% c("PR","SC","RS")) 

# Remove all entries with missing or misfiled data
df <- na.omit(df)
df <- df %>% filter(ano_fabricacao_veiculo != 0)
df <- df %>% filter(idade >= 1)
df <- df %>% filter(idade <= 120)
df <- df %>% filter(sexo == "Masculino" | sexo == "Feminino")
df <- df %>% filter(condicao_metereologica != "Ignorado")
df <- df %>% filter(sentido_via != "Não Informado")
df <- df %>% filter(tipo_veiculo != "Não Informado")

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

# Concatenates road_id and road_km in br_km
df <- df %>% mutate(km = round(km / 5) * 5)
df <- df %>%
  mutate(br_km = paste0(br, "_", km)) %>%
  select(-br, -km)

df <- df %>%
  mutate(tipo_veiculo = as.character(tipo_veiculo)) %>%
  mutate(tipo_acidente = as.character(tipo_acidente))

# Standardizes categorical feeatures
df <- df %>%
  mutate(tipo_veiculo = ifelse(tipo_veiculo %in% c("Camioneta","Caminhonete","Caminhão-trator","Caminhão","Reboque","Semireboque","Trator de rodas"), "Caminhão", tipo_veiculo)) %>%
  mutate(tipo_veiculo = ifelse(tipo_veiculo %in% c("Ônibus","Micro-ônibus"), "Ônibus", tipo_veiculo)) %>%
  mutate(tipo_veiculo = ifelse(tipo_veiculo %in% c("Motocicleta","Motoneta"), "Motocicleta", tipo_veiculo)) %>%
  mutate(tipo_veiculo = ifelse(tipo_veiculo %in% c("Chassi-plataforma","Ciclomotor","Triciclo","Outros"), "Outros", tipo_veiculo)) %>%
  mutate(tipo_veiculo = ifelse(tipo_veiculo %in% c("Automóvel","Utilitário"), "Carro", tipo_veiculo)) %>%
  filter(tipo_veiculo != "Outros")

df <- df %>%
  mutate(tipo_acidente = ifelse(tipo_acidente %in% c("Colisão traseira","Colisão com objeto","Colisão frontal"), "Colisão", tipo_acidente)) %>%
  mutate(tipo_acidente = ifelse(tipo_acidente %in% c("Colisão lateral sentido oposto","Colisão lateral mesmo sentido","Colisão lateral","Colisão transversal"), "Colisão lateral", tipo_acidente)) %>%
  mutate(tipo_acidente = ifelse(tipo_acidente %in% c("Atropelamento de Pedestre","Atropelamento de Animal"), "Atropelamento", tipo_acidente)) %>%
  mutate(tipo_acidente = ifelse(tipo_acidente %in% c("Eventos atípicos","Queda de ocupante de veículo","Derramamento de carga"), "Eventos atípicos", tipo_acidente))

# Convert tipo_veiculo back to a factor
df <- df %>%
  mutate(tipo_veiculo = as.factor(tipo_veiculo)) %>%
  mutate(tipo_acidente = as.factor(tipo_acidente))


# Encodes categorical variables in the dataset
features.categorical <- names(col_types$cols)[sapply(col_types$cols, function(x) inherits(x, "collector_character"))]
features.categorical <- c(features.categorical, 'br_km')
features.categorical <- features.categorical[-4]
df <- kfold_encode(df, "gravidade", features.categorical)

# Remove UF column
df <- df %>% select(-uf)

# Center and scales all numerical data
df <- df %>% mutate_if(is.numeric, ~ as.numeric(scale(.)))

# Splits data for further training, testing and validation
train.idx <- createDataPartition(df$gravidade, p = 0.8, list = FALSE)
df <- df[train.idx, ]
df.test <- df[-train.idx, ]

# Performs upscaling using SMOTE
# To train models without oversampling just comment the following four lines
res.smote <- SMOTE(subset(df, select = -gravidade), df$gravidade, K = 3, dup_size = 12)
df <- res.smote$data
colnames(df)[ncol(df)] <- "gravidade"
df$gravidade <- as.factor(df$gravidade)

df$gravidade <- as.factor(make.names(df$gravidade))
df.test$gravidade <- as.factor(make.names(df.test$gravidade))

### Train Suport Vector Machine, K-nn, Decision Tree and Single Hidden Layer Neural Network models
# Defines 80/20 monte carlo cross-validation partitioning 
models.ctrl <- trainControl(method = "LGOCV", 
                            number = 5, 
                            search = "grid", 
                            p = 0.8,
                            summaryFunction=twoClassSummary,
                            metric = "ROC",
                            classProbs=T,
                            savePredictions = T)

# Sets hyperparameters tunning grids
knn.grid <- expand.grid(k = seq(3, 11, by = 2))

nnet.grid <- expand.grid(
  size = seq(11, 155, by = 8),
  decay = 10^seq(-4, -1, by = 1)
)

rf.grid <- expand.grid(
  mtry = 1:(ncol(df) - 1),
  splitrule = c("gini", "extratrees"),
  min.node.size = c(1, 5, 10)
) 

# doParallel cluster config for parallel execution
# !!! Be carefull with memory usage !!!
numCores <- detectCores() - 2
cl <- makeCluster(numCores)
registerDoParallel(cl)

# Models training 
knn.res <- train(gravidade ~ ., data = df, method = "knn",
                 trControl = models.ctrl, tuneGrid = knn.grid)

rf.res <- train(gravidade ~ ., data = df, method = "ranger",
                trControl = models.ctrl, tuneGrid = rf.grid,
                allowParallel = TRUE)

nnet.res <- train(gravidade ~ ., data = df, method = "nnet",
                  trControl = models.ctrl, tuneGrid = nnet.grid,
                  allowParallel = TRUE)

# Results aggregation
model.list <- list(
  "kNN" = knn.res,
  "RF"  = rf.res,
  "MLP" = nnet.res
)

saveRDS(model.listm, "models_withSMOTE_south.rds")

for (name in names(model.list)) {
  model <- model.list[[name]]
  pred <- predict(model, newdata = df.test)
  cmat <- confusionMatrix(pred, df.test$gravidade)
  cat("\n======", name, "======\n")
  print(model)
  print(cmat)
}

stopCluster(cl)
unregister_dopar()
