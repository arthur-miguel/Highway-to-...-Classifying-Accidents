library(caret)
library(ranger)
library(ggplot2)
library(shapviz)
library(fastshap)
library(patchwork)
library(doParallel)

set.seed(123)

## Calculates SHAP values for RF model with oversampling
# options(ranger.num.threads = 1)
# 
# dataset <- readRDS('df_withSMOTE_south.rds')
# dataset$gravidade <- as.factor(make.names(dataset$gravidade))
# 
# models <- readRDS('models_withSMOTE_south.rds')
# rf <- models$RF
# 
# X <- dataset[which(names(dataset) != "gravidade")]
# X <- as.data.frame(X)
# 
# pred_fun <- function(model, newdata) {
#   predict(model, newdata = newdata, type = "prob")[, "Com.Vítimas.Fatais"]
# }
# 
# numCores <- 16
# numCores <- detectCores() - 2
# cl <- makeCluster(numCores)
# registerDoParallel(cl)
# 
# s <- explain(rf, X = X, pred_wrapper = pred_fun, nsim = 100, adjust = T, shap_only = F, parallel = T)
# 
# saveRDS(s, "shap_values_full.rds")
# 
# stopCluster(cl)

# Loads pre calculates shap values
s <- readRDS("shap_values.rds")

sv <- shapviz(s)

new_labels <- c(
  br_km = "br_km",
  tipo_acidente = "accident_type",
  tipo_pista = "road_type",
  uso_solo = "road_out",
  fase_dia = "day period",
  tipo_veiculo = "vehicle_type",
  dia_semana = "day_week",
  sentido_via = "road_direction",
  condicao_metereologica = "met_cond",
  ano_fabricacao_veiculo = "year_vehicle",
  idade = "age",
  sexo = "gender"
)

# Creates beautiful plots used in the paper :)
plot_bee <- sv_importance(sv[sample(nrow(sv), 5000), ], kind = "bee", alpha = 0.5) +
  scale_color_gradient2(low = "blue4", mid = "gray", high = "red", midpoint = 0.5,
                        guide = guide_colorbar(
                             direction = "horizontal",
                             title.position = "left",
                             title.vjust = 1,
                             barwidth = 15,
                             barheight = 0.5
                           )) +
  scale_y_discrete(labels = new_labels) +
  theme_classic() +
  labs(x = "SHAP Value") + 
  theme(legend.position = "bottom")

plot_values <- sv_importance(sv, fill = 'blue4') + 
  scale_y_discrete(labels = new_labels) +
  theme_classic()

plot_km <- sv_dependence(sv, 
              v = c("br_km"), 
              alpha = 0.3, 
              interactions = F, 
              color_var = NULL, color = "blue4") + theme_classic()

plot_acidente <- sv_dependence(sv, 
                         v = c("tipo_acidente"), 
                         alpha = 0.3, 
                         interactions = F, 
                         color_var = NULL, color = "blue4") + 
  labs(x = "accident_type", y = NULL) + theme_classic()

p <- plot_values / plot_bee + 
  plot_layout(heights = c(0.5, 1))


#=====================

df_shap <- as.data.frame(s$shapley_values)
encoding <- readRDS('df_encoding.rds')

#======tipo acidente=====
accident_type <- data.frame(value = df_shap$tipo_acidente, category = encoding$tipo_acidente)

mean_shap <- accident_type %>%
  group_by(category) %>%
  summarise(mean_value = mean(value))

accident_type <- left_join(accident_type, mean_shap, by = "category")
accident_type$category <- reorder(accident_type$category, -accident_type$mean_value)

accident_types_en <- c(
  "Atropelamento"             = "Run over",
  "Capotamento"               = "Rollover",
  "Colisão"                   = "Front Collision",
  "Colisão lateral"           = "Side collision",
  "Engavetamento"             = "Rear-end collision",
  "Eventos atípicos"          = "Atypical events",
  "Incêndio"                  = "Fire",
  "Saída de leito carroçável" = "Off-road",
  "Tombamento"                = "Overturn"
)

p_acc_type <- ggplot(accident_type, aes(x = category, y = value, fill = mean_value)) +
  geom_violin(color = "black", alpha = 0.8) +
  geom_boxplot(width = 0.1, outlier.shape = NA, fill = "white") +
  scale_fill_gradient2(
    low = "blue4",
    mid = "gray",
    high = "red",
    midpoint = 0,
    name = "Mean"
  ) +
  scale_x_discrete(labels = accident_types_en) +
  theme_classic() +
  theme(
    legend.position = "none", 
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(x = "accident_type", y = "")
 
#======tipo pista=====
road_type <- data.frame(value = df_shap$tipo_pista, category = encoding$tipo_pista)

mean_shap <- road_type %>%
  group_by(category) %>%
  summarise(mean_value = mean(value))

road_type <- left_join(road_type, mean_shap, by = "category")
road_type$category <- reorder(road_type$category, -road_type$mean_value)

road_type_en <- c(
  "Dupla" = "Double lane",
  "Simples" = "Single lane",
  "Múltipla" = "Multiple lanes"
)

p_road_type <- ggplot(road_type, aes(x = category, y = value, fill = mean_value)) +
  geom_violin(color = "black", alpha = 0.8) +
  geom_boxplot(width = 0.1, outlier.shape = NA, fill = "white") +
  scale_fill_gradient2(
    low = "blue4",
    mid = "gray",
    high = "red",
    midpoint = 0,
    name = "Mean"
  ) +
  scale_x_discrete(labels = road_type_en) +
  theme_classic() +
  theme(
    legend.position = "none", 
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(x = "road_type", y = "SHAP Value")

#======saida da pista========

road_out <- data.frame(value = df_shap$uso_solo, category = encoding$uso_solo)

mean_shap <- road_out %>%
  group_by(category) %>%
  summarise(mean_value = mean(value))

road_out <- left_join(road_out, mean_shap, by = "category")
road_out$category <- reorder(road_out$category, -road_out$mean_value)

road_out_en <- c(
  "Não" = "No",
  "Sim" = "Yes"
)

p_road_out <- ggplot(road_out, aes(x = category, y = value, fill = mean_value)) +
  geom_violin(color = "black", alpha = 0.8) +
  geom_boxplot(width = 0.1, outlier.shape = NA, fill = "white") +
  scale_fill_gradient2(
    low = "blue4",
    mid = "gray",
    high = "red",
    midpoint = 0,
    name = "Mean"
  ) +
  scale_x_discrete(labels = road_out_en) +
  theme_classic() +
  theme(
    legend.position = "none", 
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(x = "road_out", y = "")


#======day period========

day_period <- data.frame(value = df_shap$fase_dia, category = encoding$fase_dia)

mean_shap <- day_period %>%
  group_by(category) %>%
  summarise(mean_value = mean(value))

day_period <- left_join(day_period, mean_shap, by = "category")
day_period$category <- reorder(day_period$category, -day_period$mean_value)

day_period_en <- c(
  "Amanhecer"   = "Dawn",
  "Anoitecer"   = "Dusk",
  "Plena Noite" = "Night",
  "Pleno dia"   = "Daylight"
)

p_day_period <- ggplot(day_period, aes(x = category, y = value, fill = mean_value)) +
  geom_violin(color = "black", alpha = 0.8) +
  geom_boxplot(width = 0.1, outlier.shape = NA, fill = "white") +
  scale_fill_gradient2(
    low = "blue4",
    mid = "gray",
    high = "red",
    midpoint = 0,
    name = "Mean"
  ) +
  scale_x_discrete(labels = day_period_en) +
  theme_classic() +
  theme(
    legend.position = "none", 
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(x = "day_period", y = "")

#======vehicle type========

vehicle_type <- data.frame(value = df_shap$tipo_veiculo, category = encoding$tipo_veiculo)

mean_shap <- vehicle_type %>%
  group_by(category) %>%
  summarise(mean_value = mean(value))

vehicle_type <- left_join(vehicle_type, mean_shap, by = "category")
vehicle_type$category <- reorder(vehicle_type$category, -vehicle_type$mean_value)

vehicle_type_en <- c(
  "Bicicleta"   = "Bicycle",
  "Caminhão"    = "Truck",
  "Carro"       = "Car",
  "Motocicleta" = "Motorcycle",
  "Motor-casa"  = "Motorhome",
  "Ônibus"      = "Bus"
)

p_vehicle_type <- ggplot(vehicle_type, aes(x = category, y = value, fill = mean_value)) +
  geom_violin(color = "black", alpha = 0.8) +
  geom_boxplot(width = 0.1, outlier.shape = NA, fill = "white") +
  scale_fill_gradient2(
    low = "blue4",
    mid = "gray",
    high = "red",
    midpoint = 0,
    name = "Mean"
  ) +
  scale_x_discrete(labels = vehicle_type_en) +
  theme_classic() +
  theme(
    legend.position = "none", 
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(x = "vehicle_type", y = "")

p_acc_type / (p_road_type + p_road_out) / (p_day_period + p_vehicle_type)

