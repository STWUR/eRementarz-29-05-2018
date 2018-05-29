## Biblioteki, dane i model ---
library(tidyverse)
library(randomForest)
load(url("https://github.com/pbiecek/DALEX_docs/raw/master/workshops/eRum2018/houses.rda"))
houses
hrf <- randomForest(sqm_price ~., data = houses)
## Archivist: zapis i wczytanie artefaktu ----
library(archivist)
### Tworzymy lokalne repo
createLocalRepo(".")
asave(hrf, repoDir = ".")
summaryLocalRepo(".")
showLocalRepo(".")
setLocalRepo(".")
rf_model <- aread("7b5eac371d81936f5c50c465f6fee3ff")
 ## DALEX: pojedyncza zmienna
library(DALEX)
rf_expl <- DALEX::explain(hrf, data = houses,
                          y = houses$sqm_price)
year_expl <- single_variable(rf_expl, "year")
plot(year_expl)
## DALEX: ważność zmiennych
global_feat_imp <- variable_importance(rf_expl)
plot(global_feat_imp)
## DALEX: ocena modelu / performance
rf_perf <- model_performance(rf_expl)
plot(rf_perf, geom = "boxplot")
plot(rf_perf, geom = "ecdf")
## DALEX + auditor: diagnostyka
library(auditor)
rf_audit <- audit(rf_expl)
plotPrediction(rf_audit)
plotResidualDensity(rf_audit, variable = "district")
## DALEX + breakDown: wyjaśnianie pojedynczej predykcji
### Model linowy
linear_model <- lm(sqm_price ~., data = houses)
lm_explainer <- DALEX::explain(linear_model, data = houses, y = houses$sqm_price)
breakdown_linear <- single_prediction(lm_explainer, houses[4036, -3])
plot(breakdown_linear)
### Model-agnostic Break Down ----
breakdown_explanation <- single_prediction(rf_expl, houses[4036, -3])
plot(breakdown_explanation)
## LIVE ----
library(live)
library(mlr)
set.seed(997)
new_dataset <- sample_locally2(data = houses,
                               explained_instance = houses[1089, ],
                               explained_var = "sqm_price",
                               size = 1500)
with_predictions <- add_predictions2(new_dataset, hrf)
live_explanation <- fit_explanation2(with_predictions, "regr.lm")
live_explanation
plot_explanation2(live_explanation, "forest")
plot_explanation2(live_explanation, "waterfall")
