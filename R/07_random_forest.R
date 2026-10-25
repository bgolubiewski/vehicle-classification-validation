# R/07_random_forest.R

# ============================================================
# Optymalizacja parametru ntree
# ============================================================

Ntree.vector <- c(1, 3, 5, 10, 15, 20, 25, 30, 40, 50, 75, 100)

df_rf <- data.frame(
  ntree = Ntree.vector, 
  all_testowy = NA,
  opt_testowy = NA
)

set.seed(87124671)
for(i in seq_along(Ntree.vector)){
  nt <- Ntree.vector[i]
  
  # Wszystkie zmienne (domyślne mtry = sqrt(p))
  model_all <- randomForest::randomForest(Class ~ ., data = learn.set.scaled, ntree = nt, importance = TRUE)
  pred_all_test <- predict(model_all, newdata = test.set.scaled)
  df_rf$all_testowy[i] <- mean(pred_all_test != test.set.scaled$Class) * 100
  
  # Wybrane 3 zmienne początkowe (mtry dopasuje się automatycznie)
  model_opt <- randomForest::randomForest(Class ~ ., data = learn.set.scaled.opt, ntree = nt, importance = TRUE)
  pred_opt_test <- predict(model_opt, newdata = test.set.scaled.opt)
  df_rf$opt_testowy[i] <- mean(pred_opt_test != test.set.scaled.opt$Class) * 100
}

best_index_all_rf <- which.min(df_rf$all_testowy)
ntree_all_z_wykresu <- df_rf$ntree[best_index_all_rf]

best_index_opt_rf <- which.min(df_rf$opt_testowy)
ntree_opt_z_wykresu <- df_rf$ntree[best_index_opt_rf]


df_rf_long <- df_rf %>%
  pivot_longer(
    cols = c(all_testowy, opt_testowy),
    names_to = "Kombinacja",
    values_to = "Blad"
  ) %>%
  separate(Kombinacja, into = c("Zmienne", "Zbior"), sep = "_")


# ============================================================
# Wszystkie zmienne
# ============================================================

rf_predict_wrapper <- function(object, newdata) {
  return(predict(object, newdata = newdata, type = "class"))
}

# wszystkie zmienne
set.seed(87124671)
rf_all <- randomForest::randomForest(Class ~ ., data = learn.set.scaled,
                                     ntree = ntree_all_z_wykresu, importance = TRUE)
set.seed(87124671)
pred_all_res <- predict(rf_all, newdata = test.set.scaled)

cm_all_rf <- table(pred_all_res, etykietki.rzecz.test)
err_all_rf <- (1 - sum(diag(cm_all_rf)) / sum(cm_all_rf)) * 100


# ============================================================
# Wybrane zmienne
# ============================================================

# wybrane zmienne początkowe
set.seed(87124671)
rf_opt <- randomForest::randomForest(Class ~ ., data = learn.set.scaled.opt,
                                     ntree = ntree_opt_z_wykresu, importance = TRUE)

set.seed(87124671)
pred_opt_res <- predict(rf_opt, newdata = test.set.scaled.opt)

cm_opt_rf <- table(pred_opt_res, etykietki.rzecz.test)
err_opt_rf <- (1 - sum(diag(cm_opt_rf)) / sum(cm_opt_rf)) * 100

# ============================================================
# CV, Bootstrap
# ============================================================

set.seed(87124671)
cv_all_rf <- errorest(Class ~ ., data = learn.set.scaled, model = randomForest::randomForest, 
                      ntree = ntree_all_z_wykresu, estimator = "cv", predict = rf_predict_wrapper,
                      est.para = control.errorest(k = 10))$error * 100
set.seed(87124671)
boot_all_rf <- errorest(Class ~ ., data = learn.set.scaled, model = randomForest::randomForest, 
                        ntree = ntree_all_z_wykresu, estimator = "boot", predict = rf_predict_wrapper,
                        est.para = control.errorest(nboot = 20))$error * 100
set.seed(87124671)
cv_opt_rf <- errorest(Class ~ ., data = learn.set.scaled.opt, model = randomForest::randomForest, 
                      ntree = ntree_opt_z_wykresu, estimator = "cv", predict = rf_predict_wrapper,
                      est.para = control.errorest(k = 10))$error * 100
set.seed(87124671)
boot_opt_rf <- errorest(Class ~ ., data = learn.set.scaled.opt, model = randomForest::randomForest, 
                        ntree = ntree_opt_z_wykresu, estimator = "boot", predict = rf_predict_wrapper,
                        est.para = control.errorest(nboot = 20))$error * 100


# ============================================================
# Powtórzenie procesu dla optymalnych zmiennych (na podstawie ważności cech)
# ============================================================

# selekcja 3 najlepszych zmiennych dla lasów
importance_matrix <- as.data.frame(randomForest::importance(rf_all))
zmienne_rf <- rownames(importance_matrix[order(-importance_matrix$MeanDecreaseGini), ])[1:3]

mu_rf <- apply(learn.set[, zmienne_rf], 2, mean)
sd_rf <- apply(learn.set[, zmienne_rf], 2, sd)

learn.set.rf.opt <- scale(learn.set[, zmienne_rf], center = mu_rf, scale = sd_rf)
test.set.rf.opt  <- scale(test.set[, zmienne_rf], center = mu_rf, scale = sd_rf)

learn.set.rf.opt <- as.data.frame(learn.set.rf.opt)
learn.set.rf.opt$Class <- etykietki.rzecz.learn

test.set.rf.opt <- as.data.frame(test.set.rf.opt)
test.set.rf.opt$Class <- etykietki.rzecz.test


set.seed(87124671)
rf_opt_fig <- randomForest::randomForest(Class ~ ., data = learn.set.rf.opt,
                                         ntree = ntree_all_z_wykresu, importance = TRUE)
set.seed(87124671)
pred_opt_fig_res <- predict(rf_opt_fig, newdata = test.set.rf.opt)

cm_opt_rf_fig <- table(pred_opt_fig_res, etykietki.rzecz.test)
err_opt_rf_fig <- (1 - sum(diag(cm_opt_rf_fig)) / sum(cm_opt_rf_fig)) * 100

# CV oraz Bootstrap dla dedykowanych zmiennych RF
set.seed(87124671)
cv_opt_rf_fig <- errorest(Class ~ ., data = learn.set.rf.opt, model = randomForest::randomForest, 
                          ntree = ntree_all_z_wykresu, estimator = "cv", predict = rf_predict_wrapper,
                          est.para = control.errorest(k = 10))$error * 100
set.seed(87124671)
boot_opt_rf_fig <- errorest(Class ~ ., data = learn.set.rf.opt, model = randomForest::randomForest, 
                            ntree = ntree_all_z_wykresu, estimator = "boot", predict = rf_predict_wrapper,
                            est.para = control.errorest(nboot = 20))$error * 100


# ============================================================
# Podsumowanie
# ============================================================

df_rf_summary <- data.frame(
  Zestaw_cech = c("Pełny zbiór (18 cech)", "Wybrane cechy (wg estymatorów gęstości)", "Optymalne cechy (wg ważności)"),
  Blad = c(round(err_all_rf, 2), round(err_opt_rf, 2), round(err_opt_rf_fig, 2)),
  CV_Blad = c(round(cv_all_rf, 2), round(cv_opt_rf, 2), round(cv_opt_rf_fig, 2)),
  Boot_Blad = c(round(boot_all_rf, 2), round(boot_opt_rf, 2), round(boot_opt_rf_fig, 2))
)

df_rf_summary_t <- as.data.frame(t(df_rf_summary[, -1]))
colnames(df_rf_summary_t) <- df_rf_summary$Zestaw_cech
rownames(df_rf_summary_t) <- c(
  "Pojedynczy podział [%]",
  "Cross-validation [%]",
  "Bootstrap [%]"
)