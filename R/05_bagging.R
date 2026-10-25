# R/05_bagging.R

# ============================================================
# Wyznaczenie parametru nbagg
# ============================================================

B.vector <- c(1, 3, 5, 10, 15, 20, 25, 30, 40, 50, 75, 100)
df_bagging <- data.frame(
  B = B.vector, 
  all_testowy  = NA,
  opt_testowy  = NA
)

set.seed(87124671)
for(i in seq_along(B.vector)){
  b <- B.vector[i]
  #print(paste0('Obliczenia dla B = ', b))
  
  # wszystkie zmienne
  model_all <- ipred::bagging(Class ~ ., data = learn.set.scaled, nbagg = b, minsplit = 1, cp = 0)
  
  pred_all_test <- predict(model_all, newdata = test.set.scaled)
  df_bagging$all_testowy[i] <- mean(pred_all_test != test.set.scaled$Class) * 100
  
  # optymalne zmienne
  model_opt <- ipred::bagging(Class ~ ., data = learn.set.scaled.opt, nbagg = b, minsplit = 1, cp = 0)
  
  pred_opt_test <- predict(model_opt, newdata = test.set.scaled.opt)
  df_bagging$opt_testowy[i] <- mean(pred_opt_test != test.set.scaled.opt$Class) * 100
}

best_index_all <- which.min(df_bagging$all_testowy)
nbagg_all_z_wykresu <- df_bagging$B[best_index_all]

best_index_opt <- which.min(df_bagging$opt_testowy)
nbagg_opt_z_wykresu <- df_bagging$B[best_index_opt]


# tranformacja danych do ggplot
df_bagging_long <- df_bagging %>%
  pivot_longer(
    cols = c(all_testowy, opt_testowy),
    names_to = "Kombinacja",
    values_to = "Blad"
  ) %>%
  separate(Kombinacja, into = c("Zmienne", "Zbior"), sep = "_")


# ============================================================
# Wszystkie i wybrane zmienne
# ============================================================

# Model dla wszystkich zmiennych
set.seed(87124671)
btree_all <- ipred::bagging(Class ~ ., data = learn.set.scaled, nbagg = nbagg_all_z_wykresu, minsplit = 1, cp = 0)
pred_all_test <- predict(btree_all, newdata = test.set.scaled)
cm_all <- table(pred_all_test, etykietki.rzecz.test)
err_all_bag <- (1 - sum(diag(cm_all)) / sum(cm_all)) * 100

# Model dla optymalnych zmiennych
set.seed(87124671)
btree_opt <- ipred::bagging(Class ~ ., data = learn.set.scaled.opt, nbagg = nbagg_opt_z_wykresu, minsplit = 1, cp = 0)
pred_opt_test <- predict(btree_opt, newdata = test.set.scaled.opt)
cm_opt <- table(pred_opt_test, etykietki.rzecz.test)
err_opt_bag <- (1 - sum(diag(cm_opt)) / sum(cm_opt)) * 100


# ============================================================
# CV, Bootstrap
# ============================================================

print("Obliczanie CV oraz Bootstrapu (może to chwilę potrwać)...")
# wszystkie zmienne
set.seed(87124671)
cv_all_bag <- errorest(Class ~ .,
                       data = learn.set.scaled,
                       model = ipred::bagging,
                       predict = predict,
                       nbagg = nbagg_all_z_wykresu, estimator = "cv", 
                       est.para = control.errorest(k = 10))$error * 100

set.seed(87124671)
boot_all_bag <- errorest(Class ~ .,
                         data = learn.set.scaled,
                         model = ipred::bagging,
                         predict = predict,
                         nbagg = nbagg_all_z_wykresu,
                         estimator = "boot", 
                         est.para = control.errorest(nboot = 30))$error * 100

# optymalne zmienne
set.seed(87124671)
cv_opt_bag <- errorest(Class ~ .,
                       data = learn.set.scaled.opt,
                       model = ipred::bagging, 
                       predict = predict,
                       nbagg = nbagg_opt_z_wykresu, estimator = "cv", 
                       est.para = control.errorest(k = 10))$error * 100

set.seed(87124671)
boot_opt_bag <- errorest(Class ~ .,
                         data = learn.set.scaled.opt,
                         model = ipred::bagging, 
                         predict = predict,
                         nbagg = nbagg_opt_z_wykresu, estimator = "boot", 
                         est.para = control.errorest(nboot = 30))$error * 100

# ============================================================
# Podsumowanie
# ============================================================

df_bagging_summary <- data.frame(
  Zestaw_cech = c("Pełny zbiór (18 cech)", "Wybrane cechy (3 cechy)"),
  Blad = c(round(err_all_bag, 2), round(err_opt_bag, 2)),
  CV_Blad = c(round(cv_all_bag, 2), round(cv_opt_bag, 2)),
  Boot_Blad = c(round(boot_all_bag, 2), round(boot_opt_bag, 2))
)

df_bagging_summary_t <- as.data.frame(t(df_bagging_summary[, -1]))
colnames(df_bagging_summary_t) <- df_bagging_summary$Zestaw_cech
rownames(df_bagging_summary_t) <- c(
  "Pojedynczy podział [%]",
  "Cross-validation [%]",
  "Bootstrap [%]"
)