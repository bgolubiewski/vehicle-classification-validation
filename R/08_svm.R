# R/08_svm.R

# ============================================================
# Optymalizacja parametru C oraz wybór jądra
# ============================================================

C.vector <- c(0.1, 0.5, 1, 2, 5, 10, 20, 50, 100)
kernel.vector <- c("linear", "polynomial", "radial")

df_svm_all <- expand.grid(cost = C.vector, kernel = kernel.vector, Blad = NA, Zbior = "Pełny zbiór (18 cech)")
df_svm_opt <- expand.grid(cost = C.vector, kernel = kernel.vector, Blad = NA, Zbior = "Wybrane zmienne (3 cechy)")

set.seed(87124671)
for(i in 1:nrow(df_svm_all)) {
  # pełny zbiór danych
  mod_all <- e1071::svm(Class ~ ., data = learn.set.scaled, 
                        kernel = as.character(df_svm_all$kernel[i]), 
                        cost = df_svm_all$cost[i])
  pred_all <- predict(mod_all, newdata = test.set.scaled)
  df_svm_all$Blad[i] <- mean(pred_all != test.set.scaled$Class) * 100
  
  # zbiór zredukowany (3 cechy)
  mod_opt <- e1071::svm(Class ~ ., data = learn.set.scaled.opt, 
                        kernel = as.character(df_svm_opt$kernel[i]), 
                        cost = df_svm_opt$cost[i])
  pred_opt <- predict(mod_opt, newdata = test.set.scaled.opt)
  df_svm_opt$Blad[i] <- mean(pred_opt != test.set.scaled.opt$Class) * 100
}

best_all <- df_svm_all[which.min(df_svm_all$Blad), ]
svm_kernel_all <- as.character(best_all$kernel)
svm_cost_all <- best_all$cost

best_opt <- df_svm_opt[which.min(df_svm_opt$Blad), ]
svm_kernel_opt <- as.character(best_opt$kernel)
svm_cost_opt <- best_opt$cost


df_svm_total <- rbind(df_svm_all, df_svm_opt)

# ============================================================
# Optymalizacja parametrów C oraz gamma dla jądra radialnego
# ============================================================

C_grid <- c(0.1, 1, 10, 50, 100)
gamma_grid <- c(0.001, 0.01, 0.05, 0.1, 0.5, 1, 5)

df_rbf_all <- expand.grid(cost = C_grid, gamma = gamma_grid, Blad = NA, Zbior = "Pełny zbiór (18 cech)")
df_rbf_opt <- expand.grid(cost = C_grid, gamma = gamma_grid, Blad = NA, Zbior = "Wybrane zmienne (3 cechy)")

set.seed(87124671)
for(i in 1:nrow(df_rbf_all)) {
  # pełny zbiór danych
  mod_all <- e1071::svm(Class ~ ., data = learn.set.scaled, kernel = "radial",
                        cost = df_rbf_all$cost[i], gamma = df_rbf_all$gamma[i])
  pred_all <- predict(mod_all, newdata = test.set.scaled)
  df_rbf_all$Blad[i] <- mean(pred_all != test.set.scaled$Class) * 100
  
  # zbiór zredukowany
  mod_opt <- e1071::svm(Class ~ ., data = learn.set.scaled.opt, kernel = "radial",
                        cost = df_rbf_opt$cost[i], gamma = df_rbf_opt$gamma[i])
  pred_opt <- predict(mod_opt, newdata = test.set.scaled.opt)
  df_rbf_opt$Blad[i] <- mean(pred_opt != test.set.scaled.opt$Class) * 100
}


best_rbf_all <- df_rbf_all[which.min(df_rbf_all$Blad), ]
best_rbf_opt <- df_rbf_opt[which.min(df_rbf_opt$Blad), ]

opt_C_all <- best_rbf_all$cost
opt_gamma_all <- best_rbf_all$gamma

opt_C_opt <- best_rbf_opt$cost
opt_gamma_opt <- best_rbf_opt$gamma


df_rbf_total <- rbind(df_rbf_all, df_rbf_opt)

svm_predict_wrapper <- function(object, newdata) {
  return(predict(object, newdata = newdata))
}

# ============================================================
# Wszystkie zmienne
# ============================================================

# model z optymalnym C, ale domyślną gammą
set.seed(87124671)
svm_C_all <- e1071::svm(Class ~ ., data = learn.set.scaled, kernel = "radial", cost = opt_C_all)
err_C_all <- mean(predict(svm_C_all, newdata = test.set.scaled) != test.set.scaled$Class) * 100

# model w pełni zoptymalizowany (C oraz gamma)
set.seed(87124671)
svm_opt_all <- e1071::svm(Class ~ ., data = learn.set.scaled, kernel = "radial", cost = opt_C_all, gamma = opt_gamma_all)
err_opt_all <- mean(predict(svm_opt_all, newdata = test.set.scaled) != test.set.scaled$Class) * 100


# ============================================================
# Wybrane zmienne
# ============================================================

# model z optymalnym C, ale domyślną gammą
set.seed(87124671)
svm_C_opt <- e1071::svm(Class ~ ., data = learn.set.scaled.opt, kernel = "radial", cost = opt_C_opt)
err_C_opt <- mean(predict(svm_C_opt, newdata = test.set.scaled.opt) != test.set.scaled.opt$Class) * 100

# model w pełni zoptymalizowany (C oraz gamma)
set.seed(87124671)
svm_opt_opt <- e1071::svm(Class ~ ., data = learn.set.scaled.opt, kernel = "radial", cost = opt_C_opt, gamma = opt_gamma_opt)
err_opt_opt <- mean(predict(svm_opt_opt, newdata = test.set.scaled.opt) != test.set.scaled.opt$Class) * 100


# ============================================================
# CV, Bootstrap
# ============================================================


set.seed(87124671)
cv_C_all <- errorest(Class ~ ., data = learn.set.scaled, model = e1071::svm, kernel = "radial", cost = opt_C_all,
                     estimator = "cv", predict = svm_predict_wrapper, est.para = control.errorest(k = 10))$error * 100

set.seed(87124671)
boot_C_all <- errorest(Class ~ ., data = learn.set.scaled, model = e1071::svm, kernel = "radial", cost = opt_C_all,
                       estimator = "boot", predict = svm_predict_wrapper, est.para = control.errorest(nboot = 20))$error * 100

set.seed(87124671)
cv_opt_all <- errorest(Class ~ ., data = learn.set.scaled, model = e1071::svm, kernel = "radial", cost = opt_C_all, gamma = opt_gamma_all,
                       estimator = "cv", predict = svm_predict_wrapper, est.para = control.errorest(k = 10))$error * 100

set.seed(87124671)
boot_opt_all <- errorest(Class ~ ., data = learn.set.scaled, model = e1071::svm, kernel = "radial", cost = opt_C_all, gamma = opt_gamma_all,
                         estimator = "boot", predict = svm_predict_wrapper, est.para = control.errorest(nboot = 20))$error * 100

# --

set.seed(87124671)
cv_C_opt <- errorest(Class ~ ., data = learn.set.scaled.opt, model = e1071::svm, kernel = "radial", cost = opt_C_opt,
                     estimator = "cv", predict = svm_predict_wrapper, est.para = control.errorest(k = 10))$error * 100

set.seed(87124671)
boot_C_opt <- errorest(Class ~ ., data = learn.set.scaled.opt, model = e1071::svm, kernel = "radial", cost = opt_C_opt,
                       estimator = "boot", predict = svm_predict_wrapper, est.para = control.errorest(nboot = 20))$error * 100

set.seed(87124671)
cv_opt_opt <- errorest(Class ~ ., data = learn.set.scaled.opt, model = e1071::svm, kernel = "radial", cost = opt_C_opt, gamma = opt_gamma_opt,
                       estimator = "cv", predict = svm_predict_wrapper, est.para = control.errorest(k = 10))$error * 100

set.seed(87124671)
boot_opt_opt <- errorest(Class ~ ., data = learn.set.scaled.opt, model = e1071::svm, kernel = "radial", cost = opt_C_opt, gamma = opt_gamma_opt,
                         estimator = "boot", predict = svm_predict_wrapper, est.para = control.errorest(nboot = 20))$error * 100


# ============================================================
# Podsumowanie
# ============================================================

# tabela 1: Zbiór pełny
df_svm_full <- data.frame(
  Metoda = c("Pojedynczy podział [%]", "Cross-validation [%]", "Bootstrap [%]"),
  Model_C = c(round(err_C_all, 2), round(cv_C_all, 2), round(boot_C_all, 2)),
  Model_Opt = c(round(err_opt_all, 2), round(cv_opt_all, 2), round(boot_opt_all, 2))
)

# tabela 2: Zbiór zredukowany (3 cechy)
df_svm_reduced <- data.frame(
  Metoda = c("Pojedynczy podział [%]", "Cross-validation [%]", "Bootstrap [%]"),
  Model_C = c(round(err_C_opt, 2), round(cv_C_opt, 2), round(boot_C_opt, 2)),
  Model_Opt = c(round(err_opt_opt, 2), round(cv_opt_opt, 2), round(boot_opt_opt, 2))
)
