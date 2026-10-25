# R/06_adaboost.R

# ============================================================
# Wyznaczenie parametru mfinal
# ============================================================

M.vector <- c(1, 3, 5, 10, 15, 20, 25, 30, 40, 50, 75, 100)

df_boosting <- data.frame(
  mfinal = M.vector, 
  all_testowy = NA,
  opt_testowy = NA
)

set.seed(87124671)
for(i in seq_along(M.vector)){
  m <- M.vector[i]
  
  model_all <- adabag::boosting(Class ~ ., data = learn.set.scaled, mfinal = m, minsplit = 1, cp = 0)
  pred_all_test <- predict(model_all, newdata = test.set.scaled)
  df_boosting$all_testowy[i] <- mean(pred_all_test$class != test.set.scaled$Class) * 100
  
  model_opt <- adabag::boosting(Class ~ ., data = learn.set.scaled.opt, mfinal = m, minsplit = 1, cp = 0)
  pred_opt_test <- predict(model_opt, newdata = test.set.scaled.opt)
  df_boosting$opt_testowy[i] <- mean(pred_opt_test$class != test.set.scaled.opt$Class) * 100
}

best_index_all_boost <- which.min(df_boosting$all_testowy)
mfinal_all_z_wykresu <- df_boosting$mfinal[best_index_all_boost]

best_index_opt_boost <- which.min(df_boosting$opt_testowy)
mfinal_opt_z_wykresu <- df_boosting$mfinal[best_index_opt_boost]



df_boosting_long <- df_boosting %>%
  pivot_longer(
    cols = c(all_testowy, opt_testowy),
    names_to = "Kombinacja",
    values_to = "Blad"
  ) %>%
  separate(Kombinacja, into = c("Zmienne", "Zbior"), sep = "_")

# ============================================================
# Wszystkie zmienne
# ============================================================

set.seed(87124671)
# wszystkie zmienne
boost_all <- adabag::boosting(Class ~ ., data = learn.set.scaled,
                              mfinal = mfinal_all_z_wykresu,
                              minsplit = 1, cp = 0)
set.seed(87124671)
pred_all_res <- predict(boost_all, newdata = test.set.scaled)
pred_all_factor <- as.factor(pred_all_res$class) 

cm_all_boost <- table(pred_all_factor, etykietki.rzecz.test)
err_all_boost <- (1 - sum(diag(cm_all_boost)) / sum(cm_all_boost)) * 100

# ============================================================
# Wybrane zmienne
# ============================================================

# optymalne zmienne
set.seed(87124671)
boost_opt <- adabag::boosting(Class ~ ., data = learn.set.scaled.opt,
                              mfinal = mfinal_opt_z_wykresu,
                              minsplit = 1, cp = 0)
set.seed(87124671)
pred_opt_res <- predict(boost_opt, newdata = test.set.scaled.opt)
pred_opt_factor <- as.factor(pred_opt_res$class)

cm_opt_boost <- table(pred_opt_factor, etykietki.rzecz.test)
err_opt_boost <- (1 - sum(diag(cm_opt_boost)) / sum(cm_opt_boost)) * 100

# ============================================================
# CV, Bootstrap
# ============================================================

custom_predict <- function(object, newdata) {
  pred <- predict(object, newdata = newdata)
  return(as.factor(pred$class))
}


set.seed(87124671)
cv_all_boost <- errorest(Class ~ ., data = learn.set.scaled, model = adabag::boosting, 
                         mfinal = mfinal_all_z_wykresu, estimator = "cv",
                         predict = custom_predict,
                         est.para = control.errorest(k = 10))$error * 100

set.seed(87124671)
boot_all_boost <- errorest(Class ~ ., data = learn.set.scaled, model = adabag::boosting, 
                           mfinal = mfinal_all_z_wykresu, estimator = "boot",
                           predict = custom_predict,
                           est.para = control.errorest(nboot = 20))$error * 100


set.seed(87124671)
cv_opt_boost <- errorest(Class ~ ., data = learn.set.scaled.opt, model = adabag::boosting, 
                         mfinal = mfinal_opt_z_wykresu, estimator = "cv",
                         predict = custom_predict,
                         est.para = control.errorest(k = 10))$error * 100
set.seed(87124671)
boot_opt_boost <- errorest(Class ~ ., data = learn.set.scaled.opt, model = adabag::boosting, 
                           mfinal = mfinal_opt_z_wykresu, estimator = "boot",
                           predict = custom_predict,
                           est.para = control.errorest(nboot = 20))$error * 100


# ============================================================
# Powtórzenie procesu dla optymalnych zmiennych (na podstawie ważności cech)
# ============================================================

zmienne_boost <- c('Max.L.Ra', 'Pr.Axis.Ra', 'Max.L.Rect') 

mu_boost <- apply(learn.set[, zmienne_boost], 2, mean)
sd_boost <- apply(learn.set[, zmienne_boost], 2, sd)

learn.set.boost.opt <- scale(learn.set[, zmienne_boost], center = mu_boost, scale = sd_boost)
test.set.boost.opt  <- scale(test.set[, zmienne_boost], center = mu_boost, scale = sd_boost)

learn.set.boost.opt <- as.data.frame(learn.set.boost.opt)
learn.set.boost.opt$Class <- etykietki.rzecz.learn

test.set.boost.opt <- as.data.frame(test.set.boost.opt)
test.set.boost.opt$Class <- etykietki.rzecz.test

set.seed(87124671)
boost_opt_fig <- adabag::boosting(Class ~ ., data = learn.set.boost.opt,
                                  mfinal = mfinal_opt_z_wykresu,
                                  minsplit = 1, cp = 0)
set.seed(87124671)
pred_opt_fig_res <- predict(boost_opt_fig, newdata = test.set.boost.opt)
pred_opt_fig_factor <- as.factor(pred_opt_fig_res$class)

cm_opt_boost_fig <- table(pred_opt_fig_factor, etykietki.rzecz.test)
err_opt_boost_fig <- (1 - sum(diag(cm_opt_boost_fig)) / sum(cm_opt_boost_fig)) * 100

# cv/boostrap
set.seed(87124671)
cv_opt_boost_fig <- errorest(Class ~ ., data = learn.set.boost.opt, model = adabag::boosting, 
                             mfinal = mfinal_opt_z_wykresu, estimator = "cv",
                             predict = custom_predict,
                             est.para = control.errorest(k = 10))$error * 100
set.seed(87124671)
boot_opt_boost_fig <- errorest(Class ~ ., data = learn.set.boost.opt, model = adabag::boosting, 
                               mfinal = mfinal_opt_z_wykresu, estimator = "boot",
                               predict = custom_predict,
                               est.para = control.errorest(nboot = 20))$error * 100


# ============================================================
# Podsumowanie
# ============================================================


df_boost_summary <- data.frame(
  Zestaw_cech = c("Pełny zbiór (18 cech)", "Wybrane cechy (wg estymatorów gęstości)", "Optymalne cechy (wg ważności)"),
  Blad = c(round(err_all_boost, 2), round(err_opt_boost, 2), round(err_opt_boost_fig, 2)),
  CV_Blad = c(round(cv_all_boost, 2), round(cv_opt_boost, 2), round(cv_opt_boost_fig, 2)),
  Boot_Blad = c(round(boot_all_boost, 2), round(boot_opt_boost, 2), round(boot_opt_boost_fig, 2))
)

df_boost_summary_t <- as.data.frame(t(df_boost_summary[, -1]))
colnames(df_boost_summary_t) <- df_boost_summary$Zestaw_cech
rownames(df_boost_summary_t) <- c(
  "Pojedynczy podział [%]",
  "Cross-validation [%]",
  "Bootstrap [%]"
)