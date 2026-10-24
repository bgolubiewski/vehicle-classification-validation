# R/04_naive_bayes.R

# ============================================================
# Wszystkie zmienne
# ============================================================

n.learning <- nrow(learn.set)
n.test     <- nrow(test.set)

model_all <- Class ~ .

# Model domyślny
set.seed(87124671)
nb_default_all <- naiveBayes(model_all, data = learn.set, laplace = 0)

# Prognozy i błędy dla modelu domyślnego
pred_learn_def_all <- predict(nb_default_all, newdata = learn.set)
pred_test_def_all  <- predict(nb_default_all, newdata = test.set)

err_learn_def_all <- (n.learning - sum(diag(table(pred_learn_def_all, learn.set$Class)))) / n.learning * 100
err_test_def_all  <- (n.test - sum(diag(table(pred_test_def_all, test.set$Class)))) / n.test * 100

cm_test_nb_all <- table(pred_test_def_all, test.set$Class)


# ============================================================
# Wybrane zmienne
# ============================================================

model_opt <- Class ~ Scat.Ra + Elong + D.Circ

# Modele dla zredukowanego zbioru cech
nb_default_opt <- naiveBayes(model_opt, data = learn.set, laplace = 0)
nb_tuned_opt   <- naiveBayes(model_opt, data = learn.set, laplace = 3)

err_learn_def_opt <- (n.learning - sum(diag(table(predict(nb_default_opt, newdata = learn.set), learn.set$Class)))) / n.learning * 100
err_test_def_opt  <- (n.test - sum(diag(table(predict(nb_default_opt, newdata = test.set), test.set$Class)))) / n.test * 100


# ============================================================
# CV, Bootstrap
# ============================================================

my.pred.nb <- function(model, newdata) predict(model, newdata = newdata)
my.nb.all  <- function(formula, data) naiveBayes(formula, data = data, laplace = 0)
my.nb.opt  <- function(formula, data) naiveBayes(formula, data = data, laplace = 0)

# Pełny zbiór zmiennych
set.seed(87124671)
cv_nb_all <- errorest(Class ~ ., data = Vehicle, model = my.nb.all, predict = my.pred.nb,
                      estimator = "cv", est.para = control.errorest(k = 10))$error * 100

set.seed(87124671)
boot_nb_all <- errorest(Class ~ ., data = Vehicle, model = my.nb.all, predict = my.pred.nb,
                        estimator = "boot", est.para = control.errorest(nboot = 50))$error * 100

# Wybrany podzbiór cech
Vehicle_opt <- Vehicle[, c("Scat.Ra", "Elong", "D.Circ", "Class")]

set.seed(87124671)
cv_nb_opt <- errorest(Class ~ ., data = Vehicle_opt, model = my.nb.opt, predict = my.pred.nb,
                      estimator = "cv", est.para = control.errorest(k = 10))$error * 100

set.seed(87124671)
boot_nb_opt <- errorest(Class ~ ., data = Vehicle_opt, model = my.nb.opt, predict = my.pred.nb,
                        estimator = "boot", est.para = control.errorest(nboot = 50))$error * 100

# ============================================================
# Podsumowanie
# ============================================================

df_nb_summary <- data.frame(
  Zestaw_cech = c("Pełny zbiór (18 cech)", "Wybrane cechy (3 cechy)"),
  Blad = c(round(err_test_def_all, 2), round(err_test_def_opt, 2)),
  CV_Blad = c(round(cv_nb_all, 2), round(cv_nb_opt, 2)),
  Boot_Blad = c(round(boot_nb_all, 2), round(boot_nb_opt, 2))
)

df_nb_summary_t <- as.data.frame(t(df_nb_summary[, -1]))
colnames(df_nb_summary_t) <- df_nb_summary$Zestaw_cech
rownames(df_nb_summary_t) <- c(
  "Pojedynczy podział [%]",
  "Cross-validation [%]",
  "Bootstrap [%]"
)
