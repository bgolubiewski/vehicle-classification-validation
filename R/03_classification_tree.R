# R/03_classification_tree.R


n.learning <- nrow(learn.set)
n.test     <- nrow(test.set)

# Definicje modeli
model_all <- Class ~ .
model_opt <- Class ~ Scat.Ra + Elong + D.Circ

# modele dla pełnego zbioru
set.seed(87124671)
tree_simple_all <- rpart(model_all, data = learn.set, method = "class")

set.seed(87124671)
tree_complex_all <- rpart(model_all, data = learn.set, method = "class",
                          control = rpart.control(cp = 0.005, minsplit = 2, maxdepth = 30))

# modele dla zredukowanego zbioru 
set.seed(87124671)
tree_simple_opt  <- rpart(model_opt, data = learn.set, method = "class")

set.seed(87124671)
tree_complex_opt <- rpart(model_opt, data = learn.set, method = "class",
                          control = rpart.control(cp = 0.005, minsplit = 2, maxdepth = 30))


# Błędy dla pełnego zbioru
err_learn_simple_all <- (n.learning - sum(diag(table(predict(tree_simple_all, type="class"), learn.set$Class)))) / n.learning * 100
err_test_simple_all  <- (n.test - sum(diag(table(predict(tree_simple_all, newdata=test.set, type="class"), test.set$Class)))) / n.test * 100

err_learn_complex_all <- (n.learning - sum(diag(table(predict(tree_complex_all, type="class"), learn.set$Class)))) / n.learning * 100
err_test_complex_all  <- (n.test - sum(diag(table(predict(tree_complex_all, newdata=test.set, type="class"), test.set$Class)))) / n.test * 100

# Błędy dla zredukowanego zbioru
err_learn_simple_opt <- (n.learning - sum(diag(table(predict(tree_simple_opt, type="class"), learn.set$Class)))) / n.learning * 100
err_test_simple_opt  <- (n.test - sum(diag(table(predict(tree_simple_opt, newdata=test.set, type="class"), test.set$Class)))) / n.test * 100

err_learn_complex_opt <- (n.learning - sum(diag(table(predict(tree_complex_opt, type="class"), learn.set$Class)))) / n.learning * 100
err_test_complex_opt  <- (n.test - sum(diag(table(predict(tree_complex_opt, newdata=test.set, type="class"), test.set$Class)))) / n.test * 100

# ============================================================
# Wyznaczenie parametru CP (metoda 1-SE)
# ============================================================

# Wyznaczenie CP metodą 1-SE dla pełnego zbioru
cptab_all   <- tree_complex_all$cptable
min_idx_all <- which.min(cptab_all[, "xerror"])
limit_all   <- cptab_all[min_idx_all, "xerror"] + cptab_all[min_idx_all, "xstd"]
cp_opt_all  <- cptab_all[which(cptab_all[, "xerror"] < limit_all)[1], "CP"]

# Wyznaczenie CP metodą 1-SE dla zredukowanego zbioru
cptab_opt   <- tree_complex_opt$cptable
min_idx_opt <- which.min(cptab_opt[, "xerror"])
limit_opt   <- cptab_opt[min_idx_opt, "xerror"] + cptab_opt[min_idx_opt, "xstd"]
cp_opt_opt  <- cptab_opt[which(cptab_opt[, "xerror"] < limit_opt)[1], "CP"]

# Przygotowanie danych do ggplot2
df_cp_all <- as.data.frame(cptab_all)
df_cp_opt <- as.data.frame(cptab_opt)
df_cp_all$Wariant <- "Pełny zbiór (18 cech)"
df_cp_opt$Wariant <- "Wybrane cechy (3 cechy)"
df_cp <- rbind(df_cp_all, df_cp_opt)
df_cp$CP[df_cp$CP == 0] <- 0.0001


# ============================================================
# Przycinanie drzewa
# ============================================================

tree_pruned_all <- prune(tree_complex_all, cp = cp_opt_all)
tree_pruned_opt <- prune(tree_complex_opt, cp = cp_opt_opt)

# Prognozy testowe
pred_test_pruned_all <- predict(tree_pruned_all, newdata = test.set, type = "class")
pred_test_pruned_opt <- predict(tree_pruned_opt, newdata = test.set, type = "class")

cm_test_pruned_all <- table(pred_test_pruned_all, test.set$Class)
cm_test_pruned_opt <- table(pred_test_pruned_opt, test.set$Class)

err_pruned_all <- (n.test - sum(diag(cm_test_pruned_all))) / n.test * 100
err_pruned_opt <- (n.test - sum(diag(cm_test_pruned_opt))) / n.test * 100


# ============================================================
# CV, Bootstrap
# ============================================================

my.pred.tree <- function(model, newdata) predict(model, newdata = newdata, type = "class")

my.rpart.all <- function(formula, data) rpart(formula, data = data, method = "class", control = rpart.control(cp = cp_opt_all))
my.rpart.opt <- function(formula, data) rpart(formula, data = data, method = "class", control = rpart.control(cp = cp_opt_opt))

# Pełny zbiór zmiennych
set.seed(87124671)
cv_tree_all <- errorest(Class ~ ., data = Vehicle, model = my.rpart.all, predict = my.pred.tree,
                        estimator = "cv", est.para = control.errorest(k = 10))$error * 100

set.seed(87124671)
boot_tree_all <- errorest(Class ~ ., data = Vehicle, model = my.rpart.all, predict = my.pred.tree,
                          estimator = "boot", est.para = control.errorest(nboot = 50))$error * 100

# Wybrany podzbiór cech
Vehicle_opt <- Vehicle[, c(zmienne_opt, "Class")]

set.seed(87124671)
cv_tree_opt <- errorest(Class ~ ., data = Vehicle_opt, model = my.rpart.opt, predict = my.pred.tree,
                        estimator = "cv", est.para = control.errorest(k = 10))$error * 100

set.seed(87124671)
boot_tree_opt <- errorest(Class ~ ., data = Vehicle_opt, model = my.rpart.opt, predict = my.pred.tree,
                          estimator = "boot", est.para = control.errorest(nboot = 50))$error * 100

# ============================================================
# Podsumowanie
# ============================================================

df_tree_summary <- data.frame(
  Zestaw_cech = c("Pełny zbiór (18 cech)", "Wybrane cechy (3 cechy)"),
  Holdout_Blad = c(round((n.test - sum(diag(cm_test_pruned_all))) / n.test * 100, 2),
                   round((n.test - sum(diag(cm_test_pruned_opt))) / n.test * 100, 2)),
  CV_Blad = c(round(cv_tree_all, 2), round(cv_tree_opt, 2)),
  Boot_Blad = c(round(boot_tree_all, 2), round(boot_tree_opt, 2))
)


df_tree_summary_t <- as.data.frame(t(df_tree_summary[, -1]))
colnames(df_tree_summary_t) <- df_tree_summary$Zestaw_cech
rownames(df_tree_summary_t) <- c(
  "Pojedynczy podział [%]",
  "Cross-validation [%]",
  "Bootstrap [%]"
)