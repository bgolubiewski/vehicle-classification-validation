# R/02_knn.R

set.seed(87124671)

# ============================================================
# Wszystkie zmienne
# ============================================================

df_knn_all <- data.frame(k = 1:200, blad_uczacy = NA, blad_testowy = NA)
cechy_all <- setdiff(names(learn.set.scaled), "Class")

for(k in c(1:200)){
  
  set.seed(87124671)
  # Zbiór testowy
  pred_test <- knn(train = learn.set.scaled[, cechy_all],
                   test = test.set.scaled[, cechy_all],,
                   cl = learn.set$Class,
                   k = k)
  
  # macierz pomyłek (confusion matrix)
  cm_test <- table(pred_test, etykietki.rzecz.test)
  # błąd klasyfikacji (wszystkie - dobre)/ wszystkie
  df_knn_all$blad_testowy[k] <- (1 - sum(diag(cm_test)) / sum(cm_test)) * 100
  
  # Zbiór uczący
  pred_learn <- knn(train = learn.set.scaled[, cechy_all],
                    test = learn.set.scaled[, cechy_all],
                    cl = learn.set$Class,
                    k = k)
  cm_learn <- table(pred_learn, etykietki.rzecz.learn)
  df_knn_all$blad_uczacy[k] <- (1 - sum(diag(cm_learn)) / sum(cm_learn)) * 100
}



k_opt_all <- df_knn_all$k[which.min(df_knn_all$blad_testowy)]
set.seed(87124671)

final_pred_learn <- knn(train = learn.set.scaled[, cechy_all], 
                        test = learn.set.scaled[, cechy_all],
                        cl = learn.set$Class,
                        k=k_opt_all)
final_pred_test <- knn(train = learn.set.scaled[, cechy_all],
                       test = test.set.scaled[, cechy_all],
                       cl = learn.set$Class,
                       k=k_opt_all)

# macierze pomyłek (confusion matrix)
cm_learn_all <- table(final_pred_learn, etykietki.rzecz.learn)
cm_test_all <- table(final_pred_test, etykietki.rzecz.test)


# ============================================================
# Wybrane, najlepiej dyskryminujące zmienne
# ============================================================

df_knn_opt <- data.frame(
  k = 1:200,
  blad_uczacy = NA,
  blad_testowy = NA
)
cechy_opt <- setdiff(names(learn.set.scaled.opt), "Class")

for(k in c(1:200)){
  
  set.seed(87124671)
  # Zbiór testowy
  pred_test <- knn(train = learn.set.scaled.opt[, cechy_opt],
                   test = test.set.scaled.opt[, cechy_opt],
                   cl = learn.set$Class,
                   k = k)
  # macierz pomyłek
  cm_test <- table(pred_test, etykietki.rzecz.test)
  # błąd klasyfikacji
  df_knn_opt$blad_testowy[k] <- (1 - sum(diag(cm_test)) / sum(cm_test)) * 100
  
  # Zbiór uczący
  pred_learn <- knn(train = learn.set.scaled.opt[, cechy_opt],
                    test = learn.set.scaled.opt[, cechy_opt],
                    cl = learn.set$Class,
                    k = k)
  # macierz pomyłek
  cm_learn <- table(pred_learn, etykietki.rzecz.learn)
  # błąd klasyfikacji
  df_knn_opt$blad_uczacy[k] <- (1 - sum(diag(cm_learn)) / sum(cm_learn)) * 100
}

k_opt_opt <- df_knn_opt$k[which.min(df_knn_opt$blad_testowy)]

set.seed(87124671)
final_pred_test_opt <- knn(train = learn.set.scaled.opt[, cechy_opt],
                           test = test.set.scaled.opt[, cechy_opt],
                           cl = learn.set$Class, k = k_opt_opt)
# macierz pomyłek
cm_test_opt <- table(final_pred_test_opt, etykietki.rzecz.test)



# ============================================================
# CV, Bootstrap
# ============================================================

my.predict  <- function(model, newdata) predict(model, newdata=newdata, type="class")
my.ipredknn <- function(formula1, data1, ile.sasiadow) ipredknn(formula=formula1,data=data1,k=ile.sasiadow)
k_zakres <- 1:20

# pełny zbiór zmiennych
set.seed(87124671)
cv_all <- sapply(k_zakres, function(k) {
  errorest(Class ~ ., data = Vehicle.scaled,
           model = my.ipredknn, predict = my.predict,
           estimator = "cv",
           est.para = control.errorest(k = 10),
           ile.sasiadow = k)$error * 100
})

set.seed(87124671)
boot_all <- sapply(k_zakres, function(k) {
  errorest(Class ~ ., data = Vehicle.scaled,
           model = my.ipredknn,
           predict = my.predict,
           estimator = "boot",
           est.para = control.errorest(nboot = 50),
           ile.sasiadow = k)$error * 100
})

# wybrany podzbiór cech
Vehicle.scaled.opt <- Vehicle.scaled[, c(zmienne_opt, "Class")]

set.seed(87124671)
cv_opt <- sapply(k_zakres, function(k) {
  errorest(Class ~ ., data = Vehicle.scaled.opt,
           model = my.ipredknn,
           predict = my.predict,
           estimator = "cv",
           est.para = control.errorest(k = 10),
           ile.sasiadow = k)$error * 100
})

set.seed(87124671)
boot_opt <- sapply(k_zakres, function(k) {
  errorest(Class ~ ., data = Vehicle.scaled.opt,
           model = my.ipredknn,
           predict = my.predict,
           estimator = "boot",
           est.para = control.errorest(nboot = 50),
           ile.sasiadow = k)$error * 100
})

# ============================================================
# Podsumowanie
# ============================================================

df_summary_knn <- data.frame(
  wariant_danych = c("Pełny zbiór (18 cech)", "Wybrane zmienne (3 cechy)"),
  k = c(as.integer(k_opt_all), as.integer(k_opt_opt)),
  CV_k = c(k_zakres[which.min(cv_all)], k_zakres[which.min(cv_opt)]),
  Boot_k = c(k_zakres[which.min(boot_all)], k_zakres[which.min(boot_opt)]),
  
  Blad = c(round(min(df_knn_all$blad_testowy), 2), round(min(df_knn_opt$blad_testowy), 2)),
  CV_Blad = c(round(min(cv_all), 2), round(min(cv_opt), 2)),
  Boot_Blad = c(round(min(boot_all), 2), round(min(boot_opt), 2))
)


df_summary_knn_t <- as.data.frame(
  t(df_summary_knn[, -1])
)
colnames(df_summary_knn_t) <- df_summary_knn$wariant_danych
rownames(df_summary_knn_t) <- c(
  "k (pojedynczy podział)",
  "k (CV)",
  "k (Bootstrap)",
  "Błąd - poj. podział (%)",
  "Błąd - CV (%)",
  "Błąd - Bootstrap (%)"
)
