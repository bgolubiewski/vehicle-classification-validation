# R/01_data_preparation.R

set.seed(87124671)

data(Vehicle)
#summary(Vehicle)

# ============================================================
# Wszystkie zmienne
# ============================================================

# wyznaczenie liczby obserwacji (wierszy), podział zbioru
n <- dim(Vehicle)[1]
learn.set.index <- sample(1:n, 2/3 * n)

learn.set <- Vehicle[learn.set.index,]
test.set <- Vehicle[-learn.set.index,]

# utworzenie wektorów z rzeczywistymi etykietkami
etykietki.rzecz.learn <- learn.set$Class
etykietki.rzecz.test <- test.set$Class

# identyfikacja zmiennych numerycznych (bez etykiety klasy)
zmienne_all <- setdiff(names(Vehicle), "Class")

# Standaryzacja
mu_all <- apply(learn.set[, zmienne_all], 2, mean)
sd_all <- apply(learn.set[, zmienne_all], 2, sd)

learn.set.scaled <- scale(learn.set[, zmienne_all], center = mu_all, scale = sd_all)
test.set.scaled  <- scale(test.set[, zmienne_all], center = mu_all, scale = sd_all)

Vehicle.scaled <- Vehicle
Vehicle.scaled[, zmienne_all] <- scale(Vehicle[, zmienne_all])


# ============================================================
# Wybrane, najlepiej dyskryminujące zmienne
# ============================================================

# zmienne do zbioru uczącego
zmienne_opt <- c('Scat.Ra', 'Elong', 'D.Circ')

# standaryzacja
mu_opt <- apply(learn.set[, zmienne_opt], 2, mean)
sd_opt <- apply(learn.set[, zmienne_opt], 2, sd)

learn.set.scaled.opt <- scale(learn.set[, zmienne_opt], center = mu_opt, scale = sd_opt)
test.set.scaled.opt  <- scale(test.set[, zmienne_opt], center = mu_opt, scale = sd_opt)

vehicle.scaled.opt <- Vehicle.scaled[, zmienne_opt]


# ============================================================
# Utworzenie ramek danych dla algorytmów
# ============================================================

learn.set.scaled <- as.data.frame(learn.set.scaled)
learn.set.scaled$Class <- etykietki.rzecz.learn

test.set.scaled <- as.data.frame(test.set.scaled)
test.set.scaled$Class <- etykietki.rzecz.test

learn.set.scaled.opt <- as.data.frame(learn.set.scaled.opt)
learn.set.scaled.opt$Class <- etykietki.rzecz.learn

test.set.scaled.opt <- as.data.frame(test.set.scaled.opt)
test.set.scaled.opt$Class <- etykietki.rzecz.test

vehicle.scaled.opt <- as.data.frame(vehicle.scaled.opt)
vehicle.scaled.opt$Class <- Vehicle$Class