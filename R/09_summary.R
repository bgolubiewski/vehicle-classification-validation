# R/09_summary.R

df_ultimate <- data.frame(
  Model = c(
    "k-Najbliższych Sąsiadów (k-NN)",
    "Pojedyncze drzewo decyzyjne",
    "Naiwny Bayes",
    "Bagging",
    "Random Forest",
    "AdaBoost",
    "SVM"
  ),
  Pelny_Zbior = c(30.57, 32.19, 54.84, 28.07, 25.94, 24.70, 18.44),
  Zbior_Zredukowany = c(36.46, 38.13, 61.67, 37.61, 33.20, 34.88, 32.41)
)