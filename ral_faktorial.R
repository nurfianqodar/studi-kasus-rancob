if (!("pacman" %in% installed.packages())) {
  install.packages("pacman")
}

pacman::p_load(dplyr, tibble, agricolae, openxlsx)

# Membaca file excel
df <- read.xlsx("data.xlsx", sheet = "ral_jagung")

# Ringkasan data
df |> str()

df |> summary()

# Merapikan data agar sesuai untuk analisis
df <- df |>
  rename(
    faktor1 = f1,
    faktor2 = f2,
    ulangan = rep
  ) |>
  mutate(
    faktor1 = as.factor(faktor1),
    faktor2 = as.factor(faktor2),
    ulangan = as.factor(ulangan)
  )

# Meninjau ulang data
df |> str()

df |> summary()

# ANOVA
model <- aov(tinggi_tanaman ~ faktor1 * faktor2, data = df)

model |> summary()


# Uji lanjut semua faktor 1 pada setiap faktor 2

faktor2 <- df$faktor2 |>
  unique()

faktor2

for (f in faktor2) {
  dd <- df |>
    filter(faktor2 == f)
  model <- aov(tinggi_tanaman ~ faktor1, data = dd)
  dmrt <- duncan.test(model, trt = "faktor1")
  tabel <- dmrt$groups |>
    as.data.frame() |>
    rownames_to_column("Faktor 1")

  paste0("Faktor 1 pada ", f, "\n") |> cat(sep = "")

  print(tabel)

  cat("\n\n")
}


# Uji lanjut semua faktor 2 pada setiap faktor 1

faktor1 <- df$faktor1 |>
  unique()

faktor1

for (f in faktor1) {
  dd <- df |>
    filter(faktor1 == f)

  model <- aov(tinggi_tanaman ~ faktor2, data = dd)

  dmrt <- duncan.test(model, trt = "faktor2")

  tabel <- dmrt$groups |>
    as.data.frame() |>
    rownames_to_column("Faktor 2")

  paste0("Faktor 2 pada ", f, "\n") |> cat(sep = "")

  print(tabel)

  cat("\n\n")
}
