#' # Memuat *library*
#'
#' Memuat *library* dapat menggunakan fungsi bawaan R yaitu `library` atau
#' gunakan pacman `pacman::p_load` (lebih cepat). Gunakan salah satu kode
#' dibawah ini:
#'
#' - Dengan `pacman` (lebih cepat)
#'
## -----------------------------------------------------------------------------
if (!("pacman" %in% installed.packages())) {
  install.packages("pacman")
}

pacman::p_load(tidyverse, agricolae, openxlsx)

#'
#' - Dengan `library` R
#'
## -----------------------------------------------------------------------------
library(tidyverse)
library(agricolae)
library(openxlsx)

#'
#' # 1. Rancangan acak lengkap (RAL)
#'
#' ## 1.1. Pratinjau data
#'
#' ### a. Muat data
#'
## -----------------------------------------------------------------------------
df <- read.xlsx("data.xlsx", sheet = "ral_jagung")

#'
#' ### b. Ringkasan data
#'
#' - Memeriksa tipe data
#'
## -----------------------------------------------------------------------------
df |> str()

#'
#' - Ringkasan data
#'
## -----------------------------------------------------------------------------
df |> summary()

#'
#' - Memeriksa data kosong
#'
## -----------------------------------------------------------------------------
df |>
  is.na() |>
  colSums()

#'
#' ### c. Membersihkan data
#'
#'
## -----------------------------------------------------------------------------
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

#'
#'
#' Tijauan ulang data setelah dirapikan:
#'
## -----------------------------------------------------------------------------
df |> str()

#'
## -----------------------------------------------------------------------------
df |> summary()

#'
#' ## 1.2. Analisis data
#'
#' ### a. Analisis sidik ragam (ANOVA)
#'
## -----------------------------------------------------------------------------
model <- aov(tinggi_tanaman ~ faktor1 * faktor2, data = df)

#'
#' Lihat hasil analisis:
#'
## -----------------------------------------------------------------------------
model |> summary()

#'
#' ### b. Uji lanjut
#'
#' Karena terdapat interaksi maka uji lanjutan akan dilakukan pada seluruh `faktor1`
#' terhadap setiap `faktor2` dan seluruh `faktor2` terhadap setiap `faktor1`.
#'
#' #### DMRT
#'
#' ##### Uji `faktor1` pada setiap `faktor2`
#'
## -----------------------------------------------------------------------------
faktor2 <- df$faktor2 |>
  unique()

faktor2

#'
## -----------------------------------------------------------------------------
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

#'
#' ##### Uji `faktor2` pada setiap `faktor1`
#'
## -----------------------------------------------------------------------------
faktor1 <- df$faktor1 |>
  unique()

faktor1

#'
## -----------------------------------------------------------------------------
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
