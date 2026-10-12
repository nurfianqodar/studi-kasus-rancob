pacman::p_load(tidyverse, openxlsx)

gen <- function(
  f1,
  nf1,
  mf1,
  f2,
  nf2,
  mf2,
  rep,
  mrep,
  scale,
  emean = 0,
  esd = 0.3
) {
  f1 <- paste0(f1, 1:nf1)
  f2 <- paste0(f2, 1:nf2)
  df <- expand.grid(
    rep = 1:rep,
    f2 = f2,
    f1 = f1
  ) |>
    as.data.frame() |>
    mutate(
      f1 = as.factor(f1),
      f2 = as.factor(f2),
      rep = as.factor(rep),
      e1 = f1 |> as.numeric() * mf1,
      e2 = f2 |> as.numeric() * mf2,
      er = rep |> as.numeric() * mrep,
      data = 5 + e1 * e2 + er + rnorm(n(), emean, esd),
      data = data / max(data),
      data = data * scale
    ) |>
    select(f1, f2, rep, data)
  df
}

wb <- createWorkbook()

df_tt_ral <- gen("A", 4, 0.4, "B", 3, 0.3, 4, 0, 190, emean = 0, esd = 0.15) |>
  rename(tinggi_tanaman = data)

addWorksheet(wb, "ral_jagung")
writeData(wb, "ral_jagung", df_tt_ral)


df_tt_rak <- gen("A", 4, 0.4, "B", 3, 0.3, 4, 0.5, 190, 0, 0.15) |>
  rename(tinggi_tanaman = data)

addWorksheet(wb, "rak_jagung")
writeData(wb, "rak_jagung", df_tt_rak)

df_tt_sp <- gen("A", 4, 0.4, "B", 3, 0.3, 4, 0.5, 190, 0, 0.15) |>
  rename(tinggi_tanaman = data)

addWorksheet(wb, "sp_jagung")
writeData(wb, "sp_jagung", df_tt_sp)


# RBSL
f1 <- paste0("A", 1:3)
f2 <- paste0("B", 1:3)

trt <- expand.grid(f1 = f1, f2 = f2)

n <- nrow(trt)

latin <- outer(
  0:(n - 1),
  0:(n - 1),
  FUN = function(i, j) (i + j) %% n + 1
)

df_tt_rbsl <- expand.grid(
  baris = 1:n,
  kolom = 1:n
)

df_tt_rbsl$trt <- as.vector(latin)

df_tt_rbsl$f1 <- trt$f1[df_tt_rbsl$trt]
df_tt_rbsl$f2 <- trt$f2[df_tt_rbsl$trt]

df_tt_rbsl <- df_tt_rbsl |>
  mutate(
    f1_num = f1 |> as.factor() |> as.numeric(),
    f2_num = f2 |> as.factor() |> as.numeric(),
    data = 9 +
      baris * 0.153 +
      kolom * 0.132 +
      f1_num * 0.171 +
      f2_num * 0.201 +
      f1_num * f2_num / pi +
      rnorm(n(), 0, 0.5),
    data = data / max(data) * 190
  )

df_tt_rbsl

addWorksheet(wb, "rbsl_jagung")
writeData(wb, "rbsl_jagung", df_tt_rbsl)

saveWorkbook(wb, "data.xlsx", overwrite = TRUE)
