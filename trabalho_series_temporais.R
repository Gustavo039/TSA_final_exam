library(readxl)
library(tidyverse)
library(fpp3)

anos = 2023:2014
ranges = lapply(1:length(anos), 
                function(i) c(6, 19)+16*(i-1))

consumo = 
  seq_along(ranges) |>
  lapply(
  function(i){
    range = ranges[[i]]
    print(range)
    read_excel(
      "CONSUMO MENSAL DE ENERGIA ELÉTRICA POR CLASSE.xlsx",
      sheet = "TOTAL",
      range = glue::glue("A{range[1]}:N{range[2]}")
    ) |>
    dplyr::mutate(ANO = anos[i])
  }
  ) |>
  map(
    ~.x |> slice(3:7)
  ) |>
  bind_rows() |>
  rename(regiao = `...1`) |> 
  pivot_longer(-c(ANO, regiao), values_to = "consumo", names_to = 'MES') |>
  filter(consumo > 0)

meses = 1:12

names(meses) = 
  unique(consumo$MES)

consumo_ts = consumo |>
  mutate(MES = meses[MES]) |>
  mutate(ano_mes = as.Date(glue::glue('{ANO}/{MES}/01')) |> tsibble::yearmonth()) |>
  tsibble::as_tsibble(index = ano_mes, key = "regiao")

consumo_ts_br =  consumo_ts |> 
  summarise(consumo = sum(consumo))  

consumo_ts_br |>
  model(x11 = X_13ARIMA_SEATS(consumo ~ x11())) |>
  components()
autoplot(x11_dcmp) +
  labs(title =
         "Decomposition of total US retail employment using X-11.")

consumo_ts_br |>
  gg_tsdisplay(difference(consumo))

consumo_ts_br |> model(m = ARIMA(consumo))
