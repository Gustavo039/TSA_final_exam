library(readxl)
library(tidyverse)
library(fpp3)
library(forecast)

### Leitura e organização dos dados

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

### Análise exploratória da Série 

## Usando a serie "Conusmo Brasil" para a analise

consumo_ts_br |>
  autoplot() +
  theme_minimal()

fore = ts(consumo_ts_br$consumo, start = c(year(min(consumo_ts_br$ano_mes)), month(min(consumo_ts_br$ano_mes))), frequency = 12)

ggseasonplot(fore, year.labels=FALSE, continuous=TRUE, polar = TRUE)


consumo_ts_br |>
  model(
    STL(consumo ~ trend(window = 6) +
          season(window = "periodic"),
        robust = TRUE)) |>
  components() |>
  autoplot()


consumo_ts_br |>
  model(x11 = X_13ARIMA_SEATS(consumo ~ x11())) |>
  components() |>
autoplot() 


## Modelagem

consumo_ts_br |>
  gg_tsdisplay((consumo), plot_type = "partial")

consumo_ts_br |>
  gg_tsdisplay(difference(consumo), plot_type = "partial")


## Modelos não Sazonais
models_ts_br = consumo_ts_br |> 
  model(arima_111 = ARIMA(consumo ~ pdq(1,1,1) +  PDQ(0,0,0)),
                                      arima_211 = ARIMA(consumo ~ pdq(2,1,1) +  PDQ(0,0,0)),
                                      arima_112 = ARIMA(consumo ~ pdq(1,1,1) +  PDQ(0,0,0)),
                                      arima_212 = ARIMA(consumo ~ pdq(2,1,2) +  PDQ(0,0,0)),
                                      arima_312 = ARIMA(consumo ~ pdq(3,1,2) +  PDQ(0,0,0)),
                                      arima_213 = ARIMA(consumo ~ pdq(2,1,3) +  PDQ(0,0,0)))

models_ts_br = consumo_ts_br |> 
  model(arima_test = ARIMA(consumo ~ pdq(0:3, 1, 0:3) + PDQ(0,0,0)))

models_ts_br |> report()
# Modelo de menor AICc e BIC: arima_212


models_ts_br |> 
  select(arima_212) |> 
  report() |>
  gg_tsresiduals()

# Resíduos do modelo quebram presuposições de correlaçoes não significativas


## Modelos Sazonais/ se mantem os parametros de menor aicc e bic nao sazonais

models_ts_br_seasonal = consumo_ts_br |> model(sarima_212 = ARIMA(consumo ~ pdq(2,1,2)),
                                               sarima_auto = ARIMA(consumo))

models_ts_br_seasonal |> report()

# Modelo de menor AICc e BIC: arima_212

models_ts_br_seasonal |> 
  select(sarima_212) |> 
  report() |>
  gg_tsresiduals()

models_ts_br_seasonal |> 
  select(sarima_auto) |> 
  report() |>
  gg_tsresiduals()


models_ts_br_seasonal |> 
  augment() |>
  features(.innov, box_pierce, lag = 13)
# O modelo sazonal nao quebrou nenhuam pressuposicao dos residuos


############ Tetsando modelo com vari´pavel exogena "pandemia"

consumo_ts_br_exo = consumo_ts_br |>
  mutate(pandemic = case_when(ano_mes > lubridate::ymd('2020-03-10') & ano_mes < lubridate::ymd('2021-12-31') ~ 1,
                              .default = 0)) |>
  mutate(pandemic = pandemic |> as.factor())


arimax_model = consumo_ts_br_exo |>
  model(arimax = ARIMA(consumo ~ pandemic))

arimax_model |> 
  report() |>
  gg_tsresiduals()

# A variável é significatica

######### Ajuste dos modelos

models_ts_br_seasonal |> 
  select(sarima_auto) |>
  augment() |>
  ggplot(aes(x = ano_mes)) +
  geom_line(aes(y = consumo, colour = "Dados")) +
  geom_line(aes(y = .fitted, colour = "Valor Ajustado")) +
  scale_colour_manual(
    values = c(Dados = "black", "Valor Ajustado" = "#D55E00")
  ) +
  labs(y = "Y",
       title = "Modelo de Regressão Linear Temporal") +
  guides(colour = guide_legend(title = "Series")) +
  theme_minimal() +
  theme(legend.position = "bottom")


arimax_model |> 
  select(arimax) |>
  augment() |>
  ggplot(aes(x = ano_mes)) +
  geom_line(aes(y = consumo, colour = "Dados")) +
  geom_line(aes(y = .fitted, colour = "Valor Ajustado")) +
  scale_colour_manual(
    values = c(Dados = "black", "Valor Ajustado" = "#D55E00")
  ) +
  labs(y = "Y",
       title = "Modelo de Regressão Linear Temporal") +
  guides(colour = guide_legend(title = "Series")) +
  theme_minimal() +
  theme(legend.position = "bottom")
