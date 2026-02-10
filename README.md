Projeto Sucupira / Análise de Dados
================
2026-02-10

<!-- README.md is generated from README.Rmd. Please edit that file -->

## 👨‍🔬 Autores

- **Regivam Antonio de Saul**  
  Graduando em Agronomia - FEIS/Unesp  
  Email: <regivan.saul@unesp.br>

- **Witória de Oliveira Araujo**  
  Pós-graduando em Agronomia - FCAV/Unesp  
  Email: <witoria.araujo@unesp.br>

- **Rodrigo Baratiere Perim**  
  Pós-graduando em Agronomia - FCAV/Unesp  
  Email: <rodrigo.perim@unesp.br>

- **Prof. Dr. Alan Rodrigo Panosso**  
  Coorientador — Departamento de Ciências Exatas - FCAV/Unesp  
  Email: <alan.panosso@unesp.br>

- **Prof. Dr. Mario Luiz Teixeira de Moraes**  
  Coorientador — Departamento de Fitotecnia, Tecnologia de Alimentos e
  Sócio-Economia - FEIS/Unesp  
  Email: [mario.moraes@unesp.br](mailto:%20mario.moraes@unesp.br)

### Carregando Pacotes

``` r
library(tidyverse)
library(corrplot)
library(spdep)
library(gstat)
library(vegan)
library(sf)
library(sp)
source("R/my-functions.R")
```

### Carregando os banco de dados

``` r
lsgo<-readxl::read_xlsx("data-raw/GO-LAG-Pop-Sucupira1.xlsx") |> 
  janitor::clean_names()
selms<-readxl::read_xlsx("data-raw/MS-SEL-Pop-Sucupira1.xlsx") |> 
  janitor::clean_names()
aposp<-readxl::read_xlsx("data-raw/SP-APO-Pop-Sucupira1.xlsx") |> 
  janitor::clean_names()

# alteração do sistema de coordenadas
data_set <- rbind(lsgo,selms,aposp) |> 
  # st_as_sf(coords = c("easting", "northing"),
  #          crs = 31983) %>%      # CRS de origem
  # st_transform(crs = 4326) %>%   # WGS84 (lat/long)
  mutate(
    longitude = easting,
    latitude = northing
    # longitude = st_coordinates(.)[,1],
    # latitude  = st_coordinates(.)[,2]
  ) #%>%
  # st_drop_geometry()
```

### Grid amostral e Gráficos

Os gráficos X–Y com gradiente de cor foram utilizados para representar a
distribuição espacial das variáveis dentro de cada população, por padrão
utilizamos 3 classes por população, em que a tonalidade indica a
magnitude dos valores observados. Essa abordagem permite identificar
visualmente gradientes espaciais, manchas e possíveis padrões de
agregação ou heterogeneidade intra-populacional.

Os boxplots foram empregados para comparar a distribuição das variáveis
entre populações, evidenciando diferenças de mediana, dispersão e
presença de valores extremos.

Em conjunto, essas visualizações auxiliam na interpretação dos
resultados de correlação e autocorrelação espacial, bem como na
identificação de padrões relevantes para análises estatísticas
subsequentes.

``` r
names_vari <- names(data_set |> select(hc_m:s_enxofre))
walk(names_vari,~{
  plot_map <- data_set |>
    select(populacao,longitude,latitude,!!sym(.x)) |>
    group_by(populacao) |>
    mutate(
      classe = cut(!!sym(.x),3)
    ) |>
    ggplot(aes(x=longitude, y=latitude,colour = classe)) +
    geom_point() +
    facet_wrap(~populacao,scale="free",ncol=2)+
    theme_bw()+
    labs(
      colour = .x
    )
  
    plot_box <- data_set |> 
    select(populacao,longitude,latitude,!!sym(.x)) |> 
    group_by(populacao) |> 
    ggplot(aes(y=!!sym(.x), fill = populacao)) +
    geom_boxplot() +
    theme_bw()+
    labs(
      fill = "populacao"
    ) + scale_fill_viridis_d(option = "B")
    
    print(plot_map)
    print(plot_box)
})
```

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-4-3.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-4-4.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-4-5.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-4-6.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-4-7.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-4-8.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-4-9.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-4-10.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-4-11.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-4-12.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-4-13.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-4-14.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-4-15.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-4-16.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-4-17.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-4-18.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-4-19.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-4-20.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-4-21.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-4-22.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-4-23.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-4-24.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-4-25.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-4-26.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-4-27.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-4-28.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-4-29.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-4-30.png)<!-- -->

### Estatística Descritiva

A estatística descritiva foi utilizada para caracterizar a distribuição
das variáveis analisadas em cada população, incluindo medidas de posição
e dispersão, bem como os coeficientes de assimetria e curtose.

O coeficiente de assimetria permite avaliar desvios em relação à
simetria da distribuição, indicando predominância de valores mais altos
ou mais baixos. A curtose descreve o grau de concentração dos dados em
torno da média, auxiliando na identificação de distribuições mais
achatadas ou mais concentradas.

``` r
data_set |> 
  select(populacao, hc_m:s_enxofre) |> 
  group_by(populacao) |> 
  reframe( across(
    .cols = hc_m:s_enxofre,
    .fns = estat_desc,
    .names = "{.col}"
  )) |>
  ungroup() |> 
  add_column(estat = rep(estat_names,3)) |> 
  relocate(estat) |> 
  writexl::write_xlsx("output/estatistica-descritiva.xlsx")
```

### Correlação linear por população

A análise de correlação linear foi realizada separadamente para cada
população, com o objetivo de avaliar as relações entre as variáveis
dentro de um mesmo contexto ambiental e espacial, evitando efeitos de
confusão associados às diferenças entre localidades.

As matrizes de correlação foram visualizadas por meio de corrplot,
permitindo identificar a direção (positiva ou negativa) e a intensidade
das correlações entre variáveis, bem como padrões consistentes ou
contrastantes entre as populações analisadas.

Ideal para interpretação das relações planta–solo em nível
intra-populacional.

``` r
data_set |> 
  filter(populacao == "Lagoa Santa/GO") |> 
  select(hc_m:s_enxofre) |> 
  cor(use = "complete.obs") |> 
  corrplot::corrplot( method = "color",
         outline = T,,
         addgrid.col = "darkgray",cl.pos = "r", tl.col = "black",
         tl.cex = 1, cl.cex = 1, type = "upper", bg="azure2",
         diag = FALSE,
         # addCoef.col = "black",
         cl.ratio = 0.2,
         cl.length = 5,
         number.cex = 0.8)
```

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- --> \###
Correlação linear - por população

``` r
data_set |> 
  filter(populacao == "Selvíria/MS") |> 
  select(hc_m:s_enxofre) |> 
  cor(use = "complete.obs") |> 
  corrplot::corrplot( method = "color",
         outline = T,,
         addgrid.col = "darkgray",cl.pos = "r", tl.col = "black",
         tl.cex = 1, cl.cex = 1, type = "upper", bg="azure2",
         diag = FALSE,
         # addCoef.col = "black",
         cl.ratio = 0.2,
         cl.length = 5,
         number.cex = 0.8)
```

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- --> \###
Correlação linear

``` r
data_set |> 
  filter(populacao == "Aparecida D'Oeste/SP") |> 
  select(hc_m:s_enxofre) |> 
  cor(use = "complete.obs") |> 
  corrplot::corrplot( method = "color",
         outline = T,,
         addgrid.col = "darkgray",cl.pos = "r", tl.col = "black",
         tl.cex = 1, cl.cex = 1, type = "upper", bg="azure2",
         diag = FALSE,
         # addCoef.col = "black",
         cl.ratio = 0.2,
         cl.length = 5,
         number.cex = 0.8)
```

![](README_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

### Autocorrelação espacial (Moran’s I)

O Índice de Moran é uma medida de autocorrelação espacial global que
avalia se valores de uma variável observados em pontos próximos no
espaço tendem a ser mais semelhantes (autocorrelação positiva), mais
diferentes (autocorrelação negativa) ou distribuídos aleatoriamente.

**Valores de Moran’s I próximos ao valor esperado sob aleatoriedade
indicam ausência de estrutura espacial. Valores significativamente
maiores que o esperado indicam autocorrelação espacial positiva
(agregação), enquanto valores menores indicam autocorrelação negativa
(repulsão).**

A significância estatística é avaliada por teste de randomização. Um
p-valor baixo (ex.: p \< 0,05) indica que o padrão espacial observado
difere do acaso, na escala definida pela matriz de vizinhança utilizada.

``` r
coords <- data_set  |> 
  filter(populacao == "Lagoa Santa/GO")  |>  
  select(longitude, latitude)

nb <- dnearneigh(as.matrix(coords), 5, 300)  # ajuste distância
lw <- nb2listw(nb, style = "W")

walk(names_vari, ~{
  x <- data_set |> filter(populacao == "Lagoa Santa/GO")|> pull(!!sym(.x))
  x[is.na(x)] <- mean(x,na.rm=TRUE) 
  print(paste0("|---------- ",.x," ---------|"))
  print(moran.test(x, lw))
  
})
#> [1] "|---------- hc_m ---------|"
#> 
#>  Moran I test under randomisation
#> 
#> data:  x  
#> weights: lw    
#> 
#> Moran I statistic standard deviate = 1.2892, p-value = 0.09867
#> alternative hypothesis: greater
#> sample estimates:
#> Moran I statistic       Expectation          Variance 
#>     -1.060348e-02     -1.886792e-02      4.109625e-05 
#> 
#> [1] "|---------- ht_m ---------|"
#> 
#>  Moran I test under randomisation
#> 
#> data:  x  
#> weights: lw    
#> 
#> Moran I statistic standard deviate = 1.6708, p-value = 0.04738
#> alternative hypothesis: greater
#> sample estimates:
#> Moran I statistic       Expectation          Variance 
#>     -7.905083e-03     -1.886792e-02      4.305184e-05 
#> 
#> [1] "|---------- dap_cm ---------|"
#> 
#>  Moran I test under randomisation
#> 
#> data:  x  
#> weights: lw    
#> 
#> Moran I statistic standard deviate = -0.038669, p-value = 0.5154
#> alternative hypothesis: greater
#> sample estimates:
#> Moran I statistic       Expectation          Variance 
#>     -1.912247e-02     -1.886792e-02      4.333142e-05 
#> 
#> [1] "|---------- dmc_m ---------|"
#> 
#>  Moran I test under randomisation
#> 
#> data:  x  
#> weights: lw    
#> 
#> Moran I statistic standard deviate = 1.2781, p-value = 0.1006
#> alternative hypothesis: greater
#> sample estimates:
#> Moran I statistic       Expectation          Variance 
#>     -1.049004e-02     -1.886792e-02      4.296434e-05 
#> 
#> [1] "|---------- eca_cm ---------|"
#> 
#>  Moran I test under randomisation
#> 
#> data:  x  
#> weights: lw    
#> 
#> Moran I statistic standard deviate = 0.93631, p-value = 0.1746
#> alternative hypothesis: greater
#> sample estimates:
#> Moran I statistic       Expectation          Variance 
#>     -1.276372e-02     -1.886792e-02      4.250254e-05 
#> 
#> [1] "|---------- fruto ---------|"
#> 
#>  Moran I test under randomisation
#> 
#> data:  x  
#> weights: lw    
#> 
#> Moran I statistic standard deviate = 3.8675, p-value = 5.498e-05
#> alternative hypothesis: greater
#> sample estimates:
#> Moran I statistic       Expectation          Variance 
#>      6.834845e-03     -1.886792e-02      4.416769e-05 
#> 
#> [1] "|---------- mat_organica ---------|"
#> 
#>  Moran I test under randomisation
#> 
#> data:  x  
#> weights: lw    
#> 
#> Moran I statistic standard deviate = 1.0341, p-value = 0.1505
#> alternative hypothesis: greater
#> sample estimates:
#> Moran I statistic       Expectation          Variance 
#>     -1.221018e-02     -1.886792e-02      4.145162e-05 
#> 
#> [1] "|---------- p_h ---------|"
#> 
#>  Moran I test under randomisation
#> 
#> data:  x  
#> weights: lw    
#> 
#> Moran I statistic standard deviate = 1.9515, p-value = 0.0255
#> alternative hypothesis: greater
#> sample estimates:
#> Moran I statistic       Expectation          Variance 
#>     -6.916190e-03     -1.886792e-02      3.750707e-05 
#> 
#> [1] "|---------- p_h_smp ---------|"
#> 
#>  Moran I test under randomisation
#> 
#> data:  x  
#> weights: lw    
#> 
#> Moran I statistic standard deviate = 1.1199, p-value = 0.1314
#> alternative hypothesis: greater
#> sample estimates:
#> Moran I statistic       Expectation          Variance 
#>     -1.162491e-02     -1.886792e-02      4.183152e-05 
#> 
#> [1] "|---------- p_fosforo ---------|"
#> 
#>  Moran I test under randomisation
#> 
#> data:  x  
#> weights: lw    
#> 
#> Moran I statistic standard deviate = 0.71946, p-value = 0.2359
#> alternative hypothesis: greater
#> sample estimates:
#> Moran I statistic       Expectation          Variance 
#>     -1.433409e-02     -1.886792e-02      3.971176e-05 
#> 
#> [1] "|---------- k_potacio ---------|"
#> 
#>  Moran I test under randomisation
#> 
#> data:  x  
#> weights: lw    
#> 
#> Moran I statistic standard deviate = 2.4313, p-value = 0.007522
#> alternative hypothesis: greater
#> sample estimates:
#> Moran I statistic       Expectation          Variance 
#>     -1.208263e-02     -1.886792e-02      7.788518e-06 
#> 
#> [1] "|---------- ca_calcio ---------|"
#> 
#>  Moran I test under randomisation
#> 
#> data:  x  
#> weights: lw    
#> 
#> Moran I statistic standard deviate = 1.1775, p-value = 0.1195
#> alternative hypothesis: greater
#> sample estimates:
#> Moran I statistic       Expectation          Variance 
#>     -1.240098e-02     -1.886792e-02      3.016313e-05 
#> 
#> [1] "|---------- mg_magnesio ---------|"
#> 
#>  Moran I test under randomisation
#> 
#> data:  x  
#> weights: lw    
#> 
#> Moran I statistic standard deviate = 3.2213, p-value = 0.000638
#> alternative hypothesis: greater
#> sample estimates:
#> Moran I statistic       Expectation          Variance 
#>     -2.069708e-04     -1.886792e-02      3.355812e-05 
#> 
#> [1] "|---------- al_aluminio ---------|"
#> 
#>  Moran I test under randomisation
#> 
#> data:  x  
#> weights: lw    
#> 
#> Moran I statistic standard deviate = 4.4586, p-value = 4.126e-06
#> alternative hypothesis: greater
#> sample estimates:
#> Moran I statistic       Expectation          Variance 
#>      0.0095907609     -0.0188679245      0.0000407419 
#> 
#> [1] "|---------- s_enxofre ---------|"
#> 
#>  Moran I test under randomisation
#> 
#> data:  x  
#> weights: lw    
#> 
#> Moran I statistic standard deviate = -0.18538, p-value = 0.5735
#> alternative hypothesis: greater
#> sample estimates:
#> Moran I statistic       Expectation          Variance 
#>     -2.007693e-02     -1.886792e-02      4.253323e-05
```

``` r
coords <- data_set  |> 
  filter(populacao == "Selvíria/MS")  |>  
  select(longitude, latitude)

nb <- dnearneigh(as.matrix(coords), 5, 500)  # ajuste distância
lw <- nb2listw(nb, style = "W")

walk(names_vari, ~{
  x <- data_set |> filter(populacao == "Selvíria/MS")|> pull(!!sym(.x))
  x[is.na(x)] <- mean(x,na.rm=TRUE) 
  print(paste0("|---------- ",.x," ---------|"))
  print(moran.test(x, lw))
  
})
#> [1] "|---------- hc_m ---------|"
#> 
#>  Moran I test under randomisation
#> 
#> data:  x  
#> weights: lw    
#> 
#> Moran I statistic standard deviate = -0.65149, p-value = 0.7426
#> alternative hypothesis: greater
#> sample estimates:
#> Moran I statistic       Expectation          Variance 
#>      -0.053694944      -0.025641026       0.001854261 
#> 
#> [1] "|---------- ht_m ---------|"
#> 
#>  Moran I test under randomisation
#> 
#> data:  x  
#> weights: lw    
#> 
#> Moran I statistic standard deviate = -0.16222, p-value = 0.5644
#> alternative hypothesis: greater
#> sample estimates:
#> Moran I statistic       Expectation          Variance 
#>      -0.032837086      -0.025641026       0.001967695 
#> 
#> [1] "|---------- dap_cm ---------|"
#> 
#>  Moran I test under randomisation
#> 
#> data:  x  
#> weights: lw    
#> 
#> Moran I statistic standard deviate = 0.15944, p-value = 0.4367
#> alternative hypothesis: greater
#> sample estimates:
#> Moran I statistic       Expectation          Variance 
#>       -0.01915098       -0.02564103        0.00165698 
#> 
#> [1] "|---------- dmc_m ---------|"
#> 
#>  Moran I test under randomisation
#> 
#> data:  x  
#> weights: lw    
#> 
#> Moran I statistic standard deviate = -0.7317, p-value = 0.7678
#> alternative hypothesis: greater
#> sample estimates:
#> Moran I statistic       Expectation          Variance 
#>      -0.057545264      -0.025641026       0.001901227 
#> 
#> [1] "|---------- eca_cm ---------|"
#> 
#>  Moran I test under randomisation
#> 
#> data:  x  
#> weights: lw    
#> 
#> Moran I statistic standard deviate = 0.77491, p-value = 0.2192
#> alternative hypothesis: greater
#> sample estimates:
#> Moran I statistic       Expectation          Variance 
#>       0.007428764      -0.025641026       0.001821233 
#> 
#> [1] "|---------- fruto ---------|"
#> 
#>  Moran I test under randomisation
#> 
#> data:  x  
#> weights: lw    
#> 
#> Moran I statistic standard deviate = -0.42419, p-value = 0.6643
#> alternative hypothesis: greater
#> sample estimates:
#> Moran I statistic       Expectation          Variance 
#>      -0.044352109      -0.025641026       0.001945691 
#> 
#> [1] "|---------- mat_organica ---------|"
#> 
#>  Moran I test under randomisation
#> 
#> data:  x  
#> weights: lw    
#> 
#> Moran I statistic standard deviate = 1.424, p-value = 0.07722
#> alternative hypothesis: greater
#> sample estimates:
#> Moran I statistic       Expectation          Variance 
#>        0.03341572       -0.02564103        0.00171986 
#> 
#> [1] "|---------- p_h ---------|"
#> 
#>  Moran I test under randomisation
#> 
#> data:  x  
#> weights: lw    
#> 
#> Moran I statistic standard deviate = -0.22548, p-value = 0.5892
#> alternative hypothesis: greater
#> sample estimates:
#> Moran I statistic       Expectation          Variance 
#>       -0.03481936       -0.02564103        0.00165703 
#> 
#> [1] "|---------- p_h_smp ---------|"
#> 
#>  Moran I test under randomisation
#> 
#> data:  x  
#> weights: lw    
#> 
#> Moran I statistic standard deviate = 0.13024, p-value = 0.4482
#> alternative hypothesis: greater
#> sample estimates:
#> Moran I statistic       Expectation          Variance 
#>      -0.020051885      -0.025641026       0.001841576 
#> 
#> [1] "|---------- p_fosforo ---------|"
#> 
#>  Moran I test under randomisation
#> 
#> data:  x  
#> weights: lw    
#> 
#> Moran I statistic standard deviate = -0.16956, p-value = 0.5673
#> alternative hypothesis: greater
#> sample estimates:
#> Moran I statistic       Expectation          Variance 
#>       -0.03265784       -0.02564103        0.00171260 
#> 
#> [1] "|---------- k_potacio ---------|"
#> 
#>  Moran I test under randomisation
#> 
#> data:  x  
#> weights: lw    
#> 
#> Moran I statistic standard deviate = -0.10478, p-value = 0.5417
#> alternative hypothesis: greater
#> sample estimates:
#> Moran I statistic       Expectation          Variance 
#>      -0.029203472      -0.025641026       0.001156041 
#> 
#> [1] "|---------- ca_calcio ---------|"
#> 
#>  Moran I test under randomisation
#> 
#> data:  x  
#> weights: lw    
#> 
#> Moran I statistic standard deviate = 0.92111, p-value = 0.1785
#> alternative hypothesis: greater
#> sample estimates:
#> Moran I statistic       Expectation          Variance 
#>       0.009236546      -0.025641026       0.001433738 
#> 
#> [1] "|---------- mg_magnesio ---------|"
#> 
#>  Moran I test under randomisation
#> 
#> data:  x  
#> weights: lw    
#> 
#> Moran I statistic standard deviate = 0.50368, p-value = 0.3072
#> alternative hypothesis: greater
#> sample estimates:
#> Moran I statistic       Expectation          Variance 
#>      -0.005690715      -0.025641026       0.001568881 
#> 
#> [1] "|---------- al_aluminio ---------|"
#> 
#>  Moran I test under randomisation
#> 
#> data:  x  
#> weights: lw    
#> 
#> Moran I statistic standard deviate = 0.76152, p-value = 0.2232
#> alternative hypothesis: greater
#> sample estimates:
#> Moran I statistic       Expectation          Variance 
#>       0.007949595      -0.025641026       0.001945708 
#> 
#> [1] "|---------- s_enxofre ---------|"
#> 
#>  Moran I test under randomisation
#> 
#> data:  x  
#> weights: lw    
#> 
#> Moran I statistic standard deviate = 2.5927, p-value = 0.004761
#> alternative hypothesis: greater
#> sample estimates:
#> Moran I statistic       Expectation          Variance 
#>       0.072643402      -0.025641026       0.001437004
```

``` r
coords <- data_set  |> 
  filter(populacao == "Aparecida D'Oeste/SP")  |>  
  select(longitude, latitude)

nb <- dnearneigh(as.matrix(coords), 5, 5100)  # ajuste distância
lw <- nb2listw(nb, style = "W")

walk(names_vari, ~{
  x <- data_set |> filter(populacao == "Aparecida D'Oeste/SP")|> pull(!!sym(.x))
  x[is.na(x)] <- mean(x,na.rm=TRUE) 
  print(paste0("|---------- ",.x," ---------|"))
  print(moran.test(x, lw))
  
})
#> [1] "|---------- hc_m ---------|"
#> 
#>  Moran I test under randomisation
#> 
#> data:  x  
#> weights: lw    
#> 
#> Moran I statistic standard deviate = 0.50784, p-value = 0.3058
#> alternative hypothesis: greater
#> sample estimates:
#> Moran I statistic       Expectation          Variance 
#>     -0.0001252212     -0.0256410256      0.0025244091 
#> 
#> [1] "|---------- ht_m ---------|"
#> 
#>  Moran I test under randomisation
#> 
#> data:  x  
#> weights: lw    
#> 
#> Moran I statistic standard deviate = -0.61418, p-value = 0.7305
#> alternative hypothesis: greater
#> sample estimates:
#> Moran I statistic       Expectation          Variance 
#>      -0.056165451      -0.025641026       0.002470066 
#> 
#> [1] "|---------- dap_cm ---------|"
#> 
#>  Moran I test under randomisation
#> 
#> data:  x  
#> weights: lw    
#> 
#> Moran I statistic standard deviate = 0.83256, p-value = 0.2025
#> alternative hypothesis: greater
#> sample estimates:
#> Moran I statistic       Expectation          Variance 
#>        0.01660582       -0.02564103        0.00257486 
#> 
#> [1] "|---------- dmc_m ---------|"
#> 
#>  Moran I test under randomisation
#> 
#> data:  x  
#> weights: lw    
#> 
#> Moran I statistic standard deviate = 0.86456, p-value = 0.1936
#> alternative hypothesis: greater
#> sample estimates:
#> Moran I statistic       Expectation          Variance 
#>       0.016311996      -0.025641026       0.002354678 
#> 
#> [1] "|---------- eca_cm ---------|"
#> 
#>  Moran I test under randomisation
#> 
#> data:  x  
#> weights: lw    
#> 
#> Moran I statistic standard deviate = -0.48749, p-value = 0.687
#> alternative hypothesis: greater
#> sample estimates:
#> Moran I statistic       Expectation          Variance 
#>      -0.049655630      -0.025641026       0.002426676 
#> 
#> [1] "|---------- fruto ---------|"
#> 
#>  Moran I test under randomisation
#> 
#> data:  x  
#> weights: lw    
#> 
#> Moran I statistic standard deviate = 1.7713, p-value = 0.03826
#> alternative hypothesis: greater
#> sample estimates:
#> Moran I statistic       Expectation          Variance 
#>       0.051646587      -0.025641026       0.001903882 
#> 
#> [1] "|---------- mat_organica ---------|"
#> 
#>  Moran I test under randomisation
#> 
#> data:  x  
#> weights: lw    
#> 
#> Moran I statistic standard deviate = -0.24528, p-value = 0.5969
#> alternative hypothesis: greater
#> sample estimates:
#> Moran I statistic       Expectation          Variance 
#>      -0.038042810      -0.025641026       0.002556393 
#> 
#> [1] "|---------- p_h ---------|"
#> 
#>  Moran I test under randomisation
#> 
#> data:  x  
#> weights: lw    
#> 
#> Moran I statistic standard deviate = 4.5487, p-value = 2.699e-06
#> alternative hypothesis: greater
#> sample estimates:
#> Moran I statistic       Expectation          Variance 
#>       0.190623204      -0.025641026       0.002260448 
#> 
#> [1] "|---------- p_h_smp ---------|"
#> 
#>  Moran I test under randomisation
#> 
#> data:  x  
#> weights: lw    
#> 
#> Moran I statistic standard deviate = 3.1007, p-value = 0.0009653
#> alternative hypothesis: greater
#> sample estimates:
#> Moran I statistic       Expectation          Variance 
#>        0.12929118       -0.02564103        0.00249668 
#> 
#> [1] "|---------- p_fosforo ---------|"
#> 
#>  Moran I test under randomisation
#> 
#> data:  x  
#> weights: lw    
#> 
#> Moran I statistic standard deviate = 0.25483, p-value = 0.3994
#> alternative hypothesis: greater
#> sample estimates:
#> Moran I statistic       Expectation          Variance 
#>      -0.015437078      -0.025641026       0.001603344 
#> 
#> [1] "|---------- k_potacio ---------|"
#> 
#>  Moran I test under randomisation
#> 
#> data:  x  
#> weights: lw    
#> 
#> Moran I statistic standard deviate = -0.094546, p-value = 0.5377
#> alternative hypothesis: greater
#> sample estimates:
#> Moran I statistic       Expectation          Variance 
#>      -0.029989707      -0.025641026       0.002115597 
#> 
#> [1] "|---------- ca_calcio ---------|"
#> 
#>  Moran I test under randomisation
#> 
#> data:  x  
#> weights: lw    
#> 
#> Moran I statistic standard deviate = 2.226, p-value = 0.01301
#> alternative hypothesis: greater
#> sample estimates:
#> Moran I statistic       Expectation          Variance 
#>       0.084158803      -0.025641026       0.002433036 
#> 
#> [1] "|---------- mg_magnesio ---------|"
#> 
#>  Moran I test under randomisation
#> 
#> data:  x  
#> weights: lw    
#> 
#> Moran I statistic standard deviate = 2.0696, p-value = 0.01924
#> alternative hypothesis: greater
#> sample estimates:
#> Moran I statistic       Expectation          Variance 
#>       0.076144327      -0.025641026       0.002418789 
#> 
#> [1] "|---------- al_aluminio ---------|"
#> 
#>  Moran I test under randomisation
#> 
#> data:  x  
#> weights: lw    
#> 
#> Moran I statistic standard deviate = 4.8176, p-value = 7.264e-07
#> alternative hypothesis: greater
#> sample estimates:
#> Moran I statistic       Expectation          Variance 
#>       0.215406962      -0.025641026       0.002503458 
#> 
#> [1] "|---------- s_enxofre ---------|"
#> 
#>  Moran I test under randomisation
#> 
#> data:  x  
#> weights: lw    
#> 
#> Moran I statistic standard deviate = 2.0442, p-value = 0.02047
#> alternative hypothesis: greater
#> sample estimates:
#> Moran I statistic       Expectation          Variance 
#>       0.075540488      -0.025641026       0.002449979
```

### Modelagem geoestatística - Dependência Espacial

#### Listando Populações

``` r
data_set$populacao |> unique()
#> [1] "Lagoa Santa/GO"       "Selvíria/MS"          "Aparecida D'Oeste/SP"
```

#### Listando Variáveis

``` r
data_set |> select(hc_m:s_enxofre) |> names()
#>  [1] "hc_m"         "ht_m"         "dap_cm"       "dmc_m"        "eca_cm"      
#>  [6] "fruto"        "mat_organica" "p_h"          "p_h_smp"      "p_fosforo"   
#> [11] "k_potacio"    "ca_calcio"    "mg_magnesio"  "al_aluminio"  "s_enxofre"
```

#### Filtrando o banco de dados para análise variográfica

``` r
pop <- "Selvíria/MS"
variavel <- "dmc_m"
data_set_aux <- data_set |> 
  filter(
    populacao == pop) |> 
  select(longitude,latitude,variavel) |> 
  rename(x=longitude, y=latitude, z = !!sym(variavel)) |> 
  group_by(x,y) # |> mutate(z = log(z+1)) ## TRANSFORMAR SE NECESSÁRIO
glimpse(data_set_aux)
#> Rows: 40
#> Columns: 3
#> Groups: x, y [40]
#> $ x <dbl> 438298.1, 438284.5, 438304.2, 437775.3, 438641.6, 438556.3, 438535.6…
#> $ y <dbl> 7726820, 7726840, 7726918, 7726573, 7726432, 7726329, 7726333, 77263…
#> $ z <dbl> 17.0, 16.9, 13.4, 19.3, 14.5, 14.6, 14.0, 21.4, 21.7, 16.6, 13.4, 16…
```

#### Criando objeto tipo sp e fórmula para análise

``` r
coordinates(data_set_aux) = ~ x + y  
form <- z ~ 1
```

#### Construção do Semivariograma Experimental

``` r
vari_exp <- variogram(form, data = data_set_aux,
                      cressie = FALSE,
                      cutoff = 200, # distância máxima do semivariograma
                      width = 15) # distância entre pontos
vari_exp  |>  
 ggplot(aes(x=dist, y=gamma)) +
 geom_point() +
 labs(x="lag (m)",
      y=expression(paste(gamma,"(h)")))
```

![](README_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

#### Ajustando modelos

``` r
patamar=10
alcance=40
epepita=1
modelo_1 <- fit.variogram(vari_exp,vgm(patamar,"Sph",alcance,epepita))
modelo_2 <- fit.variogram(vari_exp,vgm(patamar,"Exp",alcance,epepita))
modelo_3 <- fit.variogram(vari_exp,vgm(patamar,"Gau",alcance,epepita))
plot_my_models(modelo_1,modelo_2,modelo_3)
```

![](README_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-17-2.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-17-3.png)<!-- -->

#### Escolha o melhor modelo (\>r²)

``` r
modelo <- modelo_2
```

#### Salvando os parâmetros ajustados

``` r
model <- modelo |> slice(2) |> pull(model)
rss <- round(attr(modelo, "SSErr"),4)
c0 <- round(modelo$psill[[1]],4)
c0_c1 <- round(sum(modelo$psill),4)
a <- ifelse(model == "Gau", round(modelo$range[[2]]*(3^.5),2),
            ifelse(model == "Exp",round(3*modelo$range[[2]],2),
            round(modelo$range[[2]],2)))
r2 <- vari_exp |> add_column( model = model, a=a, c0 = c0,
                                  c0_c1 = c0_c1) |>
    mutate(
      gamma_m = ifelse(model == "Sph",
        ifelse(dist <= a, c0 + (c0_c1 - c0) * (3/2 * (dist/a) - 1/2 * (dist/a)^3),c0_c1), ifelse(model == "Exp", c0 + (c0_c1-c0)*(1-exp(-3*(dist/a))),c0 + (c0_c1-c0)*(1-exp(-(dist/a)^2)))),
      residuo_total = (gamma-mean(gamma))^2,
      residuo_mod = (gamma - gamma_m)^2
    ) |>
    summarise(
      r2=(sum(residuo_total) - sum(residuo_mod))/sum(residuo_total)
    ) |> pull(r2)

tibble(
  pop, variavel, model, c0, c0_c1, a, rss, r2
) |> mutate(gde = c0/c0_c1, .after = "a") |>
  write_csv(paste0("output/best-fit/",str_replace_all(pop,"/| ","_"),"-",variavel,".csv"))

ls_csv <- list.files("output/best-fit/",full.names = TRUE,pattern = ".csv")
map_df(ls_csv, read_csv) |>
  arrange(pop,variavel) |> 
  writexl::write_xlsx("output/semivariogram-model.xlsx")
```

#### Cluster hierárquico (intra-população) - Lagoa Santa/GO

``` r
arv_matriz <- data_set |> 
  filter(populacao == "Lagoa Santa/GO") |> 
  pull(arv_matriz)
da_pad<-decostand(data_set |> 
                    filter(populacao == "Lagoa Santa/GO") |> 
                    select(hc_m:s_enxofre) |> 
                    mutate(
                      eca_cm = ifelse(is.na(eca_cm),mean(eca_cm,na.rm=TRUE),eca_cm),
                      fruto = ifelse(is.na(fruto),mean(eca_cm,na.rm=TRUE),fruto)
                    ), 
                  method = "standardize",
                  na.rm=TRUE)
da_pad_solo <- da_pad |> select(mat_organica:s_enxofre)
da_pad_planta <- da_pad |> select(-(mat_organica:s_enxofre)) 
```

##### Atributos do Solo

``` r
da_pad_euc<-vegdist(da_pad_solo,"euclidean") 
da_pad_euc_ward<-hclust(da_pad_euc, method="ward.D")
da_pad_euc_ward$labels <- arv_matriz
plot(da_pad_euc_ward,
     ylab = "Distância Euclidiana",
     xlab = "Municípios",
     main = "",
     hang = -1,
     col = "black",
     lwd = 1.5,
     cex = .75,
     las = 1,
     axes = FALSE)
```

![](README_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

``` r
grupo_solo<-cutree(da_pad_euc_ward,3)
```

##### Atributos da Planta

``` r
da_pad_euc<-vegdist(da_pad_planta,"euclidean") 
da_pad_euc_ward<-hclust(da_pad_euc, method="ward.D")
da_pad_euc_ward$labels <- arv_matriz
plot(da_pad_euc_ward,
     ylab = "Distância Euclidiana",
     xlab = "Municípios",
     main = "",
     hang = -1,
     col = "black",
     lwd = 1.5,
     cex = .75,
     las = 1,
     axes = FALSE)
```

![](README_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

``` r
grupo_planta<-cutree(da_pad_euc_ward,3)
```

##### Solo + Planta

``` r
da_pad_euc<-vegdist(da_pad,"euclidean") 
da_pad_euc_ward<-hclust(da_pad_euc, method="ward.D")
da_pad_euc_ward$labels <- arv_matriz
plot(da_pad_euc_ward,
     ylab = "Distância Euclidiana",
     xlab = "Municípios",
     main = "",
     hang = -1,
     col = "black",
     lwd = 1.5,
     cex = .75,
     las = 1,
     axes = FALSE)
```

![](README_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

``` r
grupo_solo_planta <- cutree(da_pad_euc_ward,3)
```

### Comparação dos grupos Espaciais

``` r
df_aux <- data_set |> 
  filter(populacao == "Lagoa Santa/GO") |> 
  add_column(grupo_planta,grupo_solo,grupo_solo_planta)
df_aux |> 
  ggplot(aes(longitude, latitude,colour = as_factor(grupo_planta))) +
  geom_point(size=2) +
  theme_bw() + 
  labs(color = "Grupo-Planta") +
  scale_color_manual(values = c("#1B9E77","#D95F02","#7570B3")) +
  theme(
    legend.position = "top"
  )
```

![](README_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

``` r

df_aux |> 
  ggplot(aes(longitude, latitude,colour = as_factor(grupo_solo))) +
  geom_point(size=2) +
  theme_bw() + 
  labs(color = "Grupo-Solo") +
  scale_color_manual(values = c("#1B9E77","#D95F02","#7570B3")) +
    theme(
    legend.position = "top"
  )
```

![](README_files/figure-gfm/unnamed-chunk-24-2.png)<!-- -->

``` r


df_aux |> 
  ggplot(aes(longitude, latitude,colour = as_factor(grupo_solo_planta))) +
  geom_point(size=2) +
  theme_bw() + 
  labs(color = "Grupo-Solo-Planta") +
  scale_color_manual(values = c("#1B9E77","#D95F02","#7570B3"))+
    theme(
    legend.position = "top"
  )
```

![](README_files/figure-gfm/unnamed-chunk-24-3.png)<!-- -->

#### Cluster hierárquico (intra-população) - Selvíria/MS

``` r
arv_matriz <- data_set |> 
  filter(populacao == "Selvíria/MS") |> 
  pull(arv_matriz)
da_pad<-decostand(data_set |> 
                    filter(populacao == "Selvíria/MS") |> 
                    select(hc_m:s_enxofre) |> 
                    mutate(
                      eca_cm = ifelse(is.na(eca_cm),mean(eca_cm,na.rm=TRUE),eca_cm),
                      fruto = ifelse(is.na(fruto),mean(eca_cm,na.rm=TRUE),fruto)
                    ), 
                  method = "standardize",
                  na.rm=TRUE)
da_pad_solo <- da_pad |> select(mat_organica:s_enxofre)
da_pad_planta <- da_pad |> select(-(mat_organica:s_enxofre)) 
```

##### Atributos do Solo

``` r
da_pad_euc<-vegdist(da_pad_solo,"euclidean") 
da_pad_euc_ward<-hclust(da_pad_euc, method="ward.D")
da_pad_euc_ward$labels <- arv_matriz
plot(da_pad_euc_ward,
     ylab = "Distância Euclidiana",
     xlab = "Municípios",
     main = "",
     hang = -1,
     col = "black",
     lwd = 1.5,
     cex = .75,
     las = 1,
     axes = FALSE)
```

![](README_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

``` r
grupo_solo<-cutree(da_pad_euc_ward,3)
```

##### Atributos da Planta

``` r
da_pad_euc<-vegdist(da_pad_planta,"euclidean") 
da_pad_euc_ward<-hclust(da_pad_euc, method="ward.D")
da_pad_euc_ward$labels <- arv_matriz
plot(da_pad_euc_ward,
     ylab = "Distância Euclidiana",
     xlab = "Municípios",
     main = "",
     hang = -1,
     col = "black",
     lwd = 1.5,
     cex = .75,
     las = 1,
     axes = FALSE)
```

![](README_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->

``` r
grupo_planta<-cutree(da_pad_euc_ward,3)
```

##### Solo + Planta

``` r
da_pad_euc<-vegdist(da_pad,"euclidean") 
da_pad_euc_ward<-hclust(da_pad_euc, method="ward.D")
da_pad_euc_ward$labels <- arv_matriz
plot(da_pad_euc_ward,
     ylab = "Distância Euclidiana",
     xlab = "Municípios",
     main = "",
     hang = -1,
     col = "black",
     lwd = 1.5,
     cex = .75,
     las = 1,
     axes = FALSE)
```

![](README_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->

``` r
grupo_solo_planta <- cutree(da_pad_euc_ward,3)
```

### Comparação dos grupos Espaciais

``` r
df_aux <- data_set |> 
  filter(populacao == "Selvíria/MS") |> 
  add_column(grupo_planta,grupo_solo,grupo_solo_planta)
df_aux |> 
  ggplot(aes(longitude, latitude,colour = as_factor(grupo_planta))) +
  geom_point(size=2) +
  theme_bw() + 
  labs(color = "Grupo-Planta") +
  scale_color_manual(values = c("#1B9E77","#D95F02","#7570B3")) +
  theme(
    legend.position = "top"
  )
```

![](README_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->

``` r

df_aux |> 
  ggplot(aes(longitude, latitude,colour = as_factor(grupo_solo))) +
  geom_point(size=2) +
  theme_bw() + 
  labs(color = "Grupo-Solo") +
  scale_color_manual(values = c("#1B9E77","#D95F02","#7570B3")) +
    theme(
    legend.position = "top"
  )
```

![](README_files/figure-gfm/unnamed-chunk-29-2.png)<!-- -->

``` r


df_aux |> 
  ggplot(aes(longitude, latitude,colour = as_factor(grupo_solo_planta))) +
  geom_point(size=2) +
  theme_bw() + 
  labs(color = "Grupo-Solo-Planta") +
  scale_color_manual(values = c("#1B9E77","#D95F02","#7570B3"))+
    theme(
    legend.position = "top"
  )
```

![](README_files/figure-gfm/unnamed-chunk-29-3.png)<!-- -->

#### Cluster hierárquico (intra-população) - Aparecida D’Oeste/SP

``` r
arv_matriz <- data_set |> 
  filter(populacao == "Aparecida D'Oeste/SP") |> 
  pull(arv_matriz)
da_pad<-decostand(data_set |> 
                    filter(populacao == "Aparecida D'Oeste/SP") |> 
                    select(hc_m:s_enxofre) |> 
                    mutate(
                      eca_cm = ifelse(is.na(eca_cm),mean(eca_cm,na.rm=TRUE),eca_cm),
                      fruto = ifelse(is.na(fruto),mean(eca_cm,na.rm=TRUE),fruto)
                    ), 
                  method = "standardize",
                  na.rm=TRUE)
da_pad_solo <- da_pad |> select(mat_organica:s_enxofre)
da_pad_planta <- da_pad |> select(-(mat_organica:s_enxofre)) 
```

##### Atributos do Solo

``` r
da_pad_euc<-vegdist(da_pad_solo,"euclidean") 
da_pad_euc_ward<-hclust(da_pad_euc, method="ward.D")
da_pad_euc_ward$labels <- arv_matriz
plot(da_pad_euc_ward,
     ylab = "Distância Euclidiana",
     xlab = "Municípios",
     main = "",
     hang = -1,
     col = "black",
     lwd = 1.5,
     cex = .75,
     las = 1,
     axes = FALSE)
```

![](README_files/figure-gfm/unnamed-chunk-31-1.png)<!-- -->

``` r
grupo_solo<-cutree(da_pad_euc_ward,3)
```

##### Atributos da Planta

``` r
da_pad_euc<-vegdist(da_pad_planta,"euclidean") 
da_pad_euc_ward<-hclust(da_pad_euc, method="ward.D")
da_pad_euc_ward$labels <- arv_matriz
plot(da_pad_euc_ward,
     ylab = "Distância Euclidiana",
     xlab = "Municípios",
     main = "",
     hang = -1,
     col = "black",
     lwd = 1.5,
     cex = .75,
     las = 1,
     axes = FALSE)
```

![](README_files/figure-gfm/unnamed-chunk-32-1.png)<!-- -->

``` r
grupo_planta<-cutree(da_pad_euc_ward,3)
```

##### Solo + Planta

``` r
da_pad_euc<-vegdist(da_pad,"euclidean") 
da_pad_euc_ward<-hclust(da_pad_euc, method="ward.D")
da_pad_euc_ward$labels <- arv_matriz
plot(da_pad_euc_ward,
     ylab = "Distância Euclidiana",
     xlab = "Municípios",
     main = "",
     hang = -1,
     col = "black",
     lwd = 1.5,
     cex = .75,
     las = 1,
     axes = FALSE)
```

![](README_files/figure-gfm/unnamed-chunk-33-1.png)<!-- -->

``` r
grupo_solo_planta <- cutree(da_pad_euc_ward,3)
```

### Comparação dos grupos Espaciais

``` r
df_aux <- data_set |> 
  filter(populacao == "Aparecida D'Oeste/SP") |> 
  add_column(grupo_planta,grupo_solo,grupo_solo_planta)
df_aux |> 
  ggplot(aes(longitude, latitude,colour = as_factor(grupo_planta))) +
  geom_point(size=2) +
  theme_bw() + 
  labs(color = "Grupo-Planta") +
  scale_color_manual(values = c("#1B9E77","#D95F02","#7570B3")) +
  theme(
    legend.position = "top"
  )
```

![](README_files/figure-gfm/unnamed-chunk-34-1.png)<!-- -->

``` r

df_aux |> 
  ggplot(aes(longitude, latitude,colour = as_factor(grupo_solo))) +
  geom_point(size=2) +
  theme_bw() + 
  labs(color = "Grupo-Solo") +
  scale_color_manual(values = c("#1B9E77","#D95F02","#7570B3")) +
    theme(
    legend.position = "top"
  )
```

![](README_files/figure-gfm/unnamed-chunk-34-2.png)<!-- -->

``` r


df_aux |> 
  ggplot(aes(longitude, latitude,colour = as_factor(grupo_solo_planta))) +
  geom_point(size=2) +
  theme_bw() + 
  labs(color = "Grupo-Solo-Planta") +
  scale_color_manual(values = c("#1B9E77","#D95F02","#7570B3"))+
    theme(
    legend.position = "top"
  )
```

![](README_files/figure-gfm/unnamed-chunk-34-3.png)<!-- --> \####
Análise de Componentes Principais - Lagoa Santa/GO

``` r
arv_matriz <- data_set |> 
  filter(populacao == "Lagoa Santa/GO") |> 
  pull(arv_matriz)
da_pad<-decostand(data_set |> 
                    filter(populacao == "Lagoa Santa/GO") |> 
                    select(hc_m:s_enxofre) |> 
                    mutate(
                      eca_cm = ifelse(is.na(eca_cm),mean(eca_cm,na.rm=TRUE),eca_cm),
                      fruto = ifelse(is.na(fruto),mean(eca_cm,na.rm=TRUE),fruto)
                    ), 
                  method = "standardize",
                  na.rm=TRUE)
da_pad_euc<-vegdist(da_pad,"euclidean") 
da_pad_euc_ward<-hclust(da_pad_euc, method="ward.D")
da_pad_euc_ward$labels <- arv_matriz
grupo_solo_planta <- cutree(da_pad_euc_ward,3)
print("======== Análise de Componentes Principais ========== ")
#> [1] "======== Análise de Componentes Principais ========== "
pca <-  prcomp(da_pad,scale.=TRUE)
# Autovalores
eig<-pca$sdev^2
print("==== Autovalores ====")
#> [1] "==== Autovalores ===="
print(round(eig,3))
#>  [1] 4.625 2.063 1.683 1.464 0.982 0.832 0.795 0.682 0.541 0.384 0.307 0.249
#> [13] 0.168 0.126 0.100
print("==== % da variância explicada ====")
#> [1] "==== % da variância explicada ===="
ve<-eig/sum(eig)
print(round(ve,4))
#>  [1] 0.3083 0.1376 0.1122 0.0976 0.0654 0.0554 0.0530 0.0454 0.0361 0.0256
#> [11] 0.0204 0.0166 0.0112 0.0084 0.0067
print("==== % da variância explicada acumulada ====")
#> [1] "==== % da variância explicada acumulada ===="
print(round(cumsum(ve),4)*100)
#>  [1]  30.83  44.59  55.81  65.57  72.11  77.66  82.96  87.50  91.11  93.67
#> [11]  95.71  97.37  98.49  99.33 100.00
print("==== Poder Discriminante ====")
#> [1] "==== Poder Discriminante ===="
mcor<-cor(da_pad,pca$x)
corrplot(mcor)
```

![](README_files/figure-gfm/unnamed-chunk-35-1.png)<!-- -->

``` r
print("==== screeplot ====")
#> [1] "==== screeplot ===="
screeplot(pca)
abline(h=1)
```

![](README_files/figure-gfm/unnamed-chunk-35-2.png)<!-- -->

``` r
pc1V<-cor(da_pad,pca$x)[,1]/sd(cor(da_pad,pca$x)[,1])
pc2V<-cor(da_pad,pca$x)[,2]/sd(cor(da_pad,pca$x)[,2])
pc3V<-cor(da_pad,pca$x)[,3]/sd(cor(da_pad,pca$x)[,3])
pc1c<-pca$x[,1]/sd(pca$x[,1])
pc2c<-pca$x[,2]/sd(pca$x[,2])
pc3c<-pca$x[,3]/sd(pca$x[,3])
nv<-ncol(da_pad) # número de variáveis utilizadas na análise
```

``` r
mc <- cor(da_pad)
# gráfico biplot
bip<-data.frame(pc1c,pc2c,pc3c,grupo_solo_planta)
texto <- data.frame(
  x = pc1V,
  y = pc2V,
  z = pc3V,
  label = rownames(mc)
)

bi_plot <- bip |> 
  ggplot(aes(x=pc1c,y=pc2c,color = as_factor(grupo_solo_planta)))+
  geom_point(size = 1) + 
  theme_minimal() +
  # scale_shape_manual(values=16:18)+
  scale_color_manual(values=c("#009E73", "#999999","#D55E00")) +
  #annotate(geom="text", x=pc1V, y=pc2V, label=names(pc1V),
  #            color="black",font=3)+
  geom_vline(aes(xintercept=0),
             color="black", size=1)+
  geom_hline(aes(yintercept=0),
             color="black", size=1)+
  annotate(geom="segment",
           x=rep(0,length(da_pad)),
           xend=texto$x,
           y=rep(0,length(da_pad)),
           yend=texto$y,color="black",lwd=.5)+
  geom_label(data=texto,aes(x=x,y=y,label=label),
             color="black",angle=0,fontface="bold",size=4,fill="white")+
  labs(x=paste("CP1 (",round(100*ve[1],2),"%)",sep=""),
       y=paste("CP2 (",round(100*ve[2],2),"%)",sep=""),
       color="",shape="")+
  theme(legend.position = "top") +
  xlim(-3,3)
bi_plot
```

![](README_files/figure-gfm/unnamed-chunk-37-1.png)<!-- -->

``` r
    print("==== Tabela da correlação dos atributos com cada PC ====")
#> [1] "==== Tabela da correlação dos atributos com cada PC ===="
    ck<-sum(pca$sdev^2>=0.98)
    tabelapca<-vector()
    for( l in 1:ck) tabelapca<-cbind(tabelapca,mcor[,l])
    colnames(tabelapca)<-paste(rep(c("PC"),ck),1:ck,sep="")
    pcat<-round(tabelapca,3)
    tabelapca<-tabelapca[order(abs(tabelapca[,1])),]
    print(tabelapca)
#>                      PC1         PC2         PC3         PC4         PC5
#> hc_m         -0.01344569  0.44286367  0.53530396  0.29194535 -0.44014251
#> fruto         0.06085044 -0.34798963 -0.58093887  0.13905920 -0.38275152
#> eca_cm       -0.10066259  0.39233425 -0.42981875 -0.33464190  0.24117623
#> ht_m         -0.13186171  0.68016066  0.33813570  0.16840800 -0.22230191
#> dap_cm       -0.14832370  0.63780357 -0.26126766 -0.27641817  0.02672020
#> dmc_m        -0.18234817  0.69782498 -0.34007432 -0.01887608  0.10417714
#> s_enxofre     0.34757526  0.24357547  0.12440234  0.40168504  0.58516262
#> mat_organica  0.36236185  0.27941305 -0.35331883  0.62563413 -0.16095113
#> p_h_smp      -0.56469153 -0.10218718 -0.57177554  0.42124400 -0.06223644
#> p_fosforo    -0.63740594  0.17422652 -0.09972320 -0.41125925 -0.19818430
#> al_aluminio   0.74971510 -0.03273123  0.03604982 -0.35381164 -0.23681332
#> p_h          -0.81198068 -0.18384222  0.14597739  0.26576901  0.13951349
#> k_potacio    -0.87302867  0.05823268 -0.07400288  0.04194003 -0.15564586
#> ca_calcio    -0.87400216 -0.07758460  0.15915379 -0.18529329  0.02915358
#> mg_magnesio  -0.90193109 -0.11518703  0.21348500  0.10568402  0.10068986
```

#### Análise de Componentes Principais - Selvíria/MS

``` r
arv_matriz <- data_set |> 
  filter(populacao == "Selvíria/MS") |> 
  pull(arv_matriz)
da_pad<-decostand(data_set |> 
                    filter(populacao == "Selvíria/MS") |> 
                    select(hc_m:s_enxofre) |> 
                    mutate(
                      eca_cm = ifelse(is.na(eca_cm),mean(eca_cm,na.rm=TRUE),eca_cm),
                      fruto = ifelse(is.na(fruto),mean(eca_cm,na.rm=TRUE),fruto)
                    ), 
                  method = "standardize",
                  na.rm=TRUE)
da_pad_euc<-vegdist(da_pad,"euclidean") 
da_pad_euc_ward<-hclust(da_pad_euc, method="ward.D")
da_pad_euc_ward$labels <- arv_matriz
grupo_solo_planta <- cutree(da_pad_euc_ward,3)
print("======== Análise de Componentes Principais ========== ")
#> [1] "======== Análise de Componentes Principais ========== "
pca <-  prcomp(da_pad,scale.=TRUE)
# Autovalores
eig<-pca$sdev^2
print("==== Autovalores ====")
#> [1] "==== Autovalores ===="
print(round(eig,3))
#>  [1] 5.135 2.018 1.594 1.190 1.101 0.934 0.794 0.666 0.490 0.334 0.287 0.187
#> [13] 0.142 0.079 0.049
print("==== % da variância explicada ====")
#> [1] "==== % da variância explicada ===="
ve<-eig/sum(eig)
print(round(ve,4))
#>  [1] 0.3423 0.1346 0.1063 0.0793 0.0734 0.0623 0.0529 0.0444 0.0326 0.0223
#> [11] 0.0191 0.0124 0.0095 0.0052 0.0033
print("==== % da variância explicada acumulada ====")
#> [1] "==== % da variância explicada acumulada ===="
print(round(cumsum(ve),4)*100)
#>  [1]  34.23  47.69  58.32  66.25  73.59  79.82  85.12  89.56  92.82  95.05
#> [11]  96.96  98.20  99.15  99.67 100.00
print("==== Poder Discriminante ====")
#> [1] "==== Poder Discriminante ===="
mcor<-cor(da_pad,pca$x)
corrplot(mcor)
```

![](README_files/figure-gfm/unnamed-chunk-39-1.png)<!-- -->

``` r
print("==== screeplot ====")
#> [1] "==== screeplot ===="
screeplot(pca)
abline(h=1)
```

![](README_files/figure-gfm/unnamed-chunk-39-2.png)<!-- -->

``` r
pc1V<-cor(da_pad,pca$x)[,1]/sd(cor(da_pad,pca$x)[,1])
pc2V<-cor(da_pad,pca$x)[,2]/sd(cor(da_pad,pca$x)[,2])
pc3V<-cor(da_pad,pca$x)[,3]/sd(cor(da_pad,pca$x)[,3])
pc1c<-pca$x[,1]/sd(pca$x[,1])
pc2c<-pca$x[,2]/sd(pca$x[,2])
pc3c<-pca$x[,3]/sd(pca$x[,3])
nv<-ncol(da_pad) # número de variáveis utilizadas na análise
```

``` r
mc <- cor(da_pad)
# gráfico biplot
bip<-data.frame(pc1c,pc2c,pc3c,grupo_solo_planta)
texto <- data.frame(
  x = pc1V,
  y = pc2V,
  z = pc3V,
  label = rownames(mc)
)

bi_plot <- bip |> 
  ggplot(aes(x=pc1c,y=pc2c,color = as_factor(grupo_solo_planta)))+
  geom_point(size = 1) + 
  theme_minimal() +
  # scale_shape_manual(values=16:18)+
  scale_color_manual(values=c("#009E73", "#999999","#D55E00")) +
  #annotate(geom="text", x=pc1V, y=pc2V, label=names(pc1V),
  #            color="black",font=3)+
  geom_vline(aes(xintercept=0),
             color="black", size=1)+
  geom_hline(aes(yintercept=0),
             color="black", size=1)+
  annotate(geom="segment",
           x=rep(0,length(da_pad)),
           xend=texto$x,
           y=rep(0,length(da_pad)),
           yend=texto$y,color="black",lwd=.5)+
  geom_label(data=texto,aes(x=x,y=y,label=label),
             color="black",angle=0,fontface="bold",size=4,fill="white")+
  labs(x=paste("CP1 (",round(100*ve[1],2),"%)",sep=""),
       y=paste("CP2 (",round(100*ve[2],2),"%)",sep=""),
       color="",shape="")+
  theme(legend.position = "top") +
  xlim(-3,3)
bi_plot
```

![](README_files/figure-gfm/unnamed-chunk-41-1.png)<!-- -->

``` r
    print("==== Tabela da correlação dos atributos com cada PC ====")
#> [1] "==== Tabela da correlação dos atributos com cada PC ===="
    ck<-sum(pca$sdev^2>=0.98)
    tabelapca<-vector()
    for( l in 1:ck) tabelapca<-cbind(tabelapca,mcor[,l])
    colnames(tabelapca)<-paste(rep(c("PC"),ck),1:ck,sep="")
    pcat<-round(tabelapca,3)
    tabelapca<-tabelapca[order(abs(tabelapca[,1])),]
    print(tabelapca)
#>                      PC1         PC2         PC3         PC4         PC5
#> dap_cm       -0.03720212 -0.49286413 -0.23685276 -0.22329512 -0.37377643
#> eca_cm        0.04465574 -0.08500643 -0.39197125  0.22387101 -0.73985780
#> p_fosforo     0.06177373 -0.62844958  0.34216040  0.27125757 -0.20569414
#> fruto         0.13276278 -0.59781621 -0.29609027  0.18386976  0.46149381
#> hc_m         -0.14590026  0.47967475 -0.53030914  0.49788401 -0.03706777
#> dmc_m        -0.20917910 -0.62469872 -0.34444468 -0.43405096  0.07382397
#> ht_m         -0.36877853 -0.12800766 -0.70660970 -0.04284303  0.18611551
#> p_h_smp      -0.56947750  0.32902202 -0.32852060 -0.10466256 -0.08081050
#> s_enxofre     0.68423802  0.12466707  0.08523328 -0.43627576 -0.25834767
#> al_aluminio   0.68816003 -0.25804327  0.01613440  0.41600038  0.08667470
#> p_h          -0.73511263  0.28466852  0.16601845 -0.10631870  0.10230079
#> mat_organica  0.74062915  0.25338315 -0.30494275 -0.36400088  0.13170398
#> k_potacio    -0.91329478 -0.14672868  0.02179283  0.09215148  0.01964008
#> mg_magnesio  -0.92330925 -0.03040635  0.12212412 -0.13132498 -0.08173184
#> ca_calcio    -0.93151443 -0.14483658  0.17079114 -0.01455222 -0.04345424
```

#### Análise de Componentes Principais - Aparecida D’Oeste/SP

``` r
arv_matriz <- data_set |> 
  filter(populacao == "Aparecida D'Oeste/SP") |> 
  pull(arv_matriz)
da_pad<-decostand(data_set |> 
                    filter(populacao == "Aparecida D'Oeste/SP") |> 
                    select(hc_m:s_enxofre) |> 
                    mutate(
                      eca_cm = ifelse(is.na(eca_cm),mean(eca_cm,na.rm=TRUE),eca_cm),
                      fruto = ifelse(is.na(fruto),mean(eca_cm,na.rm=TRUE),fruto)
                    ), 
                  method = "standardize",
                  na.rm=TRUE)
da_pad_euc<-vegdist(da_pad,"euclidean") 
da_pad_euc_ward<-hclust(da_pad_euc, method="ward.D")
da_pad_euc_ward$labels <- arv_matriz
grupo_solo_planta <- cutree(da_pad_euc_ward,3)
print("======== Análise de Componentes Principais ========== ")
#> [1] "======== Análise de Componentes Principais ========== "
pca <-  prcomp(da_pad,scale.=TRUE)
# Autovalores
eig<-pca$sdev^2
print("==== Autovalores ====")
#> [1] "==== Autovalores ===="
print(round(eig,3))
#>  [1] 4.008 2.181 1.813 1.480 1.248 1.038 0.905 0.667 0.551 0.362 0.301 0.170
#> [13] 0.152 0.074 0.050
print("==== % da variância explicada ====")
#> [1] "==== % da variância explicada ===="
ve<-eig/sum(eig)
print(round(ve,4))
#>  [1] 0.2672 0.1454 0.1208 0.0986 0.0832 0.0692 0.0603 0.0445 0.0367 0.0241
#> [11] 0.0200 0.0114 0.0102 0.0049 0.0033
print("==== % da variância explicada acumulada ====")
#> [1] "==== % da variância explicada acumulada ===="
print(round(cumsum(ve),4)*100)
#>  [1]  26.72  41.26  53.34  63.21  71.53  78.45  84.48  88.93  92.60  95.01
#> [11]  97.02  98.15  99.17  99.67 100.00
print("==== Poder Discriminante ====")
#> [1] "==== Poder Discriminante ===="
mcor<-cor(da_pad,pca$x)
corrplot(mcor)
```

![](README_files/figure-gfm/unnamed-chunk-43-1.png)<!-- -->

``` r
print("==== screeplot ====")
#> [1] "==== screeplot ===="
screeplot(pca)
abline(h=1)
```

![](README_files/figure-gfm/unnamed-chunk-43-2.png)<!-- -->

``` r
pc1V<-cor(da_pad,pca$x)[,1]/sd(cor(da_pad,pca$x)[,1])
pc2V<-cor(da_pad,pca$x)[,2]/sd(cor(da_pad,pca$x)[,2])
pc3V<-cor(da_pad,pca$x)[,3]/sd(cor(da_pad,pca$x)[,3])
pc1c<-pca$x[,1]/sd(pca$x[,1])
pc2c<-pca$x[,2]/sd(pca$x[,2])
pc3c<-pca$x[,3]/sd(pca$x[,3])
nv<-ncol(da_pad) # número de variáveis utilizadas na análise
```

``` r
mc <- cor(da_pad)
# gráfico biplot
bip<-data.frame(pc1c,pc2c,pc3c,grupo_solo_planta)
texto <- data.frame(
  x = pc1V,
  y = pc2V,
  z = pc3V,
  label = rownames(mc)
)

bi_plot <- bip |> 
  ggplot(aes(x=pc1c,y=pc2c,color = as_factor(grupo_solo_planta)))+
  geom_point(size = 1) + 
  theme_minimal() +
  # scale_shape_manual(values=16:18)+
  scale_color_manual(values=c("#009E73", "#999999","#D55E00")) +
  #annotate(geom="text", x=pc1V, y=pc2V, label=names(pc1V),
  #            color="black",font=3)+
  geom_vline(aes(xintercept=0),
             color="black", size=1)+
  geom_hline(aes(yintercept=0),
             color="black", size=1)+
  annotate(geom="segment",
           x=rep(0,length(da_pad)),
           xend=texto$x,
           y=rep(0,length(da_pad)),
           yend=texto$y,color="black",lwd=.5)+
  geom_label(data=texto,aes(x=x,y=y,label=label),
             color="black",angle=0,fontface="bold",size=4,fill="white")+
  labs(x=paste("CP1 (",round(100*ve[1],2),"%)",sep=""),
       y=paste("CP2 (",round(100*ve[2],2),"%)",sep=""),
       color="",shape="")+
  theme(legend.position = "top") +
  xlim(-3,3)
bi_plot
```

![](README_files/figure-gfm/unnamed-chunk-45-1.png)<!-- -->

``` r
    print("==== Tabela da correlação dos atributos com cada PC ====")
#> [1] "==== Tabela da correlação dos atributos com cada PC ===="
    ck<-sum(pca$sdev^2>=0.98)
    tabelapca<-vector()
    for( l in 1:ck) tabelapca<-cbind(tabelapca,mcor[,l])
    colnames(tabelapca)<-paste(rep(c("PC"),ck),1:ck,sep="")
    pcat<-round(tabelapca,3)
    tabelapca<-tabelapca[order(abs(tabelapca[,1])),]
    print(tabelapca)
#>                       PC1         PC2          PC3          PC4         PC5
#> hc_m         -0.007701919  0.33140530  0.292005126 -0.692417578  0.31039452
#> dmc_m        -0.099490427  0.06213369 -0.046413383  0.719174185  0.29750295
#> s_enxofre    -0.104315535 -0.66982496  0.104510361 -0.298377276  0.27324817
#> p_fosforo     0.130469980 -0.38369564 -0.423703268 -0.313271306  0.46796367
#> eca_cm        0.156637261 -0.29488829 -0.564685702  0.062957614 -0.09982151
#> fruto         0.165347176  0.02153140 -0.228286183  0.387992630  0.69465976
#> dap_cm       -0.181111005  0.34502710  0.450054022  0.191981868 -0.03110640
#> mat_organica -0.250494351 -0.57523699  0.571463217  0.274808905 -0.02104698
#> ht_m         -0.274131751  0.62102169  0.153597094 -0.012506628  0.45509587
#> k_potacio     0.441435243  0.63949427 -0.009255651  0.003441361 -0.04150786
#> p_h_smp       0.741954491 -0.32333303  0.509223485  0.135572306  0.06165233
#> al_aluminio  -0.826955684  0.08538641 -0.360629913  0.084417459 -0.19879671
#> ca_calcio     0.870214128  0.08510161 -0.315365438  0.040814381 -0.05872738
#> p_h           0.877842107 -0.04992832  0.279665014 -0.002029451 -0.00122782
#> mg_magnesio   0.889141672  0.13290896 -0.209472802  0.040596379 -0.14122471
#>                       PC6
#> hc_m         -0.245125728
#> dmc_m        -0.153658747
#> s_enxofre     0.090748601
#> p_fosforo    -0.171068948
#> eca_cm       -0.641855791
#> fruto         0.193900434
#> dap_cm       -0.645468823
#> mat_organica -0.013888397
#> ht_m         -0.038888030
#> k_potacio     0.195387354
#> p_h_smp      -0.003418716
#> al_aluminio   0.096134856
#> ca_calcio    -0.033475324
#> p_h          -0.018629376
#> mg_magnesio  -0.007516970
```

### Análise de Redundância (RDA) - Lagoa Santa/GO

A Análise de Redundância (RDA) é um método de ordenação canônica que
combina regressão múltipla e análise de componentes principais,
permitindo avaliar a proporção da variabilidade das variáveis da planta
explicada pelos atributos do solo.

``` r
da_pad<-decostand(data_set |> 
                    filter(populacao == "Lagoa Santa/GO") |> 
                    select(hc_m:s_enxofre) |> 
                    mutate(
                      eca_cm = ifelse(is.na(eca_cm),mean(eca_cm,na.rm=TRUE),eca_cm),
                      fruto = ifelse(is.na(fruto),mean(eca_cm,na.rm=TRUE),fruto)
                    ), 
                  method = "standardize",
                  na.rm=TRUE)
da_pad_solo <- da_pad |> select(mat_organica:s_enxofre)
da_pad_planta <- da_pad |> select(-(mat_organica:s_enxofre)) 
form_rda <- as.formula(
  paste("scale(da_pad_planta) ~",
        paste(names(da_pad_solo), collapse = " + "))
)

# Modelo Completo
rda_GO <- rda(form_rda,
              da_pad)

# Correlação entre solo
cor(da_pad_solo, use = "complete.obs") #|r| > 0.7 → não entram juntas
#>              mat_organica        p_h    p_h_smp  p_fosforo  k_potacio
#> mat_organica   1.00000000 -0.2741123  0.1873028 -0.2370836 -0.1827872
#> p_h           -0.27411226  1.0000000  0.4740552  0.3029287  0.6687919
#> p_h_smp        0.18730282  0.4740552  1.0000000  0.2203569  0.5067828
#> p_fosforo     -0.23708359  0.3029287  0.2203569  1.0000000  0.5425596
#> k_potacio     -0.18278719  0.6687919  0.5067828  0.5425596  1.0000000
#> ca_calcio     -0.48933734  0.6329082  0.3095196  0.5842945  0.7442333
#> mg_magnesio   -0.36118211  0.7920935  0.3983234  0.4830007  0.7484484
#> al_aluminio    0.08495813 -0.6661925 -0.5668446 -0.2778595 -0.5290026
#> s_enxofre      0.27996967 -0.1267618 -0.2145912 -0.3273393 -0.3196130
#>               ca_calcio mg_magnesio al_aluminio  s_enxofre
#> mat_organica -0.4893373  -0.3611821  0.08495813  0.2799697
#> p_h           0.6329082   0.7920935 -0.66619250 -0.1267618
#> p_h_smp       0.3095196   0.3983234 -0.56684458 -0.2145912
#> p_fosforo     0.5842945   0.4830007 -0.27785947 -0.3273393
#> k_potacio     0.7442333   0.7484484 -0.52900260 -0.3196130
#> ca_calcio     1.0000000   0.8288475 -0.52847154 -0.2691153
#> mg_magnesio   0.8288475   1.0000000 -0.67842664 -0.1663193
#> al_aluminio  -0.5284715  -0.6784266  1.00000000  0.1219868
#> s_enxofre    -0.2691153  -0.1663193  0.12198683  1.0000000
cor(da_pad_solo, use = "complete.obs") |> 
  corrplot()
```

![](README_files/figure-gfm/unnamed-chunk-47-1.png)<!-- -->

``` r

# Olhando a Multicolinearidade pelo VIF
vif.cca(rda_GO) #VIF > 10 → remove
#> mat_organica          p_h      p_h_smp    p_fosforo    k_potacio    ca_calcio 
#>     1.897377     3.350243     1.993960     1.704707     3.597656     4.907596 
#>  mg_magnesio  al_aluminio    s_enxofre 
#>     5.983799     2.470169     1.355552

# Não foi necessário remover
# Modelo final
rda_GO
#> Call: rda(formula = scale(da_pad_planta) ~ mat_organica + p_h + p_h_smp +
#> p_fosforo + k_potacio + ca_calcio + mg_magnesio + al_aluminio + s_enxofre,
#> data = da_pad)
#> 
#> -- Model Summary --
#> 
#>               Inertia Proportion Rank
#> Total          6.0000     1.0000     
#> Constrained    1.2047     0.2008    6
#> Unconstrained  4.7953     0.7992    6
#> 
#> Inertia is variance
#> 
#> -- Eigenvalues --
#> 
#> Eigenvalues for constrained axes:
#>   RDA1   RDA2   RDA3   RDA4   RDA5   RDA6 
#> 0.5572 0.4288 0.0941 0.0857 0.0343 0.0045 
#> 
#> Eigenvalues for unconstrained axes:
#>    PC1    PC2    PC3    PC4    PC5    PC6 
#> 1.5194 1.0082 0.7805 0.6956 0.5546 0.2370

#9 variáveis de solo explicaram as variáveis da planta.
# O conjunto de atributos do solo explica ≈ 20% da variação total das características da planta. Constrained = 1.2047  → 20.08% e Unconstrained = 4.7953 → 79.92% o solo, como conjunto, tem papel relevante na estruturação da variação da planta.

# Eixos canônicos (RDA 1 e 2)  0.5572 + 0.4288 ≈ 82% da variação explicada pelo solo 

#A Análise de Redundância indicou que o conjunto de atributos químicos do solo explicou 20,1% da variabilidade total das características da planta. Os dois primeiros eixos canônicos concentraram aproximadamente 82% da variação explicada, indicando que os principais gradientes ambientais associados ao solo estão representados nesses eixos.


anova(rda_GO)            # modelo global - 
#> Permutation test for rda under reduced model
#> Permutation: free
#> Number of permutations: 999
#> 
#> Model: rda(formula = scale(da_pad_planta) ~ mat_organica + p_h + p_h_smp + p_fosforo + k_potacio + ca_calcio + mg_magnesio + al_aluminio + s_enxofre, data = da_pad)
#>          Df Variance      F Pr(>F)
#> Model     9   1.2047 1.2282  0.168
#> Residual 44   4.7953
# essa explicação não é maior do que o esperado ao acaso
# há tendência / efeito moderado, mas sem significância global

anova(rda_GO, by="term") # cada variável do solo
#> Permutation test for rda under reduced model
#> Terms added sequentially (first to last)
#> Permutation: free
#> Number of permutations: 999
#> 
#> Model: rda(formula = scale(da_pad_planta) ~ mat_organica + p_h + p_h_smp + p_fosforo + k_potacio + ca_calcio + mg_magnesio + al_aluminio + s_enxofre, data = da_pad)
#>              Df Variance      F Pr(>F)   
#> mat_organica  1   0.0614 0.5632  0.721   
#> p_h           1   0.0194 0.1777  0.980   
#> p_h_smp       1   0.3423 3.1409  0.005 **
#> p_fosforo     1   0.2163 1.9851  0.063 . 
#> k_potacio     1   0.1278 1.1724  0.338   
#> ca_calcio     1   0.0509 0.4669  0.815   
#> mg_magnesio   1   0.0922 0.8455  0.521   
#> al_aluminio   1   0.1899 1.7426  0.110   
#> s_enxofre     1   0.1046 0.9596  0.408   
#> Residual     44   4.7953                 
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# apenas o pH SMP apresentou efeito estatisticamente significativo sobre a variação multivariada dos atributos das plantas (F = 3,14; p = 0,009)

# O fósforo apresentou efeito marginal (p = 0,078), sugerindo possível influência secundária, porém insuficiente para explicar de forma independente a estrutura dos dados. As demais variáveis do solo  não contribuíram significativamente para o modelo quando consideradas conjuntamente.

# a resposta multivariada das plantas está associada principalmente ao gradiente de acidez potencial do solo

anova(rda_GO, by="axis") # eixos
#> Permutation test for rda under reduced model
#> Forward tests for axes
#> Permutation: free
#> Number of permutations: 999
#> 
#> Model: rda(formula = scale(da_pad_planta) ~ mat_organica + p_h + p_h_smp + p_fosforo + k_potacio + ca_calcio + mg_magnesio + al_aluminio + s_enxofre, data = da_pad)
#>          Df Variance      F Pr(>F)
#> RDA1      1   0.5572 5.4614  0.188
#> RDA2      1   0.4288 4.2031  0.351
#> RDA3      1   0.0941 0.9222  1.000
#> RDA4      1   0.0857 0.8404       
#> RDA5      1   0.0343 0.3362       
#> RDA6      1   0.0045 0.0444       
#> Residual 47   4.7953

# O teste de permutação aplicado aos eixos canônicos da RDA indica que nenhum dos eixos (RDA1–RDA6) é estatisticamente significativo (p > 0.05), apesar de os dois primeiros eixos apresentarem valores relativamente mais altos de variância explicada.

# Isso significa que, quando todas as variáveis do solo são incluídas simultaneamente no modelo, a estrutura multivariada dos dados das plantas não é bem representada por eixos canônicos individuais
```

### Análise de Redundância (RDA) - Selvíria/MS

``` r
da_pad<-decostand(data_set |> 
                    filter(populacao == "Selvíria/MS") |> 
                    select(hc_m:s_enxofre) |> 
                    mutate(
                      eca_cm = ifelse(is.na(eca_cm),mean(eca_cm,na.rm=TRUE),eca_cm),
                      fruto = ifelse(is.na(fruto),mean(eca_cm,na.rm=TRUE),fruto)
                    ), 
                  method = "standardize",
                  na.rm=TRUE)
da_pad_solo <- da_pad |> select(mat_organica:s_enxofre)
da_pad_planta <- da_pad |> select(-(mat_organica:s_enxofre)) 
form_rda <- as.formula(
  paste("scale(da_pad_planta) ~",
        paste(names(da_pad_solo), collapse = " + "))
)

# Modelo Completo
rda_MS <- rda(form_rda,
              da_pad)

# Correlação entre solo
cor(da_pad_solo, use = "complete.obs") #|r| > 0.7 → não entram juntas
#>              mat_organica        p_h    p_h_smp    p_fosforo    k_potacio
#> mat_organica    1.0000000 -0.4594077 -0.1506884 -0.259214764 -0.726102879
#> p_h            -0.4594077  1.0000000  0.3284700 -0.209860847  0.629505482
#> p_h_smp        -0.1506884  0.3284700  1.0000000 -0.239191060  0.493075141
#> p_fosforo      -0.2592148 -0.2098608 -0.2391911  1.000000000  0.003705889
#> k_potacio      -0.7261029  0.6295055  0.4930751  0.003705889  1.000000000
#> ca_calcio      -0.7736735  0.6506286  0.3801273  0.057410848  0.838387979
#> mg_magnesio    -0.6577408  0.6784769  0.4407070  0.015978791  0.853447381
#> al_aluminio     0.3189071 -0.4793893 -0.5494431  0.251970035 -0.442912657
#> s_enxofre       0.5771967 -0.3307715 -0.3665436  0.002283157 -0.655184764
#>                ca_calcio mg_magnesio al_aluminio    s_enxofre
#> mat_organica -0.77367353 -0.65774081   0.3189071  0.577196682
#> p_h           0.65062858  0.67847687  -0.4793893 -0.330771454
#> p_h_smp       0.38012727  0.44070700  -0.5494431 -0.366543601
#> p_fosforo     0.05741085  0.01597879   0.2519700  0.002283157
#> k_potacio     0.83838798  0.85344738  -0.4429127 -0.655184764
#> ca_calcio     1.00000000  0.88251131  -0.6379842 -0.623490424
#> mg_magnesio   0.88251131  1.00000000  -0.6550394 -0.493558804
#> al_aluminio  -0.63798424 -0.65503937   1.0000000  0.284993730
#> s_enxofre    -0.62349042 -0.49355880   0.2849937  1.000000000
cor(da_pad_solo, use = "complete.obs") |> 
  corrplot()
```

![](README_files/figure-gfm/unnamed-chunk-48-1.png)<!-- -->

``` r

# Olhando a Multicolinearidade pelo VIF
vif.cca(rda_MS) #VIF > 10 → remove
#> mat_organica          p_h      p_h_smp    p_fosforo    k_potacio    ca_calcio 
#>     3.476261     2.187351     2.138698     1.504848     8.111156     9.027499 
#>  mg_magnesio  al_aluminio    s_enxofre 
#>     8.050087     3.522434     2.183733

# Não foi necessário remover
# Modelo final
rda_MS
#> Call: rda(formula = scale(da_pad_planta) ~ mat_organica + p_h + p_h_smp +
#> p_fosforo + k_potacio + ca_calcio + mg_magnesio + al_aluminio + s_enxofre,
#> data = da_pad)
#> 
#> -- Model Summary --
#> 
#>               Inertia Proportion Rank
#> Total          6.0000     1.0000     
#> Constrained    1.2992     0.2165    6
#> Unconstrained  4.7008     0.7835    6
#> 
#> Inertia is variance
#> 
#> -- Eigenvalues --
#> 
#> Eigenvalues for constrained axes:
#>   RDA1   RDA2   RDA3   RDA4   RDA5   RDA6 
#> 0.5693 0.2755 0.2336 0.1273 0.0837 0.0098 
#> 
#> Eigenvalues for unconstrained axes:
#>    PC1    PC2    PC3    PC4    PC5    PC6 
#> 1.4119 1.0343 0.8191 0.7630 0.4995 0.1730

# 9 variáveis de solo explicaram as variáveis da planta.
# O conjunto de atributos do solo explica ≈ 22% da variação total das características da planta. Inércia não constrangida: 78,35% o conjunto de variáveis do solo explica pouco da estrutura multivariada das plantas.

# Eixos canônicos (RDA 1 e 2)  0.5693 + 0.2755 ≈ 84% da variação explicada pelo solo 

#A Análise de Redundância indicou que o conjunto de atributos químicos do solo explicou 22% da variabilidade total das características da planta. Os dois primeiros eixos canônicos concentraram aproximadamente 84% da variação explicada, indicando que os principais gradientes ambientais associados ao solo estão representados nesses eixos.


anova(rda_MS)            # modelo global - 
#> Permutation test for rda under reduced model
#> Permutation: free
#> Number of permutations: 999
#> 
#> Model: rda(formula = scale(da_pad_planta) ~ mat_organica + p_h + p_h_smp + p_fosforo + k_potacio + ca_calcio + mg_magnesio + al_aluminio + s_enxofre, data = da_pad)
#>          Df Variance      F Pr(>F)
#> Model     9   1.2992 0.9213    0.6
#> Residual 30   4.7008
# essa explicação não é maior do que o esperado ao acaso
# há tendência / efeito moderado, mas sem significância global. A estrutura dos dados sugere que outros fatores (espaciais, genéticos, microambiente, histórico de manejo) são mais importantes do que os atributos químicos do solo.

anova(rda_MS, by="term") # cada variável do solo
#> Permutation test for rda under reduced model
#> Terms added sequentially (first to last)
#> Permutation: free
#> Number of permutations: 999
#> 
#> Model: rda(formula = scale(da_pad_planta) ~ mat_organica + p_h + p_h_smp + p_fosforo + k_potacio + ca_calcio + mg_magnesio + al_aluminio + s_enxofre, data = da_pad)
#>              Df Variance      F Pr(>F)  
#> mat_organica  1   0.0268 0.1710  0.983  
#> p_h           1   0.2400 1.5314  0.200  
#> p_h_smp       1   0.1288 0.8219  0.539  
#> p_fosforo     1   0.0997 0.6365  0.671  
#> k_potacio     1   0.3321 2.1195  0.057 .
#> ca_calcio     1   0.1099 0.7015  0.619  
#> mg_magnesio   1   0.1446 0.9229  0.468  
#> al_aluminio   1   0.0458 0.2924  0.932  
#> s_enxofre     1   0.1715 1.0942  0.360  
#> Residual     30   4.7008                
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# k_potacio é a única com tendência marginal.

# A análise sequencial dos termos da RDA indicou que nenhuma variável do solo apresentou efeito significativo sobre a variação dos atributos das plantas (p > 0,05), embora o teor de potássio tenha apresentado efeito marginal (F = 2,12; p = 0,065). Os resultados sugerem que não há um único fator edáfico dominante estruturando a variação das plantas nesta localidade, com apenas uma influência fraca e não significativa do potássio.

anova(rda_MS, by="axis") # eixos
#> Permutation test for rda under reduced model
#> Forward tests for axes
#> Permutation: free
#> Number of permutations: 999
#> 
#> Model: rda(formula = scale(da_pad_planta) ~ mat_organica + p_h + p_h_smp + p_fosforo + k_potacio + ca_calcio + mg_magnesio + al_aluminio + s_enxofre, data = da_pad)
#>          Df Variance      F Pr(>F)
#> RDA1      1   0.5693 3.9963  0.585
#> RDA2      1   0.2755 1.9338  0.988
#> RDA3      1   0.2336 1.6397  0.990
#> RDA4      1   0.1273 0.8939  1.000
#> RDA5      1   0.0837 0.5877       
#> RDA6      1   0.0098 0.0691       
#> Residual 33   4.7008

# O teste de permutação aplicado aos eixos canônicos da RDA indica que nenhum dos eixos (RDA1–RDA6) é estatisticamente significativo (p > 0.05), apesar de os dois primeiros eixos apresentarem valores relativamente mais altos de variância explicada.

# Não há evidência de que as variáveis edáficas expliquem de forma consistente a variação multivariada dos atributos das plantas nesta localidade.
```

### Análise de Redundância (RDA) - Aparecida D’Oeste/SP

``` r
da_pad<-decostand(data_set |> 
              filter(populacao == "Aparecida D'Oeste/SP") |> 
              select(hc_m:s_enxofre) |> 
              mutate(
                      eca_cm = ifelse(is.na(eca_cm),mean(eca_cm,na.rm=TRUE),eca_cm),
                      fruto = ifelse(is.na(fruto),mean(eca_cm,na.rm=TRUE),fruto)
                    ), 
                  method = "standardize",
                  na.rm=TRUE)
da_pad_solo <- da_pad |> select(mat_organica:s_enxofre)
da_pad_planta <- da_pad |> select(-(mat_organica:s_enxofre)) 
form_rda <- as.formula(
  paste("scale(da_pad_planta) ~",
        paste(names(da_pad_solo), collapse = " + "))
)

# Modelo Completo
rda_SP <- rda(form_rda,
              da_pad)

# Correlação entre solo
cor(da_pad_solo, use = "complete.obs") #|r| > 0.7 → não entram juntas
#>              mat_organica          p_h      p_h_smp    p_fosforo   k_potacio
#> mat_organica  1.000000000  0.009670605  0.329100401 -0.034181850 -0.46938181
#> p_h           0.009670605  1.000000000  0.810192254 -0.017837537  0.27446930
#> p_h_smp       0.329100401  0.810192254  1.000000000  0.006849213  0.08990715
#> p_fosforo    -0.034181850 -0.017837537  0.006849213  1.000000000 -0.09099024
#> k_potacio    -0.469381808  0.274469301  0.089907150 -0.090990237  1.00000000
#> ca_calcio    -0.422815449  0.695053936  0.437130174  0.156545793  0.32007542
#> mg_magnesio  -0.390686460  0.661004300  0.489152706  0.073971365  0.43618123
#> al_aluminio   0.050875606 -0.756492057 -0.810452162 -0.115494981 -0.34207478
#> s_enxofre     0.188669028 -0.096023650  0.103069066  0.277542994 -0.36533912
#>               ca_calcio mg_magnesio al_aluminio   s_enxofre
#> mat_organica -0.4228154 -0.39068646  0.05087561  0.18866903
#> p_h           0.6950539  0.66100430 -0.75649206 -0.09602365
#> p_h_smp       0.4371302  0.48915271 -0.81045216  0.10306907
#> p_fosforo     0.1565458  0.07397136 -0.11549498  0.27754299
#> k_potacio     0.3200754  0.43618123 -0.34207478 -0.36533912
#> ca_calcio     1.0000000  0.87115644 -0.53257902 -0.21541501
#> mg_magnesio   0.8711564  1.00000000 -0.60680789 -0.21625247
#> al_aluminio  -0.5325790 -0.60680789  1.00000000 -0.15262757
#> s_enxofre    -0.2154150 -0.21625247 -0.15262757  1.00000000
cor(da_pad_solo, use = "complete.obs") |> 
  corrplot()
```

![](README_files/figure-gfm/unnamed-chunk-49-1.png)<!-- -->

``` r

# Olhando a Multicolinearidade pelo VIF
vif.cca(rda_SP) #VIF > 10 → remove
#> mat_organica          p_h      p_h_smp    p_fosforo    k_potacio    ca_calcio 
#>     3.117696     5.617234     8.220561     1.264645     1.991763     6.784262 
#>  mg_magnesio  al_aluminio    s_enxofre 
#>     5.409560     5.634397     1.630697

# Não foi necessário remover
# Modelo final
rda_SP
#> Call: rda(formula = scale(da_pad_planta) ~ mat_organica + p_h + p_h_smp +
#> p_fosforo + k_potacio + ca_calcio + mg_magnesio + al_aluminio + s_enxofre,
#> data = da_pad)
#> 
#> -- Model Summary --
#> 
#>               Inertia Proportion Rank
#> Total          6.0000     1.0000     
#> Constrained    1.4604     0.2434    6
#> Unconstrained  4.5396     0.7566    6
#> 
#> Inertia is variance
#> 
#> -- Eigenvalues --
#> 
#> Eigenvalues for constrained axes:
#>   RDA1   RDA2   RDA3   RDA4   RDA5   RDA6 
#> 0.6804 0.4517 0.1377 0.0980 0.0618 0.0309 
#> 
#> Eigenvalues for unconstrained axes:
#>    PC1    PC2    PC3    PC4    PC5    PC6 
#> 1.2637 1.0496 0.9136 0.5872 0.3981 0.3274

# 9 variáveis de solo explicaram as variáveis da planta.
# O conjunto de atributos do solo explica ≈ 24% da variação total das características da planta. Inércia não constrangida: 75,66% o conjunto de variáveis do solo explica pouco da estrutura multivariada das plantas.

# Eixos canônicos (RDA 1 e 2)   ≈ 77.5% da variação explicada pelo solo 

#A Análise de Redundância indicou que o conjunto de atributos químicos do solo explicou 24% da variabilidade total das características da planta. Os dois primeiros eixos canônicos concentraram aproximadamente 77% da variação explicada, indicando que os principais gradientes ambientais associados ao solo estão representados nesses eixos.


anova(rda_SP)            # modelo global - 
#> Permutation test for rda under reduced model
#> Permutation: free
#> Number of permutations: 999
#> 
#> Model: rda(formula = scale(da_pad_planta) ~ mat_organica + p_h + p_h_smp + p_fosforo + k_potacio + ca_calcio + mg_magnesio + al_aluminio + s_enxofre, data = da_pad)
#>          Df Variance      F Pr(>F)
#> Model     9   1.4604 1.0723  0.381
#> Residual 30   4.5396
# essa explicação não é maior do que o esperado ao acaso
# há tendência / efeito moderado, mas sem significância global. A estrutura dos dados sugere que outros fatores (espaciais, genéticos, microambiente, histórico de manejo) são mais importantes do que os atributos químicos do solo.

anova(rda_SP, by="term") # cada variável do solo
#> Permutation test for rda under reduced model
#> Terms added sequentially (first to last)
#> Permutation: free
#> Number of permutations: 999
#> 
#> Model: rda(formula = scale(da_pad_planta) ~ mat_organica + p_h + p_h_smp + p_fosforo + k_potacio + ca_calcio + mg_magnesio + al_aluminio + s_enxofre, data = da_pad)
#>              Df Variance      F Pr(>F)  
#> mat_organica  1   0.0979 0.6471  0.661  
#> p_h           1   0.0699 0.4618  0.823  
#> p_h_smp       1   0.1020 0.6739  0.667  
#> p_fosforo     1   0.2207 1.4585  0.174  
#> k_potacio     1   0.1681 1.1107  0.372  
#> ca_calcio     1   0.3570 2.3591  0.030 *
#> mg_magnesio   1   0.0671 0.4432  0.826  
#> al_aluminio   1   0.1266 0.8367  0.539  
#> s_enxofre     1   0.2512 1.6600  0.141  
#> Residual     30   4.5396                
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# A variação multivariada dos atributos das plantas está associada principalmente ao gradiente de cálcio no solo.

# A análise de redundância indicou que, entre as variáveis edáficas avaliadas, apenas o teor de cálcio apresentou efeito significativo sobre a variação multivariada dos atributos das plantas (p = 0,023). As demais variáveis não contribuíram significativamente para o modelo.

anova(rda_SP, by="axis") # eixos
#> Permutation test for rda under reduced model
#> Forward tests for axes
#> Permutation: free
#> Number of permutations: 999
#> 
#> Model: rda(formula = scale(da_pad_planta) ~ mat_organica + p_h + p_h_smp + p_fosforo + k_potacio + ca_calcio + mg_magnesio + al_aluminio + s_enxofre, data = da_pad)
#>          Df Variance      F Pr(>F)
#> RDA1      1   0.6804 4.9463  0.234
#> RDA2      1   0.4517 3.2832  0.665
#> RDA3      1   0.1377 1.0006  1.000
#> RDA4      1   0.0980 0.7122       
#> RDA5      1   0.0618 0.4489       
#> RDA6      1   0.0309 0.2248       
#> Residual 33   4.5396

# O teste de permutação aplicado aos eixos canônicos da RDA indica que nenhum dos eixos (RDA1–RDA6) é estatisticamente significativo (p > 0.05), apesar de os dois primeiros eixos apresentarem valores relativamente mais altos de variância explicada.

# Não há evidência de que as variáveis edáficas expliquem de forma consistente a variação multivariada dos atributos das plantas nesta localidade.

# o efeito do Ca existe, mas está disperso entre mais de um eixo e não forte o suficiente para tornar um eixo individualmente significativo.
```
