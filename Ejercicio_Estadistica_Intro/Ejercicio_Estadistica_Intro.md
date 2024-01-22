Ejercicio_Estadistica_Intro
================
Francisco Alfaro B
2024-01-22

\#Cargando Base de Datos

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ forcats   1.0.0     ✔ readr     2.1.5
    ## ✔ ggplot2   3.4.4     ✔ stringr   1.5.1
    ## ✔ lubridate 1.9.3     ✔ tibble    3.2.1
    ## ✔ purrr     1.0.2     ✔ tidyr     1.3.0

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
data <- read.csv("C:\\Users\\kir8d\\OneDrive\\Desktop\\Data Science\\Experto en Ciencia de datos\\Iowa_Liquor_Sales.csv",
                           stringsAsFactors = FALSE,
                           header = TRUE)
```

\#Transformando la Base de Datos

``` r
datos <- data %>% 
  mutate(Sale..Dollars. = (as.numeric(substr(data$Sale..Dollars.,2,15))),
         City = toupper(City),
         Date = as.Date(Date,format = "%m/%d/%Y"),
         Año = lubridate::year(Date)) %>% 
  rename(Ventas = Sale..Dollars.,
         Ciudad = City,
         Categoria = Category.Name,
         Nombre_Tienda = Store.Name)
```

\#¿Cuál el top 5 de tiendas (promedio de ventas), para el año 2016, para
la ciudad CEDAR RAPIDS?

``` r
datos %>% 
  filter(Ciudad == "CEDAR RAPIDS" & Año == "2016") %>% 
  group_by(Nombre_Tienda) %>% 
  summarize(Promedio_Ventas = mean(Ventas, na.rm = T)) %>% 
  arrange(-Promedio_Ventas) %>% 
  head(Promedio_Ventas, n = 5L)
```

    ## # A tibble: 5 × 2
    ##   Nombre_Tienda                      Promedio_Ventas
    ##   <chr>                                        <dbl>
    ## 1 Sam's Club 8162 / Cedar Rapids                354.
    ## 2 Fareway Stores #151 / Cedar Rapids            338.
    ## 3 Benz Distributing                             171.
    ## 4 Leo1  /  Cedar Rapids                         163.
    ## 5 Target Store T-1771 / Cedar Rapids            162.

\#¿Cuáles fueron los 5 últimos vendedores (promedio de ventas, para el
2016, para DAVENPORT)?

``` r
datos %>% 
  filter(Ciudad == "DAVENPORT" & Año == "2016") %>% 
  group_by(Vendor.Name) %>% 
  summarize(Promedio_Ventas = mean(Ventas, na.rm = T)) %>% 
  arrange(Promedio_Ventas) %>% 
  head(Promedio_Ventas, n = 5L)
```

    ## # A tibble: 5 × 2
    ##   Vendor.Name                   Promedio_Ventas
    ##   <chr>                                   <dbl>
    ## 1 Luxco-St Louis                           36.5
    ## 2 A HARDY USA LTD                          37.0
    ## 3 Rumcoqui and Co                          38.3
    ## 4 Prestige Wine & Spirits Group            42.1
    ## 5 Dehner Distillery                        42.7

\#¿Cuál es el top 5 de productos más vendidos, para el 2016 y 2017, por
ciudad?

``` r
datos %>%
  filter(Año == "2016" | Año == "2017") %>%
  group_by(Ciudad,Año,Item.Description) %>% 
  summarise_at(vars(Ventas),
               list(Cant_Ventas = ~ sum(.,na.rm = TRUE))) %>%
  select(Ciudad, Cant_Ventas, Año, Item.Description) %>%
  pivot_wider(names_from  = c(Ciudad,Año),
              values_from = c(Cant_Ventas),
              names_prefix = "Ventas_",
              values_fill = 0) %>%
  arrange(-n()) %>%
  head(n = 5L)
```

    ## # A tibble: 5 × 7
    ##   Item.Description                 Ventas_CEDAR RAPIDS_…¹ Ventas_CEDAR RAPIDS_…²
    ##   <chr>                                             <dbl>                  <dbl>
    ## 1 "\"Rumchata \"\"GoChatas\"\"\""                   2079                     594
    ## 2 "\"Rumchata \"\"MiniChatas\"\" …                  1890                     504
    ## 3 "360 Bing Cherry"                                  284.                      0
    ## 4 "360 Concord Grape Vodka"                           21                       0
    ## 5 "360 Double Chocolate Vodka"                       924                       0
    ## # ℹ abbreviated names: ¹​`Ventas_CEDAR RAPIDS_2016`, ²​`Ventas_CEDAR RAPIDS_2017`
    ## # ℹ 4 more variables: Ventas_DAVENPORT_2016 <dbl>, Ventas_DAVENPORT_2017 <dbl>,
    ## #   Ventas_WATERLOO_2016 <dbl>, Ventas_WATERLOO_2017 <dbl>
