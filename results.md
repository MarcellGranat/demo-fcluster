Results
================

``` r
source("codes/utils.R")
load("data/settings.RData")
load("data/cluster_fit.RData")
```

``` r
theme_set(
  theme_minimal() + 
    theme(
      legend.position = "bottom"
    )
)
```

``` r
fit_df %>% 
  transmute(time, k,  silhuette = map_dbl(fit, "criterion")) %>% 
  ggplot() + 
  aes(k , time, fill = silhuette) + 
  geom_tile(color = "black")  +
  scale_x_continuous(breaks = 2:10, labels = as.integer, name = "# clusters")
```

<img src="figures/unnamed-chunk-3-1.png" style="display: block; margin: auto;" />

``` r
h_df %>% 
  pivot_longer(-(1:2)) %>% 
  ggplot() + 
  aes(time, value, color = as.factor(clus)) + 
  facet_wrap(~ name) + 
  geom_line() +
  labs(color = "Cluster", y = "Scaled center")
```

<img src="figures/unnamed-chunk-4-1.png" style="display: block; margin: auto;" />

-   A klaszeterezés minden évben fut, majd minden klasztert átnevezünk a
    hozzá leghasonlóbb 2000-es klaszeternek

-   pl.: az az 1-es klaszter, aki euklédeszi távolság szerint a
    legközelebb van az 1-es klaszterhez

-   A klaszterezást megelőző standardizálás is évente van futtatva, így
    ezen az ábrán mindig a relatív különbségeket látjuk

``` r
h_rescaled_df %>% 
  pivot_longer(-(1:2)) %>% 
  ggplot() + 
  aes(time, value, color = as.factor(clus)) + 
  facet_wrap(~ name, scales = "free_y") + 
  geom_line() +
  labs(color = "Cluster", y = "Center")
```

<img src="figures/unnamed-chunk-5-1.png" style="display: block; margin: auto;" />

-   Az előző ábrán bemutatott klaszterközepeket felszoroztuk az adott
    évi szórással és hozzáadtuk az adott évi átlagot, így az értékekre
    hatással van a teljes minta elmozdulása is

``` r
u_df %>% 
  rowwise() %>% 
  mutate(which_max_u = which.max(across(starts_with("u")))) %>% 
  left_join(euro_map) %>% 
  group_by(time) %>% 
  group_split() %>% 
  walk(
    ~ {
      p <- ggplot(., aes(geometry = geometry, fill = as.factor(which_max_u))) + 
        geom_sf(color = "black") + 
        labs(title = .$time[[1]], fill = "cluster") + 
        scale_x_continuous(limits = c(-10, 33)) +
        scale_y_continuous(limits = c(35, 65))
      print(p)
    }
  )
```

    ## Joining, by = "geo"

<img src="figures/unnamed-chunk-6-.gif" style="display: block; margin: auto;" />

-   közép-EU országoknál érdekes, hogy sokan mentek át zöldből kékbe

``` r
u_df %>% 
  group_by(geo) %>% 
  slice(1, n()) %>% 
  ungroup() %>% 
  select(time:geo, starts_with("u")) %>% 
  pivot_longer(starts_with("u"), names_to = "clus", values_to = "u", names_transform = parse_number) %>% 
  mutate(l = ifelse(u > .15, u, NA)) %>% 
  group_by(country) %>% 
  group_walk(.keep = T, ~ {
    p <- ggplot(.) + 
      aes(u, geo, fill = clus) + 
      facet_wrap(~ time) + 
      geom_col(color = "black") + 
      geom_text(aes(label = scales::percent(l, accuracy = .1)), position = position_fill(vjust = .5)) +
      ggtitle(countrycode::countrycode(.$country[[1]], "iso2c", "country.name"))
    
    print(p)
  }
  )
```

<img src="figures/unnamed-chunk-7-1.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-7-2.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-7-3.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-7-4.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-7-5.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-7-6.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-7-7.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-7-8.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-7-9.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-7-10.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-7-11.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-7-12.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-7-13.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-7-14.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-7-15.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-7-16.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-7-17.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-7-18.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-7-19.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-7-20.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-7-21.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-7-22.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-7-23.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-7-24.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-7-25.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-7-26.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-7-27.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-7-28.png" style="display: block; margin: auto;" />

``` r
u_df %>% 
  pivot_longer(starts_with("u"), names_to = "clus", values_to = "u", names_transform = parse_number) %>% 
  group_by(country) %>% 
  group_split() %>% 
  walk(~ {
    p <- ggplot(., aes(time, u, fill = as.factor(clus))) + 
      facet_wrap(~ geo) + 
      geom_area(color = "black")  + 
      ggtitle(.$country[[1]])
    print(p)
  })
```

<img src="figures/unnamed-chunk-8-1.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-8-2.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-8-3.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-8-4.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-8-5.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-8-6.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-8-7.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-8-8.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-8-9.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-8-10.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-8-11.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-8-12.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-8-13.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-8-14.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-8-15.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-8-16.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-8-17.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-8-18.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-8-19.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-8-20.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-8-21.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-8-22.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-8-23.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-8-24.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-8-25.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-8-26.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-8-27.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-8-28.png" style="display: block; margin: auto;" />
