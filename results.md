Results
================

``` r
source("codes/utils.R")
load("data/settings.RData")
load("data/cluster_fit.RData")
load("data/eco_clustering.RData")
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
  facet_wrap(~ NiceName(name)) + 
  geom_line() +
  geom_point() + 
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
  facet_wrap(~ NiceName(name), scales = "free_y") + 
  geom_line() +
  geom_point() +
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

## Eco clustering

### Clustering comparison

hard: mindenki abba a klaszterbe került besorolásra, amihez a leginkább
tartozik, majd kiátlagoltuk soft: mindenki olyan arányban kerül bele a
klaszterek súlyozott átlagába, amilyen arányban oda tartozik

``` r
eco_hard_df %>% 
  pivot_longer(p80p20:rd_ppl) %>% 
  mutate(type = "hard") %>% 
  bind_rows(
    eco_soft_df %>% 
      pivot_longer(p80p20:rd_ppl) %>% 
      mutate(type = "soft") 
  ) %>% 
  mutate(name = NiceName(name)) %>% 
  ggplot(aes(time, value, color = type)) +
  facet_wrap(str_c("Cluster:", clus) ~ name, scales = "free") + 
  geom_line() +
  geom_point() + 
  labs(color = "Aggregation") + 
  theme_bw() + 
  theme(legend.position = "bottom")
```

<img src="figures/unnamed-chunk-7-1.png" style="display: block; margin: auto;" />

### Hard clustering result

``` r
eco_hard_df %>% 
  pivot_longer(p80p20:rd_ppl) %>% 
  ggplot(aes(time, value, color = as.factor(clus))) + 
  facet_wrap(~ name, scales = "free_y") + 
  geom_line() + 
  geom_point()
```

<img src="figures/unnamed-chunk-8-1.png" style="display: block; margin: auto;" />

### Soft clustering result

``` r
eco_soft_df %>% 
  pivot_longer(p80p20:rd_ppl) %>% 
  ggplot(aes(time, value, color = as.factor(clus))) + 
  facet_wrap(~ name, scales = "free_y") + 
  geom_line() + 
  geom_point()
```

<img src="figures/unnamed-chunk-9-1.png" style="display: block; margin: auto;" />

## Crossing

``` r
walk(c("tfr", "lifexp", "ageing", "motherrate", "emp"), function(x){
  p <- demog_df %>% 
    select(time, geo, x) %>% 
    left_join(eco_hard_df) %>% 
    select(- (time:geo)) %>% 
    filter(!is.na(clus)) %>% 
    mutate(clus = as.factor(clus)) %>% 
    rename_all(NiceName, wrap = 15) %>% 
    GGally::ggpairs(aes(color = clus))
  print(p)
}
)
```

    ## Registered S3 method overwritten by 'GGally':
    ##   method from   
    ##   +.gg   ggplot2

    ## Note: Using an external vector in selections is ambiguous.
    ## ℹ Use `all_of(x)` instead of `x` to silence this message.
    ## ℹ See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
    ## This message is displayed once per session.
    ## Joining, by = "time"
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## 
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## 
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## 
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## 
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## 
    ## Joining, by = "time"

<img src="figures/unnamed-chunk-10-1.png" style="display: block; margin: auto;" />

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## Joining, by = "time"

<img src="figures/unnamed-chunk-10-2.png" style="display: block; margin: auto;" />

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## Joining, by = "time"

<img src="figures/unnamed-chunk-10-3.png" style="display: block; margin: auto;" />

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## Joining, by = "time"

<img src="figures/unnamed-chunk-10-4.png" style="display: block; margin: auto;" />

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

<img src="figures/unnamed-chunk-10-5.png" style="display: block; margin: auto;" />

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
      aes(u, geo, fill = as.factor(clus)) + 
      facet_wrap(~ time) + 
      geom_col(color = "black") + 
      geom_text(aes(label = scales::percent(l, accuracy = .1)), position = position_fill(vjust = .5)) +
      ggtitle(countrycode::countrycode(.$country[[1]], "iso2c", "country.name"))
    
    print(p)
  }
  )
```

<img src="figures/unnamed-chunk-11-1.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-11-2.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-11-3.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-11-4.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-11-5.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-11-6.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-11-7.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-11-8.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-11-9.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-11-10.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-11-11.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-11-12.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-11-13.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-11-14.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-11-15.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-11-16.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-11-17.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-11-18.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-11-19.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-11-20.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-11-21.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-11-22.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-11-23.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-11-24.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-11-25.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-11-26.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-11-27.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-11-28.png" style="display: block; margin: auto;" />

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

<img src="figures/unnamed-chunk-12-1.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-12-2.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-12-3.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-12-4.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-12-5.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-12-6.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-12-7.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-12-8.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-12-9.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-12-10.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-12-11.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-12-12.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-12-13.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-12-14.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-12-15.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-12-16.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-12-17.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-12-18.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-12-19.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-12-20.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-12-21.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-12-22.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-12-23.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-12-24.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-12-25.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-12-26.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-12-27.png" style="display: block; margin: auto;" /><img src="figures/unnamed-chunk-12-28.png" style="display: block; margin: auto;" />
