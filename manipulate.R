library(manipulate)

manipulate::manipulate(
  fit_df %>% 
    filter(k == 3) %>% 
    transmute(
      time, id, map(fit, ~ data.frame(.$U))
    ) %>% 
    unnest() %>% 
    janitor::clean_names() %>% 
    pivot_longer(starts_with("Clu")) %>% 
    mutate(clust = parse_number(name)) %>% 
    left_join(unique(select(center_rescaled_df, time, clust, clust_2000))) %>% 
    filter(country == cc) %>% 
    ggplot(aes(x = time, y = value, fill = as.factor(clust_2000))) + 
    geom_area(color = "black") + 
    facet_wrap(~ geo),
  cc = picker("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "EL", "ES",
              "FI", "FR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT",
              "NL", "PL", "PT", "RO", "SE", "SI", "SK", "UK", initial = "BE")
)

