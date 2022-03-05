plan(multisession, workers = 6)

fit_df <- demog_df %>% 
  na.omit() %>% 
  group_by(time) %>% 
  nest() %>% 
  mutate(
    id = map(data, select_if, is.character),
    data = map(data, select_if, is.numeric),
    data_scaled = map(data, ~ data.frame(scale(.)))
  ) %>% 
  crossing(k = 2:10) %>% 
  mutate(fit = future_map2(data_scaled, k, ~ fclust::Fclust(X = .x, k = .y), .progress = T))

fit_df %>% 
  transmute(time, k,  silhuette = map_dbl(fit, "criterion")) %>% 
  ggplot() + 
  aes(k , time, fill = silhuette) + 
  geom_tile(color = "black")  +
  scale_x_continuous(breaks = 2:10, labels = as.integer, name = "Klaszterek száma")

center_df <- fit_df %>% 
  filter(k == 3) %>% 
  transmute(time, 
            H = map(fit, ~ data.frame(.$H)),
            H = map(H, rownames_to_column, "clust")
  ) %>% 
  unnest() %>% 
  mutate(clust = parse_number(clust))

center_2000_df <- center_df %>% 
  filter(time == 2000) %>% 
  pivot_longer(-(1:2)) %>% 
  rename_at(c(2, 4), str_c, "_2000")

distance_df <- center_df %>% 
  # eucledian distance among clusters
  filter(time != 2000) %>% 
  pivot_longer(-(1:2)) %>% 
  left_join(select(center_2000_df, - time), by = "name") %>% 
  group_by(time, clust, clust_2000) %>% 
  summarise(distance = sum((value-value_2000)^2))

proper_cluster_df <- distance_df %>% 
  group_by(time) %>% 
  nest() %>% 
  ungroup() %>% 
  transmute(time, proper = map(data, function(x) {
    
    x_arranged <- x %>% 
      arrange(distance)
    out <- tibble() # collector
    
    while (T) {
      out <- x_arranged %>% 
        head(1) %>% 
        bind_rows(out)
      
      x_arranged <- x_arranged %>% 
        anti_join(out[1, 1]) %>% 
        anti_join(out[1, 2])
      
      if (nrow(x_arranged) == 0) break
    }
    out
  })) %>% 
  unnest() %>%
  bind_rows(tibble(time = 2000, clust = 1:3, clust_2000 = 1:3, distance = 0)) %>% 
  arrange(time, clust)

demog_descriptive_df <- demog_df %>% 
  # makes available to rescale to original
  na.omit() %>% 
  select(time, where(is.numeric)) %>% 
  pivot_longer(-time) %>% 
  group_by(time, name) %>% 
  summarise(mean = mean(value), sd = sd(value))

center_rescaled_df <- center_df %>% 
  pivot_longer(-(1:2)) %>% 
  left_join(demog_descriptive_df) %>% 
  mutate(value = value * sd + mean) %>% 
  left_join(proper_cluster_df) %>% 
  select(-mean, -sd, -distance)


center_2000_df %>% 
  ggplot(aes(time, value, color = as.factor(clust_2000))) + 
  geom_line() + 
  facet_wrap(~ name, scales = "free")
