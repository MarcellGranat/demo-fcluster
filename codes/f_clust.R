load("data/settings.RData")
set.seed(1)

fit_df <- demog_df %>% 
  na.omit() %>% 
  group_by(time) %>% 
  nest() %>% 
  mutate(
    id = map(data, select_if, is.character), # geo
    data = map(data, select_if, is.numeric),
    data_scaled = map(data, ~ data.frame(scale(.)))
  ) %>% 
  crossing(k = 2:10) %>% 
  mutate(fit = map2(data_scaled, k, ~ fclust::Fclust(X = .x, k = .y))) # fit the model

center_df <- fit_df %>% 
  filter(k == 3) %>% 
  transmute(time, 
            H = map(fit, ~ data.frame(.$H)), # centers of clusters
            H = map(H, rownames_to_column, "clust")
  ) %>% 
  unnest() %>% 
  mutate(clust = parse_number(clust))

center_2000_df <- center_df %>% # find the one most similar to 2000s clusters
  filter(time == 2000) %>% 
  pivot_longer(-(1:2)) %>% 
  rename_at(c(2, 4), str_c, "_2000")

distance_df <- center_df %>% 
  # eucledian distance among clusters
  pivot_longer(-(1:2)) %>% 
  left_join(select(center_2000_df, - time), by = "name") %>% 
  group_by(time, clust, clust_2000) %>% 
  summarise(distance = sum((value-value_2000)^2)) # ^^

proper_cluster_df <- combinations(3) %>% 
  rename(clust = x, clust_2000 = y) %>% 
  left_join(distance_df) %>% 
  group_by(time, comb) %>% 
  mutate(
    total_distance = sum(distance)
  ) %>% 
  group_by(time) %>% 
  filter(total_distance == min(total_distance)) %>% 
  arrange(time) 

proper_cluster_v <- proper_cluster_df %>% 
  arrange(time, clust) %>% 
  group_by(time) %>% 
  select(clust_2000) %>% 
  nest(clust_2000 = clust_2000) %>% 
  mutate(clust_2000 = map(clust_2000, pull))

pull(proper_cluster_v)

u_df <- fit_df %>%
  filter(k == 3) %>% 
  transmute(
    time, id, data,
    u = map(fit, ~ data.frame(.$"U")) # estimated prop of clusters
  ) %>% 
  unnest() %>% 
  janitor::clean_names() %>% 
  left_join(proper_cluster_v) %>% # sorting into 2000 kind order...
  mutate(
    proper_u = pmap(list(clus_1, clus_2, clus_3, clust_2000), function(clus_1, clus_2, clus_3, clust_2000) {
      # cluster prop in correct order (to 2000s clusters) 
      print(clust_2000)
      c(clus_1, clus_2, clus_3)[clust_2000] %>% # sorting vector
        enframe(name = NULL) %>% # to df…
        t() %>% 
        data.frame() %>% 
        set_names("clus_1", "clus_2", "clus_3") # ^
    })
  ) %>% 
  select(- starts_with("clus")) %>% # remove original clus cols to replace
  unnest() # ^

h_df <- center_df %>% 
  left_join(proper_cluster_df) %>% 
  select(- clust, - distance) %>% 
  select(time, clust = clust_2000, everything()) # cluster in correct order

demog_descriptive_df <- demog_df %>% # re-scaling estimated H values…
  select(- country, - geo) %>% 
  pivot_longer(-1) %>% 
  group_by(time, name) %>% 
  summarise(m = mean(value, na.rm = TRUE), s = sd(value, na.rm = T)) %>% 
  na.omit()

h_rescaled_df <- h_df %>% 
  pivot_longer(-(1:2)) %>% # unstandardize by variables…
  left_join(demog_descriptive_df) %>% 
  mutate(value = value * s  + m) %>% # <<
  select(-m, -s) %>% 
  pivot_wider() # transform back ^

save(fit_df, u_df, proper_cluster_df, h_df, h_rescaled_df, file = "data/cluster_fit.RData")
