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


max_u_df <- fit_df %>% 
  filter(k == 3) %>% 
  transmute(id, time, u = map(fit, ~ data.frame(.$"U"))) %>% 
  unnest() %>% 
  janitor::clean_names() %>% 
  rowwise() %>% 
  mutate(which_max_u = which.max(across(starts_with("clus")))) %>% 
  ungroup() %>% 
  select(geo, time, which_max_u)

best_comb_df <- max_u_df %>% 
  mutate(time = time + 1) %>% 
  rename(prev_which_max_u = which_max_u) %>% 
  inner_join(max_u_df) %>% 
  group_by(time) %>% 
  mutate(n_time = n()) %>% # number of areas at that year
  crossing(combinations(3)) %>% 
  filter(prev_which_max_u == x, which_max_u == y) %>% # matching number of areas
  group_by(time, comb) %>% 
  summarise(correct_rate = n() / first(n_time)) %>% # rate of matching
  group_by(time) %>% 
  slice_max(correct_rate) # choose best

proper_cluster_df <- best_comb_df %>% # trans all to 1999 cluster orders
  left_join(combinations(3)) %>% 
  group_split() %>% 
  set_names(2000:2019) %>% 
  map(select, x, y) %>% 
  imap(~ set_names(.x, str_c("time_", c(as.numeric(.y) - 1, .y)))) %>% 
  reduce(left_join) %>% # match to prev
  mutate(cluster_to = row_number()) %>% 
  pivot_longer(- cluster_to, names_to = "time", 
               values_to = "clust", 
               names_transform = parse_number)

cluster_match <- function(cluster, t) {
  # cluster: original cluster
  # t: time
  # output: proper cluster to 1999s cluster order
  map2_dbl(cluster, t, function(x, y) {
    filter(proper_cluster_df, clust == x, time == y) %>% 
      pull(cluster_to)
  })
}

u_df <- fit_df %>%
  filter(k == 3) %>% 
  transmute(
    time, id, data,
    u = map(fit, ~ data.frame(.$"U")) # estimated prop of clusters
  ) %>% 
  unnest() %>% 
  janitor::clean_names() %>% 
  pivot_longer(starts_with("Clus"), names_transform = parse_number) %>% 
  mutate(name = cluster_match(name, time)) %>% 
  pivot_wider(names_prefix = "u")

h_df <- fit_df %>% 
  filter(k == 3) %>% 
  transmute(time, 
            H = map(fit, ~ data.frame(.$H)), # centers of clusters
            H = map(H, rownames_to_column, "clus")
  ) %>% 
  unnest() %>% 
  mutate(
    clus = parse_number(clus),
    clus = cluster_match(clus, time)
    )

demog_descriptive_df <- demog_df %>% # re-scaling estimated H values > mean & sd
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

save(fit_df, u_df, h_df, proper_cluster_df, cluster_match, h_rescaled_df, file = "data/cluster_fit.RData")
