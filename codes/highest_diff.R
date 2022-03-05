clust2000_df <- fit_df %>% 
  filter(time == 2000, k == 3) %>% 
  transmute(id, map(fit, ~ data.frame(.$clus))) %>% 
  unnest() %>% 
  select(geo, clust_2000 = Cluster)

u2000_df <- fit_df %>%
  filter(k == 3) %>% 
  transmute(time, id, map(fit, ~ data.frame(.$U))) %>% 
  unnest() %>% 
  janitor::clean_names() %>% 
  pivot_longer(starts_with("cl"), names_to = "clust") %>% 
    mutate(clust = as.numeric(str_remove_all(clust, "\\D"))) %>% 
  left_join(select(center_rescaled_df, time, clust, clust_2000)) %>% 
  semi_join(clust2000_df)

highest_diff_df <- u2000_df %>% 
  group_by(geo) %>% 
  summarise(highest_diff = max(value) - min(value)) %>% 
  ungroup() %>% 
  arrange(desc(highest_diff))

highest_diff_df %>% 
  ggplot(aes(highest_diff)) + 
  geom_histogram(color = "black")
