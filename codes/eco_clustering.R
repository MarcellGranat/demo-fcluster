load("data/settings.RData")
load("data/cluster_fit.RData")

eco_hard_df <- u_df %>% 
  rowwise() %>% 
  mutate(which_max_u = which.max(across(starts_with("u")))) %>% # hard clustering
  ungroup() %>% 
  select(time, geo, which_max_u) %>% 
  right_join(eco_df) %>% 
  group_by(time, which_max_u) %>% 
  summarise(across(p80p20:rd_ppl, ~ mean(.x, na.rm = TRUE))) %>% 
  rename(clus = which_max_u) %>% 
  mutate_all(~ ifelse(is.nan(.), NA, .)) %>% 
  filter(!is.na(clus))

eco_soft_df <- u_df %>% 
  select(time, geo, starts_with("u")) %>% 
  pivot_longer(starts_with("u"), names_to = "clus", names_transform = parse_number, values_to = "u") %>% 
  right_join(eco_df) %>% 
  filter(!is.na(clus)) %>% 
  group_by(time, clus) %>% 
  summarise(across(p80p20:rd_ppl, ~ weighted.mean(., w = u, na.rm = TRUE))) %>% 
  mutate_all(~ ifelse(is.nan(.), NA, .)) %>% 
  ungroup()

save(eco_hard_df, eco_soft_df, file = "data/eco_clustering.RData")
  
  
