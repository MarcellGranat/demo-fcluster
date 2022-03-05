library(gganimate)

fitted_df <- fit_df %>% 
  filter(k == 3) %>% 
  transmute(
    time, id, data, cluster = map(fit, ~ data.frame(.$clus))
  ) %>% 
  unnest() %>% 
  janitor::clean_names() %>% 
  left_join(select(center_rescaled_df, time, cluster = clust, clust_2000)) 

animation1 <- fitted_df %>% 
  ggplot() + 
  geom_point(aes(tfr, ageing, color = as.factor(clust_2000), group = geo, alpha = membership_degree)) +
  scale_color_viridis_d() +
  labs(color = "Klaszter", alpha = "Hozzátartozási fok", 
       title = "{as.integer(frame_time)}") +
  scale_x_continuous(limits = c(0, 2.5)) +
  transition_time(time = time) +
  enter_fade() +
  exit_fade()

animation2 <- fitted_df %>% 
  ggplot() + 
  geom_point(aes(motherrate, emp, color = as.factor(clust_2000), group = geo, alpha = membership_degree)) +
  scale_color_viridis_d() +
  labs(color = "Klaszter", alpha = "Hozzátartozási fok", 
       title = "{as.integer(frame_time)}") +
  scale_x_continuous(limits = c(0.4, 0.56)) +
  transition_time(time = time) +
  enter_fade() +
  exit_fade()

a_gif <- animate(animation1,
                 fps = 10,
                 duration = 25,
                 width = 500, height = 400,
                 renderer = gifski_renderer("figures/animation1.gif"))

b_gif <- animate(animation2,
                 fps = 10,
                 duration = 25,
                 width = 500, height = 400,
                 renderer = gifski_renderer("figures/animation2.gif"))



library(magick)
a_mgif <- image_read(a_gif)
b_mgif <- image_read(b_gif)

new_gif <- image_append(c(a_mgif[1], b_mgif[1]), stack = FALSE)
for(i in 2:n_distinct(fitted_df$time)){
  combined <- image_append(c(a_mgif[i], b_mgif[i]), stack = FALSE)
  new_gif <- c(new_gif, combined)
}

tibble(time = unique(fitted_df$time)) %>% 
  mutate(
    a = list(image_read(a_gif)[row_number()]),
    b = list(image_read(b_gif)[row_number()]),
    new = future_map2(a, b, ~ image_append(c(.x, .y), stack = FALSE),
                      .progress = TRUE)
  ) %>% 
  pull(new) %>% 
  reduce(c) %>% 
  image_write_gif("figures/merged_animate.gif")
