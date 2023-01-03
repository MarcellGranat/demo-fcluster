load("clustering_results.RData")

u_df |> 
  select(time, geo, u1, u2, u3) |> 
  group_by(time, geo) |> 
  pivot_longer(cols = - group_cols()) |> 
  slice_max(value, n = 1) |> 
  mutate(clust = str_remove(name, "\\D")) |> 
  filter(time == 2020) |> 
  plot_map(clust) +
  scale_fill_manual(values = c("#102929", "#69b8bc", "#bea100")) +
  theme(
    legend.position = "none", 
    panel.grid = element_line(color = "gray60", size = .4, linetype = 2),
    panel.background = element_rect(fill = "#d0ccc6", size = 0),
    plot.background = element_rect(fill = "#d0ccc6", size = 0)
  )

ggsave(filename = "sticker_map.png", height = 5, width = 6)

hexSticker::sticker(
  subplot = "sticker_map.png",
  package = "", 
  p_size=20, s_x=1, s_y=1, s_width=.75,
  h_fill = "#c5c1ba", h_color = "#102929",
  filename="logo.png"
)
