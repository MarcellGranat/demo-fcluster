library(tidyverse)
library(factoextra)
library(sf)
library(eurostat)
library(pins)
library(granatlib)

NiceName <- function(x, wrap = NULL) {
  out <- case_when(
    x == "tfr" ~ "Total fertility rate",
    x == "lifexp" ~ "Life expectancy",
    x == "ageing" ~ "Ageing index",
    x == "motherrate" ~ "% of women at childbearing age",
    x == "emp" ~ "Employment rate of women at chlidbearing age",
    x == "p80p20" ~ "Income quintile share ratio S80/S20",
    x == "income" ~ "Income of households",
    x == "edu" ~ "Students aged 17 as % of population",
    x == "rd_ppl" ~ "R&D personnel and researchers",
    TRUE ~ x
  )
  if (!is.null(wrap)) {
    out <- str_wrap(out, wrap)
  }
  out
}

theme_set(
  theme_minimal() + 
    theme(
      legend.position = "bottom"
    )
)

suppressMessages({
  tryCatch({
    od <- Microsoft365R::get_business_onedrive(tenant = "common")
    
    board <- board_ms365(
      drive = od, 
      path = "demo-fcluster"
    )
  })
})

plot_map <- function(.data, .var) {
  right_join(.data, euro_map, by = "geo") |> 
    ggplot() +
    aes(geometry = geometry, fill ={{ .var }} , alpha = is.na({{ .var }})) + 
    geom_sf(color = "black", size = .2) + 
    labs(fill = "Cluster") + 
    scale_alpha_manual(values = c(1, .3), guide = guide_none()) +
    scale_x_continuous(limits = c(-10, 33)) +
    scale_y_continuous(limits = c(35, 65)) + 
    theme(
      axis.text = element_blank()
    )
}

