library(tidyverse)
library(factoextra)
library(sf)
library(furrr)
library(eurostat)

extractorRData <- function(file, object) {
  #' Function for extracting an object from a .RData file created by R's save() command
  #' Inputs: RData file, object name
  E <- new.env()
  load(file=file, envir=E)
  return(get(object, envir=E, inherits=F))
}

combinations <- function(n = 3) {
  # list all possible pairings of n numbers
  # >> looking for the pairing with the lowest distance
  if (n > 10) stop("This can lead high computation time...")
  
  out <- tibble(v1 = 1:n)
  
  for (i in 2:n) {
    out <- out %>% 
      crossing(1:n) 
    names(out)[i] <- str_c("v", i)
  }
  
  out %>% 
    rowwise() %>% 
    filter(n_distinct(c_across(where(is.numeric))) == n) %>% 
    ungroup() %>% 
    mutate(comb = row_number()) %>% 
    pivot_longer(starts_with("v"), names_to = "x", values_to = "y") %>% 
    mutate(x = parse_number(x)) %>% 
    select(comb, everything()) %>% 
    group_by(comb)
}

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
