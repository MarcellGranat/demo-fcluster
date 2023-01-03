df_tfr <- get_eurostat("demo_r_find2", time_format = "num") %>% 
  filter(indic_de == "TOTFERRT") %>% 
  select(time, geo, tfr = values)

df_lifexp <- get_eurostat("demo_r_mlifexp", time_format = "num") %>% 
  filter(sex == "T" & age == "Y_LT1") %>% 
  select(time, geo, lifexp = values)

df_ageing <- get_eurostat("demo_r_d2jan", time_format = "num") %>% 
  filter(age != "TOTAL" & age != "UNK" & sex == "T") %>% 
  mutate(
    age = ifelse(age == "Y_LT1", 0, age),
    age = ifelse(age == "Y_OPEN", 100, age),
    age = str_remove_all(age, "Y"),
    age = as.numeric(age),
    young = age <= 14
  ) %>% 
  filter(age <= 14 | age >= 65) %>% 
  group_by(time, geo, young) %>% 
  summarise(values = sum(values)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = young, values_from = values, names_prefix = "young") %>% 
  transmute(time, geo, ageing = youngFALSE / youngTRUE)

df_motherrate <- get_eurostat("demo_r_d2jan", time_format = "num") %>% 
  filter(sex == "F" & age %in% c("TOTAL", str_c("Y", 15:49))) %>% 
  mutate(t = age == "TOTAL") %>% 
  group_by(time, geo, t) %>% 
  summarise(values = sum(values)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = t, values_from = values, names_prefix = "t") %>% 
  transmute(time, geo, motherrate = tFALSE / tTRUE)

df_emp <- get_eurostat("lfst_r_lfe2emprtn", time_format = "num") %>% 
  filter(isced11 == "TOTAL" & citizen == "TOTAL" & age == "Y15-64" & sex == "F") %>% 
  select(time, geo, emp = values)

demog_df <- list(df_tfr, df_lifexp, df_ageing, df_motherrate, df_emp) %>% 
  reduce(full_join) %>% 
  filter(str_length(geo) == 4 & geo != "HUXX") %>% 
  mutate(country = str_sub(str_sub(geo, end = 2))) %>% 
  filter(country %in% eu_countries$code) %>% 
  filter(time <= 2020) |> # last obs at time of writing
  select(time, country, geo, everything())

board |> 
  pin_write(demog_df, 
            name = "demog_df", 
            description = "Demographic variables on NUTS-2 level included in the clustering. Source: Eurostat."
            )

p80p20 <- eurostat::get_eurostat("ilc_di11_r", time_format = "num") %>% 
  # Income quintile share ratio S80/S20 by NUTS 2 regions - EU-SILC survey
  select(time, geo, p80p20 = values)

income <- eurostat::get_eurostat("nama_10r_2hhinc", time_format = "num") %>%   
  # Income of households by NUTS 2 regions
  filter(unit == "EUR_HAB", direct == "BAL", na_item == "B6N") %>% 
  select(time, geo, income = values)


edu <- eurostat::get_eurostat("educ_regind", time_format = "num") %>% 
  # Education indicators by NUTS 2 regions
  filter(indic_ed == "R04_3") %>%  
  # Students (all ISCED levels) aged 17 at regional level - as % of corresponding age population
  select(time, geo, edu = values)

rd_ppl <- eurostat::get_eurostat("rd_p_persreg", time_format = "num") %>% 
  # R&D personnel and researchers by sector of performance, sex and NUTS 2 regions
  filter(prof_pos == "TOTAL", sex == "T", sectperf == "TOTAL", unit == "PC_EMP_HC") %>% 
  select(time, geo, rd_ppl = values)

eco_df <- list(p80p20, income, edu, rd_ppl) %>% 
  reduce(full_join) %>% 
  filter(str_length(geo) == 4 & geo != "HUXX") %>% 
  mutate(country = str_sub(str_sub(geo, end = 2))) %>% 
  filter(country %in% eu_countries$code) %>% 
  filter(time <= 2020) |> # last obs at time of writing
  select(time, country, geo, everything())

board |> 
  pin_write(
    eco_df,
    name = "eco_df", 
    description = "Socio-economic variables. Source: Eurostat."
  )

euro_map <- get_eurostat_geospatial(nuts_level = "2", year = "2016")

board |> 
  pin_write(
    euro_map,
    name = "euro_map",
    description = "Shape file for NUT-2 regions. Source: Eurostat."
  )