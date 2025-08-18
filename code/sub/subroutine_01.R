

# add in missing countries as NA values
df_map_data_fill <- subset(df_map_data, cow_code == 700)

# Djibouti
df_map_data_522 <- df_map_data_fill %>%
  mutate(cow_code = 522,
         labor_workinjury_coverage_pct_lf = NA,
         labor_workinjury_replacement_rate_perm = 0,
         labor_workinjury_replacement_rate_temp = 0)
# Eritrea
df_map_data_531 <- df_map_data_fill %>%
  mutate(cow_code = 531,
         labor_workinjury_coverage_pct_lf = NA,
         labor_workinjury_replacement_rate_perm = 0,
         labor_workinjury_replacement_rate_temp = 0)

# Suriname
df_map_data_115 <- df_map_data_fill %>%
  mutate(cow_code = 115,
         labor_workinjury_coverage_pct_lf = NA,
         labor_workinjury_replacement_rate_perm = 0,
         labor_workinjury_replacement_rate_temp = 0)

# Guyana
df_map_data_110 <- df_map_data_fill %>%
  mutate(cow_code = 110,
         labor_workinjury_coverage_pct_lf = NA,
         labor_workinjury_replacement_rate_perm = 0,
         labor_workinjury_replacement_rate_temp = 0)



df_map_data <- bind_rows(df_map_data, df_map_data_522, 
                         df_map_data_531,
                         df_map_data_115,
                         df_map_data_110)

rm(sf_guy, sf_btn, sf_kos, sf_manual)
