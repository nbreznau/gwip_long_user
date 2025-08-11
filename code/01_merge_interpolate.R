library('tidyverse')
library('countrycode')
library('zoo')
library('readxl')

# Data deposted here, but downloaded from 
# https://doi.org/10.7910/DVN/IVKYIE
# Check for updates!

df_gwip_long <- read_csv("data/gwip_long_v1.0.csv")

# Merge in GDP per capita from the Maddison data https://www.rug.nl/ggdc/historicaldevelopment/maddison/releases/maddison-project-database-2023
# This will be useful for imputing values in between measurements of coverage

df_gdp <- read_xlsx("data/mpd2023_web.xlsx", sheet = 'Full data') %>%
  filter(year > 1879)

# get cow codes for merging

df_gdp <- df_gdp %>%
  mutate(cow_code = countrycode(countrycode, "iso3c", "cown"),
         cow_code = ifelse(countrycode == "PRI", 6,
                    ifelse(countrycode == "SRB", 345, cow_code))) %>%
  select(cow_code, year, gdppc, pop)

# Expand to include all years since 1880

# Generate all years
year_range <- data.frame(year = 1880:2020)

# Note that Germany and Austria have laws that we found prior to 1880, recode these to 1880 for purposes of interpolating data
df_gwip_long <- df_gwip_long %>%
  mutate(year = ifelse(year < 1880, 1880, year))


# Expand
df_gwip_long_ext <- df_gwip_long %>%
  right_join(year_range, by = "year") %>%
  group_by(country) %>%
  complete(year = 1880:2020) %>%
  ungroup()

# Strangely it creates an 'NA' country here
df_gwip_long_ext <- df_gwip_long_ext %>%
  filter(!is.na(country))


# Fill in so that no values for 1880 for each country are coded to zero
# They remain zero until reaching a real value and then switch to that value
# Then the new value is filled in chronologically until reaching a new value and so forth

df_gwip_long_ext <- df_gwip_long_ext %>%
  group_by(country) %>%
  mutate(
    cow_code = max(cow_code, na.rm = T),
    labor_workinjury_coverage_pct_lf = na.locf(labor_workinjury_coverage_pct_lf, na.rm = FALSE),
    labor_workinjury_replacement_rate_perm = na.locf(labor_workinjury_replacement_rate_perm, na.rm = FALSE),
    labor_workinjury_replacement_rate_temp = na.locf(labor_workinjury_replacement_rate_temp, na.rm = FALSE)
  ) %>%
  # now fill in leading NAs in the time series (that occur before the first real value in the GWIP)
  mutate(
    labor_workinjury_coverage_pct_lf = ifelse(is.na(labor_workinjury_coverage_pct_lf), 0, labor_workinjury_coverage_pct_lf),
    labor_workinjury_replacement_rate_perm = ifelse(is.na(labor_workinjury_replacement_rate_perm), 0, labor_workinjury_replacement_rate_perm),
    labor_workinjury_replacement_rate_temp = ifelse(is.na(labor_workinjury_replacement_rate_temp), 0, labor_workinjury_replacement_rate_temp)
  )

# Merge in Maddison data

df_gwip_long_ext <- df_gwip_long_ext %>%
  left_join(df_gdp, by = c("cow_code", "year"))

# Linearly interpolate NA in the gdppc variable after the first valid observation

df_gwip_long_ext <- df_gwip_long_ext %>%
  mutate(
    # Only apply interpolation if at least one non-NA
    gdppc = if (any(!is.na(gdppc))) {
      zoo::na.approx(gdppc, x = year, rule = 2)
    } else {
      gdppc  
    }
  ) %>%
  ungroup()

# Regression impute missing coverage rates

df_gwip_long_ext <- df_gwip_long_ext %>%
  mutate(labor_workinjury_coverage_pct_lf_i = ifelse(is.na(labor_workinjury_coverage_pct_lf), 0,
                                              ifelse(labor_workinjury_coverage_pct_lf == -99, NA, 
                                                     labor_workinjury_coverage_pct_lf)))

m1 <- lm(labor_workinjury_coverage_pct_lf_i ~ country*gdppc + country*year, data = df_gwip_long_ext)

# Many tiny countries have no gdp data in Maddison. As they will not appear on the map anyways they can be dropped
df_gwip_long_ext_filtered <- df_gwip_long_ext %>%
  filter(country %in% unique(m1$model$country))


df_gwip_long_ext_filtered$labor_workinjury_coverage_pct_lf_i_pred = predict(m1, newdata = df_gwip_long_ext_filtered)

# clean up and merge in to fill NAs

df_gwip_long_ext_filtered <- df_gwip_long_ext_filtered %>%
  # recode negative coverage predictions to zero
  mutate(labor_workinjury_coverage_pct_lf_i_pred = ifelse(labor_workinjury_coverage_pct_lf_i_pred < 0,
                                                          0, labor_workinjury_coverage_pct_lf_i_pred),
         labor_workinjury_coverage_pct_lf_ii = ifelse(!is.na(labor_workinjury_coverage_pct_lf_i), labor_workinjury_coverage_pct_lf_i,
                                                      labor_workinjury_coverage_pct_lf_i_pred))

write.csv(df_gwip_long_ext_filtered, "data/gwip_long_expanded.csv", row.names = F)
