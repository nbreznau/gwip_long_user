library('tidyverse')
library('sf')
library('ggplot2')
library('viridis')
library('rnaturalearth')
library('rnaturalearthdata')
library('countrycode')


df_gwip_long_ext <- read_csv("data/gwip_long_expanded.csv")

# the world map data are pretty crazy, the iso_a3 variable is all wrong, fix here
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  mutate(iso3c = countrycode(name_long, "country.name", "iso3c"))

# create categories for viridis color plotting
coverage_categories <- c(0, 0.001, 5, 10, 20, 40, 60, 80, 100)

df_map_data <- df_gwip_long_ext %>%
  filter(year %in% c(1910, 1955, 1980, 2020)) %>%
  mutate(
    coverage_cat = cut(labor_workinjury_coverage_pct_lf_ii, 
                       breaks = coverage_categories, 
                       labels = c("0", "<5%", "5-10%", "10-20%", "20-40%", "40-60%", "60-80%", "80-100%"),
                       include.lowest = TRUE),
  )


df_map_data <- df_map_data %>%
  mutate(iso3c = countrycode(cow_code, "cown", "iso3c"),
         iso3c = ifelse(cow_code == 6, "PRI",
                  ifelse(cow_code == 345, "SRB",
                         iso3c)))

world_coverage <- world %>%
  left_join(df_map_data, by = c("iso3c")) %>%
  select(coverage_cat, everything())

# Map for 1910
plot_map <- function(year, title, leg_pos) {
  ggplot(data = world_coverage %>% filter(year == year)) +
    geom_sf(aes(fill = coverage_cat), color = "white") +
    scale_fill_manual(
      values = c("0" = viridis::viridis(8)[8],
                 "<5%" = viridis::viridis(8)[7], 
                 "5-10%" = viridis::viridis(8)[6],
                 "10-20%" = viridis::viridis(8)[5],
                 "20-40%" = viridis::viridis(8)[4],
                 "40-60%" = viridis::viridis(8)[3],
                 "60-80%" = viridis::viridis(8)[2],
                 "80-100%" = viridis::viridis(8)[1]),
      na.value = "lightgrey",  # NA values light grey
      breaks = c("0", "<5%", "5-10%", "10-20%", "20-40%", "40-60%", "60-80%", "80-100%"),
      labels = c("none", "<5%", "5-10%", "10-20%", "20-40%", "40-60%", "60-80%", "80-100%")
    ) +
    labs(title = title) +
    theme_classic() +
    theme(
      legend.position = leg_pos,
      legend.title = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.line.x = element_blank(),
      axis.line.y = element_blank(),
      axis.ticks = element_blank()
    )
}

map_1910 <- plot_map(1910, "1910. Industrial Revolution", "none")
map_1955 <- plot_map(1955, "1955. After WWII", "none")
map_1980 <- plot_map(1980, "1980. Colonialism Decline", "none")
map_2020 <- plot_map(2020, "2020. Most Recent Data", "bottom")
