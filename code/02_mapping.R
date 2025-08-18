library('tidyverse')
library('sf')
library('ggplot2')
library('viridis')
library('rnaturalearth')
library('rnaturalearthdata')
library('countrycode')
library('ggpubr')
library('ragg')
library('magick')


df_gwip_long_ext <- read_csv("data/gwip_long_expanded.csv")

# Create categories for replacement rates
df_gwip_long_ext <- df_gwip_long_ext %>%
  mutate(rep_numeric = as.numeric(labor_workinjury_replacement_rate_temp),
         rep_rate_plot = case_when(labor_workinjury_replacement_rate_temp == "none" ~ 0,
                                   labor_workinjury_replacement_rate_temp == "-99" ~ 0,
                                   labor_workinjury_replacement_rate_temp == "0" ~ 1,
                                   rep_numeric < 50 ~ 2,
                                   rep_numeric > 49 & rep_numeric < 67 ~ 3,
                                   rep_numeric > 66.999 & rep_numeric < 76 ~ 4,
                                   rep_numeric > 75.999 & rep_numeric < 86 ~ 5,
                                   rep_numeric > 85.999 ~ 6))

labels <- c("No policy or missing",
            "Auxiliary benefits only",
            "50% or less",
            "< 67%",
            "< 76%",
            "< 86%",
            "86 to 100%+")

df_gwip_long_ext <- df_gwip_long_ext %>%
  mutate(rep_rate_plot = factor(rep_rate_plot,
                                levels = 0:6,
                                labels = labels,
                                ordered = TRUE))

### Setup map data
# the world map data are pretty crazy, the iso_a3 variable is all wrong, fix here
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  mutate(iso3c = countrycode(name_sort, "country.name", "iso3c"),
         iso3c = ifelse(sovereignt == "FR1", "FRA", iso3c)) %>%
         select(iso3c, geometry)

# French Guinea, Kosovo and Bhutan are all missing geometrically from the map

# create categories for viridis color plotting
coverage_categories <- c(0, 0.001, 5, 10, 20, 40, 60, 80, 100)

df_map_data <- df_gwip_long_ext %>%
  filter(year %in% c(1910, 1955, 1980, 2020)) %>%
  mutate(
    labor_workinjury_coverage_pct_lf_ii0 = ifelse(is.na(labor_workinjury_coverage_pct_lf_ii), 0, labor_workinjury_coverage_pct_lf_ii),
    coverage_cat = cut(labor_workinjury_coverage_pct_lf_ii0, 
                       breaks = coverage_categories, 
                       labels = c("0", "<5%", "5-10%", "10-20%", "20-40%", "40-60%", "60-80%", "80-100%"),
                       include.lowest = TRUE),
  )

# subroutine adds in a few missing countries
source("code/sub/subroutine_01.R")

df_map_data <- df_map_data %>%
  mutate(iso3c = countrycode(cow_code, "cown", "iso3c"),
         iso3c = ifelse(cow_code == 6, "PRI",
                 ifelse(cow_code == 345, "SRB",
                 ifelse(cow_code == 347, "XKX",
                               iso3c))))

world_coverage <- world %>%
  full_join(df_map_data, by = "iso3c") %>%
  mutate(rep_rate_plot, ifelse(is.na(rep_rate_plot), -99, rep_rate_plot)) %>%
  select(coverage_cat, rep_rate_plot, everything())

#### Fig 1 Coverage

# Plotting function coverage
plot_map <- function(yearin, title, leg_pos) {
  ggplot(data = world_coverage %>% filter(year == yearin)) +
    geom_sf(aes(fill = coverage_cat), color = NA) +
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
      labels = c("none", "<5%", "5-10%", "10-20%", "20-40%", "40-60%", "60-80%", "80-100%"),
      guide = guide_legend(nrow = 1, byrow = T)) +
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
map_1980 <- plot_map(1980, "1980. Colonialism Decline", "bottom")
map_2020 <- plot_map(2020, "2020. Most Recent Data", "none")

# Save as high def plot
# the ggarrange legend comes from the first plot which does not have all categories

# Combine 
fig1 <- ggarrange(
  map_1910, map_1955,
  map_1980, map_2020,
  ncol = 2, nrow = 2,
  common.legend = T,
  legend = "none"
)

# Plot Fig1
agg_png("out/Fig1.png",
        width = 4000, height = 2000, res = 300)
print(fig1)
dev.off()

# Plot Figure to cutout legend
agg_png("out/Fig1_leg.png",
        width = 2000, height = 1000, res = 300)
print(map_1980)
dev.off()

# Cutout legend and save
img <- image_read("out/Fig1_leg.png")
img_crop <- image_crop(img, geometry = "2500x200+0+800")
image_write(img_crop, path = "out/Fig1_leg_crop.png")

#### Fig 2 Replacement Rates

# Plotting function coverage
plot_map2 <- function(yearin, title, leg_pos) {
  ggplot(data = world_coverage %>% filter(year == yearin)) +
    geom_sf(aes(fill = rep_rate_plot), color = NA) +
    scale_fill_manual(
      values = c("No policy or missing" = viridis::viridis(7)[7],
                 "Auxiliary benefits only" = viridis::viridis(7)[6], 
                 "50% or less" = viridis::viridis(7)[5],
                 "< 67%" = viridis::viridis(7)[4],
                 "< 76%" = viridis::viridis(7)[3],
                 "< 86%" = viridis::viridis(7)[2],
                 "60-80%" = viridis::viridis(8)[2],
                 "86 to 100%+" = viridis::viridis(8)[1])
      #,breaks = c("0", "<5%", "5-10%", "10-20%", "20-40%", "40-60%", "60-80%", "80-100%"),
      #labels = c("none", "<5%", "5-10%", "10-20%", "20-40%", "40-60%", "60-80%", "80-100%"),
      #guide = guide_legend(nrow = 1, byrow = T)
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

map_19102 <- plot_map2(1910, "1910. Industrial Revolution", "none")
map_19552 <- plot_map2(1955, "1955. After WWII", "none")
map_19802 <- plot_map2(1980, "1980. Colonialism Decline", "bottom")
map_20202 <- plot_map2(2020, "2020. Most Recent Data", "none")

# Save as high def plot
# the ggarrange legend comes from the first plot which does not have all categories

# Combine 
fig2 <- ggarrange(
  map_19102, map_19552,
  map_19802, map_20202,
  ncol = 2, nrow = 2,
  common.legend = T,
  legend = "none"
)

# Plot Fig2
agg_png("out/Fig2.png",
        width = 4000, height = 2000, res = 300)
print(fig2)
dev.off()

# Plot Figure to cutout legend
agg_png("out/Fig2_leg.png",
        width = 2000, height = 1000, res = 300)
print(map_19802)
dev.off()

# Cutout legend and save
img <- image_read("out/Fig2_leg.png")
img_crop <- image_crop(img, geometry = "2500x250+0+750")
image_write(img_crop, path = "out/Fig2_leg_crop.png")


         