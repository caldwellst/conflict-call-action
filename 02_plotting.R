library(tidyverse)
library(priceR)
library(gghdx)
library(countrycode)
library(sf)
gghdx()

df <- read_csv(
  file.path(
    "data",
    "df_joined.csv"
  )
)

sf_conflict <- read_sf(
  file.path(
    "data",
    "sf_conflict.gpkg"
  )
)

sf_inform <- read_sf(
  file.path(
    "data",
    "sf_inform.gpkg"
  )
)

sf_pin <- read_sf(
  file.path(
    "data",
    "sf_pin.gpkg"
  )
)

#########################
#### FINANCIAL FLOWS ####
#########################

df_fin_plot <- df %>%
  filter(
    between(year, 2015, 2022),
    !is.na(inform_class)
  ) %>%
  group_by(
    year, inform_class
  ) %>%
  summarize(
    funding = sum(funding),
    requirements = sum(requirements),
    .groups = "drop_last"
  ) %>%
  mutate(
    funding_pct = funding / sum(funding),
    requirements_pct = requirements / sum(requirements),
    funding_real = adjust_for_inflation(
      price = funding,
      from_date = as.Date(paste0(year, "-06-01")),
      country = "US",
      to_date = as.Date("2021-12-31")
    ),
    requirements_real = adjust_for_inflation(
      price = requirements,
      from_date = as.Date(paste0(year, "-06-01")),
      country = "US",
      to_date = as.Date("2021-12-31")
    )
  ) %>%
  ungroup()

# looking from 2015 based on INFORM Crisis type

df_fin_plot %>%
  ggplot() +
  geom_area(
    aes(
      x = year,
      y = funding_real,
      group = inform_class,
      fill = inform_class
    ),
    color = "white",
    linewidth = 0.3,
    alpha = 0.8
  )+
  scale_y_continuous(
    labels = scales::number_format(
      scale = 1 / 10^9,
      prefix = "$",
      suffix = "B")
  ) +
  labs(
    y = "Funding (2021 dollars)",
    x = "",
    title = "Humanitarian funding by crisis type, 2015 to 2022",
    fill = "",
    caption = paste0(
      "Data from the UN OCHA Financial Tracking Service, https://fts.unocha.org.\n",
      "Crisis classification from INFORM Severity, https://drmkc.jrc.ec.europa.eu/inform-index/INFORM-Severity."
    )
  ) +
  theme(
    legend.box.margin = margin(-15, 0, 0, 0)
  )

ggsave(
  filename = file.path(
    "plots",
    "funding_real.png"
  ),
  width = 6,
  height = 5,
  units = "cm"
)

# look at requirements instead of actual funding
df_fin_plot %>%
  ggplot() +
  geom_area(
    aes(
      x = year,
      y = requirements_real,
      group = inform_class,
      fill = inform_class
    ),
    color = "white",
    linewidth = 0.3,
    alpha = 0.8
  )+
  scale_y_continuous(
    labels = scales::number_format(
      scale = 1 / 10^9,
      prefix = "$",
      suffix = "B")
  ) +
  labs(
    y = "Requirements (2021 dollars)",
    x = "",
    title = "Humanitarian funding requirements by crisis type, 2015 to 2022",
    fill = "",
    caption = paste0(
      "Funding data from the UN OCHA Financial Tracking Service, https://fts.unocha.org.\n",
      "Crisis classification from INFORM Severity, https://drmkc.jrc.ec.europa.eu/inform-index/INFORM-Severity."
    )
  ) +
  theme(
    legend.box.margin = margin(-15, 0, 0, 0)
  )

ggsave(
  filename = file.path(
    "plots",
    "requirements_real.png"
  ),
  width = 6,
  height = 5,
  units = "cm"
)

# Looking as % of total contribution

df_fin_plot %>%
  ggplot() +
  geom_area(
    aes(
      x = year,
      y = funding_pct,
      group = inform_class,
      fill = inform_class
    ),
    color = "white",
    linewidth = 0.3,
    alpha = 0.8
  ) +
  scale_y_continuous(
    labels = scales::label_percent()
  ) +
  labs(
    y = "Funding (% of yearly total)",
    x = "",
    title = "Humanitarian funding by crisis type, 2015 to 2022",
    fill = "",
    caption = paste0(
      "Data from the UN OCHA Financial Tracking Service, https://fts.unocha.org.\n",
      "Crisis classification from INFORM Severity, https://drmkc.jrc.ec.europa.eu/inform-index/INFORM-Severity."
    )
  ) +
  theme(
    legend.box.margin = margin(-15, 0, 0, 0)
  )

ggsave(
  filename = file.path(
    "plots",
    "funding_pct.png"
  ),
  width = 6,
  height = 5,
  units = "cm"
)
# Looking at requirements instead of actual funding

df_fin_plot %>%
  ggplot() +
  geom_area(
    aes(
      x = year,
      y = requirements_pct,
      group = inform_class,
      fill = inform_class
    ),
    color = "white",
    linewidth = 0.3,
    alpha = 0.8
  ) +
  scale_y_continuous(
    labels = scales::label_percent()
  ) +
  labs(
    y = "Required funding (% of yearly total)",
    x = "",
    title = "Humanitarian funding requirements by crisis type, 2015 to 2022",
    fill = "",
    caption = paste0(
      "Data from the UN OCHA Financial Tracking Service, https://fts.unocha.org.\n",
      "Crisis classification from INFORM Severity, https://drmkc.jrc.ec.europa.eu/inform-index/INFORM-Severity."
    )
  ) +
  theme(
    legend.box.margin = margin(-15, 0, 0, 0)
  )

ggsave(
  filename = file.path(
    "plots",
    "requirements_pct.png"
  ),
  width = 6,
  height = 5,
  units = "cm"
)

###################
#### WORLD MAP ####
###################

bbox <- st_bbox(sf_conflict)

sf_inform %>%
  mutate(
    inform_bin = cut(
      x = inform_severity_2021,
      breaks = 1:5,
      include.lowest = TRUE,
      labels = paste(1:4, 2:5, sep = " - "),
      ordered_results = TRUE
    )
  ) %>%
  ggplot() +
  geom_sf(
    aes(
      fill = inform_bin
    ),
    linewidth = 0.1
  ) +
  geom_sf(
    data = sf_conflict,
    fill = NA,
    alpha = 0.05,
    shape = 1,
    color = "black",
    size = 0.2
  ) +
  scale_fill_hdx_tomato(
    na.translate = FALSE
  ) +
  coord_sf(
    xlim = bbox[c(1,3)],
    ylim = bbox[c(2,4)],
    datum = NA
  ) +
  labs(
    fill = "INFORM Severity\nIndex",
    title = "Conflict incidents and humanitarian severity",
    subtitle = "Conflict (2021) and INFORM severity (January 2022) measured at country-level",
    caption = paste(
      "Data on conflict sourced from the UCDP Dataset Download Center, https://ucdp.uu.se/downloads/index.html.\n",
      "Data on INFORM Severity sourced from the INFORM site, https://drmkc.jrc.ec.europa.eu/inform-index/INFORM-Severity.",
      collapse = ""
    )
  ) +
  theme(
    plot.margin = grid::unit(c(0,0,0,0), "mm"),
    legend.text = element_text(size = 12),
    legend.title = element_text(lineheight = 0.3, vjust = 0.5, hjust = 1, size = 12, face = "bold"),
    plot.caption = element_text(lineheight = 0.5, hjust = 1, size = 8),
    legend.key.size = unit(0.2, "cm"),
    legend.key = element_rect(
      linewidth = 1
    )
  )

ggsave(
  filename = file.path(
    "plots",
    "inform_map.png"
  ),
  width = 6,
  height = 5,
  units = "cm"
)

###############################
#### DISPLACEMENT SEVERITY ####
###############################

sf_disp <- sf_inform %>%
  mutate(
    inform_disp_bins = cut(
      x = inform_displacement,
      breaks = 0:5,
      include.lowest = TRUE,
      labels = paste(0:4, 1:5, sep = " - "),
      ordered_result = TRUE
    )
  ) 

sf_disp %>%
  filter(
    !is.na(inform_displacement)
  ) %>%
  ggplot() +
  geom_point(
    aes(
      x = conflict_events,
      y = inform_severity,
      color = inform_disp_bins
    ),
    size = 0.3
  ) +
  scale_x_log10() +
  coord_cartesian(
    clip = "off"
  ) +
  scale_color_manual(
    values = c(
      "0 - 1" = unname(hdx_hex("tomato-ultra-light")),
      "1 - 2" = "#F5AEA9",
      "2 - 3" = unname(hdx_hex("tomato-light")),
      "3 - 4" = unname(hdx_hex("tomato-hdx")),
      "4 - 5" = unname(hdx_hex("tomato-dark"))
    )
  ) +
  labs(
    x = "# of conflict events",
    y = "INFORM Severity Index",
    color = "INFORM Displacement\nScore",
    title = "Crisis severity, conflict, and displacement",
    subtitle = "Conflict (2021) and INFORM severity (January 2023) measured at country-level",
    caption = paste(
      "Data on conflict sourced from the UCDP Dataset Download Center, https://ucdp.uu.se/downloads/index.html.\n",
      "Data on severity and displacement sourced from the INFORM site, https://drmkc.jrc.ec.europa.eu/inform-index/INFORM-Severity.",
      collapse = ""
    )
  ) +
  theme(
    legend.key.width = unit(0.01, "cm"),
    legend.title = element_text(lineheight = 0.3, vjust = 0.5, hjust = 1, size = 12, face = "bold"),
    plot.caption = element_text(lineheight = 0.5, hjust = 1, size = 8),
    legend.margin = margin(-10, 0, 0, 0)
  )

ggsave(
  filename = file.path(
    "plots",
    "inform_conf_disp.png"
  ),
  width = 5.5,
  height = 5,
  units = "cm"
)

##########################
#### PIN AND CONFLICT ####
##########################
# bring in the WPP population data to recalculate pin pct
df_wpp <- read_csv(
  file.path(
    "data",
    "wpp.csv"
  )
)

# use area to normalize events
sf_use_s2(FALSE)
sf_pin$area_km2 <- as.numeric(st_area(sf_pin) * 1e-6)

df_pin_iso3 <- sf_pin %>%
  mutate(
    iso3 = countrycode::countrycode(
      str_extract(
        admin_code,
        "^[A-Z]{2}"
      ),
      origin = "iso2c",
      destination = "iso3c"
    )
  ) %>%
  as_tibble() %>%
  group_by(iso3) %>%
  summarize(
    pin = sum(pin),
    conflict_events = sum(conflict_events),
    affected_population = sum(affected_population),
    best = sum(best),
    deaths_civilians = sum(deaths_civilians),
    area_km2 = sum(area_km2),
    .groups = "drop"
  ) %>%
  left_join(
    df_wpp, by = "iso3"
  ) %>%
  mutate(
    pin_pct = pin / affected_population,
    pin_pct_wpp = pin / population,
    pin_pct_best = ifelse(
      iso3 %in% c("NGA", "MOZ"),
      pin_pct, # only subnational crises in our data
      pin_pct_wpp # otherwise use this pct
    )
  )

# admin 1 plot

sf_pin %>%
  ggplot() +
  geom_point(
    aes(
      x = conflict_events,
      y = pin_pct
    ),
    color = hdx_hex("tomato-hdx"),
    alpha = 0.3,
    shape = 16,
    size = 0.8
  ) +
  scale_x_log10() +
  scale_y_continuous(
    labels = scales::label_percent()
  ) +
  labs(
    x = "# of conflict events",
    y = "People in need (% of population)",
    title = "Relationship between conflict and humanitarian need",
    subtitle = "Conflict and need measured at admin 1 level in 14 countries",
    caption = paste(
      "Data on conflict sourced from the UCDP Dataset Download Center, https://ucdp.uu.se/downloads/index.html.\n",
      "Data on humanitarian need sourced from private UN OCHA data, with some public data on https://data.humdata.org.",
      collapse = ""
    )
  ) +
  coord_cartesian(
    clip = "off"
  ) +
  theme(
    plot.caption = element_text(lineheight = 0.5, hjust = 1, size = 8, margin = margin(5, 0, 0, 0)),
  )

ggsave(
  filename = file.path(
    "plots",
    "pin_adm1.png"
  ),
  width = 5.5,
  height = 5,
  units = "cm"
)

# normalize by area

sf_pin %>%
  ggplot() +
  geom_point(
    aes(
      x = conflict_events / area_km2,
      y = pin_pct
    ),
    color = hdx_hex("tomato-hdx"),
    alpha = 0.3,
    shape = 16,
    size = 0.8
  ) +
  scale_x_log10() +
  scale_y_continuous(
    labels = scales::label_percent()
  ) +
  labs(
    x = "Conflict events per sq. km.",
    y = "People in need (% of population)",
    title = "Relationship between conflict and humanitarian need",
    subtitle = "Conflict and need measured at admin 1 level in 14 countries",
    caption = paste(
      "Data on conflict sourced from the UCDP Dataset Download Center, https://ucdp.uu.se/downloads/index.html.\n",
      "Data on humanitarian need sourced from private UN OCHA data, with some public data on https://data.humdata.org.",
      collapse = ""
    )
  ) +
  coord_cartesian(
    clip = "off"
  ) +
  theme(
    plot.caption = element_text(lineheight = 0.5, hjust = 1, size = 8, margin = margin(5, 0, 0, 0)),
  )

ggsave(
  filename = file.path(
    "plots",
    "pin_adm1_sqkm.png"
  ),
  width = 5.5,
  height = 5,
  units = "cm"
)

# look at country level normalized by square kilometers

df_pin_iso3 %>%
  ggplot(
    aes(
      x = conflict_events / area_km2,
      y = pin_pct_best
    )
  ) +
  geom_point(
    aes(
      color = log10(best)
    ),
    size = 0.5
  ) +
  geom_text_hdx(
    aes(
      label = iso3
    ),
    nudge_y = 0.05
  ) + 
  scale_x_log10() +
  scale_y_continuous(
    labels = scales::label_percent()
  ) +
  labs(
    x = "Conflict events per sq. km.",
    y = "People in need (% of population)",
    title = "Relationship between conflict and humanitarian need",
    color = expression(log[10]*" of fatalities"),
    subtitle = "Conflict and need measured at the country level",
    caption = paste(
      "Data on conflict sourced from the UCDP Dataset Download Center, https://ucdp.uu.se/downloads/index.html.\n",
      "Data on humanitarian need sourced from private UN OCHA data, with some public data on https://data.humdata.org.",
      collapse = ""
    )
  ) +
  coord_cartesian(
    clip = "off"
  )  +
  theme(
    plot.caption = element_text(lineheight = 0.5, hjust = 1, size = 8, margin = margin(5, 0, 0, 0)),
    legend.key.size = unit(0.2, "cm"),
    legend.key = element_rect(
      linewidth = 1
    ),
    legend.box.margin = margin(-11, 0, -6, 0)
  ) +
  scale_color_gradient(
    low = "white",
    high = hdx_hex("tomato-hdx"),
    limits = c(0, 5)
  )

ggsave(
  filename = file.path(
    "plots",
    "pin_iso3_sqkm.png"
  ),
  width = 5.5,
  height = 5.5,
  units = "cm"
)

# look at country level

df_pin_iso3 %>%
  ggplot(
    aes(
      x = conflict_events,
      y = pin_pct_best
    )
  ) +
  geom_point(
    aes(
      color = log10(best)
    ),
    size = 0.5
  ) +
  geom_text_hdx(
    aes(
      label = iso3
    ),
    nudge_y = 0.05
  ) + 
  scale_x_log10() +
  scale_y_continuous(
    labels = scales::label_percent()
  ) +
  labs(
    x = "# of conflict events",
    y = "People in need (% of population)",
    color = expression(log[10]*" of fatalities"),
    title = "Relationship between conflict and humanitarian need",
    subtitle = "Conflict and need measured at the country level",
    caption = paste(
      "Data on conflict sourced from the UCDP Dataset Download Center, https://ucdp.uu.se/downloads/index.html.\n",
      "Data on humanitarian need sourced from private UN OCHA data, with some public data on https://data.humdata.org.",
      collapse = ""
    )
  ) +
  coord_cartesian(
    clip = "off"
  )  +
  theme(
    plot.caption = element_text(lineheight = 0.5, hjust = 1, size = 8, margin = margin(5, 0, 0, 0)),
    legend.key.size = unit(0.2, "cm"),
    legend.key = element_rect(
      linewidth = 1
    ),
    legend.box.margin = margin(-11, 0, -6, 0)
  ) +
  scale_color_gradient(
    low = "white",
    high = hdx_hex("tomato-hdx"),
    limits = c(0, 5)
  )

ggsave(
  filename = file.path(
    "plots",
    "pin_iso3.png"
  ),
  width = 5.5,
  height = 5.5,
  units = "cm"
)
