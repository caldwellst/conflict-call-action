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
    .groups = "drop_last"
  ) %>%
  mutate(
    funding_pct = funding / sum(funding),
    funding_real = adjust_for_inflation(
      price = funding,
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
    caption = "Data from the UN OCHA Financial Tracking Service, https://fts.unocha.org"
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
    caption = "Data from the UN OCHA Financial Tracking Service, https://fts.unocha.org"
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
    .groups = "drop"
  ) %>%
  left_join(
    df_wpp, by = "iso3"
  ) %>%
  mutate(
    pin_pct = pin / affected_population,
    pin_pct_wpp = pin / population
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

df_pin_iso3 %>%
  ggplot(
    aes(
      x = conflict_events,
      y = pin_pct_wpp
    )
  ) +
  geom_point(
    color = hdx_hex("tomato-hdx"),
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
  )

ggsave(
  filename = file.path(
    "plots",
    "pin_iso3.png"
  ),
  width = 5.5,
  height = 5,
  units = "cm"
)

