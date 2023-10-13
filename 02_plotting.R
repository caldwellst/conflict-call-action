library(tidyverse)
library(gghdx)
library(priceR)
library(patchwork)
gghdx()

######################
#### READ IN DATA ####
######################

df <- read_csv(
  file.path(
    "data",
    "df_combined.csv"
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

p_fin_plot <- df_fin_plot %>%
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
      suffix = "B"),
    expand = c(0, NA)
  ) +
  labs(
    y = "Funding (2021 dollars)",
    x = "",
    title = bquote(bold("Humanitarian funding, 2021 dollars, 2015 to 2022")),
    fill = ""
  ) +
  theme(
    legend.box.margin = margin(-15, 0, 0, 0),
    plot.title = element_text(size = 30),
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 24),
    legend.text = element_text(size = 20)
  )

######################################
#### CONFLICT AND CRISIS SEVERITY ####
######################################

df_conflict_sev <- df %>%
  filter(
    year >= 2013
  ) %>%
  group_by(
    iso3
  ) %>%
  summarize(
    fatalities = sum(fatalities, na.rm = TRUE),
    fatalities_log10 = log10(fatalities),
    inform_severity = unique(inform_severity),
    hrp_rrp = unique(hrp_rrp)
  ) %>%
  filter(
    !is.na(inform_severity)
  )

p_conflict_sev <- df_conflict_sev %>%
  ggplot(
    aes(
      x = fatalities,
      y = inform_severity,
      color = hrp_rrp
    )
  ) +
  geom_point(
    position = position_jitter(
      height = 0.02,
      seed = 123
    )
  ) +
  scale_color_manual(
    values = hdx_hex(c("grey-medium", "sapphire-hdx")),
    guide = "none"
  ) +
  geom_text_hdx(
    x = 2.3,
    y = 4.4,
    label = "Countries with a 2023 humanitarian\nor regional response plan",
    color = hdx_hex("sapphire-hdx"),
    fontface = "bold",
    check_overlap = TRUE,
    hjust = 1,
    size = 7,
    lineheight = 0.5
  ) +
  geom_curve(
    x = 2.3,
    xend = 2.6,
    y = 4.4,
    yend = 4.1,
    curvature = -0.3,
    arrow = arrow(length = unit(0.1, "inches")),
    color = hdx_hex("sapphire-hdx")
  ) +
  coord_cartesian(
    clip = "off"
  ) +
  scale_x_log10(
    labels = scales::label_comma()
  ) +
  labs(
    x = "Conflict fatalities, 2013 - 2022",
    y = "INFORM Severity",
    color = "",
    title = bquote(bold("Severity of humanitarian conditions and intensity of conflict"))
  ) +
  theme(
    plot.title = element_text(size = 30),
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 24),
    legend.text = element_text(size = 20)
  )

########################
#### PATCHWORK PLOT ####
########################

p_combined <- p_fin_plot + p_conflict_sev

ggsave(
  filename = file.path(
    "plots",
    "combined_plot.png"
  ),
  plot = p_combined,
  width = 9,
  height = 4,
  units = "in",
  device = ragg::agg_png
)
