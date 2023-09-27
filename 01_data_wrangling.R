library(tidyverse)
library(countrycode)
library(rhdx)
library(readxl)
library(sf)

##########################
#### FTS FUNDING DATA ####
##########################

#' Scrape data from HDX
resource_extractor <- function(dataset, query) {
  suppressMessages(
    get_resources(dataset, pattern = query) %>%
      pluck(1) %>%
      read_resource()
  )
}

#' Don't generate errors if resource unavailable
poss_extractor <- possibly(.f = resource_extractor, otherwise = NULL)

# getting the full country-level funding data
# requires some wrangling to bind rows and drop duplicates
df_fts <- map(
  .x = search_datasets(
    query = "Requirements and Funding Data",
    rows = 500L
  ),
  .f = \(x) {
    res <- poss_extractor(x, query = "fts_requirements_funding_[a-z]{3}.csv")
    if (!is.null(res)) {
      if ("typeName" %in% names(res)) {
        res$typeName <- as.character(res$typeName)
      }
    }
    res
  }
) %>%
  list_rbind() %>%
  distinct() %>%
  group_by(
    iso3 = countryCode,
    year
  ) %>%
  summarize(
    requirements = sum(requirements, na.rm = TRUE),
    funding = sum(funding, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  complete(
    iso3,
    year = 2000:2022,
    fill = list(
      requirements = 0,
      funding = 0
    )
  )

###################
#### UCDP DATA ####
###################

download.file(
  url = "https://ucdp.uu.se/downloads/ged/ged231-rds.zip",
  destfile = f <- tempfile()
)

# load the data into the environment
GEDEvent_v23_1 <- unz(f, filename = "GEDEvent_v23_1.rds") %>%
  gzcon() %>%
  readRDS()

df_conflict <- GEDEvent_v23_1 %>%
  mutate(
    iso3 = ifelse(
      country_id == 678,
      "YEM",
      countrycode(
        sourcevar = country_id,
        origin = "gwn",
        destination = "iso3c"
      )
    )
  ) %>%
  group_by(
    iso3,
    year
  ) %>%
  summarize(
    events = n(),
    fatalities = sum(best),
    .groups = "drop"
  ) %>%
  filter(
    !is.na(iso3)
  ) %>%
  complete(
    iso3 = unique(c(iso3, df_fts$iso3)),
    year = 1989:2022,
    fill = list(
      events = 0,
      fatalities = 0
    )
  )

# split PSE fatalities separate from Israel
# first get the PSE adm0 boundaries
sf_pse <- pull_dataset("cod-ab-pse") %>%
  get_resources() %>%
  pluck(3) %>%
  read_resource() %>%
  st_as_sf()

# join to the conflict dataset when reading in the shapefiles
df_conflict_pse <- GEDEvent_v23_1 %>%
  filter(
    country_id == 666
  ) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  filter(
    as.logical(
      st_intersects(
        .,
        sf_pse,
        sparse = FALSE
      )
    )
  ) %>%
  mutate(
    iso3 = "PSE"
  ) %>%
  as_tibble() %>%
  group_by(
    iso3,
    year
  ) %>%
  summarize(
    events = n(),
    fatalities = sum(best),
    .groups = "drop"
  ) %>%
  complete(
    year = 1989:2022,
    fill = list(
      events = 0,
      fatalities = 0
    )
  )

# subtract the PSE data from ISR
df_conflict_isr <- df_conflict %>%
  filter(
    iso3 == "ISR"
  ) %>%
  left_join(
    select(
      df_conflict_pse,
      year,
      events_pse = events,
      fatalities_pse = fatalities
    ),
    by = "year"
  ) %>%
  mutate(
    events = events - events_pse,
    fatalities = fatalities - fatalities_pse
  ) %>%
  select(
    -ends_with("pse")
  )

df_conflict <- df_conflict %>%
  filter(
    !(iso3 %in% c("ISR", "PSE"))
  ) %>%
  bind_rows(
    df_conflict_pse,
    df_conflict_isr
  ) %>%
  arrange(
    iso3,
    year
  )

#########################
#### INFORM SEVERITY ####
#########################

# classifying "complex" shocks as those that would occur alongside conflict
# to make it complex

other_shocks <- c(
  "Drought",
  "Earthquake",
  "Socio-political",
  "Floods",
  "Other seasonal event",
  "Cyclone",
  "Tecnological Disaster",
  "Food Security",
  "Epidemic"
)

download.file(
  url = "https://drmkc.jrc.ec.europa.eu/inform-index/Portals/0/InfoRM/Severity/2023/20230202_inform_severity_-_january_2023_update.xlsx",
  destfile = f <- tempfile(fileext = ".xlsx")
)

df_inform <- read_excel(
  f,
  sheet = "INFORM Severity - country",
  skip = 1
) %>%
  tail(
    -2
  ) %>%
  type_convert() %>%
  transmute(
    iso3 = ISO3,
    conflict_crisis = str_detect(DRIVERS, "Conflict|Violence"),
    other_crisis = str_detect(DRIVERS, paste(other_shocks, collapse = "|")),
    complex_crisis = conflict_crisis & other_crisis,
    inform_class = case_when(
      complex_crisis ~ "Complex",
      conflict_crisis ~ "Conflict",
      other_crisis ~ "Other"
    ),
    inform_severity = as.numeric(`INFORM Severity Index`),
    inform_pin = as.numeric(`People in need`)
  )
  
####################
#### INFORM MAP ####
####################

download.file(
  url = "https://public.opendatasoft.com/api/explore/v2.1/catalog/datasets/world-administrative-boundaries/exports/geojson",
  destfile = f <- tempfile()
)

sf_inform <- read_sf(f) %>%
  left_join(
    df_inform,
    by = "iso3"
  ) %>%
  st_as_sf()

# add conflict to the data
sf_inform_conf <- st_join(sf_inform, sf_conf_2021 %>% select(-region)) %>%
  group_by(
    across(
      geo_point_2d:inform_displacement
    )
  ) %>%
  summarize(
    conflict_events = sum(!is.na(best)),
    best = sum(best, na.rm = TRUE),
    deaths_civilians = sum(deaths_civilians, na.rm = TRUE),
    .groups = "drop"
  )

write_sf(
  sf_inform_conf,
  file.path(
    "data",
    "sf_inform.gpkg"
  )
)

######################################
#### USE GHO TO GET HRP COUNTRIES ####
######################################

df_hrp <- pull_dataset("global-humanitarian-overview-2023") %>%
  get_resource(1) %>%
  read_resource(sheet = "Export data") %>%
  transmute(
    iso3 = ifelse(
      Plans != "Rohingya (JRP)",
      countryname(Plans, destination = "iso3c"),
      "BGD"
    ),
    hrp = `Plan type` == "HRP",
    rrp = `Plan type` == "RRP",
    hrp_rrp = hrp | rrp,
    plan = TRUE
  ) %>%
  group_by(
    iso3
  ) %>%
  summarize(
    across(
      .cols = everything(),
      .fns = any
    ),
    .groups = "drop"
  )

###################################
#### CREATE ONE COMMON DATASET ####
###################################

df <- left_join(
  df_fts,
  df_inform,
  by = "iso3"
) %>%
  left_join(
    df_conflict,
    by = c("iso3", "year")
  ) %>%
  left_join(
    df_hrp, 
    by = "iso3"
  ) %>%
  mutate(
    across(
      hrp:plan,
      \(x) replace_na(x, FALSE)
    )
  )

write_csv(
  df,
  file.path(
    "data",
    "df_combined.csv"
  )
)
