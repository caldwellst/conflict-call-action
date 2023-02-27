library(tidyverse)
library(countrycode)
library(rhdx)
library(readxl)
library(sf)

###################
#### UCDP DATA ####
###################

download.file(
  url = "https://ucdp.uu.se/downloads/ged/ged221-RData.zip",
  destfile = f <- tempfile()
)

# load the data into the environment
unzip(f, list = TRUE) %>%
  pull(Name) %>%
  unz(description = f, filename = .) %>%
  load(envir = globalenv())

df_conflict <- GEDEvent_v22_1 %>%
  mutate(
    iso3 = countrycode(
      sourcevar = country_id,
      origin = "gwn",
      destination = "iso3c"
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
    year = 1989:2021,
    fill = list(
      events = 0,
      fatalities = 0
    )
  ) %>%
  mutate(
    conflict_dummy = ifelse(
      events > 0,
      "Countries with conflict",
      "Other countries"
    )
  )

# join to the conflict dataset when reading in the shapefiles
sf_conf_2021 <- GEDEvent_v22_1 %>%
  filter(
    year == 2021
  ) %>%
  st_as_sf(coords = c("longitude", "latitude"))

st_crs(sf_conf_2021) <- 4326

write_sf(
  sf_conf_2021,
  file.path(
    "data",
    "sf_conflict.gpkg"
  )
)

##########################
#### FTS FUNDING DATA ####
##########################

resource_extractor <- function(dataset, query) {
  suppressMessages(
    get_resources(dataset, pattern = query) %>%
      pluck(1) %>%
      read_resource()
  )
}

poss_extractor <- possibly(.f = resource_extractor, otherwise = NULL)

df_fts_cluster <- map(
  .x = search_datasets(
    query = "Requirements and Funding Data",
    rows = 300L
  ),
  .f = poss_extractor,
  query = "fts_requirements_funding_globalcluster"
) %>%
  list_rbind() %>%
  mutate(
    cluster_clean = case_when(
      cluster == "Not specified" ~ "Other",
      str_detect(cluster, "Protection") ~ "Protection",
      clusterCode > 20 ~ "Other",
      TRUE ~ cluster
    )
  ) %>%
  group_by(
    iso3 = countryCode,
    cluster = cluster_clean,
    year
  ) %>%
  summarize(
    requirements = sum(requirements, na.rm = TRUE),
    funding = sum(funding, na.rm = TRUE),
    .groups = "drop"
  )

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

#####################
#### INFORM 2021 ####
#####################

download.file(
  url = "https://drmkc.jrc.ec.europa.eu/inform-index/Portals/0/InfoRM/Severity/2022/20220204_inform_severity_-_january_2022.xlsx",
  destfile = f <- tempfile(fileext = ".xlsx")
)

df_inform_2021 <- read_excel(
  f,
  sheet = "INFORM Severity - country",
  skip = 1
) %>%
  tail(
    -2
  ) %>%
  transmute(
    iso3 = ISO3,
    inform_severity_2021 = as.numeric(`INFORM Severity Index`)
  )

# get displacement data
df_inform_2021 <- read_excel(
  f,
  sheet = "Impact of the crisis",
  skip = 1
) %>%
  tail(
    -3
  ) %>%
  transmute(
    iso3 = `...5`,
    inform_displaced = `People displaced`
  ) %>%
  separate_longer_delim(
    cols = iso3,
    delim = ","
  ) %>%
  group_by(
    iso3
  ) %>%
  summarize(
    inform_displacement_2021 = mean(as.numeric(inform_displaced), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  full_join(
    df_inform_2021,
    by = "iso3"
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
    inform_severity = as.numeric(`INFORM Severity Index`)
  )

# displacement score

df_inform_disp <- read_excel(
  f,
  sheet = "Impact of the crisis",
  skip = 4
) %>%
  transmute(
    iso3 = Iso3,
    inform_displacement = as.numeric(`...23`)
  ) %>%
  group_by(
    iso3
  ) %>%
  summarize(
    inform_displacement = mean(inform_displacement, na.rm = TRUE),
    .groups = "drop"
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
  left_join(
    df_inform_2021,
    by = "iso3"
  ) %>%
  left_join(
    df_inform_disp,
    by = "iso3"
  ) %>%
  st_as_sf()

# add conflict to the data
sf_inform$conflict_events <- sapply(st_intersects(sf_inform, sf_conf_2021), length)

write_sf(
  sf_inform,
  file.path(
    "data",
    "sf_inform.gpkg"
  )
)

#####################
#### GLOBAL NEED ####
#####################

df_pin <- pull_dataset("global-humanitarian-overview-2023") %>%
  get_resource(1) %>%
  read_resource(sheet = "GHO 2023") %>%
  slice(-1) %>%
  head(-2) %>%
  type_convert() %>%
  transmute(
    iso3 = ifelse(
      Plan != "Rohingya",
      countryname(Plan, destination = "iso3c"),
      "BGD"
    ),
    pin = `People in need`
  ) %>%
  group_by(
    iso3
  ) %>%
  summarize(
    pin = sum(pin),
    year = 2022
  )

########################
#### JOINED DATASET ####
########################

reduce(
  list(
    df_fts,
    df_conflict,
    df_pin
  ),
  ~ full_join(.x, .y, by = c("iso3", "year"))
) %>%
  mutate(
    across(
      c(requirements, funding),
      ~ ifelse(
        between(year, 2000, 2022),
        replace_na(.x, 0),
        .x
      )
    )
  ) %>%
  full_join(
    df_inform,
    by = "iso3"
  ) %>%
  arrange(
    iso3,
    year
  ) %>%
  write_csv(
    file.path(
      "data",
      "df_joined.csv"
    ),
    na = ""
  )

################################
#### CREATING PIN SHAPEFILE ####
################################

df_pin_sector <- read_csv(
  file.path(
    "input",
    "2022_sectoral_pins.csv"
  )
) %>%
  filter(
    sector_general == "intersectoral"
  ) %>%
  group_by(
    adm0_pcode,
    adm1_pcode
  ) %>%
  summarize(
    pin = sum(pin),
    affected_population = sum(affected_population),
    pin_pct = pin / affected_population,
    .groups = "drop"
  ) %>%
  mutate(
    admin_level = 1,
    admin_code = adm1_pcode
  )
  

# scrape shapefile data of each of these

codab_reader <- function(iso3, admin_level) {
  # Afghanistan shapefiles are private, so need to use publicly available ones
  if (iso3 == "AFG") {
    sf <- read_sf(file.path("input", "geoBoundaries-AFG-ADM1_simplified.geojson"))
    adm_pcode <- read_csv(file.path("input", "afg_pcode_map.csv"))
    
    sf <- left_join(sf, adm_pcode, "shapeID")
  } else {
    iso3 <- tolower(iso3)
    
    ds <- pull_dataset(paste0("cod-ab-", iso3))
    # search patterns for relevant country files
    pattern <- paste0(
      "(.*)adm(.*)SHP.zip$|",
      "(.*)adm(.*)[0-9]{2,}.zip$|",
      "Administrative Divisions Shapefiles.zip$|",
      iso3, "(.*)adm", admin_level, "(.*).zip$|",
      "(.*)_adm_2022_v2.zip"
    )
    res <- ds %>%
      get_resources(pattern = pattern) %>%
      pluck(1)
    
    # download zipped file to tempdir 
    d <- tempdir()
    fn <- paste0(iso3, ".zip")
    
    zip_path <- file.path(
      d,
      fn
    )
    
    download_resource(
      res,
      folder = d,
      filename = fn,
      force = TRUE
    )
    
    # extract files from the zipped file to the dir
    unzip(
      zip_path,
      exdir = d
    )
    
    # load in the relevant level shapefile
    fn <- list.files(
      d,
      pattern = paste0(
        iso3, "(.*)adm", admin_level, "(.*).shp$|",
        iso3, "_admin", str_extract(admin_level, "[1-2]"), ".shp$"
      ),
      recursive = TRUE
    )
    
    sf <- read_sf(
      file.path(
        d,
        fn[1]
      )
    )
  }
  
  names(sf)[str_detect(names(sf), paste0("(adm|ADM)(.*)", admin_level, "(.*)P(?!T)|^PCODE$"))] <- "admin_code"
  st_as_sf(sf)
}

# get list of all adminstrative boundary shapefiles
shp_list <- df_pin_sector %>%
  distinct(
    iso3 = adm0_pcode,
    admin_level
  ) %>%
  pmap(
    codab_reader
  )

# clean up the files by creating a singular admin column that matches
# what is in the `admin_code` column in df_pin_sector

sf_pin <- map(
  .x = shp_list,
  .f = ~ select(.x, geometry, admin_code)
) %>%
  list_rbind() %>%
  inner_join(select(df_pin_sector, admin_code, pin, affected_population, pin_pct), by = "admin_code") %>%
  st_as_sf()

# needed due to some geometry errors when intersecting
sf_use_s2(FALSE)

sf_pin$conflict_events <- sapply(st_intersects(sf_pin, sf_conf_2021), length)

sf_pin %>%
  write_sf(
    file.path(
      "data",
      "sf_pin.gpkg"
    )
  )

####################################
#### WORLD POPULATION PROSPECTS ####
####################################

# used for country level needs analysis

download.file(
  url = "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/EXCEL_FILES/1_General/WPP2022_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT_REV1.xlsx",
  destfile = f <- tempfile(fileext = ".xlsx")
)

df_wpp <- read_excel(
  f,
  sheet = "Medium variant",
  skip = 16,
  guess_max = 20000
)

df_wpp %>%
  filter(
    !is.na(`ISO3 Alpha-code`),
    Year == 2022
  ) %>%
  transmute(
    iso3 = `ISO3 Alpha-code`,
    population = as.numeric(`Total Population, as of 1 July (thousands)`) * 1000
  ) %>%
  write_csv(
    file.path(
      "data",
      "wpp.csv"
    )
  )
