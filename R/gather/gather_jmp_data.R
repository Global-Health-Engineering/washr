# header ------------------------------------------------------------------

# R script to download and tidy data from JMP WASH website www.washdata.org
# Created by Lars Schoebitz
# MIT Licence
# 16.01.2018

# comments ----------------------------------------------------------------

## this script downloads the world file from the JMP database
## this script manipulates the data to produce a final tidy dataset for further analysis

# libraries ---------------------------------------------------------------

library(tidyverse)
library(stringr)

# load data ---------------------------------------------------------------

## create temporary file with file extension xlsx
temp_file <- tempfile(fileext = ".xlsx")

# temp_file <- here::here("data/raw_data/WLD.xlsx")

## download world data file to temporary file
download.file("https://washdata.org/data/country/WLD/download", destfile = temp_file, mode = "wb")

## read sheet 3 of downloade excel file into R
data <- openxlsx::read.xlsx(xlsxFile = temp_file, sheet = 3, startRow = 4, colNames = FALSE) %>% 
    as_tibble()

jmp_world_wat <- openxlsx::read.xlsx(xlsxFile = temp_file, sheet = 3, colNames = TRUE) %>% 
    as_tibble()

jmp_world_san <- openxlsx::read.xlsx(xlsxFile = temp_file, sheet = 5, colNames = TRUE) %>% 
    as_tibble()

jmp_world_hyg <- openxlsx::read.xlsx(xlsxFile = temp_file, sheet = 7, colNames = TRUE) %>% 
    as_tibble()


## generate table of all jmp variables

#c(names(jmp_world_wat), names(jmp_world_san), names(jmp_world_hyg)) %>% 
#    enframe() %>% 
#    googlesheets4::write_sheet(sheet = "Sheet2", ss = "1w0FmGTByjvBTs0ohp2NIIsdukBrsc1t66GtdKiScJBc")

# these variables do not help here because they are on technology level
# jmp_vars <- read_csv(file = "data/derived_data/jmp_wash_variables.csv")

# manipulate data ---------------------------------------------------------

# The CSV from the code above was copied into Google Sheets and names for
# variables were added by hand from the JMP World file

# googlesheets4::read_sheet("1w0FmGTByjvBTs0ohp2NIIsdukBrsc1t66GtdKiScJBc") %>% 
#     rename(var_short = value) %>% 
#     write_csv("data/derived_data/jmp_wash_variables_complete.csv")

jmp_vars <- read_csv("data/derived_data/jmp_wash_variables_complete.csv") %>% 
    select(-name) %>% 
    filter(!is.na(var_long)) %>% 
    unique()

## STEP X: ... general tidying

jmp_world_wat_join <- jmp_world_wat |> 
    select(name, iso3, where(is.double))

jmp_world_san_join <- jmp_world_san %>% 
    select(name, iso3, where(is.double)) 

jmp_world_hyg_join <- jmp_world_hyg %>% 
    select(name, iso3, where(is.double)) 

jmp_world_tidy <- jmp_world_wat_join %>% 
    left_join(jmp_world_san_join) %>% 
    left_join(jmp_world_hyg_join) %>% 
    select(-sl) %>% 
    relocate(name, iso3) %>% 
    
    gather(key = var_short, value = percent, wat_bas_n:hyg_nfac_u) %>% 
    left_join(jmp_vars, by = c("var_short" = "var_short"))   %>% 
    
    ## remove these variables because they are unknown
    filter(!is.na(var_long)) %>% 
    mutate(
        residence = case_when(
            variable = str_detect(var_short, "_n$") == TRUE ~ "national",
            variable = str_detect(var_short, "_r$") == TRUE ~ "rural",
            variable = str_detect(var_short, "_u$") == TRUE ~ "urban"
        )
    ) %>% 
    mutate(var_short = str_replace(var_short, pattern =  "_n$", replacement = "")) %>% 
    mutate(var_short = str_replace(var_short, pattern =  "_r$", replacement = "")) %>% 
    mutate(var_short = str_replace(var_short, pattern =  "_u$", replacement = "")) %>% 
    mutate(
        service = case_when(
            var_short = str_detect(var_short, "^san") == TRUE ~ "sanitation",
            var_short = str_detect(var_short, "^wat") == TRUE ~ "water",
            var_short = str_detect(var_short, "^hyg") == TRUE ~ "hygiene",
        )
    ) 

## enrich data
## sanitation

jmp_world_tidy_san <- jmp_world_tidy %>% 
    filter(service == "sanitation") %>% 
    mutate(sanitation_technology = case_when(
        var_short = str_detect(var_short, "(lat|sep|sew)$") == TRUE ~ var_long
    )) %>% 
    mutate(safely_managed_sanitation = case_when(
        var_short = str_detect(var_short, "sm$") == TRUE ~ var_long
    )) %>% 
    mutate(sanitation_ladder = case_when(
        var_short = str_detect(var_short, "(bas|lim|unimp|od)$") == TRUE ~ var_long 
    )) %>% 
    pivot_longer(cols = sanitation_technology:sanitation_ladder, 
                 names_to = "indicator_type", 
                 values_to = "indicator") %>% 
    filter(!is.na(indicator)) %>% 
    
    ## remove san_sm (safely managed sanitation variable as it is the sum of
    ## value safely_managed_sanitation under indicator type)
    
    filter(var_short != "san_sm") 


## enrich data
## water

jmp_world_tidy_wat <- jmp_world_tidy %>%
    filter(service == "water") %>% 
    mutate(water_technology = case_when(
        var_short = str_detect(var_short, "(pip|npip)$") == TRUE ~ var_long
    )) %>% 
    mutate(safely_managed_drinking_water = case_when(
        var_short = str_detect(var_short, "(ses|ble|ity)$") == TRUE ~ var_long
    )) %>% 
    ## remove wat_sm (safely managed drinking water is the of three other 
    ## indicators
    filter(var_short != "wat_sm") %>% 
    mutate(water_ladder = case_when(
        var_short = str_detect(var_short, "(bas|lim|unimp|sur)$") == TRUE ~ var_long 
    )) %>% 
    pivot_longer(cols = water_technology:water_ladder, 
                 names_to = "indicator_type", 
                 values_to = "indicator") %>% 
    filter(!is.na(indicator)) 

## enrich data
## hygiene

jmp_world_tidy_hyg <- jmp_world_tidy %>%
    filter(service == "hygiene") %>% 
    mutate(hygiene_ladder = case_when(
        var_short = str_detect(var_short, "(bas|lim|nfac)$") == TRUE ~ var_long 
    )) %>% 
    pivot_longer(cols = hygiene_ladder,
                 names_to = "indicator_type",
                 values_to = "indicator")


## bind rows back together
jmp_world_tidy_enriched <- jmp_world_tidy_san %>% 
    bind_rows(
        jmp_world_tidy_wat,
        jmp_world_tidy_hyg
    ) %>% 
    mutate(percent = as.double(percent)) %>% 
    select(-var_long)

write_csv(jmp_world_tidy_enriched, "data/derived_data/jmp-washdata-indicators.csv")

# write smaller dataset for teaching

jmp_world_tidy_enriched |> 
    select(-pop_n, -prop_u, -arc_hyg_bas_u, -var_short) |> 
    filter(service == "sanitation") |> 
    filter(indicator_type == "sanitation_ladder") |> 
    select(-indicator_type, -service) |> 
    filter(year %in% seq(2011, 2020, 1)) |>
    relocate(c(residence, indicator), .after = year) |>
    write_csv(file = "data/derived_data/jmp-washdata-indicators-sanitation-small.csv")

# How to calculate safely managed drinking water from the data

jmp_world_tidy_enriched %>% 
    filter(service == "water", 
           indicator_type == "safely_managed_drinking_water") %>% 
    group_by(iso3, year, service, residence) %>% 
    summarise(
        percent_safely_managed = max(percent, na.rm = TRUE)
    ) 

# How to calculate safely managed sanitation from the data

jmp_world_tidy_enriched %>% 
    filter(service == "sanitation",
           indicator_type == "safely_managed_sanitation") %>% 
    group_by(iso3, year, service, residence) %>% 
    summarise(
        percent_safely_managed = sum(percent, na.rm = TRUE)
    )

