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

googlesheets4::read_sheet("1w0FmGTByjvBTs0ohp2NIIsdukBrsc1t66GtdKiScJBc") %>% 
    rename(var_short = value) %>% 
    write_csv("data/derived_data/jmp_wash_variables_complete.csv")

jmp_vars <- read_csv("data/derived_data/jmp_wash_variables_complete.csv") %>% 
    select(-name) %>% 
    filter(!is.na(var_long)) %>% 
    unique()

## STEP X: ... general tidying

jmp_world_wat_join <- jmp_world_wat  %>% 
    select(name, iso3, where(is.double))

jmp_world_san_join <- jmp_world_san %>% 
    select(name, iso3, where(is.double)) 

jmp_world_hyg_join <- jmp_world_hyg %>% 
    select(name, iso3, where(is.double)) 

jmp_world_tidy <- jmp_world_wat_join %>% 
    left_join(jmp_world_san_join) %>% 
    left_join(jmp_world_hyg_join) %>% 

    gather(key = var_short, value = percent, prop_u:hyg_nfac_u) %>% 
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


jmp_world_tidy %>%
    filter(iso3 == "SEN") %>% 
    filter(year == 2017) %>% 
    filter(residence == "urban") %>%  
    filter(service == "water")


write_csv(jmp_world_tidy, "data/derived_data/jmp_washdata_indicators.csv")

### testing a proof for Rick

# jmp_data_tidy %>% 
#     filter(
#         variable == "safely_managed",
#         residence == "national",
#         year == 2015,
#         country != "World"
#     ) %>%
#     mutate(
#         total_pop = population * percent / 100
#     ) %>% 
#     summarise(
#         safe = sum(total_pop),
#         total_pop = sum(population)
#     ) %>% 
#     mutate(
#         not_safe = total_pop - safe
#     )



# jmp_data_tidy %>% 
#     filter(
#         variable == "safely_managed",
#         residence == "national",
#         year == 2015,
#         country == "World"
#     ) %>% 
#     mutate(
#         total_pop = population * percent / 100
#     )


### STEP X: Make sure that all zeros are actual zeros and not NAs

jmp_data_tidy2 <- jmp_data_tidy %>% 
    
    ### bring into wide format
    
    spread(var_short, percent)  %>% 
    
    ### fix the fact that sewer, septic and pit can actually be zero
    
    mutate(
        septic_tanks = case_when(
            is.na(septic_tanks) == TRUE & is.na(sewer_connections) == FALSE & is.na(latrines_and_others) == FALSE ~ 0,
            TRUE ~ septic_tanks
        )
    ) %>% 
    
    mutate(
        sewer_connections = case_when(
            is.na(sewer_connections) == TRUE & is.na(septic_tanks) == FALSE & is.na(latrines_and_others) == FALSE ~ 0,
            TRUE ~ sewer_connections
        )
        
    ) %>% 
    mutate(
        latrines_and_others = case_when(
            is.na(latrines_and_others) == TRUE & is.na(sewer_connections) == FALSE & is.na(septic_tanks) == FALSE ~ 0,
            TRUE ~ latrines_and_others
            
        )
    )

### STEP X: Add additional variables

jmp_data_tidy3 <- jmp_data_tidy2 %>% 
    
    ### include three additional variables that include percentages for each sanitation technology that is shared
    ### this is limited * fraction of the technology out of all technologies
    
    mutate(
        sewer_connections_shared = (100-(sewer_connections + septic_tanks + latrines_and_others + unimproved_service + open_defecation)) * (sewer_connections/(sewer_connections + septic_tanks + latrines_and_others)),
        septic_tanks_shared = (100-(sewer_connections + septic_tanks + latrines_and_others + unimproved_service + open_defecation)) * (septic_tanks/(sewer_connections + septic_tanks + latrines_and_others)),
        latrines_and_others_shared = (100-(sewer_connections + septic_tanks + latrines_and_others + unimproved_service + open_defecation)) * (latrines_and_others/(sewer_connections + septic_tanks + latrines_and_others))
    ) %>% 
    
    ### include variable that highlights improved facilities (sewer, septic and latrine incl. shared)
    
    mutate(
        improved_sanitation_facility = sewer_connections + septic_tanks + latrines_and_others + sewer_connections_shared + septic_tanks_shared + latrines_and_others_shared
        
    ) %>% 
    
    ### include a total variable. this variable is based on sums of other variables and might therefore result in NA values for some countries where this information is not available
    
    mutate(
        
        total_population = improved_sanitation_facility + unimproved_service + open_defecation
        
    ) %>%   
    
    ### rename variables to make more explicit that there are two categories "shared" and "not shared"
    
    rename(
        sewer_connections_not_shared = sewer_connections,
        septic_tanks_not_shared = septic_tanks,
        latrines_and_others_not_shared = latrines_and_others
        
    ) %>% 
    
    ### sum shared and not shared to get total percentage of technologies
    
    mutate(
        sewer_connections = sewer_connections_not_shared + sewer_connections_shared,
        septic_tanks = septic_tanks_not_shared + septic_tanks_shared,
        latrines_and_others = latrines_and_others_not_shared + latrines_and_others_shared
    ) 




# STEP X: bring data into long format ------------------------------------


jmp_data_tidy4 <- jmp_data_tidy3 %>% 
    
    ### long format
    
    
    gather(key = variable, value = percent, basic_service:latrines_and_others) %>% 
    
    ### include variable for population percentage 
    
    mutate(
        population_percentage = percent / 100 * population
    ) 


# STEP X: add some more variables

## manipulate to identify whether the variable is a servce or technology type variable

jmp_data_tidy5 <- jmp_data_tidy4 %>% 
    mutate(
        variable_type = case_when(
            str_detect(variable, "service") ~ "service",
            variable == "safely_managed" ~ "service",
            variable == "open_defecation" ~ "service",
            variable == "emptied_and_treated" ~ "sub_indicator",
            variable == "disposed_in_situ" ~ "sub_indicator",
            variable == "wastewater_treated" ~ "sub_indicator",
            variable == "total_population" ~ "total",
            TRUE ~ "sanitation_technology"
        ) 
    ) %>% 
    mutate(
        sanitation_technology_type = case_when(
            variable == "sewer_connections" ~ "offsite_sanitation",
            variable == "septic_tanks" ~ "onsite_sanitation",
            variable == "latrines_and_others" ~ "onsite_sanitation",
            variable_type == "total" ~ "total"
        )
    )





# Step X: Add shared variable ---- not yet implemented.

### add variable that indicates shared and not shared

jmp_data_tidy6 <- jmp_data_tidy5 %>%     
    
    mutate(
        shared_facility = case_when(
            variable_type == "sanitation_technology" & str_detect(variable, "not_shared") ~ "no",
            variable_type == "sanitation_technology" & str_detect(variable, "shared") ~ "yes",
            
            variable_type == "sanitation_technology" & str_detect(variable, "improved") ~ NA_character_,
            
            variable_type == "sanitation_technology" ~ "total"
        )
    ) 


# STEP X: Write data ------------------------------------------------------

# write data 1 - old data previous to changes from 12.02.2018

## write_csv(jmp_data_tidy, "data/jmp_2017_database_tidy.csv")

# write data 2 - new data 

write_csv(jmp_data_tidy5, paste0("data/jmp/", Sys.Date(), "_", "jmp_2017_database_tidy_additional_variables", ".csv"))


jmp_data <- read_csv("data/jmp/2019-02-12_jmp_2017_database_tidy_additional_variables.csv")

jmp_data %>% 
    filter(variable == "emptied_and_treated") %>% 
    filter(year == 2015) %>%
    filter(residence == "national") %>% 
    mutate(percent = factor(round(percent, 0))) %>% 
    select(country, iso3, percent)


## code is wrong because ISO3 code is not carried along
jmp_data_tidy5 %>% 
    filter(variable == "emptied_and_treated") %>% 
    filter(!is.na(percent)) %>% 
    filter(iso3 == 2015) %>% 
    filter(residence == "national") %>% 
    mutate(percent = factor(round(percent, 0))) %>% 
    select(country, iso3, percent) %>% 
    arrange(percent)




