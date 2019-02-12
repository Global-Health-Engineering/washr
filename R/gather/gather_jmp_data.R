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

## download world data file to temporary file
download.file("https://washdata.org/data/country/WLD/download", destfile = temp_file)

## read sheet 3 of downloade excel file into R
data <- openxlsx::read.xlsx(xlsxFile = temp_file, sheet = 3, startRow = 4, colNames = FALSE)

data2 <- as_tibble(data)

# manipulate data ---------------------------------------------------------


## STEP X: ... general tidying

jmp_data_tidy <- data2 %>% 
    select(1:46) %>% 
    select(-X5, -X10, -X11, -X16, -X17, -X22, -X23, -X24, -X25) %>% 
    gather(key = variable, value = percent, X6:X46) %>% 
    mutate(
        variable = case_when(
            variable == "X6" ~ "basic_service_n",
            variable == "X7" ~ "limited_service_n",
            variable == "X8" ~ "unimproved_service_n",
            variable == "X9" ~ "open_defecation_n",
            variable == "X12" ~ "basic_service_r",
            variable == "X13" ~ "limited_service_r",
            variable == "X14" ~ "unimproved_service_r",
            variable == "X15" ~ "open_defecation_r",
            variable == "X18" ~ "basic_service_u",
            variable == "X19" ~ "limited_service_u",
            variable == "X20" ~ "unimproved_service_u",
            variable == "X21" ~ "open_defecation_u",
            variable == "X26" ~ "safely_managed_n",
            variable == "X27" ~ "disposed_in_situ_n",
            variable == "X28" ~ "emptied_and_treated_n",
            variable == "X29" ~ "wastewater_treated_n",
            variable == "X30" ~ "latrines_and_others_n",
            variable == "X31" ~ "septic_tanks_n",
            variable == "X32" ~ "sewer_connections_n",
            variable == "X33" ~ "safely_managed_r",
            variable == "X34" ~ "disposed_in_situ_r",
            variable == "X35" ~ "emptied_and_treated_r",
            variable == "X36" ~ "wastewater_treated_r",
            variable == "X37" ~ "latrines_and_others_r",
            variable == "X38" ~ "septic_tanks_r",
            variable == "X39" ~ "sewer_connections_r",
            variable == "X40" ~ "safely_managed_u",
            variable == "X41" ~ "disposed_in_situ_u",
            variable == "X42" ~ "emptied_and_treated_u",
            variable == "X43" ~ "wastewater_treated_u",
            variable == "X44" ~ "latrines_and_others_u",
            variable == "X45" ~ "septic_tanks_u",
            variable == "X46" ~ "sewer_connections_u"
        )
    ) %>% 
    mutate(
        residence = case_when(
            variable = str_detect(variable, "_n") == TRUE ~ "national",
            variable = str_detect(variable, "_r") == TRUE ~ "rural",
            variable = str_detect(variable, "_u") == TRUE ~ "urban"
        )
    ) %>%
    mutate(variable = str_replace(variable, pattern =  "_n", replacement = "")) %>% 
    mutate(variable = str_replace(variable, pattern =  "_r", replacement = "")) %>% 
    mutate(variable = str_replace(variable, pattern =  "_u", replacement = "")) %>% 
    rename(
        country = X1,
        iso3 = X2,
        year = X3,
        population = X4
    ) %>% 
    
    ### set all "-" to NAs, keep in mind that not all NAs are actual NAs but might also be zeros. see later mutations.
    
    mutate(percent = "is.na<-"(percent, percent == "-")) %>%   ## https://stackoverflow.com/a/27909247/6816220
    mutate(percent = as.double(percent)) %>% 
    mutate(population = population * 1000) %>% 
    filter(!is.na(percent))

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
    
    spread(variable, percent)  %>% 
    
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


