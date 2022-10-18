# header ------------------------------------------------------------------

# R script to download and tidy data from JMP WASH website www.washdata.org
# Contact: Lars Schoebitz
# 2020-09-03

# comments ----------------------------------------------------------

# This scripts downloads each JMP country file and extracts raw data underlying
# the sanitation service chain. The output is one dataframe for all countries

# libraries ---------------------------------------------------------------

library(tidyverse)
library(stringr)

# load data ----------------------------------------------------------------

## download world data file to temporary file
download.file("https://washdata.org/data/country/WLD/download", destfile = "data/raw_data/WLD.xlsx", mode = "wb")

## get iso3 country codes from existing World file
country_codes <- readxl::read_excel(path = "data/raw_data/WLD.xlsx", sheet = 3) %>% 
    select(iso3, name) %>% 
    # country file for Saint Barthelemy does not exist anymore 2022-10-18
    filter(!iso3 %in% c("BLM", "MAF")) %>% 
    unique()

## get list of variables stored in 'Chart Data' Tab from one country file
download.file("https://washdata.org/data/country/UGA/download", destfile = "data/raw_data/UGA.xlsx", mode = "wb")

var_list <- readxl::read_excel(path = "data/raw_data/UGA.xlsx", sheet = "Chart Data", skip = 3, col_names = FALSE) %>% 
    slice(1:2) %>% 
    t() %>% 
    as_tibble() %>% 
    rename(
        var_long = V1,
        var_short = V2
    ) 

write_csv(x = var_list, "data/derived_data/jmp_wash_variables.csv")

## define factor level for sanitation service chain variable 
ssc_levels = c("open defecation", "sharing", "user interface", "containment", "emptying", "transport", "FS treatment","WW treatment")

## get var list for sanitation only
var_list_san <- var_list %>% 
    spread(key = var_short, value = var_long) %>% 
    
    ## select vars starting with s_
    select(starts_with("s_")) %>% 
    
    gather(key = var_short, value = var_long) %>% 
    ## add variable for residence
    mutate(residence = case_when(
        var_short = str_detect(var_short, "_n") == TRUE ~ "national",
        var_short = str_detect(var_short, "_r") == TRUE ~ "rural",
        var_short = str_detect(var_short, "_u") == TRUE ~ "urban")
    ) %>% 

    ## add variable for sanitation service chain
    mutate(san_service_chain = case_when(
        str_detect(var_short, "od") ~ "open defecation",
        str_detect(var_short, "imp") ~ "user interface",
        str_detect(var_short, "con|net") ~ "containment",
        
        # latrines not included because var_short name has "con" in it
        # str_detect(var_short, "lat") ~ "containment",
        
        str_detect(var_short, "ebo|edl|ero|nemp") ~ "emptying",
        str_detect(var_short, "dtp") ~ "transport",
        str_detect(var_short, "rtp") ~ "transport",
        str_detect(var_short, "treat_fstp") ~ "FS treatment",
        str_detect(var_short, "treat_wtp") ~ "WW treatment",
        str_detect(var_short, "^s_sep|^s_lat|^s_sew") ~ "user interface",
        str_detect(var_short, "shared") ~ "sharing")
    ) %>%
    mutate(san_service_chain = factor(san_service_chain, levels = ssc_levels))

## write variable list 
var_list_san %>% 
    write_csv(here::here("data/derived_data/jmp_sanitation_variables_residence.csv"))


# download country files and extract data

## create a path for a new directory
countryfile_dir <- here::here("data/raw_data/country_files/")

## check if dir exists, if not create it
if (dir.exists(countryfile_dir) == FALSE) {
    
    dir.create(countryfile_dir)

    }

## create empty list for results


for (name in country_codes$iso3) {
    
    if (file.exists(paste0(countryfile_dir, name, ".xlsx")) == FALSE) {
        
        download.file(
            paste0("https://washdata.org/data/country/", name, "/download"), 
            destfile = str_c(countryfile_dir, name, ".xlsx"), 
            mode = "wb")
        
    } else {
        
    }
}

# create an empty list
country_list <- list()

for (name in country_codes$iso3) {
    
    country_list[[name]] <- readxl::read_excel(path = str_c(countryfile_dir, name, ".xlsx"), sheet = "Chart Data", skip = 4, col_names = TRUE) %>% 
        select(source, type, year, starts_with("s")) %>% 
        gather(key = var_short, value = value, s_imp_n:s_shared_r) %>% 
        filter(!is.na(value)) %>% 
        mutate(iso3 = name)
    
}


# five countries have no data at all

no_data_iso3 <- country_list %>% 
    map(count) %>% 
    tibble::enframe() %>% 
    tidyr::unnest(cols = value) %>% 
    filter(n == 0) %>% 
    .$name


no_data_tib <- tibble(
    source = NA_character_,
    type = NA_character_,
    year = NA,
    var_short = NA_character_,
    value = NA,
    iso3 = no_data_iso3
)


## export data

jmp_sanitation_raw_data <- country_list %>% 
    map(mutate, source = as.character(source)) %>% 
    map(mutate, type = as.character(type)) %>% 
    
    ## turn list into dataframe
    bind_rows() %>% 
    
    ## readd countries without any data
    bind_rows(no_data_tib) %>% 
    
    ## enrich dataframe
    left_join(var_list_san)


jmp_sanitation_raw_data %>%     
    ## write data to Rds file
    write_rds(path = here::here("data/derived_data", paste0(Sys.Date(), "_jmp_sanitation_raw_data.rds"))) 

jmp_sanitation_raw_data %>%     
    ## write data to Rds file
    write_csv(path = here::here("data/derived_data", paste0(Sys.Date(), "_jmp_sanitation_raw_data.csv"))) 



    
