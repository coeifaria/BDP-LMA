#```{r formatting2_fileloading, message=FALSE, warning=FALSE, chunk_progress=TRUE}
library(readxl)
# List files matching the criteria

file_path <- file.path(getwd(), "..", "data")

demand_hires <- list.files(
  path = file_path,  # Assuming the files are in a "data" folder
  pattern = "^Occupation.*?(NCV|SCV|CVML|California).*\\.xlsx$",
  full.names = T,
  ignore.case = TRUE
)

datavista <- list.files(
  pattern = "^datavista.*\\.rds",
  full.names = T,
  ignore.case = T
)

#table(swfp_stacked$YEAR_ID)
#table(state$YEAR_ID)
swfp_stacked <- readRDS(datavista)

gemini_api <- "AIzaSyA8hDaMYyUkHhb6XcWe5R_8YzKkl9S11Gw"
gemini.R::setAPI(gemini_api)



# Read data from the specified Excel file and sheet for NCV/NML demand analysis
#ncv_demand <- read_csv(demand_hires[str_detect(demand_hires, pattern = "NCV")])
#scv_demand <- read_csv(demand_hires[str_detect(demand_hires, pattern = "SCV")])
#cvml_demand <- read_csv(demand_hires[str_detect(demand_hires, pattern = "CVML")])
#ca_demand <- read_csv(demand_hires[str_detect(demand_hires, pattern = "California")])
library(dplyr)
library(stringr)
library(readxl)

# Step 1: Identify problematic columns
find_problematic_columns <- function(df, pattern) {
  cols_with_issues <- vector("logical", length(names(df)))
  for (l in seq_along(names(df))) {
    cols_with_issues[l] <- any(str_detect(df[[l]], pattern))
  }
  return(na.omit(names(df)[cols_with_issues]))
}

# Step 2: Fix "<10" values
fix_under10 <- function(df, col_name) {
  row <- which(str_detect(df[[col_name]], "<10"))

  # Replace "<10" with "0"
  df[row, col_name] <- "0"

  # Convert the column to numeric
  df[[col_name]] <- as.numeric(df[[col_name]]) %>% round()

  # Extrapolate missing values
  df[[col_name]][row] <- last(df[[col_name]]) - sum(df[[col_name]][1:(nrow(df) - 1)])

  return(df)
}

# Step 3: Handle "Insf. Data"
fix_insf_data <- function(df, col_name, col_index) {
  rows <- which(str_detect(df[[col_name]], "Insf. Data"))

  for (row in rows) {
    if (col_index == 5) {
      # Calculate "2023 - 2028 Change"
      df[row, col_name] <- as.character(
        as.numeric(df[[4]][row]) - as.numeric(df[[3]][row])  # 2028 Jobs...4 - 2023 Jobs...3
      )
    } else if (col_index == 6) {
      # Calculate "2023 - 2028 % Change"
      df[row, col_name] <- as.character(
        round((as.numeric(df[[4]][row]) - as.numeric(df[[3]][row])) / as.numeric(df[[3]][row]), 3)
      )
    } else {
      # Replace "Insf. Data" with NA for other columns
      df[row, col_name] <- NA
    }
  }

  # Convert column to numeric
  df[[col_name]] <- suppressWarnings(as.numeric(df[[col_name]]))
  return(df)
}

# Step 4: Main processing function
process_dataframe <- function(df) {
  # Identify columns with "<10"
  columns_with_under10 <- find_problematic_columns(df, "<10")

  # Fix "<10" values
  for (col in columns_with_under10) {
    df <- fix_under10(df, col)
  }

  # Identify columns with "Insf. Data"
  columns_with_insf_data <- find_problematic_columns(df, "Insf. Data")

  # Fix "Insf. Data"
  for (col_name in columns_with_insf_data) {
    col_index <- which(names(df) == col_name)
    df <- fix_insf_data(df, col_name, col_index)
  }

  return(df)
}

# Step 5: Wrapper function for cleaning
demand_func <- function(df) {
  df <- process_dataframe(df)
  return(df)
}

ncv_demand <- read_excel(demand_hires[str_detect(demand_hires, pattern = "NCV")], sheet = "Occs") %>% demand_func()
scv_demand <- read_excel(demand_hires[str_detect(demand_hires, pattern = "SCV")], sheet = "Occs") %>% demand_func()
cvml_demand <- read_excel(demand_hires[str_detect(demand_hires, pattern = "CVML")], sheet = "Occs") %>% demand_func()
ca_demand <- read_excel(demand_hires[str_detect(demand_hires, pattern = "California")], sheet = "Occs") %>% demand_func()

soc_code_titles_df <- pull(ca_demand, Description)[!is.na(pull(ca_demand, SOC))]
soc_title <- pull(ca_demand, Description)[!is.na(pull(ca_demand, SOC))]

subregion_demand <- if (str_detect(region, pattern = "S")) {
  scv_demand
} else {
  ncv_demand
}

demand_postings <- list.files(
  path = "../data",  # Assuming the files are in a "data" folder
  pattern = "^Job_Posting.*?(NCV|SCV|CVML).*\\.xlsx$",
  full.names = T,
  ignore.case = TRUE
)

# Read salary information from an Excel sheet, starting from the 5th row; convert "Observations" to numeric
#jp_salary <- read_excel(demand_postings[str_detect(demand_postings, pattern = region_acro)],
#    sheet = "Advertised Salary", skip = 5) %>%
#  mutate(Observations = as.numeric(Observations))

# Read job postings data from the specified Excel sheet, skipping the first row
jp_top_postings <-  read_excel(demand_postings[str_detect(demand_postings, pattern = region_acro)],
                               sheet = "Job Postings Top Occs",
                               skip = 1)

# Read top companies posting jobs from the specified Excel sheet, skipping the first row
jp_top_posting_companies <-  read_excel(demand_postings[str_detect(demand_postings, pattern = region_acro)],
                                        sheet = "Job Postings Top Companies",
                                        skip = 1)

ed_attainment_file <- list.files(
  #path = data_path,  # Assuming the files are in a "data" folder
  pattern = "^Educational Attainment.*\\.xlsx$",
  full.names = T,
  ignore.case = TRUE
)

supply_file <- list.files(
  #path = data_path,  # Assuming the files are in a "data" folder
  pattern = "^Supply.*\\.xlsx$",
  full.names = T,
  ignore.case = TRUE
)

EducationalAttainment <- readxl::read_excel(ed_attainment_file,
                                            sheet = "Table 5.3", skip = 1)


EducationalAttainment_filtered <- EducationalAttainment %>%
  inner_join(subregion_demand, by = c("2023 National Employment Matrix code" = "SOC")) %>%
  filter(!is.na(`2023 National Employment Matrix code`))  %>%
  mutate(`2023 National Employment Matrix title` = str_remove_all(`2023 National Employment Matrix title`, pattern = "\\[1\\]"),
         `2023 National Employment Matrix title` = str_to_title(`2023 National Employment Matrix title`))

#cip <- CIP
TOP_CIP_SOC_Current$`CIP Code` <- as.character(TOP_CIP_SOC_Current$`CIP Code`)
TOP_CIP_SOC_Current_supplementary$`CIP Code` <- as.character(TOP_CIP_SOC_Current_supplementary$`CIP Code`)
CIP_string <- CIP
#CIP <- as.character(TOP_CIP_SOC_Current$`CIP Code`)

ncc_supply <-   read_excel(supply_file, sheet = "Data_Other Ed Only") %>%
  mutate(CIP = sprintf("%07.4f", as.numeric(as.character(CIP)))) %>%
  filter(CIP %in% CIP_string) %>%
  filter(Regions == "Central")

ccc_supply_v2 <- read_excel(supply_file, sheet = "Data_Supply CC Only") %>%
  #mutate(CIP = sprintf("%07.4f", as.numeric(as.character(`TOP6 or CIP`)))) %>%
  inner_join(
    (#TOP_CIP_SOC_Current
      TOP_CIP_SOC_Current_supplementary %>%
        mutate(TOP2 = str_remove_all(`TOP Code`, "\\.")) %>%
        filter(TOP2 %in% str_remove_all(TOP, "\\."))
    ),
    by = c("TOP6" = "TOP2"),
    relationship = "many-to-many") %>%
  filter(#`TOP.Code` == str_remove(TOP, pattern = "\\.") &
    `CIP Code` %in% CIP,
    `Community College Flag` == "Community College")

ccc_supply_v3 <- read_excel(supply_file, sheet = "Data_Supply CC Only") %>%
  #mutate(CIP = sprintf("%07.4f", as.numeric(as.character(`TOP6 or CIP`)))) %>%
  inner_join(
    (#TOP_CIP_SOC_Current
      TOP_CIP_SOC_Current_supplementary
      %>% mutate(TOP2 = str_remove_all(as.character(`TOP Code`), "\\."))
    ),
    by = c("TOP6" = "TOP2"),
    relationship = "many-to-many") %>%
  filter(#`TOP.Code` == str_remove(TOP, pattern = "\\.") &
    #`CIP Code` %in% CIP,
    `Community College Flag` == "Community College")

#sw <- list.files(path = "./data", pattern = "StrongWorkforce", full.names = T)
#ssm_cte <- sw[sw %>%  str_detect(pattern = "Statewide") %>%  which()] %>% read.csv()

#community_college_supply_file  <- list.files(
#  path = "data",  # Assuming the files are in a "data" folder
#  pattern = "^ProgAwardsSumm.*\\.txt$",
#  full.names = T,
#  ignore.case = TRUE
#)

CA2024_SSS <- readxl::read_excel("CA2024_SSS.xlsx",
                                 sheet = "SSW",
                                 skip = 8) %>%
  select(c(1:3))

names(CA2024_SSS) <- c("Counties", "tablenumber", "Adult")

#livingwages_county <- read_delim("data/livingwages_county.txt",
#                                 delim = "\t", escape_double = FALSE,
#                                 trim_ws = TRUE)

average_living_wage <- function(county_wages) {
  CA2024_SSS %>%
    filter(str_detect(Counties, pattern = paste(county_wages, collapse = "|"))) %>%
    mutate(wage = as.numeric(str_remove(Adult, pattern = "\\$"))) %>%
    arrange(wage) %>%
    summarize(median = median(wage)) %>%
    mutate(median = paste0("$", median)) %>%
    as.character()
}

scv_living_wage <- average_living_wage(scv_counties)
ncv_living_wage <- average_living_wage(ncv_counties)
cvml_living_wage <- average_living_wage(paste(c(ncv_counties, scv_counties), collapse = "|"))
ca_living_wage <- average_living_wage(".")


hourly <- if (str_detect(region, pattern = "S")) {
  scv_living_wage
} else {
  ncv_living_wage
}

salary <- paste0("($", format(round(as.numeric(str_remove(hourly, "\\$")) * 2080, 0), big.mark = ",", scientific = FALSE), " annually)")
replacement_text <- paste(requesting_region, "is", hourly, "per hour", salary, sep = " ")
text_date <- format(Sys.Date(), "%B %Y")

#requesting_region_living_wage <- ifelse()

#ProgAwardsSumm <- read_tsv(community_college_supply_file, show_col_types = FALSE)

#supply <- ProgAwardsSumm %>%
#  mutate(TOP2 = str_remove_all(TOP, '\\.'),
#         region = case_when(
#           str_detect(`College Name`, paste0(scv_colleges, collapse = "|")) ~ "scv",
#           str_detect(`College Name`, paste0(ncv_colleges, collapse = "|")) ~ "ncv",
#           !str_detect(`College Name`, paste0(c(scv_colleges ,ncv_colleges), collapse = "|")) ~"remove")) %>%
#  filter(str_detect(`Program Type - TOP6`, pattern = TOP2) &
#           region != "remove") %>%
#  pivot_longer(
#    cols = starts_with("Annual"),
#    names_to = "AcadYr",
#    values_to = "Completions") %>%
#  mutate(AcadYr = str_remove_all(AcadYr, pattern = "Annual "))

#n <- read.csv("data/ProgAwardsSumm-N-Datamart.csv")
#s <- read.csv("data/ProgAwardsSumm-S-Datamart.csv")
#n$region <- 'ncvml'
#s$region <- 'scvml'

#list.files(path = "data", pattern = "Supply")
#years_e11 <- str_replace_all(str_remove_all(names(ns)[str_detect(names(ns), pattern = "20")], "Annual."), "\\.", "-")

#ns <- n %>%  bind_rows(s)

#ns <- ns[str_detect(ns$Program.Type...TOP6, pattern = paste(str_remove_all(TOP, '\\.'), collapse = "|")),]

#ns2 <- ns %>%
#  mutate(`Award.Type` = str_trim(ifelse(str_detect(`Award.Type`, pattern = "Associate"), "Associate Degree", `Award.Type`), side = "both")) %>%
#  pivot_longer(cols = starts_with("Annual"),
#               values_to = "Completions",
#               names_to = "AY") %>%
#  group_by(`Award.Type`) %>%
#    summarise(Completions = sum(as.numeric(Completions), na.rm = T))

#supply <- ns %>%
#  pivot_longer(
#    cols = starts_with("Annual"),
#    values_to = "Completions") %>%
#  mutate(Completions = as.numeric(Completions)) %>%
#  rename(`College Name` = `College.Name`)

suppy_completions <- ccc_supply_v2  %>%
  filter(Regions == "Central") %>%
  #select(`SOC.Title`,County, `Econ Subregion`, starts_with("20")) %>%
  select(`TOP6`,`Award Level`,County, `Econ Subregion`, starts_with("20")) %>%
  distinct() %>%
  filter(str_detect(`Econ Subregion`, "Valley"))  %>%
  pivot_longer(cols = starts_with("20"),
               values_to = "completions",
               names_to = "acadyr") %>%
  #group_by(acadyr) %>%
  summarize(completions = sum(completions, na.rm = T))

suppy_completions_college_counts <- ccc_supply_v3  %>%
  filter(Regions == "Central") %>%
  #select(`SOC.Title`,County, `Econ Subregion`, starts_with("20")) %>%
  select(`institution name`, `TOP6`,`Award Level`,County, `Econ Subregion`, starts_with("20")) %>%
  distinct() %>%
  filter(str_detect(`Econ Subregion`, "Valley"))  %>%
  pivot_longer(cols = starts_with("20"),
               values_to = "completions",
               names_to = "acadyr") %>%
  #group_by(acadyr) %>%
  pull(`institution name`) %>%
  unique() %>%
  length()

area_direction <- if (str_sub(region, 1, 1) == "N") {
  "North"
} else if (str_sub(region, 1, 1) == "S") {
  "South"
}
#area_direction

suppy_completions_college_counts_regional <- ccc_supply_v3  %>%
  filter(Regions == "Central") %>%
  #select(`SOC.Title`,County, `Econ Subregion`, starts_with("20")) %>%
  select(`institution name`, `TOP6`,`Award Level`,County, `Econ Subregion`, starts_with("20")) %>%
  distinct() %>%
  filter(str_detect(`Econ Subregion`, "Valley") &
         str_detect(`Econ Subregion`, area_direction))  %>%
  pivot_longer(cols = starts_with("20"),
               values_to = "completions",
               names_to = "acadyr") %>%
  #group_by(acadyr) %>%
  pull(`institution name`) %>%
  unique() %>%
  length()

ncc_suppy_completions_college_counts_regional <- ncc_supply  %>%
  filter(Regions == "Central") %>%
  #select(`SOC.Title`,County, `Econ Subregion`, starts_with("20")) %>%
  select(`Institution Name`, CIP,`Award Level`,County, `Econ Subregion`, starts_with("20")) %>%
  distinct() %>%
  filter(str_detect(`Econ Subregion`, area_direction))  %>%
  pivot_longer(cols = starts_with("20"),
               values_to = "completions",
               names_to = "acadyr") %>%
  #group_by(acadyr) %>%
  pull(`Institution Name`) %>%
  unique() %>%
  length()
#supply$AcadYr <- str_replace_all(str_remove_all(supply$name, pattern = "Annual."), "\\.", "-")


#supply_yr_start <- min(str_sub(unique(supply$AcadYr), 0, 4))
#supply_yr_end <- max(str_sub(unique(supply$AcadYr), 0, 4))

supply_yr_start <- ccc_supply_v3  %>%
  filter(Regions == "Central") %>%
  #select(`SOC.Title`,County, `Econ Subregion`, starts_with("20")) %>%
  select(`TOP6`,`Award Level`,County, `Econ Subregion`, starts_with("20")) %>%
  distinct() %>%
  filter(str_detect(`Econ Subregion`, "Valley"))  %>%
  pivot_longer(cols = starts_with("20"),
               values_to = "completions",
               names_to = "acadyr") %>%
  group_by(acadyr) %>%
  summarize(completions = sum(completions, na.rm = T)) %>%
  ungroup()  %>%
  pull(acadyr) %>%
  min() %>%
  str_extract("\\d{4}")

supply_yr_end <- ccc_supply_v3  %>%
  filter(Regions == "Central") %>%
  #select(`SOC.Title`,County, `Econ Subregion`, starts_with("20")) %>%
  select(`TOP6`,`Award Level`,County, `Econ Subregion`, starts_with("20")) %>%
  distinct() %>%
  filter(str_detect(`Econ Subregion`, "Valley"))  %>%
  pivot_longer(cols = starts_with("20"),
               values_to = "completions",
               names_to = "acadyr") %>%
  group_by(acadyr) %>%
  summarize(completions = sum(completions, na.rm = T)) %>%
  ungroup()  %>%
  pull(acadyr) %>%
  max() %>% str_sub(6,7)
supply_yr_end <- paste0("20", supply_yr_end)

opposite_region <- if (region=="NCV/NML"){
  "SCV/SML"
} else if(region=="SCV/SML") {
  "NCV/NML"
}

#SWFP <- list.files(
#  path = "data",  # Assuming the files are in a "data" folder
#  pattern = "^StrongWorkforceProgram.*?(District|Northern|Southern|Statewide).*\\.csv$",
#  full.names = T,
#  ignore.case = TRUE
#)

#swfp_stacked <- read_csv("../data/swfp_stacked.csv")

#top_code_title
topdf<-TOP_CIP_SOC_Current_supplementary[,c("TOP Code", "TOP Code Title")] %>%
  distinct()

SWFP <- swfp_stacked %>%
  filter(PROGRAM %in% str_remove_all(TOP, pattern = "\\.")) %>%
  filter(
    case_when(
      LOCALE_TYPE %in% c("Statewide", "Macroregion") ~ TRUE,
      LOCALE_TYPE == "Microregion" & str_detect(
        LOCALE_NAME,
        unique(pull(colleges_df[colleges_df$County%in% ifelse(requesting_region=="San Joaquin County",
                                                              "San Joaquin",
                                                              str_split(requesting_region, " ", simplify=T)[1])
                                ,], Classification))
      ) ~ TRUE,
      LOCALE_TYPE == "District" & LOCALE_NAME == unique(pull(colleges_df[colleges_df$County%in% ifelse(requesting_region=="San Joaquin County","San Joaquin", str_split(requesting_region, " ", simplify=T)[1]),], District)) ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>%
  select(-LOCALE_TYPE) %>%
  mutate(metric = ifelse(METRIC_MENU_NAME == "Students", str_squish(prettyNum(metric, big.mark = ",")), metric)) %>%
  mutate(metric = ifelse(METRIC_MENU_NAME == "Median Annual Earnings", paste0("$",str_squish(prettyNum(round(as.numeric(metric)), big.mark = ","))), metric)) %>%
  mutate(metric = ifelse(METRIC_MENU_NAME %in% c("Students", "Median Annual Earnings"), metric, paste0(round(as.numeric(metric)*100), "%"))) %>%
  rename(metric = METRIC_MENU_NAME, value = metric, academic_year = YEAR_ID, location = LOCALE_NAME, reportfor = PROGRAM) %>%
  mutate(file_index = "1", file_name = "datavista",
         type = "dunno",
         YoY = "dunno") %>%
  left_join(mutate(topdf, `TOP Code` = str_remove_all(`TOP Code`, "\\.")), by = c("reportfor" = "TOP Code")) %>%
  mutate(`TOP Code` = NULL,
         reportfor = NULL) %>%
  rename(reportfor = `TOP Code Title`) %>%
  select(file_index, metric, value, type, YoY, academic_year, location, reportfor, file_name)

north_or_south_cvml <- paste0(
  unique(colleges_df[colleges_df$College==str_replace_all(requesting_college, "\\|", " "), "Classification"]),
  " Central Valley/",
  unique(colleges_df[colleges_df$College==str_replace_all(requesting_college, "\\|", " "), "Classification"]),
  "ern Mother Lode"
) %>% str_squish()

