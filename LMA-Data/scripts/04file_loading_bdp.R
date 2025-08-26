# List files matching the criteria

file_path <- file.path(getwd(), "..", "data")
#orignal_wd <- "../LMA-Data"
orignal_wd <- "LMA-Data"
#setwd(orignal_wd)
xls_files <- list.files("../data", pattern = "\\.xls$")
xlsx_files <- list.files("../data", pattern = "\\.xlsx")

if (length(xls_files) > 0 & length(xlsx_files)==0){

  source_file <- "convert.ps1"
  destination_file <- "../convert.ps1" # One directory up from R's current working directory
  source_path_full <- normalizePath(source_file, winslash="\\", mustWork=TRUE)
  destination_path_full <- normalizePath(destination_file, winslash="\\", mustWork=FALSE) # Destination might not exist yet
  # Path to the PowerShell script that performs the move
  move_ps_script <- normalizePath("move_convert_script.ps1", winslash="\\", mustWork=TRUE)

  system2(
    "powershell.exe",
    args = c(
      "-NoProfile",
      "-ExecutionPolicy", "Bypass",
      "-File", paste0('"', move_ps_script, '"'),
      "-SourcePath", paste0('"', source_path_full, '"'),
      "-DestinationPath", paste0('"', destination_path_full, '"')
    ),
    stdout = TRUE, # Capture output for debugging
    stderr = TRUE, # Capture errors for debugging
    wait = TRUE
  )
  setwd("../")
  setwd("data")
  print(getwd())
  output <- system2(
    "powershell.exe",
    args = c("-NoProfile", "-ExecutionPolicy", "Bypass", "-File", "../convert.ps1"),
    stdout = TRUE, # Capture standard output
    stderr = TRUE # Capture standard error
  )
  cat(output)
  print(list.files())
  setwd("../")
  #setwd("../LMA-Data")
  setwd("LMA-Data")
  print(getwd())
} else {
  print(".xlsx files present")
}

demand_hires <- list.files(
  path = file_path,  # Assuming the files are in a "data" folder
  pattern = "^Occupation.*?(CVML|California).*\\.xlsx$",
  full.names = T,
  ignore.case = TRUE
)

find_problematic_columns <- function(df, pattern) {
  cols_with_issues <- vector("logical", length(names(df)))
  for (l in seq_along(names(df))) {
    cols_with_issues[l] <- any(str_detect(df[[l]], pattern))
  }
  return(na.omit(names(df)[cols_with_issues]))
}

soc_2018_definitions <- read_excel("soc_2018_definitions.xlsx",
                                   sheet = "2018 SOC", skip = 2)

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

#if(any(ncv_demand$SOC=="* Your selected institution is not in the selected region.", na.rm = T))

not_in_selected_region <- function(data){
  if(any(data$SOC=="* Your selected institution is not in the selected region.", na.rm = T)){
    data <- data[-which(data$SOC=="* Your selected institution is not in the selected region."),]
    return(data)
  } else {
    return(data)
  }
}

remove_added_rows <- function(table) {
  if (sum(tail(is.na(pull(table, 1)), 2)) == 2){
    fixed <- table[1:(nrow(table)-2),]
    return(fixed)
  } else {
    return(table)
  }
}

format_perc_func <- function(x, p=F){
  final_offer <- paste0(round(100*x, 0), "%")
  if(p) {final_offer <- paste0("(",final_offer,")")}
  return(final_offer)
}

CIPCode2020 <- read_csv("CIPCode2020.csv") #%>% mutate(CIPCode = str_pad(CIPCode, side = "right", width = 8, pad = "0"))

cvml_demand <- read_excel(demand_hires[str_detect(demand_hires, pattern = "CVML")], sheet = "Occs") %>% demand_func() %>% not_in_selected_region() %>% remove_added_rows() %>% suppressMessages()
cvml_demand <- cvml_demand[cvml_demand$SOC %in% SOC,]
ca_demand <- read_excel(demand_hires[str_detect(demand_hires, pattern = "California")], sheet = "Occs") %>% demand_func() %>% not_in_selected_region() %>% remove_added_rows() %>% suppressMessages()
ca_demand <- ca_demand[ca_demand$SOC %in% SOC,]

all_ca_data <- read_excel("Occupation_Table_All_Occupations_in_California_081825.xlsx", sheet = "Occs") %>% suppressMessages()

soc_code_titles_df <- pull(ca_demand, Description)[!is.na(pull(ca_demand, SOC))]
soc_title <- pull(ca_demand, Description)[!is.na(pull(ca_demand, SOC))]

if (two_or_more_soc == 1){
  jobs <- soc_title
  long_name_jobs_rule <- F
} else if (two_or_more_soc == 2){
  jobs <- paste0(soc_title[1], " and ", soc_title[2])
  long_name_jobs_rule <- F
  if(str_length(jobs) >= 50) {
    jobs <- str_to_lower(general_field)
    long_name_jobs_rule <- T
  }
} else if (two_or_more_soc > 2){
  jobs <- str_to_lower(general_field)
  long_name_jobs_rule <- T
} else {"error"}




demand_postings <- list.files(
  path = "../data",  # Assuming the files are in a "data" folder
  pattern = "^Job_Posting_Analytics.*\\.xlsx$",
  full.names = T,
  ignore.case = TRUE
)

advertised_salaries <- list.files(
  path = "../data",  # Assuming the files are in a "data" folder
  pattern = "^Job_Postings_Table.*\\.xlsx$",
  full.names = T,
  ignore.case = TRUE
)

# Read salary information from an Excel sheet, starting from the 5th row; convert "Observations" to numeric
#jp_salary <- read_excel(demand_postings[str_detect(demand_postings, pattern = region_acro)],
#    sheet = "Advertised Salary", skip = 5) %>%
#  mutate(Observations = as.numeric(Observations))

# Read job postings data from the specified Excel sheet, skipping the first row

for(jp_file in demand_postings) {

job_postings <-  read_excel(jp_file, sheet = "Parameters") %>% suppressMessages()
#job_postings <-  read_excel(demand_postings[1], sheet = "Parameters") %>% suppressMessages()
bachelors_preferred_filter <- length(which(str_detect(job_postings$Parameters, "Bachelor's")))
california_jp <- which(str_detect(job_postings$...2, "California"))

bachelors_preferred_filter <- bachelors_preferred_filter>0
california_present <- length(california_jp)>0

if(bachelors_preferred_filter & california_present) {
  bp_job_postings <- jp_file
}

if(california_present & !bachelors_preferred_filter)  {
  job_postings_all <- jp_file
}
}



ed_attainment_file <- list.files(
  #path = data_path,  # Assuming the files are in a "data" folder
  pattern = "^Educational Attainment.*\\.xlsx$",
  full.names = T,
  ignore.case = TRUE
)

supply_file <- list.files(
  #path = data_path,  # Assuming the files are in a "data" folder
  #pattern = "^Supply.*\\.xlsx$",
  pattern = "^Supply.*\\.xlsm$",
  full.names = T,
  ignore.case = TRUE
)

EducationalAttainment <- readxl::read_excel(ed_attainment_file,
                                            sheet = "Table 5.3", skip = 1)

#saveWorkbook(work_check_wb, "data_validation.xlsx", overwrite = TRUE)

EducationalAttainment_filtered <- EducationalAttainment %>%
  filter(!is.na(`2023 National Employment Matrix code`))  %>%
  filter(`2023 National Employment Matrix code` %in% SOC) %>%
  mutate(`2023 National Employment Matrix title` = str_remove_all(`2023 National Employment Matrix title`, pattern = "\\[1\\]"),
         `2023 National Employment Matrix title` = str_to_title(`2023 National Employment Matrix title`))

TOP_CIP_SOC_Current$`CIP Code` <- as.character(TOP_CIP_SOC_Current$`CIP Code`)
TOP_CIP_SOC_Current_supplementary$`CIP Code` <- as.character(TOP_CIP_SOC_Current_supplementary$`CIP Code`)
CIP_string <- unique(pull(TOP_CIP_SOC_Current_supplementary[TOP_CIP_SOC_Current_supplementary$`CIP Code`%in% CIP,], "CIP Code"))
cip_titles_v2 <- pull(TOP_CIP_SOC_Current_supplementary[which(TOP_CIP_SOC_Current_supplementary$`CIP Code` %in% CIP_string),], "CIP Title") %>% unique()
cip_codes_supplementary <- unique(TOP_CIP_SOC_Current_supplementary$`CIP Code`)


#CIP <- as.character(TOP_CIP_SOC_Current$`CIP Code`)
cc_supply_raw <- read_excel(supply_file, sheet = "Data_Supply CC Only")
ncc_supply_raw <- read_excel(supply_file, sheet = "Data_Other Ed Only")

ncc_supply <- read_excel(supply_file, sheet = "Data_Other Ed Only") %>%
  mutate(CIP = sprintf("%07.4f", as.numeric(as.character(CIP)))) %>%
  filter(CIP %in% str_split(params[["CIP"]], ", ", simplify = F)[[1]])

ncc_supply_cvml <- ncc_supply %>%
  filter(Regions == "Central")

ccc_supply <- read_excel(supply_file, sheet = "Data_Supply CC Only") %>%
  filter(`TOP6 or CIP` %in% str_remove(str_split(params[["TOP_supplementary"]], ", ", simplify = F)[[1]], "\\.")) %>%
  filter(`Community College Flag` == "Community College")

ccc_supply_cvml <- ccc_supply %>%
  filter(Regions == "Central", `Community College Flag` == "Community College")

CA2024_SSS <- readxl::read_excel("CA2024_SSS.xlsx",
                                 sheet = "SSW",
                                 skip = 8) %>%
  select(c(1:3))

CA2024_SSS <- readxl::read_excel("CA2024_SSS.xlsx",
                                 sheet = "SSW",
                                 skip = 8) %>% select(c(1:4))

names(CA2024_SSS) <- c("Counties", "tablenumber", "Adult", "Adult_Infant")

cvml_counties <- c(ncv_counties, scv_counties)


cvml_counties_string <- paste(cvml_counties, collapse = "|")

text_date <- format(Sys.Date(), "%B %Y")

cvml_suppy_completions <- ccc_supply_cvml  %>%
    select(`TOP6`,`Award Level`,County, `Econ Subregion`, starts_with("20")) %>%
  distinct() %>%
  pivot_longer(cols = starts_with("20"),
               values_to = "completions",
               names_to = "acadyr") %>%
  #group_by(acadyr) %>%
  summarize(completions = sum(completions, na.rm = T))

suppy_completions_college_counts <- ccc_supply_cvml  %>%
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

ncc_suppy_completions_college_counts_regional <- ncc_supply_cvml  %>%
  select(`Institution Name`, CIP,`Award Level`,County, `Econ Subregion`, starts_with("20")) %>%
  distinct() %>%
  pivot_longer(cols = starts_with("20"),
               values_to = "completions",
               names_to = "acadyr") %>%
  pull(`Institution Name`) %>%
  unique() %>%
  length()


supply_yr <- ccc_supply_cvml  %>%
  select(`TOP6`,`Award Level`,County, `Econ Subregion`, starts_with("20")) %>%
  distinct() %>%
  filter(str_detect(`Econ Subregion`, "Valley"))  %>%
  pivot_longer(cols = starts_with("20"),
               values_to = "completions",
               names_to = "acadyr") %>%
  group_by(acadyr) %>%
  summarize(completions = sum(completions, na.rm = T)) %>%
  ungroup()  %>%
  pull(acadyr)

supply_yr_start <- supply_yr %>%
  min() %>%
  str_extract("\\d{4}")

supply_yr_end <- supply_yr %>%
  max() %>% str_sub(6,7)

if(is.na(supply_yr_start)) {
  supply_yr_start <- ccc_supply_cvml %>%
    select(starts_with("20")) %>%
    distinct() %>%
    first() %>%
    names() %>%
    min() %>%
    str_extract("\\d{4}")

}

if(is.na(supply_yr_end)|str_length(supply_yr_end)==2) {
  supply_yr_end <- ccc_supply_cvml %>%
    select(starts_with("20")) %>%
    distinct() %>%
    first() %>%
    names() %>%
    max() %>%
    str_sub(6,7)
  supply_yr_end <- paste0("20", supply_yr_end)
}

topdf <-TOP_CIP_SOC_Current_supplementary[,c("TOP Code", "TOP Code Title")] %>%
  distinct()


top_supps <- paste0(
  TOP_CIP_SOC_Current_supplementary$`TOP Code Title`,
  " (",
  TOP_CIP_SOC_Current_supplementary$`TOP Code`,
  ")"
) %>% unique()



top_code_title_w_top_code <- paste0(top_code_title, " (", TOP, ")")

top_code_title_w_top_code_supplementary <-
  paste(
    unique(
      paste0(
        pull(TOP_CIP_SOC_Current_supplementary, `TOP Code Title`),
        " (",
        paste0(str_sub(TOP_CIP_SOC_Current_supplementary$`TOP Code`, 1, 4),
               ".",
               str_sub(TOP_CIP_SOC_Current_supplementary$`TOP Code`, 6, 7)),
        ")")
    ), collapse = ", ")


soc_code_titles_df <- pull(ca_demand, Description)[!is.na(pull(ca_demand, SOC))]

soc_code_titles_string <- str_replace_all(paste0(soc_code_titles_df, collapse = "\n"), "\\n", ", ")

soc_code_titles_w_soc_code <- paste0(pull(ca_demand, Description)[!is.na(pull(ca_demand, SOC))], " (", pull(ca_demand, SOC)[!is.na(pull(ca_demand, SOC))], ")")

soc_code_titles <- soc_code_titles_df


all_soc_codes <- unique(pull(soc_2018_definitions[soc_2018_definitions$`SOC Group`=="Detailed", ], 2))


soc_2digit <- paste0(unique(str_extract(SOC, "\\d{2}")), "-0000")
#soc_2digit <- paste0(unique(str_extract(SOC_tester, "\\d{2}")), "-0000")

soc_2digit_titles <- soc_2018_definitions %>%
  filter(str_detect(`SOC Code`, pattern = "\\-0000")) %>%
  filter(`SOC Code` %in% soc_2digit) %>%
  mutate(SOC_2digit_titles = paste0(`SOC Title`, " (", `SOC Code`, ")")) %>%
  pull(SOC_2digit_titles)


