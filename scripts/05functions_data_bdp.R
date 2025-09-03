
a_degree_fix <- function(x, a=F, b=F) {
  fixed <- ifelse(b, str_to_lower(x), x)
  if(str_detect(fixed, pattern = "[Aa]ssociate's degree")) {
    fixed <- fixed %>% str_remove_all(pattern = "\\'s")
    a_or_and <- " an "
  } else {
    a_or_and <- " a "
  }
  a_or_and <- ifelse(!a, "", a_or_and)
  fixed <- paste0(a_or_and, fixed)
  fixed <- str_replace(fixed, pattern = "ged", replacement = "GED")
  fixed <- str_replace(fixed, pattern = "high school", replacement = "high school diploma")
  return(fixed)
}

a_degree_fix <- Vectorize(a_degree_fix)

#a_degree_fix("Associate's degree",a=T,b=T)
# Update year on August of every year

year_set_func <- function(){
  date <- Sys.Date()
  year_set <- year(ymd(date))
  # Raise year on August
  if(year_set < 8) {
    year_set <- year_set - 2
  } else {
    year_set <- year_set - 1
  }
  return(year_set)
}



repair_ps   <- normalizePath("repair_excel.ps1", winslash="\\", mustWork=TRUE)
macro_ps    <- normalizePath("run_vba_macro.ps1", winslash="\\", mustWork=TRUE)
template_xl <- normalizePath("vba_bdp.xlsm", winslash="\\", mustWork=TRUE)

exhibit_excel_fix <- function(tbl, file){
  #if(TRUE){
  if(file %in% skip_exhibit()){

    # Check if the file exists, if not create an empty .xlsx file
    #openxlsx::write.xlsx(list("Sheet" = data.frame()), file, overwrite=TRUE, asTable=TRUE)
    #openxlsx::write.xlsx("", "2_Exhibit.xlsx")
    #openxlsx::write.xlsx("", file, overwrite=TRUE, asTable=TRUE)
    openxlsx::write.xlsx(tbl, file, overwrite=TRUE, asTable=TRUE)
    full_output_filepath <- normalizePath(file, winslash="\\", mustWork=TRUE)

    # 1) Write the data to the .xlsx file
    openxlsx::write.xlsx(list("Sheet" = tbl), full_output_filepath, overwrite=TRUE, asTable=TRUE)

    # 2) Run repair_single.ps1 on the R-generated .xlsx file for Power Query metadata
    # This uses the 'repair_single.ps1' script.

    system2("powershell", args = c(
      "-NoProfile","-ExecutionPolicy","Bypass",
      "-File",          paste0('"', repair_ps, '"'), # Path to repair_xlsx_metadata.ps1
      "-FilePath",      paste0('"', full_output_filepath, '"') # Pass the XLSX path as -FilePath
    ), wait=TRUE)
    # 3) Trigger the VBA macro in the .xlsm template
    # This uses the script that 'macro_ps' points to.
    # THIS IS WHERE WE ADD THE -DataFilePath PARAMETER.
  } else {
    print(skip_exhibit())
  }
}

exhibit_word_doc <- function(file, macro) {
  if(file %in% skip_exhibit()){
    full_output_filepath <- normalizePath(file, winslash="\\", mustWork=TRUE)
    system2("powershell", args = c(
      "-NoProfile","-ExecutionPolicy","Bypass",
      "-File",          paste0('"', macro_ps, '"'), # <-- THIS IS YOUR MACRO RUNNER SCRIPT
      "-MacroName",     paste0('"', macro, '"'),
      "-WorkbookPath", paste0('"', template_xl, '"'),
      "-DataFilePath",  paste0('"', full_output_filepath, '"') # <-- NEW PARAMETER ADDED HERE
    ), wait=TRUE)

    # Give a moment before next (optional, but harmless)
    Sys.sleep(5)
  } else {
    print(skip_exhibit())
  }
}

fetch_soc_data <- function(soc_code) {
  base_url <- "https://services.onetcenter.org/ws/online/occupations/"
  username <- "coeccc_region_centra"
  password <- "8349uqy"

  endpoints <- list(
    summary = "summary",
    education = "summary/education",
    job_zone = "summary/job_zone"
  )

  results <- list()

  for (endpoint in names(endpoints)) {
    url <- paste0(base_url, soc_code, "/", endpoints[[endpoint]])
    response <- httr::GET(
      url = url,
      httr::authenticate(username, password, type = "basic")
    )
    if (response$status_code == 200) {
      results[[endpoint]] <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"), flatten = FALSE)
    } else {
      results[[endpoint]] <- list(error = TRUE, status_code = response$status_code, reason = response$reason)
    }
  }

  return(results)
}

digit_adder_onet <- function(digits) {

  length_digits <- length(digits)
  new_vec <- vector(mode="character", length = length_digits)
  for (i in seq_along(new_vec)){
    activate <- str_detect(digits[i], "\\.\\d{2}")
    if(!activate) {
      new_soc <- paste0(digits[i], ".00")
    } else {
      new_soc <- digits[i]
    }
    new_vec[i] <- new_soc
  }
  return(new_vec)
}

exhibit_doc_func <- function(exhibit_ft) {
  number <- str_pad(str_extract(deparse(substitute(exhibit_ft)), "\\d+"), side = "left", width = 2, pad = "0")
  document <- read_docx("bdp_template.docx") %>%
    body_add_flextable(exhibit_ft)
  #return(assign(paste0("exhibit_doc", number), document, envir = .GlobalEnv))
  return(print(document, paste0("exhibit_0", number, ".docx")))
}

typical_entry_level_is_bachelors_func <- function(data){
  typical_entry_level_is_bachelors_index <- which(str_detect(pull(data,which(str_detect(names(data), "Typical Entry Level Education"))), "Bachelor"))
  typical_entry_level_is_bachelors <- pull(data,2)[typical_entry_level_is_bachelors_index]
  if(length(typical_entry_level_is_bachelors)==0) {
    typical_entry_level_is_bachelors <- "no jobs with bachelors"
  }
  return(typical_entry_level_is_bachelors)
}

f_job_growth <- function(x){
  x %>%
    select(SOC, Description, names(x)[str_detect(names(x), pattern = "^(20[0-9]{2} Jobs)")], `Avg. Annual Openings`) %>%
    pivot_longer(
      cols = contains("Jobs"),   # Pivot the job columns (e.g., '2012 Jobs', '2013 Jobs', etc.)
      names_to = "Year",         # New column for the year
      names_pattern = "(\\d+)",  # Extract the year from the column names
      values_to = "Jobs_n"       # Job numbers in this column
    ) %>%
    distinct() %>%
    arrange(SOC, Year) %>%
    select(SOC, Description, Year, Jobs_n,`Avg. Annual Openings`) %>%
    rename(Annual_Openings = `Avg. Annual Openings`) %>%
    #mutate(Jobs_n = round(Jobs_n, 0)) %>%
    group_by(SOC) %>%
    mutate(perc_growth = ((Jobs_n - lag(Jobs_n))/lag(Jobs_n))*100) %>%
    ungroup()
}

f_job_growth_v2 <- function(x){
  x %>%
    select(SOC, Description, names(x)[str_detect(names(x), pattern = "^(20[0-9]{2} Jobs)")], `Avg. Annual Openings`) %>%
    pivot_longer(
      cols = contains("Jobs"),   # Pivot the job columns (e.g., '2012 Jobs', '2013 Jobs', etc.)
      names_to = "Year",         # New column for the year
      names_pattern = "(\\d+)",  # Extract the year from the column names
      values_to = "Jobs_n"       # Job numbers in this column
    ) %>%
    distinct() %>%
    arrange(SOC, Year) %>%
    select(SOC, Description, Year, Jobs_n,`Avg. Annual Openings`) %>%
    rename(Annual_Openings = `Avg. Annual Openings`) %>%
    mutate(Jobs_n = round(Jobs_n, 0)) %>%
    group_by(SOC) %>%
    #mutate(perc_growth = ((Jobs_n - lag(Jobs_n))/lag(Jobs_n))*100) %>%
    ungroup()
}

f_job_growth_v3 <- function(x){
  x %>%
    select(SOC, Description, names(x)[str_detect(names(x), pattern = "^(20[0-9]{2} Jobs)")], `Avg. Annual Openings`) %>%
    pivot_longer(
      cols = contains("Jobs"),   # Pivot the job columns (e.g., '2012 Jobs', '2013 Jobs', etc.)
      names_to = "Year",         # New column for the year
      names_pattern = "(\\d+)",  # Extract the year from the column names
      values_to = "Jobs_n"       # Job numbers in this column
    ) %>%
    distinct() %>%
    arrange(SOC, Year) %>%
    select(SOC, Description, Year, Jobs_n,`Avg. Annual Openings`) %>%
    rename(Annual_Openings = `Avg. Annual Openings`) %>%
    #mutate(Jobs_n = round(Jobs_n, 0)) %>%
    group_by(Year) %>%
    reframe(Jobs_n = sum(Jobs_n)) %>%
    mutate(perc_growth = ((Jobs_n - lag(Jobs_n))/lag(Jobs_n))*100) %>%
    ungroup()
}


random_soc_gen_f <- function(n=1){
  sample(all_soc_codes, n, T)
}

test_demand_data <- function(socs){
  all_ca_data[all_ca_data$SOC%in%socs,] %>% demand_func() %>% not_in_selected_region() %>% remove_added_rows() %>% suppressMessages()
}


average_living_wage <- function(x, family_composition = "Adult", char=T) {
  if (x != "All") {
    ca_set <- CA2024_SSS %>% filter(str_detect(Counties, x))
  } else {
    ca_set <- CA2024_SSS
  }
  p2 <- median(ca_set[[family_composition]])

  if(char==T){
    p2 <- round(p2, 2) %>% as.character()
    p22 <- str_split_1(p2, "\\.")[1]
    p23 <- str_split_1(p2, "\\.")[2] %>% str_pad(width = 2, side = "right", pad = "0", use_width = T)
    p2 <- paste0("$", p22, ".", p23)

  }
  return(p2)
}

wage_compare <- function(counties = cvml_counties_string, adult_or_infant = "Adult", compare_wage){
  wage_standard <- average_living_wage(counties, adult_or_infant, F)
  compare  <- (compare_wage - wage_standard)
  return(compare)
}



#wage_compare(cvml_counties_string, "Adult", 18)
#wage_compare("All", "Adult_Infant", 50)
#wage_compare("Fresno", "Adult_Infant", 30)

#number_rand <- runif(1, -10000, 10000)

#format_num_func(number_rand, "#", F, F)




