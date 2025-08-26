#```{r formatting0_notes, chunk_progress=TRUE}
library(tidyverse)
library(kableExtra)
data_path <- "LMA Data"


TOP <- str_squish(str_split(params[["TOP"]], ",", simplify = T))
TOP_supplementary <- str_squish(str_split(params[["TOP_supplementary"]], ",", simplify = T))
CIP <- str_squish(str_split(params[["CIP"]], ",", simplify = T))
SOC <- str_squish(str_split(params[["SOC"]], ",", simplify = T))

#TOP <- c("1307.10", "1306.30")
#TOP <- c("1307.10", "1306.30", "1306.20", "1306.00", "1307.20")
#TOP_supplementary <- c("1307.10", "1306.30", "1306.20", "1306.00", "1307.20")
#TOP_supplementary <- c("1307.10", "1306.30")
#CIP <- params[["CIP"]]
#CIP <- unlist(str_split("19.0505 52.0905 12.0500 12.0505 12.0500", " "))

#SOC <- params[["SOC"]]
#"19.0505"
#"12.0500"
#"12.0503"
#"12.0505"
#"52.0905"


crosswalk <- list.files(pattern = "Crosswalk")
# Function to rename columns based on regex patterns
rename_columns <- function(data) {
  data %>%
    rename_with(
      ~ case_when(
        # Rename TOP Code columns
        str_detect(., regex("TOP.*Code", ignore_case = TRUE)) ~ "TOP Code",
        str_detect(., regex("TOP.*Title", ignore_case = TRUE)) ~ "TOP Code Title",

        # Rename CIP columns
        str_detect(., regex("CIP.*Code", ignore_case = TRUE)) ~ "CIP Code",
        str_detect(., regex("CIP.*Title", ignore_case = TRUE)) ~ "CIP Title",

        # Rename SOC columns
        str_detect(., regex("SOC.*Code", ignore_case = TRUE)) ~ "SOC Code",
        str_detect(., regex("SOC.*Title", ignore_case = TRUE)) ~ "SOC Title",

        # Keep original column name if no match
        TRUE ~ .
      )
    )
}

TOP_CIP_SOC_Current <- readxl::read_excel(
  crosswalk,
  sheet = "TOP-CIP-SOC"
)

# Apply the renaming function
TOP_CIP_SOC_Current <- rename_columns(TOP_CIP_SOC_Current) %>%
  filter(`TOP Code` %in% TOP) %>%
  filter(`CIP Code` %in% paste(str_split(CIP, pattern = ",", simplify = T), sep = ","))

#TOP_CIP_SOC_Current <- crosswalk(c("1307.10", "1306.30", "1306.20", "1306.00", "1307.20"))
#TOP_CIP_SOC_Current_supplementary <- TOP_CIP_SOC_Current #crosswalk(TOP_supplementary)

TOP_CIP_SOC_Current_supplementary <- readxl::read_excel(
  crosswalk,
  sheet = "TOP-CIP-SOC"
) %>%
  rename_columns() %>%
  filter(`TOP Code` %in% str_remove_all(TOP_supplementary, "\\.") |
           `CIP Code` %in% paste(str_split(CIP, pattern = ",", simplify = T), sep = ","))

top_code_title <- TOP_CIP_SOC_Current %>%
  select(`TOP Code`, `TOP Code Title`, `CIP Title`, `SOC Code`, `SOC Title`) %>%
  pull(`TOP Code Title`) %>%
  unique()

top_titles <- unique(TOP_CIP_SOC_Current$`TOP Code Title`)
#prompt_text <- "Summarize the following job fields into a group that accurately includes them all while excluding other fields that sound similar (like wildfire technicians). Only reply with the shortened string that can be used in a sentence within a template."

cip_titles <- unique(TOP_CIP_SOC_Current$`CIP Title`)
#soc_title <- unique(TOP_CIP_SOC_Current$SOC.Title)



#CIP <- TOP_CIP_SOC_Current$`CIP Code`
degree_title <- params[["degree_title"]]
general_field <- params[["general_field"]]
jobs <- params[["jobs"]]


#colleges_df[colleges_df$College==str_replace(params[["requesting_college"]], "\\|", " "), "College"]

#requesting_college <-unique(colleges_df[colleges_df$College==str_replace(params[["requesting_college"]], "\\|", " "), "College"])
requesting_college <- unique(colleges_df[colleges_df$College==params[["requesting_college"]], "College"])
#requesting_region <- paste0(unique(colleges_df[colleges_df$College==str_replace(params[["requesting_college"]], "\\|", " "), "County"]), " County")
requesting_region <- paste0(unique(colleges_df[colleges_df$College==params[["requesting_college"]], "County"]), " County")
region <- ifelse(unique(pull(colleges_df[colleges_df$College==requesting_college,], "Classification" ))== "South", "SCV/SML", "NCV/NML")
region_acro <- ifelse(str_detect(region, "S"), "SCV", "NCV")
district_acronym <- unique(colleges_df$DistrictAcronym[which(colleges_df$County==str_split(requesting_region, " ", simplify = T)[1])])[1]

econ_subregion_region_match <- function(region, econ_subregion){
  str_detect(econ_subregion, str_sub(region, 1, 3))
}

CIP_titles_code <- unique(paste(TOP_CIP_SOC_Current_supplementary$`CIP Code`, TOP_CIP_SOC_Current_supplementary$`CIP Title`, sep = "-"))

average_wage <- function(reg){
  round(sum(reg$`Avg. Annual Openings` * reg$`Avg. Hourly Earnings`)/sum(reg$`Avg. Annual Openings`),2)
}
