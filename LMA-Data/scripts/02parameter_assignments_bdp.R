#```{r formatting0_notes, chunk_progress=TRUE}

data_path <- "LMA-Data"

TOP <- str_squish(str_split(params[["TOP"]], ",", simplify = T))
TOP_supplementary <- str_squish(str_split(params[["TOP_supplementary"]], ",", simplify = T))
CIP <- str_squish(str_split(params[["CIP"]], ",", simplify = T))
SOC <- str_squish(str_split(params[["SOC"]], ",", simplify = T))

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

TOP_CIP_SOC_Current_original <- readxl::read_excel(
  #path = data_path,
  path = crosswalk,
  sheet = "TOP-CIP-SOC"
)

# Apply the renaming function
TOP_CIP_SOC_Current <- rename_columns(TOP_CIP_SOC_Current_original) %>%
  filter(`TOP Code` %in% TOP) %>% # This is the original method that operated under the assumption that TOP and CIP codes will be perfectly aligned
  filter(`CIP Code` %in% paste(str_split(CIP, pattern = ",", simplify = T), sep = ","))

TOP_CIP_SOC_Current_TOP_or_CIP <- rename_columns(TOP_CIP_SOC_Current_original) %>%
  filter((`TOP Code` %in% TOP) | (`CIP Code` %in% paste(str_split(CIP, pattern = ",", simplify = T), sep = ",")))

TOP_CIP_SOC_Current_supplementary <- readxl::read_excel(
  crosswalk,
  sheet = "TOP-CIP-SOC") %>%
  rename_columns() %>%
  #filter(`TOP Code` %in% str_remove_all(TOP_supplementary, "\\.") |
  filter(`TOP Code` %in% TOP_supplementary | `CIP Code` %in% paste(str_split(CIP, pattern = ",", simplify = T), sep = ","))

if(nrow(TOP_CIP_SOC_Current)==0) {
  TOP_CIP_SOC_Current <- TOP_CIP_SOC_Current_TOP_or_CIP
  TOP_AND_OR_CIP_MATCHING <- "TOP and CIP do not inner join"
} else {
  TOP_CIP_SOC_Current <- TOP_CIP_SOC_Current_TOP_or_CIP
  TOP_AND_OR_CIP_MATCHING <- "TOP and CIP match"
}

top_code_title <- rename_columns(TOP_CIP_SOC_Current_original) %>%
  filter(`TOP Code` %in% TOP) %>%
  select(`TOP Code`, `TOP Code Title`, `CIP Title`, `SOC Code`, `SOC Title`) %>%
  pull(`TOP Code Title`) %>%
  unique()

cip_titles <-
  rename_columns(TOP_CIP_SOC_Current_original) %>%
  filter(`CIP Code` %in% CIP) %>%
  select(`CIP Code`, `CIP Title`, `SOC Code`, `SOC Title`) %>%
  pull(`CIP Title`) %>%
  unique()

CIP_titles_code_supplementary <- rename_columns(TOP_CIP_SOC_Current_original) %>%
  filter(`CIP Code` %in% CIP) %>%
  mutate(CIP_title = paste0(`CIP Code`, '-', `CIP Title`)) %>%
  pull(CIP_title) %>%
  unique()

average_wage <- function(reg){
  round(sum(reg$`Avg. Annual Openings` * reg$`Avg. Hourly Earnings`)/sum(reg$`Avg. Annual Openings`),2)
}

