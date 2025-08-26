#```{r formatting3_commonobjects, chunk_progress=TRUE}
#top_code_title <- TOP_CIP_SOC_Current %>%
#  filter(`TOP Code` == str_remove(TOP, "\\.")) %>%
#  select(`TOP Code Title`) %>%
#  distinct() %>%
#  as.character()

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

#pull(ca_demand, Description)[!is.na(pull(ca_demand, SOC))]
#soc_code_titles_df

#soc_code_titles_df <- TOP_CIP_SOC_Current %>%
#  filter(``CIP Code`` %in% CIP &
#         `SOC.Title` != "Postsecondary Teachers") %>%
#  pull(`SOC.Title`) %>%
#  unique()

soc_code_titles_string <- str_replace_all(paste0(soc_code_titles_df, collapse = "\n"), "\\n", ", ")

soc_code_titles_w_soc_code <- paste0(pull(ca_demand, Description)[!is.na(pull(ca_demand, SOC))], " (", pull(ca_demand, SOC)[!is.na(pull(ca_demand, SOC))], ")")

#soc_code_titles_w_soc_code <- TOP_CIP_SOC_Current %>%
#  filter(``CIP Code`` %in% CIP &
#         `SOC.Title` != "Postsecondary Teachers") %>%
#  mutate(title_code = paste0(`SOC.Title`, " (", `SOC.Code` ,")")) %>%
# pull(title_code) %>%  # Extract the column as a vector
#  unique() %>%
#  paste(collapse = ", ")  # Collapse the vector into a single string

# Split the string into a vector
#soc_code_titles <- str_split(soc_code_titles_string, ", ")[[1]]
soc_code_titles <- soc_code_titles_df
# Create the R Markdown bullet list

e1 <- subregion_demand %>%  select(SOC, Description, `Pct. 25 Hourly Earnings`, `Typical Entry Level Education`, `Avg. Annual Openings`)


awards_converred <- EducationalAttainment_filtered %>%
  select(CC) %>%
  sum()

annual_job_openings <- tail(round(e1$`Avg. Annual Openings`, 0),1)
scv_living_wage



# Ensure this outputs a single value
annual_job_openings <- tail(round(e1$`Avg. Annual Openings`, 0), 1)

# Check the correctness of the following part, ensure a single value:
min_education_req <- as.character(e1 %>%
                                    filter(!is.na(SOC)) %>%
                                    slice(1) %>%
                                    pull(`Typical Entry Level Education`))

#percent_change <- paste0(round(last(scv_demand$`2023 - 2028 % Change`)*100,0), '%')
percent_change <- subregion_demand %>% select(ends_with("% Change")) %>% last()
#ave_annual_openings <- last(subregion_demand$`Avg. Annual Openings`)
ave_annual_openings <- round(last(subregion_demand$`Avg. Annual Openings`),0)

# add footnote of the requestor to
EducationalAttainment_filtered$`2023 National Employment Matrix code`

# Define the counties and colleges
scv_counties <- c("Fresno", "Inyo", "Kern", "Kings", "Madera", "Mono", "Tulare")
ncv_counties <- c("Amador", "Alpine", "Calaveras", "Mariposa", "Merced", "San Joaquin", "Stanislaus", "Tuolumne")

scv_colleges <- c("Madera", "Fresno", "Clovis", "Reedley", "Bakersfield", "Cerro Coso", "Porterville", "Sequoias", "Taft", "Lemoore", "Coalinga")
ncv_colleges <- c("San Joaquin Delta", "Columbia", "Modesto", "Merced")

# Create a dataframe
colleges_df <- data.frame(
  College = c(scv_colleges, ncv_colleges),
  FullName = c(
    "Madera Community College", "Fresno City College", "Clovis Community College", "Reedley College",
    "Bakersfield College", "Cerro Coso Community College", "Porterville College",
    "College of the Sequoias", "Taft College", "West Hills College Lemoore", "West Hills College Coalinga",
    "San Joaquin Delta College", "Columbia College", "Modesto Junior College", "Merced College"
  ),
  District = c(
    "State Center", "State Center", "State Center", "State Center",
    "Kern", "Kern", "Kern",
    "Sequoias", "West Kern",
    "West Hills", "West Hills",
    "San Joaquin Delta", "Yosemite", "Yosemite", "Merced"
  ),
  DistrictFullName = c(
    "State Center Community College District", "State Center Community College District",
    "State Center Community College District", "State Center Community College District",
    "Kern Community College District", "Kern Community College District", "Kern Community College District",
    "Sequoias Community College District", "West Kern Community College District",
    "West Hills Community College District", "West Hills Community College District",
    "San Joaquin Delta Community College District", "Yosemite Community College District",
    "Yosemite Community College District", "Merced Community College District"
  ),
  DistrictAcronym = c(
    "SCCCD", "SCCCD", "SCCCD", "SCCCD",
    "KCCD", "KCCD", "KCCD",
    "SCCD", "WKCCD",
    "WHCCD", "WHCCD",
    "SJDCCD", "YCCD", "YCCD", "MCCD"
  ),
  County = c(
    "Madera", "Fresno", "Fresno", "Fresno",
    "Kern", "Kern", "Tulare",
    "Tulare", "Kern",
    "Kings", "Fresno",
    "San Joaquin", "Tuolumne", "Stanislaus", "Merced"
  ),
  Classification = c(
    rep("South", length(scv_colleges)),
    rep("North", length(ncv_colleges))
  )
)
requestor_names <- colleges_df[colleges_df$County == str_remove(requesting_region, pattern = " County"),][1,]

