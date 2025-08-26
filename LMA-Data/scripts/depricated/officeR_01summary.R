#```{r officer_01summary, chunk_progress=TRUE}
#### Text

min_wage_summary_figure <- subregion_demand %>%
  prepare_data() %>%
  filter(str_detect(lev, "Entry-Level")==T) %>%
  pull(earnings)

min_wage_range <- paste0("between $", str_pad(range(min_wage_summary_figure[!is.na(min_wage_summary_figure)])[1], "right", width = 5, "0"), " and $", str_pad(range(min_wage_summary_figure[!is.na(min_wage_summary_figure)])[2], "right", width = 5, "0"))

text_h2_1 <- "Demand:"
text_h2_2 <- "Supply:"

text_h3_1 <- "Exhibit 1: Labor Market Endorsement Summary"



# Data from SCV Demand

N_S_CVML <- function(x){
  if(x == "N"){
    return("NCV/NML")
  } else if(x == "S"){
    return("SCV/SML")
  } else {
    return("Central")
  }
}
jobs_increase_or_decrease <- ifelse(last(pull(subregion_demand, ends_with("% Change")))>0, "increase ", "decrease ")
# percent_change <-
d_b_1 <- paste0("The number of jobs related to ",
                jobs,
                " is projected to ", jobs_increase_or_decrease,
                paste0(abs(round(tail(pull(subregion_demand, ends_with("% Change")),1),2))*100, "%"),
                " through ", str_split(names(subregion_demand)[4], " ")[[1]][1],
                c(if(jobs_increase_or_decrease == "increase"){
                paste0(", equating to ",
                prettyNum(ave_annual_openings, big.mark = ","),

                " annual job openings",
                " (",
                N_S_CVML(str_sub(area_direction, 1,1)),
                 ")."
                )
                } else {
                  paste0(
                ". However, there will be ",
                prettyNum(ave_annual_openings, big.mark = ","),
                  " annual job openings ",
                  "(",
                  N_S_CVML(str_sub(area_direction, 1,1)),
                  ")."
                  )
                }
                )
                )

# Data from jp_salary:

#jp_salary <- read_excel(demand_postings[str_detect(demand_postings, pattern = region_acro)],
#    sheet = "Advertised Salary", skip = 4) %>%
#  mutate(Observations = as.numeric(Observations))
#AS <-  as.character(jp_salary$`Advertised Salary`)
#min_hourly_wage <- (((AS %>%
#  min() %>%
#  str_split(pattern = "\\-"))[[1]][2] %>%
#  str_remove('\\,') %>%
#  str_remove('\\$') %>%
#  as.numeric())/2080) %>%
#  round(2)

d_b_2 <- paste0( "The entry-level hourly wages for ", CHANGEME2_ITALICS_JOBS2)
#jobs,
#" is ",
#min_wage_range,
#" in ",
#region,
#", which is ",
#ifelse(range(min_wage_summary_figure[!is.na(min_wage_summary_figure)])[1] >  as.numeric(str_remove(scv_living_wage, "\\$")), "above", "less than "),
#" the living wage of ",
#scv_living_wage,
#".")

d_b_3 <-paste0(
  "There were ",
  prettyNum(unique_job_postings, big.mark = ","),
  " online job postings for ",
  general_field,
  " over the past 12 months. The highest number of postings were for ",
  most_posted_job,
  ".")

#d_b_4 <- typical_education_summary_2
d_b_4 <- typical_education_summary_2_fix

#  paste0(
#  "The typical entry-level education for ",
#  general_field,
#  " is a ",
#  min_ed,
#  ".")

#typical_entry_level_education <- unique(subregion_demand$`Typical Entry Level Education`[!is.na(subregion_demand$`Typical Entry Level Education`)])


d_b_5 <- paste0(
  "Between ",
  paste0(paste(range(exhibit9_2[exhibit9_2$ed_levels == "Some College or Associate Degree", "per"]), collapse = "% to "), "%"),
  " of workers in the field have completed ",
  "some college or an associate degree",
  " as their highest level of education."
)

avg_ncc_awards <- ncc_supply %>%
  filter(Regions == "Central") %>%
  select(`Institution Name`, `CIP with Title`,County, `Econ Subregion`, starts_with("20")) %>%
  distinct() %>%
  pivot_longer(cols = starts_with("20"),
               names_to = "acadyr",
               values_to = "awards") %>%
  filter(awards > 0) %>%
  group_by(acadyr) %>%
  summarize(sum = sum(awards),
            ave = mean(awards),
            idk = n()) %>%
  ungroup() %>%
  summarize(total_yrs = sum(idk),
            total = sum(sum),
            avg = total/ total_yrs,
            avg_all = total/3) %>%
  select(avg_all) %>%
  round() %>%
  as.character()

avg_ncc_awards_years <- ncc_supply %>%
  #filter(Regions == "Central") %>%
  select(`Institution Name`, `CIP with Title`,County, `Econ Subregion`, starts_with("20")) %>%
  distinct() %>%
  pivot_longer(cols = starts_with("20"),
               names_to = "acadyr",
               values_to = "awards") %>%
  filter(awards > 0) %>%
  pull(acadyr) %>%
  range() %>%
  paste(collapse = " to ")

median_salary_range <- SWFP %>%
  #filter(location == str_split(requesting_region, " ", simplify = T)[1] &
  #filter(str_detect(location, "South") &
  filter(str_detect(location, str_sub(str_split(region, " ", simplify = T)[1], 1, 3)) &
           reportfor == top_code_title &
           metric == "Median Annual Earnings") %>%
  group_by(metric) %>%
  filter(academic_year == max(academic_year)) %>%
  pull(value) %>%
  prettyNum(big.mark = ",")

median_salary_range_year <- SWFP %>%
  #filter(location == str_split(requesting_region, " ", simplify = T)[1] &
  filter(str_detect(location, str_sub(str_split(region, " ", simplify = T)[1], 1, 3)) &
           reportfor == top_code_title &
           metric == "Median Annual Earnings") %>%
  group_by(metric) %>%
  filter(academic_year == max(academic_year)) %>%
  pull(academic_year)

#s_b_1 <- paste0(
#   "There were ",
#   suppy_completions,
#   " awards conferred by ",
#   suppy_completions_college_counts,
#   " Community Colleges in ",
#   region,
#   " from ",
#   supply_yr_start,
#   "to ",
#   supply_yr_end,
#   ".")

average_summary_completions_CC <- if(str_sub(region, 1,1)=="N"){
  sum(pull(ccc_supplyn, which(str_detect(names(ccc_supplyn), "Average"))))
}else{
  sum(pull(ccc_supplys, which(str_detect(names(ccc_supplys), "Average"))))
}

average_summary_completions_nCC <- round(sum(pull(filter(ncc_supply, str_detect(`Econ Subregion`, region)), which(str_detect(names(ncc_supply), "Average")))))


#s_b_1 <- paste0("Between ", str_sub(supply_yr_start, 1, 4), " and ", str_sub(supply_yr_end, 1, 4), ", there were ", suppy_completions, " awards conferred by ", suppy_completions_college_counts, " community colleges in the ", region, " subregion.")
s_b_1 <- paste0("Between ", str_sub(supply_yr_start, 1, 4), " and ", str_sub(supply_yr_end, 1, 4), ", there were an average of ", prettyNum(average_summary_completions_CC, big.mark = ","), " awards conferred by ", suppy_completions_college_counts_regional, " community colleges in the ", CHANGEME_REGION_COUNTY, " subregion.")


#s_b_2 <- paste0("Between ", str_sub(paste(str_split(supply_yr_start, pattern = "-", simplify = T) %>% as.numeric() -1, collapse = "-"), 1, 4), " and ", str_sub(paste(str_split(supply_yr_end, pattern = "-", simplify = T) %>% as.numeric() -1, collapse = "-"), 1, 4), ", there were ", avg_ncc_awards, " awards conferred by non-community college institutions for relevant programs.")
#s_b_2 <- paste0("Between ", str_sub(paste(str_split(supply_yr_start, pattern = "-", simplify = T) %>% as.numeric() -1, collapse = "-"), 1, 4), " and ", str_sub(paste(str_split(supply_yr_end, pattern = "-", simplify = T) %>% as.numeric() -1, collapse = "-"), 1, 4),
#                ", there were ", avg_ncc_awards, " awards conferred by non-community college institutions for relevant programs.")

s_b_2 <- paste0("Between ", str_sub(paste(str_split(supply_yr_start, pattern = "-", simplify = T) %>% as.numeric() -1, collapse = "-"), 1, 4), " and ", str_sub(paste(str_split(supply_yr_end, pattern = "-", simplify = T) %>% as.numeric() -1, collapse = "-"), 1, 4),
                ", there were an average of ", average_summary_completions_nCC, " awards conferred by ", ncc_suppy_completions_college_counts_regional, " non-community college institutions for relevant programs.")

#s_b_2 <- paste0(
#   "Non-community college institutions conferred an average of ",
#   avg_ncc_awards,
#   " awards from ",
#   paste(str_split(supply_yr_start, pattern = "-", simplify = T) %>% as.numeric() -1, collapse = "-"),
#   " to ",
#    paste(str_split(supply_yr_end, pattern = "-", simplify = T) %>% as.numeric() -1, collapse = "-"))

s_b_3 <- paste0(
  region,
  " community college students that exited ",
  top_code_title,
  " programs in the ",
  median_salary_range_year,
  " had a median annual wage of ",
  median_salary_range,
  " after exiting the program and ",
  living_wage_per,
  " attained the regional living wage.")


s_b_4 <- paste0(
  "Throughout the ",
  region,
  ", ",
  job_closely_related,
  " of ",
  general_field,
  " students that exited their program in ",
  max(pull(unique(SWFP[SWFP$metric== "Job Closely Related to Field of Study", "academic_year"]))),
  " reported that they are working in a job closely related to their field of study.")

# Demand bullet points
demand_bullets <- c(
  d_b_1,
  d_b_2,
  d_b_3,
  d_b_4,
  d_b_5
)

# Supply bullet points
supply_bullets <- c(
  s_b_1,
  s_b_2,
  s_b_3,
  s_b_4
)

#### Formatting

#img_size_1 <- adjust_image_size("exhibit1.png")

#### Create Components

#doc <- read_docx()

# In the #### Create Components section, replace the body_fpar_1 line with:


body_fpar_1 <-
  fpar(
    ftext("Exhibit 1 lists the occupational demand, supply, typical entry-level education, and educational attainment for ", prop = body_text_style),
    ftext(jobs, prop = fp_text(italic = TRUE, font.size = 11, font.family = "Tw Cen MT")),
    ftext(".", prop = body_text_style_italic)
  )

header3_fpar1 <- fpar(ftext(text_h3_1, H3),  fp_p = fp_par(text.align = "center"))
#img_exhibit1 <- fpar(external_img("exhibit1.png", width = img_size_1$width, height = img_size_1$height), fp_p = fp_par(text.align = "center"))

header2_fpar1 <- fpar(ftext(text_h2_1, H2))
header2_fpar2 <- fpar(ftext(text_h2_2, H2))

#### CONSTRUCT DOCUMENT

doc1 <- read_docx("lma_blank_template.docx") %>%
  body_add_par("", style = "Normal") %>%
  body_add_fpar(body_fpar_1) %>%
  body_add_par("", style = "Normal") %>%
  body_add_fpar(header3_fpar1) %>%
  body_add_flextable(exhibit1_2_cleaned_ft) %>%
  #body_add_fpar(img_exhibit1) %>%
  body_add_fpar(header2_fpar1) %>%
  body_add_par("", style = "Normal")


# Add Demand bullet points
for (bullet in demand_bullets) {
  doc1 <- doc1 %>%
    body_add_par(bullet, style = "List Paragraph")
}

doc1 <- doc1 %>%
  body_add_par("", style = "Normal") %>%
  body_add_fpar(header2_fpar2) %>%
  body_add_par("", style = "Normal")

# Add Supply bullet points
for (bullet in supply_bullets) {
  doc1 <- doc1 %>%
    body_add_par(bullet, style = "List Paragraph")
}

doc1 <- doc1 %>%
  body_add_par("", style = "Normal")

# Save the document
print(doc1, target = "officeR_01summary.docx")
#rm(doc)
