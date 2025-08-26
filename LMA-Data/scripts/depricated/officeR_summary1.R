#```{r officer_replace_summary1, chunk_progress=TRUE}
### Supply Gap


# Bind all the data frames together
combined_exhibit10 <- bind_rows(exhibit10_north, exhibit10_south)
combined_exhibit10_NCC <- bind_rows(exhibit10_NCC_North, exhibit10_NCC_South)

cc_supply_average_summary <- combined_exhibit10 %>%
  filter(College == paste(ifelse(str_sub(region, 1, 1)=="N", "North", "South"), "CVML")) %>%
  pull(last(names(combined_exhibit10))) %>%
  sum()

cc_supply_average_summary <- ifelse(is.na(cc_supply_average_summary), 0, cc_supply_average_summary)

ncc_supply_average_summary <- combined_exhibit10_NCC %>%
  filter(College == paste(ifelse(str_sub(region, 1, 1)=="N", "North", "South"), "CVML")) %>%
  pull(last(names(combined_exhibit10_NCC))) %>%
  sum()

ncc_supply_average_summary <- ifelse(is.na(ncc_supply_average_summary), 0, ncc_supply_average_summary)

CHANGEME_BOLD_NUMBER <- e3_2 %>%
  filter(Geography == ifelse(str_detect(region, pattern = "S"), "SCV/SML", "NCV/NML")) %>%
  rename('Annual Openings' = AnnualOpenings) %>%
  pull(`Annual Openings`)


regional_openings_summary <- e3_2 %>%
  filter(Geography == ifelse(str_detect(region, pattern = "S"), "SCV/SML", "NCV/NML")) %>%
  rename('Annual Openings' = AnnualOpenings) %>%
  pull(`Annual Openings`)



CHANGEME_REGION_COUNTY <- region
CHANGEME_ITALICS_JOBS <- jobs

#CHANGEME_BOLD_AWARD_NUMBER <- e10 %>%
#  filter(College == ifelse(str_detect(region, pattern = "S"), "South CVML", "North CVML")) %>%
#  pull(`3-Year<br>Award<br>Average`)

CHANGEME_BOLD_AWARD_NUMBER <- prettyNum(cc_supply_average_summary + ncc_supply_average_summary, big.mark = ",")

CHANGEME_BOLD_MORE_OR_LESS <- ifelse(CHANGEME_BOLD_NUMBER > (cc_supply_average_summary + ncc_supply_average_summary), "more", "less")

supply_gap_box <- ifelse(CHANGEME_BOLD_MORE_OR_LESS == "more", intToUtf8(254), intToUtf8(111))

### Living Wage
CHANGEME2_REGION2 <- region
CHANGEME2_BOLD_DOLLARS_LIVINGWAGE2 <- (ifelse(str_detect(region, "S"), scv_living_wage, ncv_living_wage) %>% str_remove_all(pattern = "\\$") %>% as.numeric())

ex4_pick <- if (str_detect(region, pattern = "S")) {
  exhibit4_scv
} else {
  exhibit4_ncv
}

ex4_pick1 <- ex4_pick %>%
  filter(str_detect(lev, "Entry")) %>%
  filter(earnings != 0) %>%
  select(Description, earnings) %>%
  mutate(Description = str_replace_all(Description, "\n", " "),
         earnings = paste0(" ($", str_pad(earnings, side = "right", width = 5, "0"), ")"),
         wages = paste0(Description, earnings)) %>%
  pull(wages) %>%
  paste(collapse = "|")

#prompt_living_wage <-   paste("Write a list of jobs by replacing the '|' character with either a ',' , an 'and', or a ', and' so that the jobs can easily be inserted #into a sentence. List which jobs have a higher entry-level hourly wage than the", CHANGEME_REGION_COUNTY, "hourly wage of", hourly,". Only responded with as few #characters as possible:", ex4_pick1, sep = " ")


gemini_api <- "AIzaSyA8hDaMYyUkHhb6XcWe5R_8YzKkl9S11Gw"
gemini.R::setAPI(gemini_api)

ex1_pick <- if (str_detect(region, pattern = "S")) {
  scv_demand_exhibit1
} else {
  ncv_demand_exhibit1
}

#CHANGEME_AWARDS <-
summary_1_table <- ex1_pick %>%
  filter(!str_detect(entry_earnings, "Insuff")) %>%
  mutate(Demand = as.numeric(str_remove_all(Demand, pattern = "NCV|NML|SCV|SML|<br>|:|/|,"))) %>%
  mutate(header = str_remove_all(header, pattern = "\\d{2}(-\\d{4})")) %>%
  mutate(weight = paste(round((Demand/sum(Demand))*100,0), "%")) %>%
  select(header, Demand, `Typical Entry Level Education`, CCAttainment,weight) %>%
  rename(jobs = header)
# Convert summary_1_table to a Markdown-like table for Gemini

readable_table <- summary_1_table %>%
  mutate_all(as.character) %>%  # Ensure all columns are character for formatting
  mutate_all(~ str_replace_all(., "\\|", "/")) %>%  # Replace any | to avoid breaking the table
  select(jobs, Demand, `Typical Entry Level Education`, CCAttainment, weight) %>%  # Ensure correct order
  mutate_all(str_trim) %>%  # Remove unnecessary spaces
  {
    data <- .  # Assign the pipeline's output to a named variable
    header_row <- paste("|", paste(names(data), collapse = " | "), "|")
    separator_row <- paste("|", paste(rep("---", ncol(data)), collapse = " | "), "|")
    data_rows <- apply(data, 1, function(row) paste("|", paste(row, collapse = " | "), "|"))
    c(header_row, separator_row, data_rows)
  } %>%
  paste(collapse = "\n")  # Join rows with line breaks

# Add table to the Gemini prompt
prompt <- paste0(
  "The following table lists jobs with their associated demand, typical entry-level education, community college attainment, and weight in the total demand. ",
  "Based on this information, determine whether future employees require a community college level education. ",
  "Respond in ONLY 1 sentence, formatted like the following: 'The majority of annual openings (XX%) for the (text written number) occupations studied in this report typically require some college, no degree':\n\n",
  readable_table
)
library(gemini.R)
cat(prompt)  # Print prompt for verification

Education_endorsement_part_1 <-
  if (use_ai) {
    str_remove_all(gemini(prompt), "\n")
  } else {
    "filler"
  }

#Education_endorsement_part_1 <- "filler"

# Convert summary_1_table to a Markdown-like table for Gemini
readable_table2 <- summary_1_table %>%
  mutate_all(as.character) %>%  # Ensure all columns are character for formatting
  mutate_all(~ str_replace_all(., "\\|", "/")) %>%  # Replace any | to avoid breaking the table
  select(jobs, CCAttainment, weight) %>%  # Ensure correct order
  mutate_all(str_trim) %>%  # Remove unnecessary spaces
  {
    data <- .  # Assign the pipeline's output to a named variable
    header_row <- paste("|", paste(names(data), collapse = " | "), "|")
    separator_row <- paste("|", paste(rep("---", ncol(data)), collapse = " | "), "|")
    data_rows <- apply(data, 1, function(row) paste("|", paste(row, collapse = " | "), "|"))
    c(header_row, separator_row, data_rows)
  } %>%
  paste(collapse = "\n")  # Join rows with line breaks

prompt2 <- paste0(
  "The following table lists jobs with their associated demand, community college attainment, and weight in the total demand. ",
  "Complete the following sentence given the following table: 'Between (minimum %) (min job) and (maximum %) (max job) of the workers in the field have completed some college or an associate degree as their highest level of education.`':\n\n",
  " ONLY RESPOND WITH OUTPUT SENTENCE."
  ,readable_table2
)


Education_endorsement_part_2 <-
  if (use_ai) {
    str_remove_all(gemini(prompt2), "\n")
  } else {
    "filler"
  }

prompt_living_wage <-   paste("List which jobs have a higher entry-level hourly wage than the", CHANGEME_REGION_COUNTY, "hourly wage of", hourly, ex4_pick1, "Respond with only 1 or 2 sentences", sep = " ")


if (use_ai) {
  # Use the Claude API to get the living wage jobs
  CHANGEME2_ITALICS_JOBS2 <- tryCatch({
    call_claude_api_for_table(
      prompt = prompt_living_wage,
      model = "claude-3-opus-20240229",
      max_tokens = 200
    )
  }, error = function(e) {
    paste("API call error:", e$message)
  })
} else {
  # Fallback to a placeholder if AI is not used
  CHANGEME2_ITALICS_JOBS2 <- "filler"
}

#CHANGEME2_ITALICS_JOBS2 <- "filler"


lw_decision <- ex4_pick %>%
  filter(str_detect(lev, "Entry")) %>%
  filter(earnings != 0) %>%
  select(Description, earnings) %>%
  mutate(living_wage = (ifelse(str_detect(region, "S"), scv_living_wage, ncv_living_wage) %>% str_remove_all(pattern = "\\$") %>% as.numeric()),
         compare = ifelse(earnings > living_wage, "yes", "no")) %>%
  pull(compare) %>%
  unique() %>%
  paste(collapse = "_")


CHANGEME2_BOLD_ABOVE_OR_BELOW2 <- if(lw_decision == "yes"){
  "above"
} else if(lw_decision == "no") {
  "below"
} else {
  "its complicated"
}

#<- CHANGEME2_ITALICS_JOBS2
### Education


library(tidyverse)
CHANGEME_AWARDS <- ex1_pick %>%
  filter(!str_detect(entry_earnings, "Insuff")) %>%
  select(header, `Typical Entry Level Education`, CCAttainment) %>%
  pull(`Typical Entry Level Education`) %>%
  unique()


ed_level_award_count <- ex1_pick %>%
  filter(!str_detect(entry_earnings, "Insuff")) %>%
  select(header, `Typical Entry Level Education`, CCAttainment) %>%
  group_by(`Typical Entry Level Education`) %>%
  count() %>%
  ungroup() %>%
  arrange(desc(n))


ed_level_award_count

collapse_items <- function(x) {
  str_flatten(x, collapse = "; ", last = "; and ")
}

award_type_listed <- list()
for(awards_types in pull(ed_level_award_count, `Typical Entry Level Education`)) {
line_awards <- str_remove_all(pull(ex1_pick[ex1_pick$`Typical Entry Level Education`==awards_types, ], "header"), " \\d{2}\\-\\d{4}")
award_type_listed[[awards_types]] <- collapse_items(line_awards)
}

#prompt_summary_ed_1 <- paste(
#  "These are entry-level education levels for several jobs in  the", region, ". Only respond with a sentence of the Typical Entry Level Education that people usually have when entering #in these jobs:",
#  (ex1_pick[,c(1,5,6)]  %>% rename(`Some College or Associates Degree` = CCAttainment) %>% capture.output() %>% paste(collapse = "\n"))
#  ,sep = " ")



#typical_education_summary_1 <- tryCatch({
#  call_claude_api_for_table(
#    prompt = prompt_summary_ed_1,
#    model = "claude-3-opus-20240229",
#    max_tokens = 100
#  )
#}, error = function(e) {
#  paste("API call error:", e$message)
#})

#typical_education_summary_1

prompt_summary_ed_2 <- paste(
  "These are entry-level education levels for several jobs in  the", region, ".
  Only respond with a sentence describes the typical entry level education for these jobs.
  Order the sentence with the most common entry-level education to the least common and separate using the listing using semicolon.
  Keep the job names the same.
  Begin with 'The Bureau of Labor Statistics (BLS) lists a [education level] for [number word] out of ", xfun::n2w(length(SOC)), ":",
  paste(paste0(ex1_pick$header, " (", ex1_pick$`Typical Entry Level Education`, ")"), collapse = "|"),sep = " ")
#(ex1_pick[,c(1,5)]   %>% capture.output() %>% paste(collapse = "\n"))
#rename(`Some College or Associates Degree` = CCAttainment) %>% capture.output() %>% paste(collapse = "\n"))

CHANGEME_AWARDS
ex1_pick$`Typical Entry Level Education`
total_demand_count <- sum(as.numeric(str_extract_all(str_extract_all(ex1_pick$Demand, paste0(region, "\\:<br> \\d+")), "\\d+")))
highest_demand_count_ind <- which.max(as.numeric(str_extract_all(str_extract_all(ex1_pick$Demand, paste0(region, "\\:<br> \\d+")), "\\d+")))
highest_demand_count <- as.numeric(str_extract_all(str_extract_all(ex1_pick$Demand, paste0(region, "\\:<br> \\d+")), "\\d+"))[highest_demand_count_ind]
demand_ed_perc <- round(highest_demand_count/total_demand_count)*100

#ex1_pick$Demand[highest_demand_count_ind] <- paste0("(", demand_ed_perc, "%)")

#ed_comments_raw2 <- as_paragraph(
#  as_chunk("Comments:", prop = body_text_style_italic), # Use your italic style
#  as_chunk(" The typical entry-level education for ", prop = body_text_style),
#  as_chunk(general_field, prop = body_text_style_italic),
#  as_chunk("-related occupations is a ", prop = body_text_style),
#  as_chunk(pull(e_compare_text, 1), prop = body_text_style), # Apply style
#  as_chunk(" (about ", prop = body_text_style),
#  as_chunk(paste0(100 * pull(e_compare_text, 2), "%"), prop = body_text_style), # Apply style
#  as_chunk("). Additionally, ", prop = body_text_style),
#  as_chunk(paste0("between ", paste(range(summary_1_table$CCAttainment), collapse = " and ")), prop = body_text_style_bold), # Apply style
#  as_chunk(" have completed some college or an associate degree as their highest level of education.", prop = body_text_style_bold)
#)

typical_education_summary_2_fix <-
  as_paragraph(
  as_chunk(
    paste0("The Bureau of Labor Statistics (BLS) lists a ", pull(ed_level_award_count[1,], `Typical Entry Level Education`), " as the typical entry-level education for ")
    ,props = body_text_style),

  if (1 == nrow(ed_level_award_count)) {

    as_chunk(paste0("all of the "),props =  body_text_style)
    as_chunk(jobs, props =  body_text_style_italic)

  } else {
    as_chunk(
      paste0(
      xfun::n2w(ed_level_award_count[1,]$n),
      " out of the ",
      xfun::n2w(length(SOC)),
      " "), props = body_text_style)

    as_chunk(jobs, props =  body_text_style_italic)
    as_chunk(
      paste0(
      ": ", award_type_listed[[1]],". ",
      "The occupation ", award_type_listed[[2]], " typically requires ", pull(ed_level_award_count[2,], `Typical Entry Level Education`)
      ,props =  body_text_style)
    )
  }
)

typical_education_summary_2_fix <- paste0(
  "The Bureau of Labor Statistics (BLS) lists a ",
  pull(ed_level_award_count[1,], `Typical Entry Level Education`),
  " as the typical entry-level education for ",

if (1 == nrow(ed_level_award_count)) {
  paste0("all of the ", jobs)
} else {
  paste0(
  xfun::n2w(ed_level_award_count[1,]$n),
  " out of the ",
  xfun::n2w(length(SOC)),
  " ",
  jobs,
  ": ", award_type_listed[[1]],". ",
  "The occupation ", award_type_listed[[2]], " typically requires ", pull(ed_level_award_count[2,], `Typical Entry Level Education`)
  )
}
)

if (use_ai) {
  # Use the Claude API to get the typical education summary
  typical_education_summary_2 <- tryCatch({
    call_claude_api_for_table(
      prompt = prompt_summary_ed_2,
      model = "claude-3-opus-20240229",
      max_tokens = 400
    )
  }, error = function(e) {
    paste("API call error:", e$message)
  })
} else {
  # Fallback to a placeholder if AI is not used
  typical_education_summary_2 <- "filler"
}


CHANGEME3_BOLD_PERC <- paste(typical_education_summary_2, "Between", range(ex1_pick$CCAttainment)[1], "to", range(ex1_pick$CCAttainment)[2], "of workers in the field have completed some college or an associate degree as their highest level of education. \n")
CHANGEME_EDUC <- dplyr::pull(ex1_pick, `Typical Entry Level Education`)[highest_demand_count_ind][highest_demand_count_ind]


#CHANGEME3_BOLD_PERC <-

#education_comments <- paste(typical_education_summary_1,typical_education_summary_2, sep = " ")

