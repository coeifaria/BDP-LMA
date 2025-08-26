#```{r officer_06_2cccsoutcomes, chunk_progress=TRUE}
#rm(doc7)
#ex12_1
#top_code_title
#### Text
LOCALE_NAME_NORTH_OR_SOUTH_MOTHERLODE <-pull(filter(distinct(swfp_stacked[swfp_stacked$LOCALE_TYPE=="Microregion","LOCALE_NAME"]), str_detect(LOCALE_NAME, str_sub(north_or_south_cvml, 1, 4))), LOCALE_NAME)
earnings_year <- function(x, region_area = requesting_county, top_code_title_number = top_code_title[1]) {
  result <- tryCatch({
    filtered_data <- x %>%
      filter(reportfor == top_code_title_number) %>%
      filter(location == region_area) %>%
      filter(str_detect(metric, pattern = "Median Annual Earnings")) %>%
      rename(academicYear = academic_year)

    if (nrow(filtered_data) == 0 || all(is.na(filtered_data$academicYear))) {
      return(data.frame(academicYear = NA, value = 0))
    } else {
      max_year <- max(filtered_data$academicYear, na.rm = TRUE)
      final_data <- filtered_data %>%
        filter(academicYear == max_year) %>%
        select(academicYear, value)

      if (nrow(final_data) == 0) {
        return(data.frame(academicYear = NA, value = 0))
      } else {
        return(final_data)
      }
    }
  }, error = function(e) {
    return(data.frame(academicYear = NA, value = 0))
  })

  return(result)
}

closely_related_job <- function(x, region_area, top_code_title_number) {
  result <- tryCatch({
    filtered_data <- x %>%
      filter(reportfor == top_code_title_number) %>%
      filter(location == region_area) %>%
      filter(str_detect(metric, pattern = "Related")) %>%
      rename(academicYear = academic_year)

    if (nrow(filtered_data) == 0 || all(is.na(filtered_data$academicYear))) {
      return(data.frame(academicYear = NA, value = 0))
    } else {
      max_year <- max(filtered_data$academicYear, na.rm = TRUE)
      final_data <- filtered_data %>%
        filter(academicYear == max_year) %>%
        select(academicYear, value)

      if (nrow(final_data) == 0) {
        return(data.frame(academicYear = NA, value = 0))
      } else {
        return(final_data)
      }
    }
  }, error = function(e) {
    return(data.frame(academicYear = NA, value = 0))
  })

  return(result)
}

#living_wage <- function(SWFP){
#  SWFP %>%
#    filter(disagg == "Overall" &
#             str_detect(title, "Living Wage")==T &
#             !is.na(perc)) %>%
#    filter(academicYear == max(academicYear)) %>%
#    select(perc) %>%
#    mutate(perc = paste0("(", round(perc*100, 0), "%)")) %>%
#    as.character()
#}

living_wage <- function(x, region_area, top_code_title_number) {
  result <- tryCatch({
    filtered_data <- x %>%
      filter(reportfor == top_code_title_number) %>%
      filter(location == region_area) %>%
      filter(str_detect(metric, pattern = "Living")) %>%
      rename(academicYear = academic_year)

    if (nrow(filtered_data) == 0 || all(is.na(filtered_data$academicYear))) {
      return(data.frame(academicYear = NA, value = 0))
    } else {
      max_year <- max(filtered_data$academicYear, na.rm = TRUE)
      final_data <- filtered_data %>%
        filter(academicYear == max_year) %>%
        select(academicYear, value)

      if (nrow(final_data) == 0) {
        return(data.frame(academicYear = NA, value = 0))
      } else {
        return(final_data)
      }
    }
  }, error = function(e) {
    return(data.frame(academicYear = NA, value = 0))
  })

  return(result)
}

swfp_students_andyear <- function(x, region_area, top_code_title_number) {
  result <- tryCatch({
    filtered_data <- x %>%
      filter(reportfor == top_code_title_number) %>%
      filter(location == region_area) %>%
      filter(str_detect(metric, pattern = "Students")) %>%
      rename(academicYear = academic_year)

    if (nrow(filtered_data) == 0 || all(is.na(filtered_data$academicYear))) {
      return(data.frame(academicYear = NA, value = 0))
    } else {
      max_year <- max(filtered_data$academicYear, na.rm = TRUE)
      final_data <- filtered_data %>%
        filter(academicYear == max_year) %>%
        select(academicYear, value)

      if (nrow(final_data) == 0) {
        return(data.frame(academicYear = NA, value = 0))
      } else {
        return(final_data)
      }
    }
  }, error = function(e) {
    return(data.frame(academicYear = NA, value = 0))
  })

  return(result)
}

text_h2 <- "Community College Student Outcomes:"

text_b_2 <- paste0(
  requestor_names$DistrictAcronym,
  " students that exited ",
  paste(top_code_title, collapse = " and "),
  " programs in the ",
  pull(swfp_students_andyear(SWFP, region_area = requesting_district, top_code_title[1]), "academicYear"),
  " academic year had")



text_b_2_1 <- " higherorlower "
text_b_2_2 <- paste0("median annual earnings (",
                     prettyNum(earnings_year(swfp_stacked, requesting_county, top_code_title[1])[2],big.mark = ","),
                     ") compared to all ",
                     top_code_title,
                     " students in ",
                     region, "(",
                     prettyNum(earnings_year(swfp_stacked, swfp_rarea, top_code_title[1])[2],big.mark = ","),
                     ") and statewide (",
                     prettyNum(earnings_year(swfp_stacked, "Statewide", top_code_title[1])[2],big.mark = ","),
                     "). Approximately",
                     "nO INFO",  #closely_related_job(swfp_stacked, requesting_county, top_code_title[1]),
                     "of ", requestor_names$DistrictAcronym, " ", top_code_title
                     ," students reported working in a job closely related to their field of study, which is ",
                     #ifelse(closely_related_job(swfp_stacked, requesting_county, top_code_title[1]) > closely_related_job(swfp_stacked, Southern Central Valley-Mother Lode, top_code_title[1]), "higher", "lower"),
                     " than ",
                     top_code_title[1], " students in ", requesting_region, closely_related_job(swfp_stacked, swfp_rarea, top_code_title[1])[2])

text_b_2_3 <- " but "
text_b_2_4 <- paste(
  #ifelse(closely_related_job(swfp_stacked, requesting_county, top_code_title[1]) > closely_related_job(swfp_stacked, "Statewide", top_code_title[1]), "higher than the", "lower than the"),
  "than the statewide average of",
  closely_related_job(swfp_stacked, "Statewide", top_code_title[1])[2], "Notably, a", sep = " ")
text_b_2_5 <-
  paste("percentage of",
        requestor_names$DistrictAcronym, top_code_title, "students attained the living wage",
        living_wage(swfp_stacked, requesting_county, top_code_title[1])[2],
        "when compared to all ",  top_code_title, " students in ", requesting_region,
        living_wage(swfp_stacked, swfp_rarea, top_code_title[1])[2],
        "However, both figures are ", sep = " ")
text_b_2_6 <- "lower"
text_b_2_7 <- paste(
  "than the percentage of",
  top_code_title,
  "students who attained the living wage statewide",
  as.character(living_wage(swfp_stacked, "Statewide", top_code_title[1])[2]),".", sep = " ")

exhibit12_1st_paragraph_numbers <- list()
for (tc_title in top_code_title) {
  # Call the function with the current title
  value1 <- swfp_students_andyear(SWFP, region_area = LOCALE_NAME_NORTH_OR_SOUTH_MOTHERLODE, tc_title)
  value2 <- swfp_students_andyear(SWFP, region_area = requesting_district, tc_title)

  # Extract and format the "value" variable
  formatted_value <- prettyNum(pull(value1, value), big.mark = ",")

  # Store the result in the list
  exhibit12_1st_paragraph_numbers[[tc_title]] <- paste(formatted_value, tc_title, CHANGEME_REGION_COUNTY, "students in the", pull(value1, academicYear), "acadmic year,",
                                                       paste0(round((round(as.numeric(pull(value2, value)))/round(as.numeric(pull(value1, value))))*100), "%"),
                                                       paste0("(", pull(value2, value), ")"), "attended a", requesting_district_acronym, "institution."
  )
  #print(exhibit12_1st_paragraph_numbers[[tc_title]])
}

exhibit12_2nd_paragraph_numbers <- list()
for (tc_title in top_code_title) {
  exhibit12_2nd_paragraph_numbers[[tc_title]] <- paste0(
    requestor_names$DistrictAcronym,
    " students that exited ",
    tc_title,
    " programs in the ",
    pull(swfp_students_andyear(SWFP, region_area = requesting_district, tc_title), "academicYear"),
    " academic year had HIGHERORLOWER median annual earnings (",
    pull(earnings_year(SWFP, requesting_district, tc_title),value),
    ") compared to all ",
    tc_title,
    " students in ",
    region, "(",
    pull(earnings_year(SWFP, LOCALE_NAME_NORTH_OR_SOUTH_MOTHERLODE, tc_title),value),
    ") and statewide (",
    pull(earnings_year(SWFP, "Statewide", tc_title),value),
    "). Approximately ",
    pull(closely_related_job(SWFP, requesting_district, tc_title),value),
    " of ", requestor_names$DistrictAcronym, " ", tc_title
    ," students reported working in a job closely related to their field of study, which is ",
    ifelse(as.numeric(pull(closely_related_job(SWFP, requesting_district, tc_title), "value")) > as.numeric(pull(closely_related_job(SWFP, LOCALE_NAME_NORTH_OR_SOUTH_MOTHERLODE, tc_title), "value")), "higher", "lower"),
    " than ",
    tc_title,
    " students in ",
    region, " ",
    pull(closely_related_job(SWFP, "Statewide", tc_title), "academicYear"),
    " but HIGHERLOWER than the statewide average of",
    pull(closely_related_job(SWFP, "Statewide", tc_title), value),
    " . Notably, ",
    pull(living_wage(SWFP, requesting_district, tc_title), value),
    " of ", requestor_names$DistrictAcronym, " ",
    tc_title, "students attained the living wage, compared to all ",  tc_title, " students in ", region, " (",
    pull(living_wage(SWFP, LOCALE_NAME_NORTH_OR_SOUTH_MOTHERLODE, tc_title),value), ").",
    " which is HIGHERLOWER than the Statewide figure of ",
    pull(living_wage(SWFP, "Statewide", tc_title), value)
  )
}

text_b_1 <- paste(
  "Exhibit 12 shows the Strong Workforce Program (SWP) metrics for",
  paste(top_code_title, collapse = " and "),
  "programs in",
  paste0(requestor_names$DistrictFullName," (",requestor_names$DistrictAcronym, "),"),
  region, ",CVML",
  ",and California." ,  paste(exhibit12_1st_paragraph_numbers, collapse = " and ")
)


#text_b_1 <- tryCatch({
#  call_claude_api_for_table(
#    prompt = paste("Make slight adjustments to the following paragraph to make it more gramatically correct. Only respond with the adjusted paragraph:",text_b_1, sep #= " "),
#    model = "claude-3-opus-20240229",
#    max_tokens = 4000
#  )
#}, error = function(e) {
#  paste("API call error:", e$message)
#})

text_b_11 <-
  if (use_ai) {
    gemini.R::gemini(paste("Make slight adjustments to the following paragraph to make it more grammatically correct. Only respond with the adjusted paragraph:",text_b_1, sep = " "))
  } else {
    text_b_1
  }



#claude_exhibit3 <- tryCatch({
#  call_claude_api_for_table(
#    prompt = paste(
#  "Make slight adjustments to the following paragraph to make it more coherent, but make sure to include every number (even if in parenthesis). Only respond with the #paragraph:",
#  text_b_2, text_b_2_1, text_b_2_2, text_b_2_3, text_b_2_4, text_b_2_5, text_b_2_6, text_b_2_7, sep = " "),
#    model = "claude-3-opus-20240229",
#    max_tokens = 4000
#  )
#}, error = function(e) {
#  paste("API call error:", e$message)
#})


#prompt_gem = paste(
#  "Make slight adjustments to the following paragraph to make it more coherent, but make sure to include every number (even if in parenthesis). Only respond with the paragraph:",
#  text_b_2, text_b_2_1, text_b_2_2, text_b_2_3, text_b_2_4, text_b_2_5, text_b_2_6, text_b_2_7, sep = " ")

#gemini_exhibit3 <-  gemini.R::gemini(prompt_gem)

#text_h3_1 <- paste0("Exhibit 12: ", top_code_title_w_top_code[1], " Strong Workforce Program Metrics, ", "2020-21")
#text_h3_2 <- paste0("Exhibit 12B: ", top_code_title_w_top_code[2], " Strong Workforce Program Metrics, ", "2020-21")
#text_h3_3 <- paste0("Exhibit 12C: ", top_code_title_w_top_code[3], " Strong Workforce Program Metrics, ", "2020-21")

#text_fn_1 <- "  All SWP metrics are for 2020-21 unless otherwise noted."

#img_size_12_1 <- adjust_image_size("exhibit12_1.png")
#img_size_12_2 <- adjust_image_size("exhibit12_2.png")
#img_size_12_3 <- adjust_image_size("exhibit12_3.png")

#### Create Components
#footnote_1 <- block_list(
#  fpar(
#    ftext(text_fn_1, fp_text(font.size = 10, color = "black", font.family = "Tw Cen MT")))
#)

#body_fpar_1  <- fpar(ftext(text_b_1, prop = body_text_style), fp_p = fp_par(text.align = "left"))
body_fpar_1  <- fpar(ftext(text_b_11, prop = body_text_style), fp_p = fp_par(text.align = "left"))

#body_fpar_2  <- fpar(
#  ftext(text_b_2, prop = body_text_style),
#  ftext(text_b_2_1, prop = body_text_style),
#  ftext(text_b_2_2, prop = body_text_style),
#  ftext(text_b_2_3, prop = body_text_style),
#  ftext(text_b_2_4, prop = body_text_style),
#  ftext(text_b_2_5, prop = body_text_style),
#  ftext(text_b_2_6, prop = body_text_style),
#  ftext(text_b_2_7, prop = body_text_style),
#  fp_p = fp_par(text.align = "left"))


#body_fpar_2_2  <- fpar(
#  ftext(#claude_exhibit3
#        gemini_exhibit3
#        , prop = body_text_style),
#  fp_p = fp_par(text.align = "left"))

# Gather all exhibit12_ files in the working directory
exhibit_files <- list.files(pattern = "^exhibit12_.*\\.png$")

doc7 <- read_docx() %>%
  body_add_fpar(fpar(ftext(text_h2, H2))) %>%
  body_add_fpar(body_fpar_1) %>%
  body_add_par("", style = "Normal") %>%
  body_add_par("", style = "Normal")

top_supps <- paste0(
  TOP_CIP_SOC_Current_supplementary$`TOP Code Title`,
  " (",
  TOP_CIP_SOC_Current_supplementary$`TOP Code`,
  ")"
)

for (i in seq_along(top_supps)) {
  tryCatch({
    file <- top_supps[i]

    # Build dynamic header
    text_h3_i <- paste0(
      "Exhibit 12", if (length(top_supps) > 1) letters[i] else "",
      ": ", top_supps[i],
      " Strong Workforce Program Metrics"#, year_range
    )

    header3_fpar_i <- fpar(
      ftext(text_h3_i, H3),
      fp_p = fp_par(text.align = "center")
    )

    # Second paragraph logic
    top_title <- pull(TOP_CIP_SOC_Current_supplementary, 2)[i]
    if (top_title %in% names(exhibit12_2nd_paragraph_numbers)) {
      text_b_2_i <- exhibit12_2nd_paragraph_numbers[[top_title]]
      text_b_ii <-
      if (use_ai) {
        gemini.R::gemini(
        paste(
          "Make adjustments to the following paragraph by replacing `HIGHERLOWER` with either `higher` or `lower`, whichever makes the most sense. Also, adjust the paragraph to make it flow more linguistically smooth. Only respond with the adjusted paragraph:",
          text_b_2_i
        )
      )
      } else {
        text_b_2_i
      }
    } else {
      text_b_ii <- ""
    }

    body_fpar_i <- fpar(
      ftext(text_b_ii, prop = body_text_style),
      fp_p = fp_par(text.align = "left")
    )

    # Dynamically get flextable object
    flextable_obj <- get(paste0("exhibit12_", letters[i]))

    # Add all elements to doc
    doc7 <- doc7 %>%
      body_add_par("", style = "Normal") %>%
      body_add_fpar(body_fpar_i) %>%
      body_add_par("", style = "Normal") %>%
      body_add_fpar(header3_fpar_i) %>%
      body_add_flextable(flextable_obj) %>%
      body_add_flextable(legend_fttt) %>%
      body_add_break()

  }, error = function(e) {
    warning(paste("Error processing program:", file, "-", e$message))
  })
}

#print(doc3, "test4.docx")

# Finalize the document
doc7 <- doc7 %>%
  body_add_break()

print(doc7, target = "office_06_2cccsoutcomes.docx")
#end officer_06_2cccsoutcomes

