#```{r officer_00Summary, chunk_progress=TRUE}
text_h0_1 <- "Labor Market Analysis for Program Modification:"

text_h0_2 <- paste0(TOP, '/ ', top_code_title)

text_h0_3 <- degree_title

#text_sub1 <- "CVML Center of Excellence, October 2024"
text_sub1 <- paste0("CVML Center of Excellence, ", lubridate::month(Sys.Date(), abbr = FALSE, label = TRUE), " ", lubridate::year(Sys.Date()))

text_h1 <- "Summary"


text_b_1 <- paste0(
  "The CVML Center of Excellence for Labor Market Research (CVML COE) prepared this report to determine whether there is a supply gap in the ",
  north_or_south_cvml,
  " regional labor market related to the following middle-skill occupation",
  ifelse(length(SOC)>1, "s", ""),
  ":"
  )

#text_b_3 <- paste0(
#  "Based on the available data, there appears to be a supply gap for ",
#  jobs,
#  " In addition to many of these occupations have entry-level wages above the subregion's living wage, particularly those with community college educational attainment between #,",range(as.numeric(str_remove_all(demand_exhibit1$CCAttainment, "%")))[1], "% to ", range(as.numeric(str_remove_all(demand_exhibit1$CCAttainment, "%")))[2], "%, the typical #education requirements align with a community college education. ")

text_b_3_1 <- "Based on the available data, there appears to be a supply gap for "
text_b_3_2italics <- jobs
text_b_3_3 <- paste(". In addition to many of these occupations having entry-level wages above the subregion's living wage, ", Education_endorsement_part_1)
#particularly those with community college educational attainment between ,",
#range(as.numeric(str_remove_all(demand_exhibit1$CCAttainment, "%")))[1], "% to ", range(as.numeric(str_remove_all(demand_exhibit1$CCAttainment, "%")))[2], "%, #the typical education requirements align with a community college education. ")


#call_claude_api(paste("You are a Labor Market Data Analyst correcting a report. Make this sound official:", text_b_3, sep = " "))

your_function2 <- function(yes_count) {
  if (yes_count == 0) {
    value <- "none"
  } else if (yes_count == 1) {
    value <-  "some"
  } else if (yes_count == 2) {
    value <-   "most"
  } else if (yes_count == 3) {
    value <-   "all"
  } else {
    value <-   "error"
  }
  return(value)
}

text_b_3_1bold <- paste0(". Therefore, due to ", your_function2(yes_count), " the regional labor market criteria being met, the COE endorses this proposed program.")

# Split the string into a vector
#soc_code_titles_w_soc_code_individual <- str_split(soc_code_titles_w_soc_code, ", ")[[1]]

#living_wage_text <- living_wage_summary
#education_text <- education_comments
library(officer)
#N_S_CVML("N")
# Load the template docx file
#doc <- read_docx("LMA_TEMPLATE.docx") %>%
#  body_replace_all_text("CHANGEME_BOLD_NUMBER", new_value = paste(prettyNum(CHANGEME_BOLD_NUMBER, big.mark = ","))) %>%
#  body_replace_all_text("CHANGEME_REGION_COUNTY", paste(CHANGEME_REGION_COUNTY)) %>%
#  body_replace_all_text("CHANGEME_ITALICS_JOBS", paste(CHANGEME_ITALICS_JOBS)) %>%
#  body_replace_all_text("CHANGEME_BOLD_MORE_OR_LESS", paste(CHANGEME_BOLD_MORE_OR_LESS)) %>%
#  body_replace_all_text("CHANGEME_BOLD_AWARD_NUMBER", paste(CHANGEME_BOLD_AWARD_NUMBER)) %>%

#  body_replace_all_text("CHANGEME2_ITALICS_JOBS2", paste(CHANGEME2_ITALICS_JOBS2)) %>%
#  body_replace_all_text("CHANGEME2BOLDABOVEORBELOW2", paste(CHANGEME2_BOLD_ABOVE_OR_BELOW2)) %>%
#  body_replace_all_text("CHANGEMEREGION", paste(CHANGEME2_REGION2)) %>%
#  body_replace_all_text("CHANGEMEBOLDDOLLARSLIVINGWAGE", paste(CHANGEME2_BOLD_DOLLARS_LIVINGWAGE2)) %>%

#  body_replace_all_text("CHANGEMEITALICSJOBS", paste(Education_endorsement_part_1)) %>%
  #body_replace_all_text("CHANGEMEAWARDS", paste(Education_endorsement_part_1)) %>%
#  body_replace_all_text("CHANGEME_EDUC", paste(CHANGEME_EDUC)) %>%
#  body_replace_all_text("CHANGEMEBOLDPERC", paste(Education_endorsement_part_2)) #%>% body_add_docx("testing_summary.docx")

doc <- read_docx("testing_summary.docx")

# Save the modified template temporarily to add to the main doc

# temp_file1 <- tempfile(fileext = ".docx")
# print(doc, target = temp_file1)

# Add modified appendix content to the main document
#doc <- body_add_docx(doc, src = temp_file1)

# Move the cursor to the beginning of the document
doc <- cursor_begin(doc)

# Add content to the top of the document
doc <- doc %>%
  body_add_fpar(fpar(ftext(text_h0_1, H0))) %>%
  body_add_fpar(fpar(ftext(text_h0_2, H0))) %>%
  body_add_fpar(fpar(ftext(text_h0_3, H0))) %>%
  body_add_fpar(fpar(ftext(text_sub1, S1), fp_p = fp_par(border.bottom = fp_border(color = "black", width = 1))))

doc <- doc %>%
  body_add_fpar(fpar(ftext(text_h1, H1)))

# Move the cursor to the end of the document
doc <- cursor_end(doc)

# Add content to the bottom of the document
doc <- doc %>%
  body_add_par("", style = "Normal") %>%
  body_add_fpar(fpar(ftext(text_b_1, body_text_style)))

#soc_code_titles_w_soc_code_individual replaced with soc_code_titles_w_soc_code

for (title in soc_code_titles_w_soc_code) {
    doc <- doc %>%
    body_add_par(str_replace(title, pattern = "\\(", replacement = "(SOC "),style = "List Paragraph")
}

doc <- doc %>%
  body_add_fpar(
    fpar(
      ftext(text_b_3_1, body_text_style),
      ftext(text_b_3_2italics, body_text_style_italic),
      ftext(text_b_3_3, body_text_style),
      ftext(text_b_3_1bold, body_text_style_bold)
    )
  )

# Save the document
print(doc, target = "officeR_00summary.docx")
