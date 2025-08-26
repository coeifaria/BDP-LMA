#```{r officer_appendix, chunk_progress=TRUE}
# Generate replacement text for living wage
#hourly <- as.character(livingwages_county[(livingwages_county$famcode == requesting_region), 3])
#living


# Load the appendix template and replace placeholder text
appendix_doc <- read_docx("LMA_appendix_template.docx") %>%
  body_replace_all_text(old_value = "CHANGEME",
                        new_value = paste(requesting_region, "is", average_living_wage(requesting_county)),
                        only_at_cursor = FALSE)

# Save the modified appendix temporarily to add to the main doc
print(appendix_doc, target = "appendix.docx")

# List files in their correct order based on numbering
file_paths <- c(
  "officeR_00summary.docx",
  "officeR_01summary.docx",
  "officeR_1demand.docx",
  "officeR_02wages.docx",
  "officeR_03jobpostings.docx",
  "officeR_04edattainment.docx",
  "officeR_05ccsupply.docx",
  "office_06_2cccsoutcomes.docx",
  "office_07ncccsoutcomes.docx"
)
shell.exec("template_attempt.xlsm")
# Combine all documents using file paths
doc9 <- read_docx("lma_blank_template.docx")
for (i in 1:length(file_paths)) {
    doc9 <- body_add_docx(doc9, src = file_paths[i])
}

# Add your final text, if needed
doc9 <- doc9 %>%
  body_add_docx(src = "appendix.docx") %>%
  body_add_fpar(fpar(ftext(text_date, body_text_outro_style1)))


# Save the final document
print(doc9, target = "final_documen2.docx")
