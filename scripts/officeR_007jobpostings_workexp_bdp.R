#SOC_tester <- random_soc_gen_f(5)


#test <- test_demand_data(random_soc_gen_f(20))

typical_entry_level_is_bachelors <- typical_entry_level_is_bachelors_func(ca_demand)
soc_2digit_titles_func(typical_entry_level_is_bachelors)

#typical_entry_level_is_bachelors <- typical_entry_level_is_bachelors_func(test)
#soc_2digit_titles_func(typical_entry_level_is_bachelors)


job_postings_paragraph4_000 <- paste0(
  "The Bureau of Labor Statistics (BLS) lists",
  typical_entry_level_work_experience_for_all,
  " as the typical entry-level work experience for ",
  jobs,
  related_occupations_or_no(),
  ". However, of the postings for these occupations with program-aligned job titles that listed a minimum experience requirement, ",
  typical_entry_level_work_experience_for_bachelor_preferance[1],
  " requested ",
  typical_entry_level_work_experience_for_bachelor_preferance[2],
  " of experience, followed by ",
  typical_entry_level_work_experience_for_bachelor_preferance_followed_by[1],
  " of postings that requested ",
  typical_entry_level_work_experience_for_bachelor_preferance_followed_by[2],
  " of experience. "
)

job_postings_paragraph4_00 <- paste0(
  "The Bureau of Labor Statistics (BLS) lists",
  typical_entry_level_work_experience_for_all,
  " as the typical entry-level work experience for ")

job_postings_paragraph4_01 <- paste0(
  ". However, of the postings for these occupations with program-aligned job titles that listed a minimum experience requirement, ",
  typical_entry_level_work_experience_for_bachelor_preferance[1],
  " requested ",
  typical_entry_level_work_experience_for_bachelor_preferance[2],
  " of experience, followed by ",
  typical_entry_level_work_experience_for_bachelor_preferance_followed_by[1],
  " of postings that requested ",
  typical_entry_level_work_experience_for_bachelor_preferance_followed_by[2],
  " of experience. "
)

job_postings_paragraph4 <- format_paragraph(job_postings_paragraph4_00, job_postings_paragraph4_01)

job_postings_paragraph5 <- "Exhibit 5 shows the requested years of experience by number of online job postings for this occupation with program-aligned job titles. "

degree_alignment_paragraph_01 <-
  paste0(
    "The proposed program aligns with the educational requirements for the target positions, as ",
    increase_or_decrease_func(perc_req_bach-.5,a=3),
    " than half ",
    format_perc_func(perc_req_bach, T),
    " of relevant online job postings with minimum education requirements require a bachelor’s degree. Additionally, the BLS lists a bachelor’s degree as the typical entry-level education for ")

degree_alignment_paragraph_02 <- paste0(
    " and ",
    bachelor_range_f(bachelor_range, 2),
    " of incumbent workers attained a baccalaureate degree as their highest level of education. "
  )

degree_alignment_paragraph <- format_paragraph(degree_alignment_paragraph_01, degree_alignment_paragraph_02, format_soc_titles(typical_entry_level_is_bachelors, T))


no_req_exp <- pull(ex5_0, `Job Postings`)[1]

footnote_ex5_1 <- paste0(
  "Of the ",
  format_num_func(jb_nb, "#"),
  " job postings with a preference for a bachelor degree (",
  "n")

footnote_ex5_2 <- paste0(") ",
  format_num_func(no_req_exp, "#", F,F),
  " (",
  format_num_func(no_req_exp/jb_nb, "%", F, F),
  "%)",
  " did not include a requirement for experience."
)


footnote_content <- fpar(
  ftext(footnote_ex5_1, prop = fp_footnote_style),
  ftext("b", update(fp_footnote_style, vertical.align = "subscript")),
  ftext(footnote_ex5_2, prop = fp_footnote_style)
  )

# --- CORRECTED CODE ---

# Step 1: Create your complex paragraph as a separate object.
# This ensures all content is listed first, followed by the named fp_p argument.

# Step 2: Add the pre-built object to your document chain.
doc7 <- read_docx("bdp_template.docx") %>%
  body_add_fpar(fpar(ftext("Entry-Level Work Experience", H2), fp_p = fp_par(text.align = "left"))) %>%
  body_add_par("") %>%
  body_add_fpar(job_postings_paragraph4) %>%
  body_add_par("") %>%
  body_add_fpar(fpar(ftext(job_postings_paragraph5, body_text_style), fp_p = fp_par(text.align = "justify"))) %>%
  body_add_par("") %>%

  # Now the call is much cleaner and less error-prone
  body_add_fpar(
    fpar(
      ftext("Exhibit 5: Requested Years of Experience by Number of Job Postings in California (n", H3),
      ftext("b", update(H3, vertical.align = "subscript")),
      ftext(paste0(" = ", format_num_func(jb_nb, "#"), ")"), H3),
      run_footnote(x = block_list(footnote_content), prop = fp_text_lite(vertical.align = "superscript")),
      fp_p = fp_par(text.align = "center")
    )
  ) %>%

  body_add_flextable(e_5_ft) %>%
  body_add_par("") %>%
  body_add_fpar(fpar(ftext("Degree Alignment", H2), fp_p = fp_par(text.align = "left"))) %>%
  body_add_par("") %>%
  body_add_fpar(degree_alignment_paragraph) %>%
  body_add_par("")

# This should now run without error
print(doc7, "officeR_007jobpostings_workexp.docx")
