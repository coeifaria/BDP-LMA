
salary_differentials_paragraph_000 <- paste0(
  "In the online job postings with a stated minimum educational requirement and an advertised salary ",
  "(ned=",
  format_num_func(jp_ned, "#", F, F),
  "),  employers for ",
  jobs,
  related_occupations_or_no(),
  " advertised ",
  format_num_func(b_a_difference, "$W"),
  "/hour",
  increase_or_decrease_func(b_a_difference,3),
  " in entry-level wages for bachelor’s degree holders relative to their associate degree counterparts. ")

salary_differentials_paragraph_01 <- paste0(
  "In the online job postings with a stated minimum educational requirement and an advertised salary ",
  "(ned=",
  format_num_func(jp_ned, "#", F, F),
  "), employers for ")

salary_differentials_paragraph_02 <- paste0(
  " advertised ",
  format_num_func(b_a_difference, "$W"),
  "/hour ",
  increase_or_decrease_func(b_a_difference,3),
  " in entry-level wages for bachelor’s degree holders relative to their associate degree counterparts. ")

salary_differentials_paragraph <- format_paragraph(salary_differentials_paragraph_01, salary_differentials_paragraph_02)

doc8 <- read_docx("bdp_template.docx") %>%
  body_add_fpar(fpar(ftext("Salary Differentials by Level of Education", H2), fp_p = fp_par(text.align = "left"))) %>%
  body_add_par("") %>%
  body_add_fpar(salary_differentials_paragraph) %>%
  body_add_par("") %>%
  body_add_fpar(fpar(ftext("Exhibit 6 lists the advertised entry-level wage for the occupation of interest by level of education.", body_text_style), fp_p = fp_par(text.align = "justify"))) %>%
  body_add_par("") %>%
  body_add_fpar(
    fpar(
      ftext(paste0("Exhibit 6: Advertised Entry-Level Wage by Level of Experience (ned=",format_num_func(jp_ned, "#"),")"), H3),
      fp_p = fp_par(text.align = "center")
    )
  ) %>%
  body_add_flextable(exhibit6_ft)

print(doc8, "officeR_008advertised_salary.docx")


