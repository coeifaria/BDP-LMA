
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

salary_differentials_paragraph_0i <-
  list(ftext("In the online job postings with a stated minimum educational requirement and an advertised salary (n", body_text_style)) %>%
  append(list(ftext("ed", update(body_text_style_italic, vertical.align = "subscript")))) %>%
  append(list(ftext(paste0("=", format_num_func(jp_ned, "#", F, F),"), employers for "), body_text_style))) %>%
  append(format_jobs_related_occ) %>%
  append(list(ftext(salary_differentials_paragraph_02, body_text_style),
  fp_p = fp_par(text.align = "justify")))

salary_differentials_paragraph <- do.call(fpar, salary_differentials_paragraph_0i)

#doc8 <- read_docx("bdp_template.docx") %>%
doc8 <- read_docx() %>%
  #cursor_end() %>%
  body_add_fpar(fpar(ftext("Salary Differentials by Level of Education", H2), fp_p = fp_par(text.align = "left"))) %>%
  body_add_par("") %>%
  body_add_fpar(salary_differentials_paragraph) %>%
  #body_add_fpar() %>%
  body_add_par("") %>%
  body_add_fpar(fpar(ftext("Exhibit 6 lists the advertised entry-level wage for the occupation of interest by level of education.", body_text_style), fp_p = fp_par(text.align = "justify"))) %>%
  body_add_par("") %>%
  body_add_fpar(
    fpar(
      ftext("Exhibit 6: Advertised Entry-Level Wage by Level of Experience (n", H3),
      ftext("ed", update(H3_italicized, vertical.align = "subscript")),
      ftext(paste0(" = ",format_num_func(jp_ned, "#")), H3),
      fp_p = fp_par(text.align = "center")
    )
  ) %>%
  body_add_flextable(exhibit6_ft)

print(doc8, "officeR_008advertised_salary.docx")


