#soc_code_tester <- random_soc_gen_f(6)
#soc_code_title_tester <- soc_titles_f(soc_code_tester)

occupational_projections_paragraph3_00 <- paste0(
  "Exhibit 8 shows the ten-year occupational demand projections for ",
  jobs,
  related_occupations_or_no(),
  #soc_titles_w_codes_func(soc_code_title_tester),
  " throughout California. The number of jobs related to ",
  this_or_these(),
  " ",
  occupation_or_occupations(),
  " is projected to ",
  increase_or_decrease_func(ex10_growth, 1),
  " by ",
  format_num_func(ex10_growth, "%"),
  " through ",
  op_date_range_max,
  ", with ",
  format_num_func(last(pull(exhibit8, which(str_detect(names(exhibit8), "Annual|Openings")))), "#"),
  " job openings available annually, throughout the state. ")

occupational_projections_paragraph3_01 <- paste0(
  "Exhibit 8 shows the ten-year occupational demand projections for ")


occupational_projections_paragraph3_02 <- paste0(
  related_occupations_or_no(),
  #soc_titles_w_codes_func(soc_code_title_tester),
  " throughout California. The number of jobs related to ",
  this_or_these(),
  " ",
  occupation_or_occupations(),
  " is projected to ",
  increase_or_decrease_func(ex10_growth, 1),
  " by ",
  format_num_func(ex10_growth, "%"),
  " through ",
  op_date_range_max,
  ", with ",
  format_num_func(last(pull(exhibit8, which(str_detect(names(exhibit8), "Annual|Openings")))), "#"),
  " job openings available annually, throughout the state. ")

occupational_projections_paragraph3 <- format_paragraph(occupational_projections_paragraph3_01, occupational_projections_paragraph3_02)

#doc10 <- read_docx("bdp_template.docx") %>%
doc10 <- read_docx() %>%
  body_add_par("") %>%
  body_add_fpar(occupational_projections_paragraph3) %>%
  body_add_par("") %>%
  body_add_fpar(
    fpar(
      ftext("Exhibit 8: 10-Year Occupational Demand in California", H3),
      fp_p = fp_par(text.align = "center")
    )
  ) %>%
  body_add_flextable(exhibit8_ft)

print(doc10, "officeR_010occupationalprojections2.docx")
