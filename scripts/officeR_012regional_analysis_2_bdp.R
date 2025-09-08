regional_analysis_paragraph_3_00 <- paste0(
  "Exhibit 10 shows the ten-year occupational demand projection for ",
  jobs,
  related_occupations_or_no(),
  " in the CVML region. The number of jobs related to these occupations is projected to ",
  increase_or_decrease_func(ex10_growth, 1),
  " ",
  format_num_func(ex10_growth, "%"),
  " through ",
  year_set_func()+10,
  ", with ",
  format_num_func(last(pull(exhibit10, which(str_detect(names(exhibit10), "Annual|Openings")))), "#"),
  " job openings available annually. ")

regional_analysis_paragraph_3_01 <- paste0(
  "Exhibit 10 shows the ten-year occupational demand projection for ")


regional_analysis_paragraph_3_02 <- paste0(
  " in the CVML region. The number of jobs related to these occupations is projected to ",
  increase_or_decrease_func(ex10_growth, 1),
  " ",
  format_num_func(ex10_growth, "%"),
  " through ",
  year_set_func()+10,
  ", with ",
  format_num_func(last(pull(exhibit10, which(str_detect(names(exhibit10), "Annual|Openings")))), "#"),
  " job openings available annually. ")

regional_analysis_paragraph_3 <- format_paragraph(regional_analysis_paragraph_3_01, regional_analysis_paragraph_3_02)

footnote_ex10 <- "10-year change represents new job additions to the workforce. Annual openings include new jobs and replacement jobs that result from retirements and separations."
footnote_content_ex10 <- fpar(ftext(footnote_ex10, prop = fp_footnote_style))

doc12 <- read_docx("bdp_template.docx") %>%
##doc12 <- read_docx() %>%
  body_add_fpar(regional_analysis_paragraph_3) %>%
  body_add_par("") %>%
  body_add_fpar(
    fpar(
      ftext("Exhibit 10: Occupational Demand in CVML region", H3),
      run_footnote(x = block_list(footnote_content_ex10), prop = fp_text_lite(vertical.align = "superscript")),
      fp_p = fp_par(text.align = "center"))
  ) %>%
  body_add_flextable(exhibit10_ft)

print(doc12, "officeR_012regional_analysis_2.docx")
