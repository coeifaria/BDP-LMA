ex10_growth <-(pull(exhibit_7, which(names(exhibit_7)==op_date_range_max))-pull(exhibit_7, which(names(exhibit_7)==op_date_range_min)))/pull(exhibit_7, which(names(exhibit_7)==op_date_range_min))

occupational_projections_paragraph1 <- paste0(
  "Exhibit 7 shows the cumulative number of jobs for ",
  jobs,
  related_occupations_or_no(),
  ", from ",
  year_set_func(),
  " through ",
  year_set_func()+10,
  ". There were ",
  format_num_func(pull(exhibit_7, which(names(exhibit_7)==year_set_func())), "#"),
  " jobs for this occupation in ",
  year_set_func(),
  ". Employment is projected to ",
  increase_or_decrease_func(ex10_growth, 1),
  " ",
  format_num_func(ex10_growth, "%"),
  " through ",
  year_set_func()+10,
  ". ")

occupational_projections_paragraph_001 <- paste0("Exhibit 7 shows the cumulative number of jobs for ")

occupational_projections_paragraph_002 <- paste0(
  ", from ",
  year_set_func(),
  " through ",
  year_set_func()+10,
  ". There were ",
  format_num_func(pull(exhibit_7, which(names(exhibit_7)==year_set_func())), "#"),
  " jobs for this occupation in ",
  year_set_func(),
  ". Employment is projected to ",
  increase_or_decrease_func(ex10_growth, 1),
  " ",
  format_num_func(ex10_growth, "%"),
  " through ",
  year_set_func()+10,
  ". ")

occupational_projections_paragraph <- format_paragraph(occupational_projections_paragraph_001, occupational_projections_paragraph_002)

doc9 <- read_docx("bdp_template.docx") %>%
  body_add_fpar(fpar(ftext("10-Year Occupational Projections", H2), fp_p = fp_par(text.align = "left"))) %>%
  body_add_par("") %>%
  body_add_fpar(occupational_projections_paragraph) %>%
  body_add_par("") %>%
  body_add_fpar(
    fpar(
      ftext(paste0("Exhibit 7: Projected Number of Jobs for ",
      jobs, related_occupations_or_no()," by Year, ", year_set_func(), "-", year_set_func()+10), H3),
      fp_p = fp_par(text.align = "center")
    )
  ) %>%
  body_add_docx("ex7_linked.docx")

print(doc9, "officeR_009occupationalprojections1.docx")




