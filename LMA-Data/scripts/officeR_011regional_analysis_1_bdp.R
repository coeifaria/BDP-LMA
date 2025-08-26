regional_analysis_paragraph_1 <- "To gain a deeper understanding of labor market trends for this occupation, this section delves into Central Valley/Mother Lode regional data, examining historical employment trends and demand, educational supply, and wages for the occupation of interest. Due to extensive workforce migration between the 15 counties that make up the CVML region, this analysis examines them together to provide a comprehensive regional view of labor market trends, in line with standard practice that recognizes the counties’ integrated workforce and economic dynamics. "

regional_analysis_paragraph_2_00 <- paste0(
  "Exhibit 9 shows the historical, annual percent change in jobs for ",
  jobs,
  related_occupations_or_no(),
  " from ",
  year_set_func()-10,
  " through ",
  year_set_func(),
  ". Employment in these occupations ",
  "remained positive throughout this period",
  " with the highest growth in ",
  ex_9_max_growth_year,
  " ",
  format_num_func(ex_9_max_growth_perc/100, "%", T, F),
  ". ")

regional_analysis_paragraph_2_01 <- paste0("Exhibit 9 shows the historical, annual percent change in jobs for ")

regional_analysis_paragraph_2_02 <- paste0(
  " from ",
  year_set_func()-10,
  " through ",
  year_set_func(),
  ". Employment in these occupations ",
  "remained positive throughout this period",
  " with the highest growth in ",
  ex_9_max_growth_year,
  " ",
  format_num_func(ex_9_max_growth_perc/100, "%", T, F),
  ". ")

regional_analysis_paragraph_2 <- format_paragraph(regional_analysis_paragraph_2_01, regional_analysis_paragraph_2_02)

doc11 <- read_docx("bdp_template.docx") %>%
  body_add_par("") %>%
  body_add_par("") %>%
  body_add_fpar(fpar(ftext("Regional Analysis", H1), fp_p = fp_par(text.align = "left"))) %>%
  body_add_par("") %>%
  body_add_fpar(fpar(ftext(regional_analysis_paragraph_1, body_text_style), fp_p = fp_par(text.align = "justify"))) %>%
  body_add_par("") %>%
  body_add_fpar(fpar(ftext("Occupational Overview in the CVML Region", H2), fp_p = fp_par(text.align = "left"))) %>%
  body_add_par("") %>%
  body_add_fpar(regional_analysis_paragraph_2) %>%
  body_add_par("") %>%
  body_add_fpar(
    fpar(
      ftext("Exhibit 9: 10-Year Historical Annual Percent Change in Jobs for ", H3),
      ftext(jobs, H3),
      ftext(related_occupations_or_no(), H3_italicized),
      fp_p = fp_par(text.align = "center"))
    ) %>%
  body_add_docx("ex9_linked.docx")

print(doc11, "officeR_011regional_analysis_1.docx")
