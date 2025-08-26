
job_postings_paragraph4 <- paste0(
  "Of the ",
  jb_all_formatted,
  " online job postings, the most requested credentials were a ",
  lcc_title[1],
  " (",
  lcc_perc[1],
  "), and ",
  lcc_title[2],
  " (",
  lcc_perc[2],
  ")",
  ". Exhibit 4 shows the most requested qualifications, sorted by number of online job postings. "
)

doc6 <- read_docx("bdp_template.docx") %>%
  body_add_fpar(fpar(ftext("Licenses, Clearances, and Certifications", H2), fp_p = fp_par(text.align = "left"))) %>%
  body_add_par("") %>%
  body_add_fpar(fpar(ftext(job_postings_paragraph4, body_text_style), fp_p = fp_par(text.align = "justify"))) %>%
  body_add_par("") %>%

  #  body_add_fpar(fpar(ftext("Entry-Level Education Requirements", H1), fp_p = fp_par(text.align = "justify"))) %>%
  #  body_add_par("") %>%
  #  body_add_fpar(fpar(ftext(entry_level_education_requirements_paragraph, body_text_style), fp_p = fp_par(text.align = "justify"))) %>%
  #  body_add_par("") %>%
  body_add_fpar(
    fpar(
      ftext(paste0("Exhibit 4: Top Licenses, Clearances, or Certifications by Number of Job Postings in California (nb=",jb_all_formatted,")"), H3),
      fp_p = fp_par(text.align = "center")
    )
  ) %>%
  body_add_flextable(e_4_ft)

print(doc6, "officeR_006jobpostings_LCC.docx")
