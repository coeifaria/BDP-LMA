educational_supply_paragraph_1 <- paste0(
  "Community colleges throughout the state offer applied ",
  str_to_lower(general_field),
  " programs. These programs are often created under ",
  pull(topdf, 2)[1],
  " (TOP ",
  pull(topdf, 1)[1],
  ") ",
  #"Agriculture Technology and Sciences, General (TOP 0101.00)",
  ". However, none provide baccalaureate degrees. Therefore, educational supply is based on non-community college awards. "
  )

educational_supply_paragraph_2_00 <- paste0(
  "Exhibit 11 shows the Classification of Instructional Programs (CIP) codes aligned with program needs and related to ",
  jobs,
  related_occupations_or_no(),
  ", as well as the number of non-community college awards conferred in each CIP code. "
  )

educational_supply_paragraph_2_01 <- paste0(
  "Exhibit 11 shows the Classification of Instructional Programs (CIP) codes aligned with program needs and related to ")
educational_supply_paragraph_2_02 <- paste0(
  ", as well as the number of non-community college awards conferred in each CIP code. ")

educational_supply_paragraph_2 <- format_paragraph(educational_supply_paragraph_2_01, educational_supply_paragraph_2_02)

doc13 <- read_docx("bdp_template.docx") %>%
  body_add_fpar(fpar(ftext("Educational Supply", H2), fp_p = fp_par(text.align = "left"))) %>%
  body_add_fpar(fpar(ftext(educational_supply_paragraph_1, body_text_style), fp_p = fp_par(text.align = "justify"))) %>%
  body_add_par("") %>%
  body_add_fpar(educational_supply_paragraph_2) %>%
  body_add_par("") %>%

  body_add_fpar(
    fpar(
      ftext("Exhibit 11: Related Classification of Instructional Program (CIP) Code", H3),
      fp_p = fp_par(text.align = "center"))
  ) %>%
  body_add_flextable(exhibit11_ft)

print(doc13, "officeR_013educational_supply.docx")

