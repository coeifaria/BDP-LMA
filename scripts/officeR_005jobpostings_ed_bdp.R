
unique_postings_formatted <- prettyNum(unique_postings, big.mark = ",")
jb_all_bach <- paste0(round(jb_bachelor/jb_all,2)*100, "%")
jb_all_formatted <- prettyNum(jb_all, big.mark = ",")
jb_bachelor_formatted <- prettyNum(jb_bachelor, big.mark = ",")

job_postings_paragraph1 <- paste0("While there were ",
         unique_postings_formatted,
         " online job postings for ")


job_postings_paragraph2 <- paste0(" over the last 12 months, ",
         jb_all_formatted,
         " of those postings were specifically related to the target positions aligned with the proposed baccalaureate degree program. These figures account for postings made publicly available; internal recruitment efforts and postings cannot be identified for analysis."
  )


job_postings_paragraph3 <- paste0(
  "Of the ",
  jb_all_formatted,
  " postings listing a minimum education requirement (n")

job_postings_paragraph4 <- paste0("=",
  jb_all_formatted,
  "), ",
  jb_all_bach,
  " (n")

job_postings_paragraph5 <- paste0("=",
  jb_bachelor_formatted,
  ") ",
  "requested a bachelor’s degree. Exhibit 3 shows the requested level of education by number of online job postings."
)

job_postings_paragraph <- format_paragraph(job_postings_paragraph1, job_postings_paragraph2)

#doc5 <- read_docx("bdp_template.docx") %>%
doc5 <- read_docx() %>%
  body_add_fpar(job_postings_paragraph) %>%
  body_add_par("") %>%
  body_add_fpar(
    fpar(
      ftext(job_postings_paragraph3, body_text_style),
      ftext("min ed", update(body_text_style_italic, vertical.align = "subscript")),
      ftext(job_postings_paragraph4, body_text_style),
      ftext("b", update(body_text_style_italic, vertical.align = "subscript")),
      ftext(job_postings_paragraph5, body_text_style),
      fp_p = fp_par(text.align = "justify"))) %>%
  body_add_par("") %>%
  body_add_fpar(
    fpar(
      ftext("Exhibit 3: Requested Education by Number of Job Postings in California (n",H3),
      ftext("min ed", update(H3_italicized, vertical.align = "subscript")),
      ftext(paste0("=", jb_all_formatted, ")"), H3),
      fp_p = fp_par(text.align = "center")
    )
  ) %>%
  body_add_flextable(e_3_ft)

print(doc5, "officeR_005jobpostings_ed.docx")
