#SOC_tester <- random_soc_gen_f(5)




purpose_paragraph_1 <- paste0(
  "The purpose of this study is to determine whether there is demand in the statewide and regional labor market for the ")
purpose_paragraph_2 <- paste0(
  " occupations that are not being met by the supply from relevant training programs. More specifically, this report addresses the labor market components of Assembly Bill 927, which require evidence of unmet workforce needs related to ",
  requesting_college,
  "’s proposed baccalaureate degree program. "
)

if(two_or_more_soc>1){
  key_occupations_title <- paste0("Key ", general_field, " Occupations")
} else {
  key_occupations_title <- ""
}
key_occupations_paragraph <- paste0(
  " occupations analyzed in this report were selected from the 2018 Standard Occupational Classification (SOC) system. These occupations are classified under ",
  soc_2digit_titles_func(soc_2digit_titles),
  " major occupational groups."
)



#doc3 <- read_docx("bdp_template.docx") %>%
doc3 <- read_docx() %>%
  body_add_fpar(
    fpar(
      ftext(purpose_paragraph_1, body_text_style),
      ftext(str_to_lower(general_field), body_text_style_italic),
      ftext(purpose_paragraph_2, body_text_style),
      fp_p = fp_par(text.align = "justify"))) %>%
  body_add_par("") %>%
  body_add_fpar(
    fpar(
      ftext(key_occupations_title, body_text_style_bold),
      fp_p = fp_par(text.align = "justify"))) %>%
  body_add_par("") %>%
  body_add_fpar(
    fpar(
      ftext("The key ", body_text_style),
      ftext(str_to_lower(general_field), body_text_style_italic),
      ftext(key_occupations_paragraph, body_text_style),
      fp_p = fp_par(text.align = "justify"))) %>%
  body_add_par("") %>%
  body_add_fpar(
    fpar(
      ftext("Exhibit 1: Key ", H3),
      ftext(jobs, H3),
      ftext(related_occupations_or_no(), H3_italicized),
      fp_p = fp_par(text.align = "center")
      )
    ) %>%
  body_add_flextable(exhibit1_ft)

print(doc3, "officeR_003keyoccupations.docx")
