
text_h0 <- paste0("Workforce Demand for ", general_field, " in the Central Valley/Mother Lode Region")

text_h0_1 <- paste0("Labor Market Supply and Demand for ", degree_title, " at ", requesting_college)

text_h0_3 <- "Prepared by:"
text_h0_4 <- "Central Valley Mother Lode (CVML)"
text_h0_5 <- "Center of Excellence for Labor Market Research"

intro_func <- function(text, style, alignment) {
  return(fpar(ftext(text, style), fp_p = fp_par(text.align = alignment)))
}

title_text <- intro_func(text = text_h0, style = H0, alignment = "center")
subtitle_text <- intro_func(text = text_h0_1, style = H0_1, alignment = "center")
prepared_by1 <- intro_func(text_h0_3, H0_1,"right")
prepared_by2 <- intro_func(text_h0_4, H0_1,"right")
prepared_by3 <- intro_func(text_h0_5, H0_1,"right")


doc <- read_docx("bdp_intro_template.docx")

# Add content to the top of the document
doc <- doc %>%
  body_add_fpar(title_text) %>%
  body_add_par("") %>%
  body_add_par("") %>%
  body_add_fpar(subtitle_text) %>%
  body_add_par("") %>%
  body_add_par("") %>%
  body_add_par("") %>%
  body_add_par("") %>%
  cursor_end() %>%
  body_add_fpar(prepared_by3) %>%
  body_add_fpar(prepared_by2, pos = "before") %>%
  body_add_fpar(prepared_by1, pos = "before")

print(doc, target = "officeR_000summary_bdp.docx")
