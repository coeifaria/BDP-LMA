
officeR_files <- list.files(pattern = "^officeR_0")

officeR_files_order <- str_extract(officeR_files, pattern = "\\d{3}")

stack_these <- tibble(
  files = officeR_files,
  order = officeR_files_order
) %>% arrange(order)

doc_sections <- pull(stack_these, files)
doc_total <- read_docx("bdp_template.docx")
for (doc in doc_sections){
doc_total <- doc_total %>% body_add_docx(doc)
}



final_doc <- doc_total %>%
  body_add_docx("bdp_appendix_template.docx") %>%
  body_add_fpar(
    fpar(
    ftext(text_date, body_text_style)
    )
  )


doc_title <- paste0("../",str_replace_all(paste(general_field, requesting_college, month(ymd(Sys.Date()), T,F), year(ymd(Sys.Date()))), " ", "_"), ".docx")

print(final_doc, doc_title)
