#```{r officer_09workoutput, chunk_progress=TRUE}
# Replace all occurrences of the variable with italicized text
# body_text_style_italic <-fp_text(font.size = 11, font.family = "Tw Cen MT", italic = TRUE)
# Define the replacement process
replace_all_with_format <- function(doc, old_value, formatted_text) {
  repeat {
    # Attempt to find the text in the document
    doc <- cursor_reach(doc, keyword = old_value)

    if (is.null(doc)) {
      # If no more occurrences are found, exit the loop
      break
    }

    # Remove the unformatted text
    doc <- body_remove(doc)

    # Add the formatted text
    doc <- body_add_fpar(doc, formatted_text)
  }

  return(doc)
}
formatted_text <- fpar(ftext(jobs, prop = body_text_style_italic))
#doc <- replace_all_with_format(doc, old_value = jobs, formatted_text = formatted_text)

#doc <- doc %>%
#  body_replace_all_text(
#    old_value = jobs,
#    new_value = ftext(jobs, prop = body_text_style_italic),
#    only_at_cursor = FALSE
#  )


#working_title <- paste0("../", paste(TOP, top_code_title, str_split(requesting_region, " ")[[1]][1], format(Sys.Date(), "%b%y"), sep = "_"), ".docx")[1]
working_title <- paste0("../", paste(TOP, gen_field, str_split(requesting_region, " ")[[1]][1], format(Sys.Date(), "%b%y"), sep = "_"), ".docx")[1]

print(doc9, target = working_title)


#print(doc, target = "testing.docx")
