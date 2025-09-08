increase_or_decrease_func <- function(x, a = 1){
  pos <- c("increase", "grow", "more", "over", "within", "above")
  neg <- c("decrease", "decline", "less", "under", "below", "below")
  value <- str_remove_all(x, pattern = "\\%|\\$|\\,") %>% as.numeric()
  output <- ifelse(x>0, pos[a], neg[a])
  return(output)
}


format_num_func <- function(x, type = "%", parenthesis=F, paranthesis_for_negative=F){
  if (type == "%"){
    final_offer <- paste0(round(100*x, 0), "%")
  } else if (type =="#") {
    final_offer <- prettyNum(round(x), big.mark = ",")
  } else if (type == "$S") {
    final_offer <- paste0("$", prettyNum(round(x), big.mark = ","))
  } else if (type == "$W") {
    wage <- str_split_1(as.character(x), "\\.")
    final_offer <- paste0("$", paste(wage[1], str_sub(str_pad(coalesce(wage[2], "0"), side = "right", pad = "0", width = 2), 1,2), sep = "."))
  } else {
    final_offer <- "error"
  }
  if(parenthesis|(paranthesis_for_negative&x<0)) {final_offer <- paste0("(",final_offer,")")}

  return(final_offer)
}
format_num_func <- Vectorize(format_num_func)

bachelor_range_f <- function(ranged, a=1){
  joiner <- c(" to ", " and ")[a]
  if(length(ranged)==1){
    final_offer <- format_num_func(ranged, "%")
  } else {
    final_offer <- paste0("between ", format_num_func(ranged[1], "%"), joiner , format_num_func(ranged[2], "%"))
  }
  return(final_offer)
}

#Pull SOC titles with SOC codes
soc_titles_f <- function(socs){
  soc_2018_definitions %>%
    filter(`SOC Code` %in% socs) %>%
    pull(`SOC Title`)
}



#Pull the 2 digit SOC title sectors
soc_2digit_titles_func <- function(socs) {
  soc_2digit_titles_length <- length(socs)
  if (soc_2digit_titles_length==1){
    final_offer <- socs
  } else if (soc_2digit_titles_length==2) {
    final_offer <- paste0(socs[1], " and ", socs[2])
  } else if (soc_2digit_titles_length>2) {

    soc_2digit_titles_length_less_1 <- soc_2digit_titles_length-1
    all_but_last <- paste(socs[1:soc_2digit_titles_length_less_1], collapse = ", ")
    final_offer <- paste0(all_but_last, ", and ", socs[soc_2digit_titles_length])
  }
  return(final_offer)
}

text_soc_codes <- random_soc_gen_f(10)
soc_code_titles_w_soc_code
soc_2digit_titles_func(text_soc_codes)

soc_titles_w_codes_func <- function(soc_codes, twodigit=F) {
  if(twodigit) {soc_filter <- str_extract(soc_codes, "\\d{2}") %>% unique() %>% sort() %>% paste0("-0000")} else {
    soc_filter <- soc_codes
  }
  soc_2018_definitions %>%
    filter(`SOC Code` %in% soc_filter) %>%
    mutate(SOC_2digit_titles = paste0(`SOC Title`, " (", `SOC Code`, ")")) %>%
  pull(SOC_2digit_titles) %>%
    return()
}


soc_titles_formatted_raw_func <- function(soc_codes) {
  soc_codes_length <- length(soc_codes)
  if (soc_codes_length==1){
    final_offer <- soc_codes
  } else if (soc_codes_length==2) {
    final_offer <- paste0(soc_codes[1], " and ", soc_codes[2])
  } else if (soc_codes_length>2) {

    soc_codes_length_length_less_1 <- soc_codes_length-1
    all_but_last <- paste(soc_codes[1:soc_codes_length_length_less_1], collapse = ", ")
    final_offer <- paste0(all_but_last, ", and ", soc_codes[soc_codes_length])
  }
  return(final_offer)
}

soc_titles_formatted_officer_func <- function(soc_titles) {
  soc_2digit_titles_length <- length(soc_titles)
  if (soc_2digit_titles_length==1){
    final_offer <- ftext(soc_titles, body_text_style)
  } else if (soc_2digit_titles_length==2) {
    final_offer <-
      fpar(
        ftext(soc_titles[1], body_text_style_italic),
        ftext(" and ", body_text_style),
        ftext(soc_titles[2], body_text_style_italic)
      )

  } else if (soc_2digit_titles_length>2) {

    soc_2digit_titles_length_less_1 <- soc_2digit_titles_length-1
    all_but_last <- paste(soc_titles[1:soc_2digit_titles_length_less_1], collapse = ", ")

    final_offer <- paste0(all_but_last, ", and ", soc_titles[soc_2digit_titles_length])
  }
  return(final_offer)
}

soc_titles_formatted_officer_func(soc_titles_formatted_raw_func(text_soc_codes))

related_occupations_helper <- function(x="number"){
  if(related_occupations_or_no()!="") {

    if(x == "number"){
      return_me <- paste0("the ", xfun::n2w(length(SOC)))
    } else if (x=="report") {
      return_me <- "in this report"
    }

  } else {
    return_me <- ""
  }
  return(return_me)
}


is_or_are <- function() {
  if(two_or_more_soc<2){
    "is"
  } else {
    "are"
  }
}

occupation_or_occupations <- function() {
  if(two_or_more_soc<2){
    "occupation"
  } else {
    "occupations"
  }
}

this_or_these <- function() {
  if(two_or_more_soc<2){
    "this"
  } else {
    "these"
  }
}

related_occupations_or_no <- function() {
  if(long_name_jobs_rule){
    "-related occupations"
  } else {
    ""
  }
}

### officeR function

related_fields <- if (two_or_more_soc == 1) {
  as_paragraph(
    as_chunk(soc_title, prop = body_text_style_italic) # Apply style to the variable
  )

} else if (two_or_more_soc == 2) {
  as_paragraph(
    as_chunk(paste0(soc_title[1], " and ", soc_title[2]), prop = body_text_style_italic) # Apply style to the variable
  )
} else if (two_or_more_soc > 2) {
  as_paragraph(
    as_chunk(general_field, prop = body_text_style_italic), # Apply style to the variable
    as_chunk(related_occupations_or_no(), prop = body_text_style_italic) # Apply style to the variable
  )
} else {
  "error"
}

# Function to format SOC titles with proper italics and separators
format_soc_titles <- function(soc_titles, addmore = T) {

  titles_length <- length(soc_titles)
  ftext_list <- list()

  if (titles_length == 1) {
    # Single title - just italicize it
    ftext_list <- list(ftext(soc_titles[1], body_text_style_italic))

  } else if (titles_length == 2) {
    # Two titles - "Title1 and Title2"
    ftext_list <- list(
      ftext(soc_titles[1], body_text_style_italic),
      ftext(" and ", body_text_style),
      ftext(soc_titles[2], body_text_style_italic)
    )

  } else {
    # Three or more titles - "Title1, Title2, and Title3"

    # Build list of ftext objects


    # Add all titles except the last one with commas
    for (i in 1:(titles_length - 1)) {
      ftext_list <- append(ftext_list, list(
        ftext(soc_titles[i], body_text_style_italic),
        ftext(", ", body_text_style)
      ))
    }

    # Add final "and" and last title
    ftext_list <- append(ftext_list, list(
      ftext("and ", body_text_style),
      ftext(soc_titles[titles_length], body_text_style_italic)
    ))
  }

  # Create fpar with all ftext objects
  if(addmore==F){
    final_offer <- do.call(fpar, ftext_list)
  } else {
    final_offer <-   ftext_list
  }
  return(final_offer)
}

format_jobs_related_occ <- list(
    ftext(jobs, body_text_style_italic),
    ftext(related_occupations_or_no(), body_text_style_related_occupations)
    )



### Main officeR function
###
format_paragraph <- function(paragraph1, paragraph2, jobs_related_or_specific=format_jobs_related_occ) {
  selected_jobs_or_all <- jobs_related_or_specific
  fpar_body <- list(ftext(paragraph1, body_text_style)) %>%
    append(selected_jobs_or_all) %>%
    append(
      list(
        ftext(paragraph2, body_text_style),
        fp_p = fp_par(text.align = "justify")))
  final_offer <- do.call(fpar, fpar_body)
  return(final_offer)
}

format_sentence <- function(line1, line2, jobs_related_or_specific=format_jobs_related_occ) {
  selected_jobs_or_all <- jobs_related_or_specific
  fpar_body <- list(ftext(line1, body_text_style)) %>%
    append(selected_jobs_or_all) %>%
    append(list(ftext(line2, body_text_style)))
  return(fpar_body)
}

format_sentence_to_paragraph <- function(lines){

  fp_p_list <- list(fp_p = fp_par(text.align = "justify"))
  fpar_body <-append(lines, fp_p_list)
  final_offer <- do.call(fpar, fpar_body)
  return(final_offer)
}
