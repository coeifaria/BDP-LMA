#```{r officer_03jobpostings, chunk_progress=TRUE}

#### Text
text_h2 <- "Job Postings:"
text_b_1 <- "Important Online Job Postings Data Note: "
text_b_1_1 <- "Online job postings data is sourced from Lightcast, a labor market analytics firm that scrapes, collects, and organizes data from online job boards such as LinkedIn, Indeed, Glassdoor, Monster, GovernmentJobs.com, and thousands more. Lightcast uses natural language processing (NLP) to determine the related company, industry, occupation, and other information for each job posting. However, NLP has limitations that include understanding contextual words of phrases; determining differences in words that can be used as nouns, verbs, and/or adjectives; and misspellings or grammatical errors^2.   For these reasons, job postings could be assigned to the wrong employer, industry, or occupation within Lightcast’s database."
text_b_2 <- "Additionally, there are several limitations when analyzing job postings. A single job posting may not represent a single job opening, as employers may be creating a pool of candidates for future openings or hiring for multiple positions with a single posting. Additionally, not all jobs are posted online, and jobs may be filled through other methods such as internal promotion, word-of-mouth advertising, physical job boards, or a variety of other channels."

text_b_3 <- paste0(
  "There were ",
  total_job_postings,
  " online job postings related to ")
text_b_3_1 <- jobs

text_b_3_2 <- paste0(" listed in the past 12 months. Exhibit 6 shows the number of job postings for ",
ifelse(length(SOC)>1, "each", "this"),
" occupation."
)
text_h3_1 <- paste0("Exhibit 6: Number of Job Postings by Occupation (n=", total_job_postings, ")")

text_b_4 <- "The top employers in the region for "
text_b_4_1 <- jobs
text_b_4_2 <- ", by number of job postings, are shown in Exhibit 7."

text_h3_2 <- paste0("Exhibit 7: Top Employers by Number of Job Postings (n=", total_job_postings, ")")
text_fn_1 <- "  K. R. Chowdhary, Fundamentals of Artificial Intelligence (Basingstoke: Springer Nature, 2020), https://link.springer.com/book/10.1007/978-81-322-3972-7."
text_b_5 <- "The top specialized, common, and software skills for "
text_b_5_1 <- jobs
text_b_5_2 <- " are listed by those most frequently mentioned in job postings (denoted in parentheses) are shown in Exhibit 8."
text_h3_3 <- paste0("Exhibit 8: Top Skills by Number of Job Postings (n=", total_job_postings, ")")

#img_size_6 <- adjust_image_size("exhibit6.png")
#img_size_7 <- adjust_image_size("exhibit7.png")
#img_size_8 <- adjust_image_size("exhibit8.png")

#### Create Components
footnote_1 <- block_list(
  fpar(
    ftext(text_fn_1, fp_text(font.size = 10, color = "black", font.family = "Tw Cen MT")))
)

body_fpar_1  <- fpar(ftext(text_b_1, prop = body_text_style_italicbold))
body_fpar_1  <- fpar(ftext(text_b_1_1, prop = body_text_style_italic))
body_fpar_2  <- fpar(ftext(text_b_2, prop = body_text_style_italic), run_footnote(x = footnote_1, prop = fp_text(bold = F, color = "black", vertical.align = "superscript")), fp_p = fp_par(text.align = "left"))

#body_fpar_3  <- fpar(ftext(text_b_3, prop = body_text_style))
#body_fpar_3_1  <- fpar(ftext(text_b_3_1, prop = body_text_style_italic))
#body_fpar_3_2  <- fpar(ftext(text_b_3_2, prop = body_text_style))

body_fpar_3  <- fpar(
  ftext(text_b_3, prop = body_text_style),
  ftext(text_b_3_1, prop = body_text_style_italic),
  ftext(text_b_3_2, prop = body_text_style)
)

body_fpar_4  <- fpar(
  ftext(text_b_4, prop = body_text_style),
  ftext(text_b_4_1, prop = body_text_style_italic),
  ftext(text_b_4_2, prop = body_text_style)
)

#body_fpar_4_1  <- fpar(ftext(text_b_4_1, prop = body_text_style_italic))
#body_fpar_4_2  <- fpar(ftext(text_b_4_2, prop = body_text_style))

header3_fpar2 <- fpar(ftext(text_h3_2, H3),  fp_p = fp_par(text.align = "center"))

body_fpar_5  <- fpar(
  ftext(text_b_5, prop = body_text_style),
  ftext(text_b_5_1, prop = body_text_style_italic),
  ftext(text_b_5_2, prop = body_text_style)
)
#body_fpar_5_1  <- fpar(ftext(text_b_4_1, prop = body_text_style_italic))
#body_fpar_5_2  <- fpar(ftext(text_b_4_2, prop = body_text_style))

# Headers and images combined in single `fpar` blocks


block_1 <- fpar(
  ftext(text_h3_1, H3),
  fp_p = fp_par(text.align = "center")#,
  #external_img("exhibit6.png", width = img_size_6$width, height = img_size_6$height)
)

block_2 <- fpar(
  ftext(text_h3_2, H3),
  fp_p = fp_par(text.align = "center")#,
  #external_img("exhibit7.png", width = img_size_7$width, height = img_size_7$height)
)

block_3 <- fpar(
  ftext(text_h3_3, H3),
  fp_p = fp_par(text.align = "center")#,
  #external_img("exhibit8.png", width = img_size_8$width, height = img_size_8$height)
)

#### CONSTRUCT DOCUMENT
doc4 <- read_docx("lma_blank_template.docx") %>%
  body_add_fpar(fpar(ftext(text_h2, H2))) %>%
  body_add_par(" ", style = "Normal") %>%
  body_add_fpar(body_fpar_1) %>%
  body_add_par("", style = "Normal") %>%
  body_add_fpar(body_fpar_2) %>%
  body_add_par("", style = "Normal") %>%
  body_add_fpar(body_fpar_3) %>%
  #body_add_fpar(body_fpar_3_1) %>%
  #body_add_fpar(body_fpar_3_2) %>%
  body_add_par("", style = "Normal") %>%
  body_add_fpar(block_1) %>%
  body_add_flextable(e_6_ft) %>%
  body_add_par("", style = "Normal") %>%
  body_add_fpar(body_fpar_4) %>%
  #body_add_fpar(body_fpar_4_1) %>%
  #body_add_fpar(body_fpar_4_2) %>%
  body_add_par("", style = "Normal") %>%
  body_add_fpar(block_2) %>%
  body_add_flextable(e_7_ft) %>%
  body_add_par("", style = "Normal") %>%
  body_add_fpar(body_fpar_5) %>%
  #body_add_fpar(body_fpar_5_1) %>%
  #body_add_fpar(body_fpar_5_2) %>%
  body_add_par("", style = "Normal") %>%
  body_add_fpar(block_3) %>%
  body_add_flextable(e_8_ft) %>%
  body_add_par("", style = "Normal")

print(doc4, "officeR_03jobpostings.docx")


# Save the document
#print(doc, "texting.docx")
#print(doc, target = "z3JobPostings.docx")
#rm(doc)
