#```{r officer_1Demand, chunk_progress=TRUE}
#### Text

year_range_ex2 <- demand_projections %>%
  filter(Year >= year(Sys.Date()) -7) %>%
  pull(Year) %>%
  range()


maxyear_growth <- which.max(pull(e2[e2$region==region,], perc_growth))
state_occ_grow_or_shrink <- pull(e2[e2$region=="CVML",], perc_growth)[maxyear_growth]

text_h1 <- "Demand"
text_h2 <- "Occupational Projections"
text_b_1 <-
  paste0(
    "Exhibit 2 shows the annual percent change in jobs for ",
    jobs,
    " from ",
    year_range_ex2[1],
    " through ",
    year_range_ex2[2],
    #". Employment in these occupations project a ",last(e3_2 %>% select(ends_with("% Change"))), ifelse(str_detect(last(e3_2 %>% select(ends_with("% Change"))), "-"), " decline ", " growth "),  " across all occupations.",
    paste0(". The ", region_region(str_sub(region,1,1)), " experienced the highest growth in ", pull(e2[e2$region==region,], Year)[maxyear_growth], " at ",
           pull(e2[e2$region==region,], perc_growth)[maxyear_growth], "%, compared to the ", abs(state_occ_grow_or_shrink), "% ",
           ifelse(state_occ_grow_or_shrink >0, "growth", "decline"), " across all CA occupations."))

#From ",  "2019 to 2020",  " due to the COVID-19 pandemic, employment for ",  jobs,  " did not experience a decline until the following year, with a ",  "####",  "% #    #decrease from ","2020 to 2021",". However, there was a notable ",   "####",   "% increase in employment for ",   jobs,   " in ",   requesting_region,
#" between 2021 and 2022. Employment for ",  jobs,   " is projected to grow at a similar rate when compared to all occupations through 2028."  )

remove_these_words <- paste0("the|",paste(xfun::n2w(1:9), collapse = "|"))
text_h3_1 <- paste0("Exhibit 2: Annual Percent Change in Jobs for\n",
                    stringr::str_to_title(str_squish(str_remove_all(jobs, remove_these_words))),", ", as.character(min(e2$Year)),'-', as.character(max(e2$Year)))

text_b_2 <- paste0(
  "Exhibit 3 shows the five-year occupational demand projections for ",
  jobs,
  ". In the ", region, " subregion, the number of jobs related to ",
  ifelse(length(SOC)>1, "these occupations are", "this occupation is"),
  " projected to",
  ifelse(str_detect(
    pull(e3_2[e3_2$Geography == CHANGEME2_REGION2,], names(e3_2)[5])
    , "-"), " decrease ", " increase "), "by ",
    pull(e3_2[e3_2$Geography == CHANGEME2_REGION2,], names(e3_2)[5]),
  " through ", year_range_ex2[2],
  ". There are projected to be ",
  prettyNum(CHANGEME_BOLD_NUMBER, big.mark = ","),
  #prettyNum(pull(exhibit3_table[exhibit3_table$Geography==CHANGEME2_REGION2,], names(exhibit3_table)[6]), big.mark = ","),
  " jobs available annually in the ", CHANGEME2_REGION2, " subregion."
)

text_h3_2 <- paste0("Exhibit 3: Occupational Demand in NCV/NML, SCV/SML, and CVML")
text_fn_1 <- "Five-year change represents new job additions to the workforce. Annual openings include new jobs and replacement jobs that result from retirements and separations."

#text_b_1
#this_paragraph <- paste("Exhibit 2 shows the annual percent change in jobs for ",
#
#                        jobs,
#                        " from ",
#                        year_range_ex2[1],
#                        " through ",
#                        year_range_ex2[2],
#                        " .Employment in these occupations has a projected ",
#                        last(e3_2 %>% select(ends_with("% Change"))), ifelse(str_detect(last(e3_2 %>% select(ends_with("% Change"))), "-"), " decline ", " growth "),
#                        " across all occupations. In 2020, due to the COVID-19 pandemic, employment for ",
#                        jobs, "...")


#prompt <- paste("This is a short comparison of the South Central Valley/South Mother Lode (SCV/SML) and North Central Valley/ North Mother-Lode (NCV/NML) regions, and their combined CVML region. Complete the following paragraph by comparing the differences between the ", region, " and the overall CVML:", this_paragraph, " Give a comparison with the ", opposite_region, " as well. Explain the growth and the projected growth trajectory. Complete the paragraph by using the original sentences and adding up to 2 new sentences given the following data: ", paste(dplyr::pull(e2, Year),dplyr::pull(e2, perc_growth),dplyr::pull(e2, region), collapse = "|"), " in under 100 tokens.")

#tab <- call_claude_api_for_table(prompt = prompt, max_tokens = 800)


gemini.R::setAPI(gemini_api)
#tab <- gemini.R::gemini(prompt, model = "1.5-flash", temperature = 0.5, maxOutputTokens = 100)

#library(magick)
#img_size_2 <- adjust_image_size("exhibit2.png", dpi = 300)
#img_size_3 <- adjust_image_size("exhibit3.png")

#### Create Components

#footnote_1 <- ""
footnote_1 <- block_list(
  fpar(
    ftext(text_fn_1, fp_text(font.size = 10, color = "black", font.family = "Tw Cen MT")))
)

header1_fpar <- fpar(ftext(text_h1, H1))

header2_fpar <- fpar(ftext(text_h2, H2))

#body_fpar_1_1 <-
#  fpar(
#    ftext("Exhibit 2 shows the annual percent change in jobs for ", prop = body_text_style),
#    ftext(jobs, prop = fp_text(italic = TRUE, font.size = 11, font.family = "Tw Cen MT")),
#    ftext(paste0(" from 2017 through 2027. ",op_date_range_min, " to ", op_date_range_max), prop = body_text_style),
#    ftext(paste0("Though there was a ",op_date_range_min, " to ", op_date_range_max), prop = body_text_style),
#    ftext(".", prop = body_text_style)
#)

body_fpar_1  <- fpar(ftext(text_b_1, prop = highlighted_text))
body_fpar_2  <- fpar(ftext(text_b_2, prop = body_text_style))
header3_fpar2 <- fpar(ftext(text_h3_2, prop = H3), run_footnote(x = footnote_1, prop = fp_text(bold = F, color = exhibit_header_color, vertical.align = "superscript")), fp_p = fp_par(text.align = "center"))

block_1 <- fpar(
  ftext(text_h3_1, H3),
  fp_p = fp_par(text.align = "center")#,
  #external_img("exhibit2.png", width = img_size_2$width, height = img_size_2$height)
)

block_2 <- fpar(
  ftext(text_h3_2, H3),
  fp_p = fp_par(text.align = "center")#,
  #external_img("exhibit3.png", width = img_size_3$width, height = img_size_3$height)
)

#text_table_e2 <- e2 %>% print() %>% capture.output() %>% str_squish()
#prompt <- paste("The following is a table regarding the annual percent change for jobs in ", jobs, " Please analyze the key trends and patterns:")
#prompt <- paste(c(prompt, text_table_e2), collapse = ",")

#result <- tryCatch({
#  call_claude_api_for_table(
#    prompt = prompt,
#    model = "claude-3-opus-20240229",
#    max_tokens = 4000
#  )
#}, error = function(e) {
#  paste("API call error:", e$message)
#})

#cat(result)

#prompt <- "Rewrite the following analysis into a paragraph with at least 3 sentences. Mention the number of years of positive growth compared to negative growth, the effect of the pandemic, and projected growth. This is for the Central Valley Mother Lode Region (CVML), South CVML, and North CVML. Keep much of the original text, correct the numbers, and respond with your output."
#prompt <- paste0(prompt, result)

#claude_exhibit2 <- tryCatch({
#  call_claude_api_for_table(
#    prompt = prompt,
#    model = "claude-3-opus-20240229",
#    max_tokens = 2000
#  )
#}, error = function(e) {
#  paste("API call error:", e$message)
#})

####


#### CONSTRUCT DOCUMENT
doc2 <- read_docx("lma_blank_template.docx")
doc2 <- doc2 %>%
  body_add_fpar(header1_fpar) %>% body_add_par("", style = "Normal") %>%
  body_add_fpar(header2_fpar) %>% body_add_par("", style = "Normal") %>%
  #body_add_fpar(fpar(ftext(tab, prop = highlighted_text))) %>%
  body_add_fpar(body_fpar_1) %>% body_add_par("", style = "Normal") %>%
  body_add_par("", style = "Normal") %>%
  #body_add_fpar(body_fpar_1_1) %>% body_add_par("", style = "Normal") %>%

  #body_add_fpar(fpar(ftext(claude_exhibit2, prop = highlighted_text))) %>% body_add_par("", style = "Normal") %>%
  body_add_fpar(block_1) %>%
  #body_add_gg(exhibit2) %>%
  body_add_docx(src = "ex2_linked.docx", pos = "after") %>%
  body_add_fpar(body_fpar_2)  %>%
  body_add_par("", style = "Normal") %>%
  body_add_fpar(block_2) %>%
  body_add_flextable(exhibit3_ft) %>%
  body_add_par("", style = "Normal") %>%  # Add a blank line before the footnote
  body_add_fpar(footnote_1)  # Add the footnote

print(doc2, "officeR_1demand.docx")
