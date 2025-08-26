# Make sure these are defined at the top of your script, or where your other font styles are.
#body_text_style <- fp_text(font.size = 11, font.family = "Tw Cen MT")
#body_text_style_bold <- fp_text(font.size = 11, font.family = "Tw Cen MT", bold = TRUE)
#body_text_style_italic <- fp_text(font.size = 11, font.family = "Tw Cen MT", italic = TRUE)
#body_text_style_italicbold <- fp_text(font.size = 11, font.family = "Tw Cen MT", italic = TRUE, bold = TRUE)

# --- Define endo_func to return ftext objects for text and checkbox ---
# This function now creates a paragraph with separate formatting for "Yes/No" and the checkbox

# Install and load necessary packages
# install.packages("flextable")
# install.packages("officer")
library(flextable)
library(dplyr)
library(officer)
library(markdown) # Keep loaded for potential markdown rendering later
library(tidyverse)

regional_ave_wage <- function(wage_region){last(pull(reg_demand(wage_region), "Avg. Hourly Earnings"))}

#last(pull(reg_demand(wage_region), "Avg. Hourly Earnings"))
#regional_ave_wage("N")

formatted_wage <- function(wage){
  paste0("$",
         str_pad(
           round(wage,2),
           "right", width = 5, "0"
         )
  )
}
region_region <- function(x){
  if (x %in% c("N", "S")) {
    paste0(x, "CV/",x,"ML", " subregion")
  } else {
    paste0("CVML region")
  }
}


N_S_CVML <- function(x, b="") {
  if (str_sub(x, 1,1)=="N") {
    paste0("NCV/NML", b)
  } else if (str_sub(x, 1,1)=="S"){
    paste0("SCV/SML", b)
  } else {
    paste0("CVML", b)
  }
}
N_S_CVML("C")

N_S_CVML_ex <- function(x)if (str_sub(x, 1,1)=="N"){
  "4a"
} else if (str_sub(x, 1,1)=="S"){
  "4b"
} else {
  "5"
}
# Define colors and border color
color_light_yellow_green = "#E5EBB0"
color_light_blue = "#71FDDE"
color_border = "#000000"
thin_border <- fp_border(color = color_border, style = "solid", width = .5)
#params
#region_subregion <- paste0(N_S_CVML(str_sub(area_direction, 1,1)), " subregion")
# Define comment strings (using raw strings to avoid issues with backslashes in data frame creation)


sum_compare_text <- function(data) {
  if(nrow(data)==1){
    opp <- tibble(
      compare = !data$compare,
      weight = 1-data$weight
    )
    d_opp <- bind_rows(data, opp)
    d_opp <- paste0(pull(d_opp[d_opp$compare==T,], "weight")*100, "%")
    return(d_opp)
  } else {
    d_opp <- paste0(pull(data[data$compare==T,], "weight")*100, "%")
  }
  return(d_opp)
}


summary_wages_func <- function(reg){

  reg_lw <- if (reg == "N") {
    ncv_living_wage
  } else if (reg == "S") {
    scv_living_wage
  } else if (reg == "CV") {
    cvml_living_wage
  }
  wages_sentence_1 <- paste0(
    str_to_title(((reg_lw_count(reg)))),
    " of the ",
    xfun::n2w(length(SOC)),
    " occupations included in this report ",
    "(representing ", sum_compare_text(lw_compare2),
    " of annual openings) ",
    "have an entry-level hourly wage")
}

summary_wages_p2 <- function(reg){

  reg_lw <- if (reg == "N") {
    ncv_living_wage
  } else if (reg == "S") {
    scv_living_wage
  } else if (reg == "CV") {
    cvml_living_wage
  }
  wage_sentenct_p2 <- paste0(" above the ",
                             N_S_CVML(str_sub(area_direction, 1,1)),
                             " living wage of ",
                             reg_lw,
                             ".")
  return(wage_sentenct_p2)
}




# Define the data frame exactly as provided by the user's tibble output structure
# Replacing placeholders with full comment strings where they seem intended based on visual and context

endo_magnitude <- c(
  "Yes" = "#17C71B",
  "Maybe"= "#FFC000",
  "No"= "#FF0000"
)

endo_func <- function(endorsement#, endo_magnitude=NULL
){
  endo <- vector(mode = "character", length = 2)
  y_n <- vector(mode = "character", length = 2)

  if(endorsement == T){
    endo[1] <- "\u2611"
    endo[2] <- "\u2610"
    #y_n <- c("\u2705", "\u2610")

  } else if(endorsement == F){
    endo[1] <- "\u2610"
    endo[2] <- "\u2611"
    #y_n <- rev(c("\u2705", "\u2610"))
  }

  y_n[1] <- paste0("Yes ", endo[1])
  y_n[2] <- paste0("No ", endo[2])

  #y_n[1] <- endo[1]
  #y_n[2] <- endo[2]

  return(y_n)
}

endo_color_func <- function(endorsement, endo_magnitude="Yes"){

  col <- list()
  if(endorsement == T){
    col[[1]] <- c("c2", "c3")

  } else if(endorsement == F){
    col[[1]] <- c("c4", "c5")
  }

  if(endo_magnitude == "Yes") {
    color <-   "#32A431"
  } else if (endo_magnitude == "Maybe") {
    color <- "#FFC000"
  } else if (endo_magnitude == "No") {
    color <-   "#C00000"
  } else {
    color <- "#FFFFFF"
  }

  col[[2]] <- color
  return(col)
}

endo_color_func(T, "Yes")[[1]]
endo_color_func(T, "Yes")[1:2]
endo_color_func(T, "Yes")[3]

endo_func(T)[[1]]
endo_func(T)[1]
endo_func(T)
supply_gap_dec <- ifelse(regional_openings_summary > (cc_supply_average_summary + ncc_supply_average_summary), T, F)
endo_func(supply_gap_dec)

summary_1_table

lw_compare <- ex4_pick %>%
  filter(str_detect(lev, "Entry")) %>%
  filter(earnings != 0) %>%
  mutate(living_wage = as.numeric(str_remove_all(living_wage, pattern = "\\$"))) %>%
  mutate(Description = str_squish(as.character(Description))) %>%
  select(Description, earnings, living_wage) %>%
  mutate(compare = ifelse(earnings > living_wage, T, F))

lw_compare2 <- summary_1_table %>%
  mutate(jobs = str_squish(jobs)) %>%
  left_join(lw_compare, by = c("jobs" = "Description")) %>%
  mutate(weight = as.numeric(str_remove_all(weight, pattern = " %"))*.01) %>%
  select(jobs, Demand, weight, earnings, living_wage, compare)  %>%
  group_by(compare) %>%
  summarize(
    weight = sum(weight)
  )


sum_compare <- function(data) {
  if(nrow(data)==1){
    opp <- tibble(
      compare = !data$compare,
      weight = 1-data$weight
    )
    d_opp1 <- bind_rows(data, opp)
    d_opp <- ifelse(pull(d_opp1[d_opp1$compare==T,], "weight") > pull(d_opp1[d_opp1$compare==F,], "weight"), T, F)
    return(d_opp)
  } else {
    d_opp <- ifelse(pull(data[data$compare==T,], "weight") > pull(data[data$compare==F,], "weight"), T, F)
  }
  return(d_opp)
}



#sum_compare_text(e_compare)

e_compare <- summary_1_table %>%
  mutate(jobs = str_squish(jobs)) %>%
  mutate(weight = as.numeric(str_remove_all(weight, pattern = " %"))*.01) %>%
  mutate(CCAttainment = as.numeric(str_remove_all(CCAttainment, pattern = "%"))) %>%
  mutate(compare = ifelse(`Typical Entry Level Education` == "Some College or Associate Degree", T, F)) %>%
  group_by(compare) %>%
  summarize(
    weight = sum(weight)
  )

e_compare_text <- summary_1_table %>%
  mutate(jobs = str_squish(jobs)) %>%
  mutate(weight = as.numeric(str_remove_all(weight, pattern = " %"))*.01) %>%
  mutate(CCAttainment = as.numeric(str_remove_all(CCAttainment, pattern = "%"))) %>%
  #mutate(compare = ifelse(`Typical Entry Level Education` == "Some College or Associate Degree", T, F)) %>%
  group_by(`Typical Entry Level Education`) %>%
  summarize(
    weight = sum(weight)
  ) %>%
  arrange(desc(weight)) %>%
  slice(1)

#e_decision <- ifelse(pull(e_compare[e_compare$compare==T,], "weight") > pull(e_compare[e_compare$compare==F,], "weight"), T, F)

e_decision <- sum_compare(e_compare)
lw_decision <- sum_compare(lw_compare2)
endo_color_func(supply_gap_dec, )
decision_color <- function(dec) {
  if(dec==T){
    "Yes"
  } else {
    "No"
  }
}


sg_comments_raw <- paste0("Comments: ",
                          "There are projected to be ",
                          paste(prettyNum(CHANGEME_BOLD_NUMBER, big.mark = ",")),
                          " annual job openings throughout the ",
                          region_region("N"),
                          " for ",
                          general_field,
                          "-related occupations, which are ",
                          CHANGEME_BOLD_MORE_OR_LESS,
                          " than the three-year average of ",
                          CHANGEME_BOLD_AWARD_NUMBER,
                          " awards conferred by educational institutions. (CC + Non-CC)."
)

lw_comments_raw <- paste0(
  "Comments: ",
  summary_wages_func(str_sub(area_direction, 1,1)),
  summary_wages_p2(str_sub(area_direction, 1,1))
)


ed_comments_raw <- paste0(
  "Comments: The typical entry-level education for ",
  general_field, "-related occupations is a ",
  pull(e_compare_text, 1),
  " (about ", 100*pull(e_compare_text, 2), "%). ",
  " Additionally, between ",
  paste(range(summary_1_table$CCAttainment), collapse = " and "),
  " have completed some college or an associate degree as their highest level of education.")

sg_comments_raw2 <- as_paragraph(
  as_chunk("Comments:", prop = body_text_style_italic), # Use your italic style
  as_chunk(" There are projected to be ", prop = body_text_style), # Use your default body style
  as_chunk(paste(prettyNum(CHANGEME_BOLD_NUMBER, big.mark = ",")), prop = body_text_style_bold), # Use your bold style
  as_chunk(" annual job openings ", prop = body_text_style_bold),
  as_chunk(paste0("throughout the ", region_region(str_sub(area_direction,1,1))), prop = body_text_style), # Apply style to the function output
  as_chunk(" for ", prop = body_text_style),
  as_chunk(general_field, prop = body_text_style_italic), # Apply style to the variable
  as_chunk("-related occupations, which are ", prop = body_text_style),
  as_chunk(CHANGEME_BOLD_MORE_OR_LESS, prop = body_text_style_bold), # Use your bold style
  as_chunk(" than the three-year average of ", prop = body_text_style_bold),
  as_chunk(CHANGEME_BOLD_AWARD_NUMBER, prop = body_text_style_bold), # Use your bold style
  as_chunk(" awards conferred by educational institutions. (CC + Non-CC).", prop = body_text_style_bold)
)

lw_comments_raw2 <- as_paragraph(
  as_chunk("Comments:", prop = body_text_style_italic), # Use your italic style
  as_chunk(" ", prop = body_text_style), # Add a space with default style
  as_chunk(summary_wages_func(str_sub(area_direction, 1,1)), prop = body_text_style), # Apply style to the function output
  as_chunk(summary_wages_p2(str_sub(area_direction, 1,1)), prop = body_text_style_bold) # Apply style to the function output
)

ed_comments_raw2 <- as_paragraph(
  as_chunk("Comments:", prop = body_text_style_italic), # Use your italic style
  as_chunk(" The typical entry-level education for ", prop = body_text_style),
  as_chunk(general_field, prop = body_text_style_italic),
  as_chunk("-related occupations is a ", prop = body_text_style),
  as_chunk(pull(e_compare_text, 1), prop = body_text_style), # Apply style
  as_chunk(" (about ", prop = body_text_style),
  as_chunk(paste0(100 * pull(e_compare_text, 2), "%"), prop = body_text_style), # Apply style
  as_chunk("). Additionally, ", prop = body_text_style),
  as_chunk(paste0("between ", paste(range(summary_1_table$CCAttainment), collapse = " and ")), prop = body_text_style_bold), # Apply style
  as_chunk(" have completed some college or an associate degree as their highest level of education.", prop = body_text_style_bold)
)
library(officer)
# --- Define endo_func to return flextable chunk objects for text and checkbox ---
# This function now creates a paragraph with separate formatting for "Yes/No" and the checkbox
endo_func_formatted <- function(endorsement, endo_magnitude_color) {
  # Define checkbox characters
  checked_box <- "\u2611" # ✅
  unchecked_box <- "\u2610" # ☐

  # Define the color for the CHECKED checkbox based on endo_magnitude_color
  checked_checkbox_color <- if (endo_magnitude_color == "Yes") {
    "#32A431" # Green for Yes
  } else if (endo_magnitude_color == "Maybe") {
    "#FFC000" # Orange for Maybe
  } else if (endo_magnitude_color == "No") {
    "#C00000" # Red for No
  } else {
    "#FFFFFF" # Default white if no specific match
  }

  # Create text properties for black "Yes" or "No"
  black_text_prop <- officer::fp_text(color = "black", font.size = 14, font.family = "Tw Cen MT")

  # Create text properties for the COLORED CHECKED checkbox
  colored_checkbox_prop <- officer::fp_text(color = checked_checkbox_color, font.size = 14, font.family = "Tw Cen MT")

  # Create text properties for the BLACK UNCHECKED checkbox
  black_checkbox_prop <- officer::fp_text(color = "black", font.size = 14, font.family = "Tw Cen MT")


  if (endorsement == TRUE) {
    # If endorsed, "Yes" is checked (colored), "No" is unchecked (black)
    yes_paragraph <- flextable::as_paragraph(
      flextable::as_chunk("Yes ", prop = black_text_prop),
      flextable::as_chunk(checked_box, prop = colored_checkbox_prop) # Apply colored prop to checked box
    )
    no_paragraph <- flextable::as_paragraph(
      flextable::as_chunk("No ", prop = black_text_prop),
      flextable::as_chunk(unchecked_box, prop = black_checkbox_prop) # Apply black prop to unchecked box
    )
  } else {
    # If not endorsed, "No" is checked (colored), "Yes" is unchecked (black)
    yes_paragraph <- flextable::as_paragraph(
      flextable::as_chunk("Yes ", prop = black_text_prop),
      flextable::as_chunk(unchecked_box, prop = black_checkbox_prop) # Apply black prop to unchecked box
    )
    no_paragraph <- flextable::as_paragraph(
      flextable::as_chunk("No ", prop = black_text_prop),
      flextable::as_chunk(checked_box, prop = colored_checkbox_prop) # Apply colored prop to checked box
    )
  }

  return(list(yes = yes_paragraph, no = no_paragraph))
}

#endo_func_formatted(supply_gap_dec, supply_gap_checkbox_color)$yes

# Determine the magnitude color for the checkboxes
supply_gap_checkbox_color <- decision_color(supply_gap_dec)
lw_checkbox_color <- decision_color(lw_decision)
e_checkbox_color <- decision_color(e_decision)

# --- Update body_data_7x5 to use the new endo_func_formatted and as_paragraph ---
body_data_7x5 <- data.frame(
  c1 = c("Program LMI Endorsement Criteria", "Supply Gap:", "Supply Gap:", "Living Wage:\n(Entry-Level, 25th):", "Living Wage:\n(Entry-Level, 25th):", "Education:", "Education:"),
  stringsAsFactors = FALSE
)

# Populate columns c2-c5 using the formatted paragraphs
body_data_7x5 <- body_data_7x5 %>%
  mutate(
    c2 = c("Program LMI Endorsement Criteria",
           endo_func_formatted(supply_gap_dec, supply_gap_checkbox_color)$yes,
           sg_comments_raw,
           endo_func_formatted(lw_decision, lw_checkbox_color)$yes,
           lw_comments_raw,
           endo_func_formatted(e_decision, e_checkbox_color)$yes,
           ed_comments_raw),
    c3 = c("Program LMI Endorsement Criteria",
           endo_func_formatted(supply_gap_dec, supply_gap_checkbox_color)$yes,
           sg_comments_raw,
           endo_func_formatted(lw_decision, lw_checkbox_color)$yes,
           lw_comments_raw,
           endo_func_formatted(e_decision, e_checkbox_color)$yes,
           ed_comments_raw),
    c4 = c("Program LMI Endorsement Criteria",
           endo_func_formatted(supply_gap_dec, supply_gap_checkbox_color)$no,
           sg_comments_raw,
           endo_func_formatted(lw_decision, lw_checkbox_color)$no,
           lw_comments_raw,
           endo_func_formatted(e_decision, e_checkbox_color)$no,
           ed_comments_raw),
    c5 = c("Program LMI Endorsement Criteria",
           endo_func_formatted(supply_gap_dec, supply_gap_checkbox_color)$no,
           sg_comments_raw,
           endo_func_formatted(lw_decision, lw_checkbox_color)$no,
           lw_comments_raw,
           endo_func_formatted(e_decision, e_checkbox_color)$no,
           ed_comments_raw)
  )

# --- Continue with your flextable creation and formatting as before ---
ft <- flextable(body_data_7x5)
ft <- delete_part(ft, part = "header")
ft <- add_header_row(ft, values = as.list(body_data_7x5[1, ]), top = TRUE)
ft <- color(ft, color = "#455F51", part = "header")
ft <- delete_rows(ft, i = 1, part = "body")
ft <- merge_at(ft, i = 1, j = 1:5, part = "header")
ft <- bg(ft, i = 1, part = "header", bg = color_light_blue)
ft <- color(ft, i = 1, part = "header", color = "#455F51")
ft <- font(ft, fontname = "Tw Cen MT", part = "header")
ft <- bold(ft, part = "header", bold = FALSE)
ft <- align(ft, align = "center", part = "header")
ft <- fontsize(ft, size = 16, part = "header")
ft <- hline_top(ft, part = "header", border = thin_border)
ft <- hline_bottom(ft, part = "header", border = thin_border)
ft <- font(ft, fontname = "Tw Cen MT", part = "body")
ft <- fontsize(ft, size = 11, part = "body")
ft <- fontsize(ft, j = 1 ,size = 12, part = "body")
ft <- align(ft, j = "c1", align = "left", part = "body")
ft <- valign(ft, part = "body", valign = "top")

# No longer need 'color' calls here as formatting is applied during 'compose'
# ft <- color(ft, i = 1, j = c_combo(supply_gap_dec)[[1]], part = "body", color = c_combo(supply_gap_dec)[[2]])
# ft <- color(ft, i = 3, j = c_combo(lw_decision)[[1]], part = "body", color = c_combo(lw_decision)[[2]])
# ft <- color(ft, i = 5, j = c_combo(e_decision)[[1]], part = "body", color = c_combo(e_decision)[[2]])


# Compose the paragraphs into the cells.
# Since the merge_h(ft, i = 1:6, part = "body") is applied later,
# we need to compose into the columns that will be merged for Yes/No cells.
# For Yes/No rows (1, 3, 5), compose into the starting cells (c2 for Yes, c4 for No)
ft <- compose(ft, i = 1, j = c("c2", "c3"), value = endo_func_formatted(supply_gap_dec, supply_gap_checkbox_color)$yes, part = "body")
#ft <- compose(ft, i = 1, j = "c3", value = endo_func_formatted(supply_gap_dec, supply_gap_checkbox_color)$yes, part = "body")
ft <- compose(ft, i = 1, j = c("c4", "c5"),value = endo_func_formatted(supply_gap_dec, supply_gap_checkbox_color)$no, part = "body")
#ft <- compose(ft, i = 1, j = "c5", value = endo_func_formatted(supply_gap_dec, supply_gap_checkbox_color)$no, part = "body")

ft <- compose(ft, i = 3, j = "c2", value = endo_func_formatted(lw_decision, lw_checkbox_color)$yes, part = "body")
ft <- compose(ft, i = 3, j = "c3", value = endo_func_formatted(lw_decision, lw_checkbox_color)$yes, part = "body")
ft <- compose(ft, i = 3, j = "c4", value = endo_func_formatted(lw_decision, lw_checkbox_color)$no, part = "body")
ft <- compose(ft, i = 3, j = "c5", value = endo_func_formatted(lw_decision, lw_checkbox_color)$no, part = "body")


ft <- compose(ft, i = 5, j = "c2", value = endo_func_formatted(e_decision, e_checkbox_color)$yes, part = "body")
ft <- compose(ft, i = 5, j = "c3", value = endo_func_formatted(e_decision, e_checkbox_color)$yes, part = "body")
ft <- compose(ft, i = 5, j = "c4", value = endo_func_formatted(e_decision, e_checkbox_color)$no, part = "body")
ft <- compose(ft, i = 5, j = "c5", value = endo_func_formatted(e_decision, e_checkbox_color)$no, part = "body")


# Compose comments as before
ft <- compose(ft, i = 2, j = c("c2", "c3", "c4", "c5"), value = sg_comments_raw2, part = "body")
ft <- compose(ft, i = 4, j = c("c2", "c3", "c4", "c5"), value = lw_comments_raw2, part = "body")
ft <- compose(ft, i = 6, j = c("c2", "c3", "c4", "c5"), value = ed_comments_raw2, part = "body")

# --- Continue with merges and other formatting ---
ft <- align(ft, i = c(1, 3, 5), j = c("c2", "c3", "c4", "c5"),
            align = "center", part = "body")
ft <- align(ft, i = c(2, 4, 6), j = c("c2", "c3", "c4", "c5"), align = "left", part = "body")
ft <- align(ft, i = c(1:6), j = "c1", align = "center", part = "body")
ft <- border(ft, i = c(1,3,5), part = "body", border.bottom = fp_border(color = "grey", style = "solid", width = .1))
ft <- border(ft, i = c(2,4,6), part = "body", border.bottom = thin_border)
ft <- border(ft, i = c(1:6), j = 1, part = "body", border.bottom = thin_border)
ft <- border(ft, i = 6, part = "body", border.bottom = fp_border(color = "white", style = "solid", width = .25))
ft <- border(ft, i = 5,j=1, part = "body", border.bottom = fp_border(color = "white", style = "solid", width = .25))
ft <- fontsize(ft, i = c(1, 3, 5), j = c("c2", "c3", "c4", "c5"), part = "body", size = 14)
ft <- valign(ft, i = c(1, 3, 5), valign = "center")
ft <- merge_v(ft, j = "c1", part = "body")

# Merge horizontally for Yes/No cells (c2 and c3 for Yes, c4 and c5 for No)
ft <- merge_h(ft, i = c(1, 3, 5), part = "body")
ft <- merge_h(ft, i = c(2, 4, 6), part = "body")

# Merge horizontally for Comment blocks (rows 2, 4, 6 in current body)
#ft <- merge_at(ft, i = c(2, 4, 6), j = c("c2", "c3", "c4", "c5"), part = "body")


ft <- set_table_properties(ft,
                           layout = "fixed",
                           width = 1,
                           align = "center") %>%
  flextable::padding(part = "header", padding.top = 0, padding.bottom = 0) %>%
  flextable::padding(part = "body", i = c(1,3,5), padding.top = 1, padding.bottom = 1)

ft <- width(ft, j = 1, width = 1.7, unit = "in")
ft <- width(ft, j = c(2:5), width = (6.5-1.7)/4, unit = "in")

# --- Continue with the rest of your flextable code (summary_header_top, last_part, and docx output) ---
# Your existing code for summary_header_top and last_part seems fine as they don't have the same mixed-color text requirement.
# Just ensure 'yes_count' is correctly derived from the outcome of 'supply_gap_dec', 'lw_decision', 'e_decision'.

# Recalculate yes_count based on decisions

yeses <- body_data_7x5[c(2,4,6),2]
yes_count <- str_detect(yeses, pattern = "\u2611") %>% sum()

yes_count_list <- list(supply_gap_dec, lw_decision, e_decision)
yes_count <- sum(unlist(lapply(yes_count_list, function(x) x == TRUE)))

# Re-run your `summary_header_top` and `last_part` creation with the updated `yes_count`
# ... (your existing code for summary_header_top and last_part) ...

colwidths <- c(10, 1, 6, 1, 6, 1, 6, 1) # sums to 32

# Create an empty flextable with 32 columns

#empty_tbl[1,] <- t(as.data.frame(rep("Program LMI Endorsement Criteria", 32)))

# Create an empty flextable with 32 columns
empty_tbl <- as_tibble(matrix(ncol = 32, nrow = 0, dimnames = list(NULL, paste0("col", 1:32))))
summary_header_top <- flextable(empty_tbl)


values = c(
  "Program LMI Endorsement",
  "Endorsed: All LMI Criteria Met ",
  "Endorsed: Some LMI Criteria Met ",
  "Not LMI Endorsed "
)

your_function <- function(yes_count) {
  if (yes_count == 1 || yes_count == 2) {
    values <- c(
      "Program LMI Endorsement", " ",
      "Endorsed: All LMI Criteria Met ", "\u2610",
      "Endorsed: Some LMI Criteria Met ", "\u2327",
      "Not LMI Endorsed ", "\u2610"
    )
  } else if (yes_count == 3) {
    values <- c(
      "Program LMI Endorsement", " ",
      "Endorsed: All LMI Criteria Met ", "\u2327",
      "Endorsed: Some LMI Criteria Met ", "\u2610",
      "Not LMI Endorsed ", "\u2610"
    )
  } else {
    values <- c(
      "Program LMI Endorsement", " ",
      "Endorsed: All LMI Criteria Met ", "\u2610",
      "Endorsed: Some LMI Criteria Met ", "\u2610",
      "Not LMI Endorsed ", "\u2327"
    )
  }
  return(values)
}


summary_header_top <- add_body_row(summary_header_top, values = your_function(yes_count), colwidths = colwidths, top = TRUE)
summary_header_top <- delete_part(summary_header_top, part = "header")
summary_header_top <- bg(summary_header_top, i = 1, part = "body", bg = color_light_yellow_green)
summary_header_top <- color(summary_header_top, color = "#455F51", part = "all")
summary_header_top <- align(summary_header_top, align = "center", part = "body")
summary_header_top <- valign(summary_header_top, valign = "center", part = "body")
summary_header_top <- fontsize(summary_header_top, size = 13, part = "body")
summary_header_top <- bold(summary_header_top, i = 1, j = 1, part = "body", bold = FALSE)
summary_header_top <- fontsize(summary_header_top, i = 1, j = 1, part = "body", size = 16)
summary_header_top <- font(summary_header_top, fontname = "Tw Cen MT", part = "body")
summary_header_top <- border_remove(summary_header_top) %>%
  set_table_properties(layout = "autofit", width = 1, align = "center")
row_height <- .4
summary_header_top <- height(summary_header_top, part = "header", height = row_height)
white_border <- fp_border(color = "white", style = "solid", width = 2)
summary_header_top <- hline_bottom(summary_header_top, border = white_border, part = "body")
summary_header_top <- hline_bottom(summary_header_top, border = white_border, part = "header")
summary_header_top <- bg(summary_header_top, i = 1, part = "header", bg = color_light_blue)
summary_header_top <- color(summary_header_top, color = "#455F51", part = "body")

# last_part definition
last_part <- as_tibble(matrix(ncol = 5, nrow = 0, dimnames = list(NULL, paste0("col", 1:5)))) %>%
  flextable() %>%
  add_body_row(values = c("Yes \u2610","Yes \u2610","", "No \u2611" , "No \u2611"), colwidths = rep(1, 5), top = TRUE) %>%
  add_body_row(values = rep("Comments: N/A", 5), colwidths = rep(1, 5), top = FALSE) %>%
  add_header_row(values = rep("Emerging Occupations(s)", 5), colwidths = rep(1, 5), top = TRUE) %>%
  flextable::delete_rows(i = 2, part = "header") %>%
  bg(part = "header", bg = "#BFBFBF") %>%
  color(part = "header", color = "#455F51") %>%
  font(fontname = "Tw Cen MT", part = "all") %>%
  align(align = "center", part = "all") %>%
  fontsize(size = 16, part = "header") %>%
  hline_top(part = "header", border = thin_border) %>%
  hline_bottom(part = "header", border = thin_border) %>%
  fontsize(size = 14, i = 1, part = "body") %>%
  fontsize(size = 12, i = 2, part = "body") %>%
  hline_bottom(part = "body", border = thin_border) %>%
  merge_h(part = "body") %>%
  merge_h(part = "header") %>%
  flextable::padding(part = "header", padding.top = 0, padding.bottom = 0) %>%
  flextable::padding(part = "body", padding.top = 1, padding.bottom = 1) %>%
  set_table_properties(layout = "autofit", width = 1, align = "center")

# Print the flextable to Word
bit <- read_docx("LMA_TEMPLATE.docx") %>%
  body_add_flextable(summary_header_top) %>%
  body_add_par(value = " ", style = "flextable_break") %>%
  body_add_flextable(ft) %>%
  body_add_par(value = " ", style = "flextable_break") %>%
  body_add_flextable(last_part)

# Save the final document
print(bit, target = "testing_summary.docx")
