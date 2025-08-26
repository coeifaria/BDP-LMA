#```{r officer_02wages, chunk_progress=TRUE}
#### Text
ex4a_w <- exhibit4_ncv %>%
  filter(str_detect(lev, "Entry")) %>%
  filter(earnings != 0) %>%
  select(Description, earnings) %>%
  mutate(Description = str_replace_all(Description, "\n", " "),
         earnings = paste0(" ($", earnings, ")"),
         wages = paste0(Description, earnings)) %>%
  pull(wages) %>%
  c(paste0("NCV/NML living wage ", ncv_living_wage)) %>%
  paste(collapse = "|")

ex4b_w <- exhibit4_scv %>%
  filter(str_detect(lev, "Entry")) %>%
  filter(earnings != 0) %>%
  select(Description, earnings) %>%
  mutate(Description = str_replace_all(Description, "\n", " "),
         earnings = paste0(" ($", earnings, ")"),
         wages = paste0(Description, earnings)) %>%
  pull(wages) %>%
  c(paste0("SCV/SML living wage ", scv_living_wage)) %>%
  paste(collapse = "|")

ex4c_w <- exhibit5_cvml %>%
  filter(str_detect(lev, "Entry")) %>%
  filter(earnings != 0) %>%
  select(Description, earnings) %>%
  mutate(Description = str_replace_all(Description, "\n", " "),
         earnings = paste0(" ($", earnings, ")"),
         wages = paste0(Description, earnings)) %>%
  pull(wages) %>%
  c(paste0("CVML living wage ", cvml_living_wage)) %>%
  paste(collapse = "|")

exhibit4_paragraph_text <- function(ex_text, region_ex4) {
  intro <- "The typical entry-level hourly wage for "

  # Ensure living wage is consistent
  living_wage_val <- unique(ex_text$living_wage)
  region_type <- ifelse(str_detect(region_ex4, "\\/"), " subregion", " region")


  if (nrow(ex_text) == 1) {
    desc <- ex_text$Description[1]
    wage <- ex_text$earnings[1]
    status <- ifelse(wage >= living_wage_val, "above", "below")

    sentence <- paste0(
      intro, desc, " ($", wage, ") is ", status,
      " the living wage for one adult in the ", region_ex4, region_type,
      " ($", living_wage_val, ")."
    )

    ending <- paste0(" The ", region_ex4, " average wage for this occupation is $", average_wage(reg_demand(region_ex4)),
                     " which is", ifelse(average_wage(reg_demand(region_ex4)) >= average_wage(ca_demand), " above", " below"),
                     " the the average statewide wage of $", average_wage(ca_demand), ".")

  } else {
    above <- ex_text %>% filter(earnings >= living_wage_val)
    below <- ex_text %>% filter(earnings < living_wage_val)

    format_list <- function(descs, wages) {
      n <- length(descs)
      parts <- paste0(descs, " ($", wages, ")")
      if (n == 1) {
        return(parts[1])
      } else if (n == 2) {
        return(paste(parts, collapse = " and "))
      } else {
        return(paste0(paste(parts[1:(n-1)], collapse = ", "), ", and ", parts[n]))
      }
    }

    sentence_parts <- c()

    if (nrow(above) > 0) {
      above_sentence <- paste0(
        format_list(above$Description, above$earnings),
        " ", ifelse(nrow(above) == 1, "is", "are"),
        " above the living wage for one adult in the ", region_ex4, region_type,
        " ($", living_wage_val, ")"
      )
      sentence_parts <- c(sentence_parts, above_sentence)
    }

    if (nrow(below) > 0) {
      below_sentence <- paste0(
        format_list(below$Description, below$earnings),
        " ", ifelse(nrow(below) == 1, "is", "are"),
        " below the living wage for one adult in the ", region_ex4, region_type,
        " ($", living_wage_val, ")"
      )
      sentence_parts <- c(sentence_parts, below_sentence)
    }

    sentence <- paste(sentence_parts, collapse = "; ")
    sentence <- paste0(sentence, ".")

    ending <- paste0(" The ", region_ex4, " average wage for these occupations is $", average_wage(reg_demand(region_ex4)),
                     " which is", ifelse(average_wage(reg_demand(region_ex4)) >= average_wage(ca_demand), " above", " below"),
                     " the the average statewide wage of $", average_wage(ca_demand), ".")
  }

  return(paste0(sentence, ending))
}

wage_prompt <- paste0("Write a concise sentence determine if the following jobs: ", ex4a_w, " are above or below the living wage for one adult within the region. Include the dollar amounts and only respond with your comparison:")
ex4a_wages <-   paste(wage_prompt,ex4a_w, sep = " ")
ex4b_wages <-   paste(wage_prompt,ex4b_w, sep = " ")
ex4c_wages <-   paste(wage_prompt,ex4c_w, sep = " ")

paste0("The typical entry-level wage for JOB ($XX.XX) is (above or below) the living wage for one adult in the ", "NCV/NML ", "subregion (",ncv_living_wage,").")
exhibit4_ncv
ex4a_wages_text <- tryCatch({
  call_claude_api_for_table(
    prompt = ex4a_wages,
    model = "claude-3-opus-20240229",
    max_tokens = 250
  )
}, error = function(e) {
  paste("API call error:", e$message)
})

ex4b_wages_text <- tryCatch({
  call_claude_api_for_table(
    prompt = ex4b_wages,
    model = "claude-3-opus-20240229",
    max_tokens = 250
  )
}, error = function(e) {
  paste("API call error:", e$message)
})

ex4c_wages_text <- tryCatch({
  call_claude_api_for_table(
    prompt = ex4c_wages,
    model = "claude-3-opus-20240229",
    max_tokens = 250
  )
}, error = function(e) {
  paste("API call error:", e$message)
})

ncv_d_1 <- ncv_demand %>%
  filter(!is.na(SOC)) %>%
  select(Description, contains("Avg. Hourly")) %>%
  rename(avg = "Avg. Hourly Earnings") %>%
  mutate(earnings = paste0(" ($", round(avg,2), ")"),
         wages = paste0(Description, earnings)) %>%
  pull(wages) %>%
  paste(collapse = "|")

scv_d_1 <- scv_demand %>%
  filter(!is.na(SOC)) %>%
  select(Description, contains("Avg. Hourly")) %>%
  rename(avg = "Avg. Hourly Earnings") %>%
  mutate(earnings = paste0(" ($", round(avg,2), ")"),
         wages = paste0(Description, earnings)) %>%
  pull(wages) %>%
  paste(collapse = "|")

cv_d_1 <- cvml_demand %>%
  filter(!is.na(SOC)) %>%
  select(Description, contains("Avg. Hourly")) %>%
  rename(avg = "Avg. Hourly Earnings") %>%
  mutate(earnings = paste0(" ($", round(avg,2), ")"),
         wages = paste0(Description, earnings)) %>%
  pull(wages) %>%
  paste(collapse = "|")

ca_d_1 <- ca_demand %>%
  filter(!is.na(SOC)) %>%
  select(Description, contains("Avg. Hourly")) %>%
  rename(avg = "Avg. Hourly Earnings") %>%
  mutate(earnings = paste0(" ($", round(avg,2), ")"),
         wages = paste0(Description, earnings)) %>%
  pull(wages) %>%
  paste(collapse = "|")

ex4a_wages2 <- paste("Write a concise sentence that compares the average wages of the following jobs in the NCV/NML region: ", ncv_d_1,
                     "to the average wages across all of California:", ca_d_1, "Include the dollar amounts and only respond with your comparison:", sep = " ")

ex4b_wages2 <- paste("Write a concise sentence that compares the average wages of the following jobs in the SCV/SML region: ", scv_d_1,
                     "to the average wages across all of California:", ca_d_1, "Include the dollar amounts and only respond with your comparison:", sep = " ")

ex4c_wages2 <- paste("Write a concise sentence that compares the average wages of the following jobs in the CVML region: ", cv_d_1,
                     "to the average wages across all of California:", ca_d_1, "Include the dollar amounts and only respond with your comparison:", sep = " ")

ex4a_wages_text2 <- tryCatch({
  call_claude_api_for_table(
    prompt = ex4a_wages2,
    model = "claude-3-opus-20240229",
    max_tokens = 250
  )
}, error = function(e) {
  paste("API call error:", e$message)
})

ex4b_wages_text2 <- tryCatch({
  call_claude_api_for_table(
    prompt = ex4b_wages2,
    model = "claude-3-opus-20240229",
    max_tokens = 250
  )
}, error = function(e) {
  paste("API call error:", e$message)
})

ex4c_wages_text2 <- tryCatch({
  call_claude_api_for_table(
    prompt = ex4c_wages2,
    model = "claude-3-opus-20240229",
    max_tokens = 250
  )
}, error = function(e) {
  paste("API call error:", e$message)
})
