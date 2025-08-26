library(tidyverse)
Job_Postings_Raw <- read_excel("C:/Users/if001/Downloads/Job_Postings_Raw.xlsx",
                               sheet = "Full Text Job Posting Samples",
                               skip = 2)

Job_Postings_Raw <- read_excel("C:/Users/if001/Downloads/Job_Postings_List_raw.xlsx",
                               sheet = "Full Text Job Posting Samples",
                               skip = 2)

Job_Postings_Raw <- read_excel("C:/Users/if001/Downloads/Job_Postings_List_eng.xlsx",
                               sheet = "Full Text Job Posting Samples",
                               skip = 2)


#Job_Postings_Raw[1,]
corpus_list <- list()
#one <- c("234", "34234", "5423442")
corpus_list_raw <- list()
#corpus_list_raw <- c("234", "34234", "5423442")
for (i in 1:(nrow(Job_Postings_Raw))){
  corpus_list_raw[[i]] <- pull(Job_Postings_Raw, `Job Description`)[i]
}

for (i in 1:(nrow(Job_Postings_Raw))) {
#corpus_list[[i]]
  salary_range <- corpus_list_raw[[i]] %>% str_extract_all(pattern = "\\$\\d+\\,\\d+") #$000,00000
  hourly_range <- corpus_list_raw[[i]] %>% str_extract_all(pattern = "\\$\\d+\\.\\d+") #$000,00000
  entry <- corpus_list_raw[[i]] %>% str_extract_all(pattern = "entry|Entry") #$000,00000
  experience <- corpus_list_raw[[i]] %>% str_extract_all(pattern = "year") #$000,00000

  corpus_list[[i]] <- tibble(
    salary_range = salary_range,
    hourly_range  = hourly_range,
    entry = entry,
    experience = experience
  )
}


# Use str_match to get experience years into separate columns
num_pattern <- "(?:zero|one|two|three|four|five|six|seven|eight|nine|ten|\\d{1,2}\\+?)"
context_keywords <- "(?:experience|qualifications|writing|managing|leading|developing|assessing)"
exp_pattern_flexible <- regex(
  paste0(
    "(", num_pattern, ")(?:\\s*(?:-|to|–)\\s*(", num_pattern, "))?\\s+years?(?:\\s+of)?",
    # Use the flexible keywords in the proximity check
    "(?:(?:\\s+\\S+){0,4}\\s+", context_keywords, ")"
  ),
  ignore_case = TRUE
)

salary_regex <- "\\$\\d{1,3}(?:,?\\d{3})*(?:\\s*(?:-|to|–)\\s*\\$\\d{1,3}(?:,?\\d{3})*)?"
hourly_regex <- "\\$\\d{1,3}(?:\\.\\d{2})?(?:\\s?(?:/hr|/hour|per hour))"

iteration <- 9
exp_matrix <- str_extract_all(Job_Postings_Raw$`Job Description`[iteration],exp_pattern_flexible) %>% unique()
salary_text <- str_extract(Job_Postings_Raw$`Job Description`[iteration], salary_regex)
hourly_text <- str_extract(Job_Postings_Raw$`Job Description`[iteration], hourly_regex)
is_entry_level <- str_detect(Job_Postings_Raw$`Job Description`[iteration], regex("entry-?level", ignore_case = TRUE))
degrees_mentioned = map(
  str_extract_all(Job_Postings_Raw$`Job Description`[iteration], regex("(?:Bachelor(?:'s)?|Master(?:'s)?|PhD|B\\.S\\.|B\\.A\\.)", ignore_case = TRUE)),
  unique
)
Job_Postings_Raw$`Job Description`[iteration]
exp_matrix
salary_text
hourly_text
is_entry_level
degrees_mentioned
corpus <- corpus_list_raw[[13]]

for (i in 1:(nrow(Job_Postings_Raw))) {

  corpus <- corpus_list_raw[[i]]

  exp_matrix <- str_extract_all(corpus, exp_pattern_flexible) %>% unlist() %>% unique() %>% paste(collapse = "|")
  salary_text <- str_extract_all(corpus, salary_regex) %>% unlist() %>% paste(collapse = "|")
  hourly_text <- str_extract_all(corpus, hourly_regex) %>% unlist() %>% paste(collapse = "|")
  is_entry_level <- str_detect(corpus, regex("entry-?level", ignore_case = TRUE)) %>% unlist() %>% paste(collapse = "|")
  degrees_mentioned <- map(
    str_extract_all(corpus, regex("(?:Bachelor(?:'s)?|Master(?:'s)?|PhD|B\\.S\\.|B\\.A\\.)", ignore_case = TRUE)),
    unique
  ) %>% unlist() %>% paste(collapse = "|")

corpus_list[[i]] <- tibble(
  exp_matrix = exp_matrix,
  salary = salary_text,
  hourly = hourly_text,
  entry = is_entry_level,
  degrees = degrees_mentioned
)
}

stacked <- do.call(bind_rows, corpus_list)


stacked_list <- list()
for (i in 1:nrow(stacked)){
  salary_row <- stacked$salary[i] %>% str_remove_all("\\$|\\,| ") %>% str_split("\\-|to|\\|") %>% unlist() %>% as.numeric()

  salaries <- if_else(salary_row>=1000, T, F)
hourly <- if_else(salaries == F & str_detect(salary_row,"\\d{2}"), T, F)

stacked_list[[i]] <- tibble(
  min_salary = min(salary_row[salaries]),
  max_salary = max(salary_row[salaries]),
  min_hourly = min(salary_row[hourly]),
  max_hourly = max(salary_row[hourly])
) %>%
  map_dfc(function(x){ifelse(is.infinite(x), NA, x)}) %>%
  suppressWarnings()
}
stacked_list_df <- do.call(bind_rows,stacked_list)
stacked$entry_level <- str_detect(stacked$exp_matrix, "0|zero|Zero|no |one|One")

job_scrape <- Job_Postings_Raw %>%
  bind_cols(stacked) %>%
  bind_cols(stacked_list_df)

write.xlsx(job_scrape, "job_scrape2.xlsx")
