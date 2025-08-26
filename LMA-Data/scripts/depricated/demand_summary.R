#```{r demand_summary, chunk_progress=TRUE}
#AS <-  as.character(jp_salary$`Advertised Salary`)

#AS <-  factor(jp_salary$`Advertised Salary`, ordered = T)
#obs <- as.numeric(jp_salary$Observations)
#rep(AS, obs)
#jp_salary %>%
#  mutate(AS = factor(`Advertised Salary`, ordered = TRUE)) %>%
#  summarize(median = )

#min_hourly_wage <- (((AS %>%
#  min() %>%
#  str_split(pattern = "\\-"))[[1]][2] %>%
#  str_remove('\\,') %>%
#  str_remove('\\$') %>%
#  as.numeric())/2080) %>%
#  round(2)



#jp_salary$cumulative_freq <- cumsum(jp_salary$Observations)

# Calculate total count
#total_count <- sum(jp_salary$Observations)

# Find median position
#median_position <- ceiling(total_count / 2)

# Find the range containing the median
#median_salary_range <- jp_salary[which(jp_salary$cumulative_freq >= median_position)[1], "Advertised Salary"] %>%  as.character()

unique_job_postings <- (jp_top_postings %>%
                          select(starts_with("Unique"))
)  %>% sum()

unique_col <- jp_top_postings %>%  select(starts_with("Unique"))

max_posts <- unique_col %>%  max()
which.max(sapply(unique_col[,1], as.numeric))
most_posted_job <- jp_top_postings[which.max(sapply(unique_col[,1], as.numeric)),1] %>% as.character()


unique_postings <- names(jp_top_postings)[str_detect(names(jp_top_postings), pattern = "Unique")]

#jp_top_postings %>%  slice(which(unique_postings==max_posts))

#2080*33.6 2080 is the number of hours per year

min_ed <- as.character(subregion_demand[which(subregion_demand$Description %in% most_posted_job), "Typical Entry Level Education"])

north_or_south_cvml <- paste0(
  unique(colleges_df[colleges_df$College==str_replace_all(requesting_college, "\\|", " "), "Classification"]),
  " Central Valley/",
  unique(colleges_df[colleges_df$College==str_replace_all(requesting_college, "\\|", " "), "Classification"]),
  "ern Mother Lode"
) %>% str_squish()
