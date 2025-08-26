
cvml_living_wage_adult <- average_living_wage(cvml_counties_string, "Adult", char = T)
cvml_living_wage_adult_infant <- average_living_wage(cvml_counties_string, "Adult_Infant")

prepare_data_ex13 <- function(data) {

  d1 <- data %>%
    filter(!is.na(SOC)) %>%
    select(Description, contains("Hourly Earnings")) %>%
    select(Description, contains(c("25", "Median", "75"))) %>%
    rename(
      `Entry-Level Hourly Earnings` = `Pct. 25 Hourly Earnings`,
      `Median Hourly Earnings` = `Median Hourly Earnings`,
      `Experienced Hourly Earnings` = `Pct. 75 Hourly Earnings`
    ) %>%
    pivot_longer(
      cols = c("Entry-Level Hourly Earnings", "Median Hourly Earnings", "Experienced Hourly Earnings"),
      values_to = "earnings",
      names_to = "lev"
    ) %>%
    mutate(
      gap = earnings - lag(earnings),
      gap = ifelse(lev == "Entry-Level Hourly Earnings", earnings, gap),
      earnings = round(earnings, 2),
      #Description = str_wrap(Description, width = 20)
    )
  descr <- pull(d1, Description)[1:(nrow(d1)-1)] %>% unique()
  d1 %>%
    mutate(Description = factor(Description, levels = (descr), ordered = T))
}

cvml_living_wage_adult <- average_living_wage(cvml_counties_string, "Adult")
cvml_living_wage_adult_infant <- average_living_wage(cvml_counties_string, "Adult_Infant")

exhibit13_cvml <- prepare_data_ex13(cvml_demand)
exhibit13_cvml$region <- "CVML"
#exhibit13_cvml$cvml_living_wage_adult <- cvml_living_wage_adult
#exhibit13_cvml$cvml_living_wage_adult_infant <- cvml_living_wage_adult_infant

reg_lw_count <- function(living_wage, number=F, all=F) {
  lw_fixed <- as.numeric(str_remove_all(living_wage, "\\$|\\,"))
  num <- exhibit13_cvml %>% filter(lev == "Entry-Level Hourly Earnings") %>%
    filter(earnings >= lw_fixed) %>%
    nrow() #%>%    xfun::n2w()
  if (two_or_more_soc==1) {
    num <- ""
  } else if (num==length(SOC)&all==T) {
    num <- "all"
  }

  if(number==T){
    return(xfun::n2w(num))
  } else {
    return(num)
  }
}

reg_lw_count(cvml_living_wage_adult, all=T)
reg_lw_count(cvml_living_wage_adult_infant, all=T)
reg_lw_count(cvml_living_wage_adult_infant, number=F, all=T)


orderer <- ca_demand[, c(1,2)]
orderer_2 <- orderer[!is.na(orderer$SOC),] %>% mutate(order = 1)
orderer_3 <- orderer_2 %>%
  arrange(SOC) %>%
  mutate(order = cumsum(order))

exhibit13_func <- function(ex13_data) {
  ex13_data %>%
    select(-gap) %>%
    pivot_wider(names_from = lev,
                values_from = earnings) %>%
    #select(1, 4, 5, 6, 2, 3) %>%
    left_join(orderer_3) %>%
    arrange(desc(order)) %>%
    select(-SOC, -order, -region)
}

exhibit13_cvml_1 <- exhibit13_func(exhibit13_cvml)

exhibit_excel_fix(exhibit13_cvml_1, "13_Exhibit.xlsx")
exhibit_word_doc("13_Exhibit.xlsx", "Process13")

entry_lev_wages <- pull(exhibit13_cvml[exhibit13_cvml$lev=="Entry-Level Hourly Earnings",], earnings)
above_lw_index <- which(average_living_wage(cvml_counties_string, "Adult", char = F) < entry_lev_wages)



