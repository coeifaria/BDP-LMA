#```{r supply_summary, chunk_progress=TRUE}
#length(suppy_completions$`College Name`)
#ssm_cte <- read_csv("data/StudentSuccessMetrics_Short-TermCareerStudents_SouthernCentralValley-MotherLode.csv")

county <- str_split_1(requesting_region, " ")[1]

#SWFP_county <- read.csv(SWFP[str_detect(SWFP, county)])
SWFP_county <- SWFP[SWFP$location == county,]

swfp_rarea <- unique(SWFP$location)[!is.na(str_extract(unique(SWFP$location), pattern = c("Southern Central Valley-Mother Lode|Northern Central Valley-Mother Lode")))]

SWFP_metric <-function(swfp = SWFP_county, metric = "Attained Living Wage"){ #metric = "SW 701"){
  swfp %>%
    filter(#!is.na(value) & disagg == "Overall" &
      #metricID == metric) %>%  #SW 701
      metric == "Attained Living Wage") %>%
    #filter(academicYear==(max(academicYear))) %>%
    group_by(metric) %>%
    filter(academic_year==(max(academic_year))) %>%
    select(value) #%>%
  #as.numeric() %>%
  #prod(100) %>%
  #round(0) %>%
  #paste0('%')
}

#living_wage_per <- SWFP_metric(SWFP_county, "Attained Living Wage") #"SW 802")
#job_closely_related <- SWFP_metric(SWFP_county, "Job Closely Related to Field of Study")    #"SW 701")

job_closely_related <- SWFP %>%
  filter(#location == str_split(requesting_region, " ", simplify = T)[1] &
    location == swfp_rarea &
      #reportfor == top_code_title &
      str_detect(metric, "Closely Related")==T) %>%
  group_by(metric) %>%
  filter(academic_year == max(academic_year)) %>%
  pull(value)

#academic_year <- SWFP_c
living_wage_per <- SWFP %>%
  filter(#location == str_split(requesting_region, " ", simplify = T)[1] &
    location == swfp_rarea &
      reportfor == top_code_title &
      metric == "Attained Living Wage") %>%
  group_by(metric) %>%
  filter(academic_year == max(academic_year)) %>%
  pull(value)
#academic_year <- SWFP_county %>%
#  rename(academicYear = academic_year) %>%
#  filter(academicYear==(max(academicYear)-1)) %>%
#  select(academicYear) %>%
#  mutate(academicYear = paste(academicYear, academicYear+1, sep = "-")) %>%
#  unique() %>%
#  as.character()
