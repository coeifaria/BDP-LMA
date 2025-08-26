#```{r formatting1_cmvl_members, chunk_progress=TRUE}
library(tidyverse)
library(readxl)
library(lubridate)
library(extrafont)
library(flextable)
library(openxlsx)
library(officer)
library(xfun)
#library(markdown) # Keep loaded for potential markdown rendering later


#library(webshot)

#require xfun library

#getwd()

# Load parameters from parameters.txt file and assign to variables
# Specify the path to your parameters.txt file
parameters_file <- "parameters.txt"

# Read the file into a character vector
lines <- readLines(parameters_file)

# Remove any empty lines
lines <- lines[lines != ""]

# Initialize a list to hold parameters
params <- list()

# Loop over each line and parse the parameter name and value
for (line in lines) {
  # Split the line at the first '=' character
  split_line <- strsplit(line, "=", fixed = TRUE)[[1]]
  if (length(split_line) >= 2) {
    # Get the parameter name and value
    name <- split_line[1]
    value <- paste(split_line[-1], collapse = "=")  # In case value contains '='
    # Trim any whitespace
    name <- trimws(name)
    value <- trimws(value)
    # Add to the params list
    params[[name]] <- value
  }
}

# Assign the parameters to variables in the global environment
list2env(params, envir = .GlobalEnv)

objective <- params$objective
scv_counties <- c("Fresno", "Inyo", "Kern", "Kings", "Madera", "Mono", "Tulare")
ncv_counties <- c("Amador", "Alpine", "Calaveras", "Mariposa", "Merced", "San Joaquin", "Stanislaus", "Tuolumne")
#Inyo Mono Amador Alpine Calveras Mariposa

scv_colleges <- c("Madera", "Fresno", "Clovis", "Reedley", "Bakersfield", "Cerro Coso", "Porterville", "Sequoias", "Taft", "Lemoore", "Coalinga")
ncv_colleges <- c("San Joaquin Delta", "Columbia", "Modesto", "Merced")

# Create a dataframe
colleges_df <- data.frame(
  College = c(scv_colleges, ncv_colleges),
  FullName = c(
    "Madera Community College", "Fresno City College", "Clovis Community College", "Reedley College",
    "Bakersfield College", "Cerro Coso Community College", "Porterville College",
    "College of the Sequoias", "Taft College", "Lemoore College", "Coalinga College",
    "San Joaquin Delta College", "Columbia College", "Modesto Junior College", "Merced College"
  ),
  District = c(
    "State Center", "State Center", "State Center", "State Center",
    "Kern", "Kern", "Kern",
    "Sequoias", "West Kern",
    "West Hills", "West Hills",
    "San Joaquin Delta", "Yosemite", "Yosemite", "Merced"
  ),
  DistrictFullName = c(
    "State Center Community College District", "State Center Community College District",
    "State Center Community College District", "State Center Community College District",
    "Kern Community College District", "Kern Community College District", "Kern Community College District",
    "Sequoias Community College District", "West Kern Community College District",
    "West Hills Community College District", "West Hills Community College District",
    "San Joaquin Delta Community College District", "Yosemite Community College District",
    "Yosemite Community College District", "Merced Community College District"
  ),
  DistrictAcronym = c(
    "SCCCD", "SCCCD", "SCCCD", "SCCCD",
    "KCCD", "KCCD", "KCCD",
    "SCCD", "WKCCD",
    "WHCCD", "WHCCD",
    "SJDCCD", "YCCD", "YCCD", "MCCD"
  ),
  County = c(
    "Madera", "Fresno", "Fresno", "Fresno",
    "Kern", "Kern", "Tulare",
    "Tulare", "Kern",
    "Kings", "Fresno",
    "San Joaquin", "Tuolumne", "Stanislaus", "Merced"
  ),
  Classification = c(
    rep("South", length(scv_colleges)),
    rep("North", length(ncv_colleges))
  )
)

requesting_college <- if (params[["requesting_college"]] == "Cerro") {
  requesting_college <- "Cerro Coso"
} else if (str_detect(params[["requesting_college"]], "^San" )) {
  requesting_college <- "San Joaquin Delta"
} else {
  requesting_college <- unique(colleges_df[colleges_df$College==params[["requesting_college"]], "FullName"])
}

two_or_more_soc <- str_split(params$SOC, pattern = ",", simplify = TRUE) %>% length()

