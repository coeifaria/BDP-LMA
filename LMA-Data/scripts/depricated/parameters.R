#```{r formatting1_cmvl_members, chunk_progress=TRUE}
library(tidyverse)
library(readxl)
library(knitr)
library(kableExtra)
library(extrafont)
#library(webshot)

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

scv_counties <- c("Fresno", "Inyo", "Kern", "Kings", "Madera", "Mono", "Tulare")
ncv_counties <- c("Amador", "Alpine", "Calaveras", "Mariposa", "Merced", "San Joaquin", "Stanislaus", "Tuolumne")
#Inyo Mono Amador Alpine Calveras Mariposa
scv_colleges <- c("Madera", "Fresno", "Clovis", "Reedley", "Bakersfield", "Cerro", "Porterville", "Sequoias", "Taft", "Lemoore", "Coalinga")
ncv_colleges <- c("San Joaquin|Delta", "Columbia", "Modesto", "Merced")


# Create a dataframe
colleges_df <- data.frame(
  College = c(scv_colleges, ncv_colleges),
  FullName = c(
    "Madera Community College", "Fresno City College", "Clovis Community College", "Reedley College",
    "Bakersfield College", "Cerro Coso Community College", "Porterville College",
    "College of the Sequoias", "Taft College", "West Hills College Lemoore", "West Hills College Coalinga",
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
requesting_district_acronym <- pull(colleges_df[colleges_df$College==requesting_college,], "DistrictAcronym")
requesting_district <- pull(colleges_df[colleges_df$College==requesting_college,], "District")
requesting_county <- pull(colleges_df[colleges_df$College==requesting_college,], "County")
