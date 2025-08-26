#```{r officer_setup}
# Load required packages
library(officer)
#library(magick)

#### Formatting

#Sys.setenv(CHROMOTE_CHROME = "C:/Users/if001/AppData/Local/Google/Chrome/Application/chrome.exe")
#font_import(pattern = "Tw Cen MT")
H0_color <- "#549E39"
H1_color <- "#0989B1"
H2_color <- "#4AB5C4"

exhibit_header_color <- "#029676"
header1_color <- "#4AB5C4"

table1_header_color <- "#E5EBB0"
table_header_fontcolor <- "#455F51"

table2_header_color <- "#d9e288"
table_bottom_sum_color <- "#4AB5C4"

solid_bar_graph_color <- "#549E39"
our_names <- "#3E762A"
total_bar_color <- "#BFBFBF"

border_settings <- fp_border(color = "#A6A6A6", width = 1)
H0 <- fp_text(font.size = 18, color = H0_color, font.family = "Tw Cen MT")
H1 <- fp_text(font.size = 18, color = H1_color, font.family = "Tw Cen MT")
H2 <- fp_text(font.size = 16, color = H2_color, font.family = "Tw Cen MT")
H3 <- fp_text(font.size = 14, color = exhibit_header_color, font.family = "Tw Cen MT")

S1 <- fp_text(font.size = 14, color = "#455F51", font.family = "Tw Cen MT")
body_text_style <- fp_text(font.size = 11, font.family = "Tw Cen MT")
body_text_style_bold <-fp_text(font.size = 11, font.family = "Tw Cen MT", bold = TRUE)
body_text_style_italic <-fp_text(font.size = 11, font.family = "Tw Cen MT", italic = TRUE)
body_text_style_italicbold <-fp_text(font.size = 11, font.family = "Tw Cen MT", italic = TRUE, bold = TRUE)
fp_p_justify <- fp_par(text.align = "justify")

text_date <- format(Sys.Date(), "%B %Y")

# Define text styles
body_text_outro_style <- fp_text(font.size = 13, color = "#000000", font.family = "Tw Cen MT")
body_text_outro_style1 <- fp_text(font.size = 12, font.family = "Tw Cen MT")

highlighted_text <- fp_text(
  font.size = 11,
  font.family = "Tw Cen MT",
  color = "darkgrey",
  #shading.color = "red"
)
highlighted_text <- body_text_style
# Function to adjust image size if necessary (optional)
adjust_image_size <- function(image_path, max_width_inch = 6.5, dpi = 96) {
  img_info <- image_read(image_path)
  img_data <- image_info(img_info)

  # Extract image width and height in pixels
  img_width <- img_data$width
  img_height <- img_data$height

  # Convert to inches (assuming specified DPI)
  img_width_inch <- img_width / dpi
  img_height_inch <- img_height / dpi

  # Adjust width and height if the image width exceeds the maximum width
  if (img_width_inch > max_width_inch) {
    scale_factor <- max_width_inch / img_width_inch
    img_width_inch <- max_width_inch
    img_height_inch <- img_height_inch * scale_factor
    # Resize the image accordingly (optional)
    img_info <- image_scale(img_info, paste0(max_width_inch * dpi, "x"))
    image_write(img_info, path=image_path)
  }

  return(list(width = img_width_inch, height = img_height_inch))
}

custom_css <- "<style>
                table { width: 6.5in; }
                table, th, td { padding: 0px !important; margin: 0px !important; font-size: 14px !important; }
              </style>"
