#```{r, chunk_progress=TRUE}
library(httr)
library(jsonlite)
library(here)

#setwd(here())

library(curl)
test_connection <- function() {
  tryCatch({
    curl::curl_fetch_memory("https://api.anthropic.com")
    return(TRUE)
  }, error = function(e) {
    message("Connection test failed: ", e$message)
    return(FALSE)
  })
}

if (!test_connection()) {
  stop("Unable to connect to Anthropic API endpoint")
}

api_key <- "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"
gemini_api <- "AIzaSyA8hDaMYyUkHhb6XcWe5R_8YzKkl9S11Gw"
call_claude_api_for_table <- function(prompt,
                                      model = "claude-3-opus-20240229",
                                      max_tokens = 100) {
  url <- "https://api.anthropic.com/v1/messages"

  headers <- c(
    "Content-Type" = "application/json",
    "x-api-key" = api_key,
    "anthropic-version" = "2023-06-01"
  )

  body <- list(
    model = model,
    max_tokens = max_tokens,
    messages = list(
      list(
        role = "user",
        content = prompt
      )
    )
  )

  response <- tryCatch({
    POST(
      url,
      add_headers(.headers = headers),
      body = toJSON(body, auto_unbox = TRUE),
      encode = "json",
      httr::timeout(30)
    )
  }, error = function(e) {
    message("Connection error: ", e$message)
    stop(e)
  })

  if (status_code(response) != 200) {
    error_content <- content(response, "parsed")
    stop("API request failed with status code: ", status_code(response),
         "\nError message: ", toJSON(error_content, auto_unbox = TRUE))
  }

  content <- fromJSON(rawToChar(response$content))

  # Extract the text from the correct location in the response
  return(content$content$text)
}
prompt_text <- "All numbers MUST REMAIN in the SAME ORDER.
                ONLY change the words (such as before, after, above below, greather than, less than, etc..) to make the statement logically true.
                Only respond with the corrected text:"

prompt_text <- "Summarize the following into a shorter string (around a third the length) that encompases everything mentioned in an accurate and official manner. Only reply with the shortened string that can be used in a sentence within a template."

#prompt <- paste(prompt_text, paste(text, collapse= ", "))

#corrected_text <- call_claude_api(prompt)
