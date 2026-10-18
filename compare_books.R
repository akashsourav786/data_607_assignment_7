params <-
list(data_base = "https://raw.githubusercontent.com/akashsourav786/data_607_assignment_7/main")

## ----setup--------------------------------------------------------------------
library(rvest)
library(jsonlite)
library(knitr)

# The fallback also lets the extracted R script run by itself.
data_base <- if (exists("params")) params$data_base else ""
source_path <- function(filename) {
  if (nzchar(data_base)) {
    paste0(sub("/$", "", data_base), "/", filename)
  } else {
    filename
  }
}
html_source <- source_path("books.html")
json_source <- source_path("books.json")


## ----html-import--------------------------------------------------------------
html_books <- read_html(html_source) |>
  html_element("table#books") |>
  html_table(trim = TRUE) |>
  as.data.frame(stringsAsFactors = FALSE)

kable(html_books, caption = "Book data imported from HTML")
str(html_books)


## ----json-import--------------------------------------------------------------
json_books <- fromJSON(json_source, simplifyDataFrame = TRUE)

str(json_books)
json_books$authors


## ----initial-comparison-------------------------------------------------------
identical_before <- identical(html_books, json_books)
identical_before

column_types <- data.frame(
  column = names(html_books),
  html_type = vapply(html_books, typeof, character(1)),
  json_type = vapply(json_books, typeof, character(1)),
  row.names = NULL
)
kable(column_types, caption = "Storage types before standardization")


## ----normalized-comparison----------------------------------------------------
json_normalized <- json_books
json_normalized$authors <- vapply(
  json_books$authors,
  function(author_names) paste(author_names, collapse = "; "),
  character(1)
)

normalize_books <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  df <- df[c("title", "authors", "publication_year", "publisher", "focus")]
  text_columns <- c("title", "authors", "publisher", "focus")
  df[text_columns] <- lapply(df[text_columns], trimws)
  df$publication_year <- as.integer(df$publication_year)
  stopifnot(!anyDuplicated(df$title))
  df <- df[order(df$title), ]
  rownames(df) <- NULL
  df
}

html_normalized <- normalize_books(html_books)
json_normalized <- normalize_books(json_normalized)

identical_after <- identical(html_normalized, json_normalized)
identical_after
all.equal(html_normalized, json_normalized)

stopifnot(nrow(html_normalized) == 3L,
          ncol(html_normalized) == 5L,
          any(lengths(json_books$authors) > 1L),
          !anyNA(html_normalized),
          identical_after)

kable(html_normalized, caption = "The common normalized data frame")

