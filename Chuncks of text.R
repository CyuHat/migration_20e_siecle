# Functions ----
# Split the text in paragraph of n phrases.
split_size <- function(a, n=5){
  x <- a[c(-1:-3)]
  
  a[3] |>
    append(
      split(x, ceiling(seq_along(x)/n))
    )
}

split_size2 <- function(a, n=5){
  a |>
    append(
      split(a, ceiling(seq_along(a)/n))
    )
}

# The whole process
the_process <- function(path, n=5){
  file_name <-
    path |>
    gregexpr("\\d{1,2}_histoire\\.txt", text=_) |>
    regmatches(path, m=_) |>
    unlist()
  
  print(file_name)
  
  readLines(path) |>
    split_size(n=n) |>
    lapply(paste, collapse=" ") |>
    paste(collapse = "\n") |>
    writeLines(con = paste0("Texts/histoire/new/", file_name))
}

the_process2 <- function(path, n=5){
  file_name <-
    path |>
    gregexpr("\\d{1,2}_histoire\\.txt", text=_) |>
    regmatches(path, m=_) |>
    unlist()
  
  print(file_name)
  
  readLines(path) |>
    Filter(
      \(a) grepl("\\d{4}", x=a),
      x=_
    ) |>
    split_size2(n=n) |>
    lapply(paste, collapse=" ") |>
    paste(collapse = "\n") |>
    writeLines(con = paste0("Texts/histoire/date/", file_name))
}

# Action ----
# Chuncks
list.files("Texts/histoire/", pattern = "txt", full.names = TRUE) |>
  lapply(the_process, n=20)
# Dates
list.files("Texts/histoire/", pattern = "txt", full.names = TRUE) |>
  lapply(the_process2, n=1)

