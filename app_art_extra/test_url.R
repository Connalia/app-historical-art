
urls <-   c("https://data.ukiyo-e.org/metro/scaled/040-005-40.jpg",
            "https://data.ukiyo-e.org/metro/scaled/035-077-42.jpg")

valid_url <- function(url_in,t=2){
  con <- url(url_in)
  check <- suppressWarnings(try(open.connection(con,open="rt",timeout=t),silent=T)[1])
  suppressWarnings(try(close.connection(con),silent=T))
  ifelse(is.null(check),TRUE,FALSE)
  return(is.null(check))
}

sapply(urls,valid_url)


