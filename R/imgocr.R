#' @title Mathematical formulas and character recognition
#'
#' @description
#' Mathematical formulas and character recognition.
#'
#' @param img An image file, supporting jpg, png, bmp format.
#' @param mode Set service model. The value can be `latex_ocr`, `latex_ocr_turbo`, or
#' `simpletex_ocr`. The default value is `latex_ocr`. The `latex_ocr` and `latex_ocr_turbo`
#' are used for formula identification. `latex_ocr` is better than `latex_ocr_turbo`, but
#' `latex_ocr_turbo` is faster. `simpletex_ocr` is suitable for general image recognition.
#'
#' @return Text or mathematical formulas for Markdown and LaTeX.
#'
#' @examples
#' \dontrun{
#'   imgocr("path/to/test.jpg")
#'   imgocr("path/to/test.png", mode = "latex_ocr_turbo")
#'   imgocr("path/to/test.bmp", mode = "simpletex_ocr")
#' }
#'
#' @export

#---------------------------------------------------------------------------

imgocr = function(img, mode = "latex_ocr") {

  # Get the APP ID and SECRET for SimpleTex API
  SIMPLETEX_APP_ID = Sys.getenv("SIMPLETEX_APP_ID")
  SIMPLETEX_APP_SECRET = Sys.getenv("SIMPLETEX_APP_SECRET")
  if (SIMPLETEX_APP_ID == "" | SIMPLETEX_APP_SECRET == "") {
    stop('Please Configure ID and SECRET of the SimpleTex API.')
  }

  # API URL
  url = paste0("https://server.simpletex.cn/api/", mode)

  # Generate signature info
  chars = c(LETTERS, letters, 0:9)
  random_str = paste(
    sample(chars, 16, replace = TRUE),
    collapse = ""
  )
  timestamp = as.character(as.integer(Sys.time()))
  sign = glue::glue(
    "app-id={SIMPLETEX_APP_ID}&",
    "random-str={random_str}&",
    "timestamp={timestamp}&",
    "secret={SIMPLETEX_APP_SECRET}"
  )
  sign = digest::digest(sign, algo = "md5", serialize = FALSE)

  # Generate authentication info
  header = c(
    `app-id` = SIMPLETEX_APP_ID,
    `random-str` = random_str,
    timestamp = timestamp,
    sign = sign
  )

  # Call the SimpleTex API to return info
  response = httr::POST(
    url = url,
    httr::add_headers(header),
    body = list(file = httr::upload_file(img)),
    encode = "multipart"
  )
  response_text = httr::content(
    x = response,
    as = "text",
    encoding = "UTF-8"
  )
  json_data = jsonlite::fromJSON(response_text)

  if (mode == "simpletex_ocr") {
    result = json_data$res$info
  } else {
    result = json_data$res$latex
  }

  return(result)

}

#---------------------------------------------------------------------------
