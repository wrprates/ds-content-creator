# Funções auxiliares
box::use(
  httr[POST, add_headers, content_type_json, content, status_code],
  jsonlite[toJSON, fromJSON]
)

#' @export
send_message_to_chatgpt <- function(message, api_key, openai_url) {
  body <- list(
    model = "gpt-4o",
    messages = list(
      list(role = "user", content = message)
    )
  )
  
  body_json <- toJSON(body, auto_unbox = TRUE)
  
  response <- POST(
    url = openai_url,
    add_headers(Authorization = paste("Bearer", api_key)),
    content_type_json(),
    body = body_json
  )
  
  if (status_code(response) != 200) {
    response_text <- content(response, "text", encoding = "UTF-8")
    cat("Resposta completa da API:\n", response_text, "\n")
    stop("Falha na requisição: ", response_text)
  }
  
  response_content <- content(response, as = "text", encoding = "UTF-8")
  response_json <- fromJSON(response_content, simplifyVector = FALSE)
  
  if (!is.null(response_json$choices) && length(response_json$choices) > 0) {
    return(response_json$choices[[1]]$message$content)
  } else {
    stop("Estrutura inesperada da resposta: ", response_content)
  }
}

#' @export
clean_html <- function(html_text) {
  # Primeiro, remove os delimitadores de código
  text <- remove_code_delimiters(html_text)
  
  # Remove todas as tags HTML, exceto as necessárias para o quiz
  text <- gsub("<(?!/?(b|p|br|ol|ul|li|div))[^>]+>", "", text, perl = TRUE)
  
  # Remove qualquer DOCTYPE, html, head ou body remanescente
  text <- gsub("<!DOCTYPE[^>]*>", "", text)
  text <- gsub("</?html[^>]*>", "", text)
  text <- gsub("</?head[^>]*>", "", text)
  text <- gsub("</?body[^>]*>", "", text)
  
  # Remove espaços em branco extras
  text <- gsub("\\s+", " ", text)
  text <- trimws(text)
  
  return(text)
}

#' @export
remove_code_delimiters <- function(text) {
  # Remove ```html no início e ``` no final, se presentes
  text <- gsub("^\\s*```html\\s*", "", text)
  text <- gsub("\\s*```\\s*$", "", text)
  return(text)
}
