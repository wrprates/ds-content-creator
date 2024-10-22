box::use(
  bslib[card, card_header, layout_sidebar, sidebar],
  httr[POST, add_headers, content_type_json, content, status_code],
  jsonlite[toJSON, fromJSON],
  shiny[NS, moduleServer, observeEvent, renderUI, HTML, selectInput, actionButton, tags, htmlOutput],
  waiter[waiter_show, waiter_hide, bs5_spinner]
)

# Valores fixos específicos do módulo
categories <- c("Estatística", "Machine Learning", "Método Científico", "Computação", "Conhecimento de Negócio")
levels <- c("Iniciante", "Intermediário", "Avançado")
num_questions <- c(1, 3, 5, 10)

#' @export
ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = FALSE,
    card_header("Gerador de Quiz"),
    layout_sidebar(
      sidebar = sidebar(
        selectInput(ns("category"), "Selecione a área de Ciência de Dados:", choices = categories),
        selectInput(ns("level"), "Selecione o nível do Quiz:", choices = levels, selected = "Intermediário"),
        selectInput(ns("num_questions"), "Número de perguntas:", choices = num_questions, selected = 3),
        actionButton(ns("generate"), "Gerar Quiz")
      ),
      htmlOutput(ns("generated_quiz"))
    )
  )
}

#' @export
server <- function(id, api_key, openai_url) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$generate, {
      waiter_show(
        html = bs5_spinner(),
        color = "rgba(255, 255, 255, 0.35)"
      )

      category <- input$category
      level <- input$level
      num_questions <- input$num_questions
      
      prompt <- paste(
        "Crie um quiz de", num_questions, "perguntas sobre", category, 
        "para um cientista de dados de nível", level, ". ",
        "Cada pergunta deve ter 4 opções de resposta, com apenas uma correta. ",
        "Forneça a resposta correta após cada pergunta. ",
        "O resultado deve ser em HTML puro, usando tags <p> para quebras de linha e <b> para destaque.",
        "Use <ol> para a lista de perguntas e <ul> para as opções de resposta.",
        "Não inclua nenhuma mensagem introdutória ou de conclusão."
      )
      
      quiz <- tryCatch({
        send_message_to_chatgpt(prompt, api_key, openai_url)
      }, error = function(e) {
        paste("Erro ao gerar o quiz:", e$message)
      })
      
      cleaned_quiz <- clean_html(quiz)
      output$generated_quiz <- renderUI({ HTML(cleaned_quiz) })
      waiter_hide()
    })
  })
}

# Funções auxiliares
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

clean_html <- function(html_text) {
  # Primeiro, remove os delimitadores de código
  text <- remove_code_delimiters(html_text)
  
  # Remove todas as tags HTML, exceto <b>, <p>, <br>, <ol>, <ul>, e <li>
  text <- gsub("<(?!/?(b|p|br|ol|ul|li))[^>]+>", "", text, perl = TRUE)
  
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

remove_code_delimiters <- function(text) {
  # Remove ```html no início e ``` no final, se presentes
  text <- gsub("^\\s*```html\\s*", "", text)
  text <- gsub("\\s*```\\s*$", "", text)
  return(text)
}
