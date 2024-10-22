box::use(
  bslib[card, card_header, layout_sidebar, sidebar],
  httr[POST, add_headers, content_type_json, content, status_code],
  jsonlite[toJSON, fromJSON],
  shiny[NS, moduleServer, observeEvent, renderUI, HTML, selectInput, actionButton, tags, htmlOutput],
  waiter[waiter_show, waiter_hide, bs5_spinner],
  xml2[read_html]
)

# Valores fixos específicos do módulo
categories <- c("Estatística", "Machine Learning", "Método Científico", "Computação", "Conhecimento de Negócio")
levels <- c("Iniciante", "Intermediário", "Avançado")
sizes <- c("Pequeno", "Médio", "Longo")

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
  doc <- read_html(html_text)
  as.character(doc)
}

#' @export
ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = FALSE,
    card_header("Gerador de Conteúdo"),
    layout_sidebar(
      sidebar = sidebar(
        selectInput(ns("category"), "Selecione a área de Ciência de Dados:", choices = categories),
        selectInput(ns("level"), "Selecione o nível do Cientista de Dados:", choices = levels, selected = "Intermediário"),
        selectInput(ns("size"), "Selecione o tamanho do post:", choices = sizes, selected = "Médio"),
        actionButton(ns("generate"), "Gerar Postagem")
      ),
      htmlOutput(ns("generated_text"))
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
      size <- input$size
      
      size_chars <- switch(
        size,
        "Pequeno" = 300,
        "Médio" = 600,
        "Longo" = 800
      )
      
      prompt <- paste(
        "Você é um especialista em ciência de dados e seu objetivo é ajudar profissionais iniciantes a entrar na área, fornecendo conteúdo técnico, informativo e interessante para postar no LinkedIn. Cada postagem deve abordar um tópico específico dentro de uma das seguintes categorias:", 
        category, 
        "A postagem deve ser clara, objetiva e fornecer benefícios ou curiosidades ao leitor. Evite parecer que o texto foi gerado por uma IA, e mostre rigor intelectual e profundidade técnica.",
        "O nível do cientista de dados é:", level, ".",
        "O tamanho desejado para o post é:", size, "com aproximadamente", size_chars, "caracteres.\n\n",
        "1. Introduza o tópico com uma frase chamativa, que deve sempre ser formatada com tag b de html apenas.\n",
        "2. Forneça uma explicação técnica, incluindo termos e conceitos relevantes.\n",
        "3. Inclua uma curiosidade ou dica prática sobre o uso do tópico na prática.\n",
        "4. Termine com uma chamada para ação, convidando o leitor a aprender mais ou compartilhar suas próprias experiências.\n\n",
        "Use este prompt para gerar postagens diárias que ajudem a construir seu perfil no LinkedIn, fornecendo valor técnico e engajando seu público.",
        "O resultado do prompt deve ser em html puro, quebre linhas com a tag p do html, trazendo direto o resultado, sem mensagens introdutórias. Evite formatações específicas e chunks de código.",
        "Priorize o tamanho do prompt. Uso de emojis é liberado."
      )

      text <- tryCatch({
        send_message_to_chatgpt(prompt, api_key, openai_url)
      }, error = function(e) {
        paste("Erro ao gerar a postagem:", e$message)
      })
      cleaned_text <- clean_html(text)
      output$generated_text <- renderUI({ HTML(cleaned_text) })
      waiter_hide()
    })
  })
}