box::use(
  bslib[card, card_header, layout_sidebar, sidebar],
  httr[POST, add_headers, content_type_json, content, status_code],
  jsonlite[toJSON, fromJSON],
  shiny[NS, moduleServer, observeEvent, renderUI, HTML, selectInput, actionButton, tags, htmlOutput, tagList, div, reactiveVal, isolate, req],
  waiter[waiter_show, waiter_hide, bs5_spinner]
)

box::use(
  app/logic/utils[send_message_to_chatgpt, clean_html, remove_code_delimiters]
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
    layout_sidebar(
      sidebar = sidebar(
        selectInput(ns("category"), "Selecione a área de Ciência de Dados:", choices = categories),
        selectInput(ns("level"), "Selecione o nível do Quiz:", choices = levels, selected = "Intermediário"),
        selectInput(ns("num_questions"), "Número de perguntas:", choices = num_questions, selected = 3),
        actionButton(ns("generate"), "Gerar Quiz"),
        actionButton(ns("reveal"), "Revelar Respostas", class = "mt-3")
      ),
      div(
        id = ns("quiz_container"),
        htmlOutput(ns("generated_quiz"))
      )
    )
  )
}

#' @export
server <- function(id, api_key, openai_url) {
  moduleServer(id, function(input, output, session) {
    quiz_content <- reactiveVal("")

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
        "Forneça a resposta correta após cada pergunta, envolvida em uma div com a classe 'answer'. ",
        "Antes da resposta, adicione o prefixo '<b>Resposta:</b> '. ",
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
      quiz_content(cleaned_quiz)
      
      waiter_hide()
    })

    output$generated_quiz <- renderUI({
      req(quiz_content())
      tagList(
        tags$style(".answer { display: none; }"),
        HTML(quiz_content()),
        tags$script(HTML(
          sprintf("
          $(document).ready(function() {
            $('#%s').off('click').on('click', function() {
              $('.answer').toggle();
              $(this).text(function(i, text) {
                return text === 'Revelar Respostas' ? 'Ocultar Respostas' : 'Revelar Respostas';
              });
            });
          });
          ", session$ns("reveal"))
        ))
      )
    })
  })
}
