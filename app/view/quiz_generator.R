box::use(
  bslib[card, card_header, layout_sidebar, sidebar],
  httr[POST, add_headers, content_type_json, content, status_code],
  jsonlite[toJSON, fromJSON],
  shiny[NS, moduleServer, observeEvent, renderUI, HTML, selectInput, actionButton, tags, htmlOutput, tagList, div, reactiveVal, isolate, req, downloadHandler, downloadButton],
  waiter[waiter_show, waiter_hide, bs5_spinner],
  rvest[read_html, html_text],
  xml2[xml_find_all],
  quarto
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
        actionButton(ns("reveal"), "Revelar Respostas", class = "mt-3"),
        downloadButton(ns("download_pdf"), "Baixar PDF", class = "mt-3")
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
        "Após a resposta, adicione uma explicação detalhada do porquê esta é a resposta correta, envolvida em uma div com a classe 'explanation'. ",
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
        tags$style("
          .answer, .explanation, .explanation-btn { display: none; }
          .explanation-btn { margin-left: 10px; }
        "),
        HTML(quiz_content()),
        tags$script(HTML(
          sprintf("
          $(document).ready(function() {
            $('#%s').off('click').on('click', function() {
              $('.answer').toggle();
              $('.explanation-btn').toggle();
              $(this).text(function(i, text) {
                return text === 'Revelar Respostas' ? 'Ocultar Respostas' : 'Revelar Respostas';
              });
            });

            $('ol > li').each(function(index) {
              var explanationBtn = $('<button>', {
                text: 'Mostrar Explicação',
                class: 'explanation-btn btn btn-sm btn-outline-primary'
              });
              $(this).append(explanationBtn);
            });

            $('.explanation-btn').off('click').on('click', function() {
              $(this).siblings('.explanation').toggle();
              $(this).text(function(i, text) {
                return text === 'Mostrar Explicação' ? 'Ocultar Explicação' : 'Mostrar Explicação';
              });
            });
          });
          ", session$ns("reveal"))
        ))
      )
    })

    output$download_pdf <- downloadHandler(
      filename = function() {
        paste("quiz_", input$category, "_", input$level, ".pdf", sep = "")
      },
      content = function(file) {
        # Criar um diretório temporário
        temp_dir <- tempdir()
        temp_qmd <- file.path(temp_dir, "quiz.qmd")
        
        tryCatch({
          # Criar o conteúdo Quarto
          quarto_content <- c(
            "---",
            paste0("title: 'Quiz de ", input$category, " para nível ", input$level, "'"),
            "format: pdf",
            "---",
            "",
            "# Perguntas"
          )
          
          # Parsear o conteúdo HTML do quiz
          html_content <- read_html(quiz_content())
          
          # Extrair perguntas, respostas e explicações
          questions <- xml_find_all(html_content, "//ol/li")
          
          for (i in seq_along(questions)) {
            question_text <- html_text(questions[[i]])
            answer <- html_text(xml_find_all(questions[[i]], ".//div[@class='answer']"))
            explanation <- html_text(xml_find_all(questions[[i]], ".//div[@class='explanation']"))
            
            # Adicionar pergunta
            quarto_content <- c(quarto_content, paste0(i, ". ", question_text))
            
            # Adicionar resposta
            quarto_content <- c(quarto_content, "", "**Resposta:**", answer)
            
            # Adicionar explicação
            quarto_content <- c(quarto_content, "", "**Explicação:**", explanation)
            
            # Adicionar espaço entre perguntas
            quarto_content <- c(quarto_content, "", "---", "")
          }
          
          # Escrever o conteúdo Quarto no arquivo temporário
          writeLines(quarto_content, temp_qmd)
          
          # Renderizar o arquivo Quarto para PDF
          quarto::quarto_render(temp_qmd)
          
          # Mover o arquivo PDF gerado para o local desejado
          file.copy(file.path(temp_dir, "quiz.pdf"), file)
          
        }, error = function(e) {
          # Log do erro
          message("Erro ao gerar PDF: ", e$message)
          # Criar um arquivo de texto simples com a mensagem de erro
          writeLines(paste("Erro ao gerar PDF:", e$message), file)
        }, finally = {
          # Limpar os arquivos temporários
          unlink(temp_dir, recursive = TRUE)
        })
      }
    )
  })
}
