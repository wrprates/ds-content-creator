box::use(
  bslib[card, card_header, layout_sidebar, sidebar],
  httr[POST, add_headers, content_type_json, content, status_code],
  jsonlite[toJSON, fromJSON],
  rvest[read_html, html_text],
  shiny[NS, moduleServer, observeEvent, renderUI, HTML, selectInput, actionButton, tags, htmlOutput, tagList, div, reactiveVal, isolate, req, radioButtons, textOutput, renderText],
  stats[setNames],
  xml2[xml_find_all, xml_find_first, xml_text],
  waiter[waiter_show, waiter_hide, bs5_spinner],
)

box::use(
  app/logic/utils[send_message_to_chatgpt, clean_html, remove_code_delimiters]
)

# Valores fixos específicos do módulo
categories <- c(
  "Qualidade de Dados (Análise Univariada)",
  "Análise Exploratória de Dados (Multivariada)",
  "Teste de Hipóteses",
  "Machine Learning Não Supervisionado",
  "Machine Learning Supervisionado",
  "Visualização de Dados",
  "Linguagem R para Data Science",
  "Linguagem Python para Data Science",
  "GIT",
  "SQL"
)
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
        actionButton(ns("check"), "Verificar Respostas", class = "mt-3"),
        htmlOutput(ns("score"))  # Mudamos de textOutput para htmlOutput
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
    correct_answers <- reactiveVal(list())

    observeEvent(input$generate, {
      waiter_show(
        html = bs5_spinner(),
        color = "rgba(255, 255, 255, 0.35)"
      )

      category <- input$category
      level <- input$level
      num_questions <- input$num_questions
      
      prompt <- paste(
        "Você é um especialista em Ciência de Dados com conhecimento profundo em todas as seguintes áreas:",
        paste(categories, collapse = ", "),
        ".\n\nCrie um quiz de", num_questions, "perguntas sobre", category, 
        "para um cientista de dados de nível", level, ". ",
        "Mantenha as perguntas estritamente dentro do escopo da categoria '", category, "', ",
        "sem avançar em outras categorias. ",
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
      
      parsed_html <- read_html(quiz_content())
      questions <- xml_find_all(parsed_html, "//ol/li")
      
      quiz_ui <- tagList()
      answers <- list()
      
      for (i in seq_along(questions)) {
        question_text <- xml_text(xml_find_first(questions[[i]], "./p[1]"))
        options <- xml_find_all(questions[[i]], ".//ul/li")
        option_texts <- xml_text(options)
        
        answer_div <- xml_find_first(questions[[i]], ".//div[@class='answer']")
        if (!is.null(answer_div)) {
          correct_answer <- xml_text(answer_div)
          correct_answer <- sub("^Resposta:\\s*", "", correct_answer)
          correct_answer <- trimws(correct_answer)
          correct_index <- which(sapply(option_texts, function(x) grepl(correct_answer, x, fixed = TRUE)))
          if (length(correct_index) > 0) {
            answers[[i]] <- as.character(correct_index[1])
          } else {
            answers[[i]] <- NA_character_
            print(paste("Aviso: Não foi possível encontrar a resposta correta para a pergunta", i))
            print("Opções:")
            print(option_texts)
            print("Resposta correta:")
            print(correct_answer)
          }
        } else {
          answers[[i]] <- NA_character_
          print(paste("Aviso: Não foi encontrada uma div de resposta para a pergunta", i))
        }
        
        explanation <- xml_text(xml_find_first(questions[[i]], ".//div[@class='explanation']"))
        
        quiz_ui[[i]] <- tagList(
          tags$p(tags$strong(paste(i, ".", question_text))),
          radioButtons(session$ns(paste0("q", i)), label = NULL, choices = setNames(seq_along(option_texts), option_texts), selected = character(0)),
          tags$div(id = session$ns(paste0("answer", i)), class = "answer", style = "display: none;", 
                   tags$p(tags$strong("Resposta:"), correct_answer)),
          tags$div(id = session$ns(paste0("explanation", i)), class = "explanation", style = "display: none;", 
                   tags$p(tags$strong("Explicação:"), explanation))
        )
      }
      
      correct_answers(answers)
      print("Respostas corretas:")
      print(answers)
      
      quiz_ui
    })

    observeEvent(input$check, {
      answers <- correct_answers()
      
      if (is.null(answers) || length(answers) == 0) {
        output$score <- renderUI(HTML("Nenhuma pergunta foi gerada ainda."))
        return()
      }

      total_questions <- length(answers)
      correct_count <- 0
      
      print("Verificando respostas:")
      for (i in 1:total_questions) {
        user_answer <- input[[paste0("q", i)]]
        correct_answer <- answers[[i]]
        
        print(paste("Pergunta", i))
        print(paste("Resposta do usuário:", user_answer))
        print(paste("Resposta correta:", correct_answer))
        
        if (!is.null(user_answer) && !is.na(correct_answer)) {
          if (user_answer == correct_answer) {
            correct_count <- correct_count + 1
            print("Resposta correta!")
          } else {
            print("Resposta incorreta.")
          }
        } else {
          print("Dados inválidos para esta pergunta.")
        }
      }
      
      score <- sprintf("Você acertou %d de %d perguntas (%.1f%%)", 
                       correct_count, total_questions, (correct_count / total_questions) * 100)
      print(paste("Pontuação final:", score))
      output$score <- renderUI(HTML(paste("<b>", score, "</b>")))
    })
  })
}
