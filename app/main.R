box::use(
  shiny[moduleServer, NS],
  bslib[page_fillable, navset_card_tab, nav_panel, bs_theme],
  waiter[use_waiter]
)

box::use(
  app/view/content_generator,
  app/view/quiz_generator
)

# Configurações globais
api_key <- Sys.getenv("OPENAI_KEY")
openai_url <- "https://api.openai.com/v1/chat/completions"

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  dark_theme <- bs_theme(version = 5, bootswatch = "darkly")
  
  page_fillable(
    use_waiter(),
    navset_card_tab(
      nav_panel(
        title = "Gerador de Conteúdo",
        content_generator$ui(ns("content_generator"))
      ),
      nav_panel(
        title = "Gerador de Quiz",
        quiz_generator$ui(ns("quiz_generator"))
      )
    ),
    theme = dark_theme
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    content_generator$server("content_generator", api_key = api_key, openai_url = openai_url)
    quiz_generator$server("quiz_generator", api_key = api_key, openai_url = openai_url)
  })
}
