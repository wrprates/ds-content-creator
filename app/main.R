box::use(
  shiny[moduleServer, NS],
  bslib[page_fillable, bs_theme],
  waiter[use_waiter],
  ./view/content_generator
)

# Configurações globais
api_key <- Sys.getenv("OPENAI_KEY")
openai_url <- "https://api.openai.com/v1/chat/completions"

#' @export
ui <- function(id) {
  ns <- NS(id)
  page_fillable(
    use_waiter(),
    content_generator$ui(ns("content_generator")),
    theme = bs_theme(bootswatch = "litera")
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    content_generator$server("content_generator", api_key = api_key, openai_url = openai_url)
  })
}
