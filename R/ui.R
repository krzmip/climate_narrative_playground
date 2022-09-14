ui <- function(){
    fluidPage(
        title = "Climate narrative playground",
        tabsetPanel(
            id = "wizard",
            type = "hidden",
            tabPanel(
                "title",
                fluidPage(
                    h2("Title"),
                    p("This is a simple app to experiment with some shiny features"),
                    hr(),
                    uiOutput("title_next_placeholder")
                )
            ),
            tabPanel(
                "main",
                fluidPage(
                    h2("Main page"),
                    p("version 0.8"),
                    h3("Inputs"),
                    fluidRow(
                        checkboxInput("modal_flag", "Show modal message when busy", value = TRUE),
                        checkboxInput("future_debug", "Future debug", value = getOption("future.debug", FALSE)),
                        textInput("text_input", "Insert any text")
                    ),
                    h3("Standard calculations"),
                    fluidRow(
                        actionButton("sync_est_pi", "Estimate pi,  wrapped in functions"),
                        actionButton("sync_pandoc", "Create html document")
                    ),
                    h3("Async calculations"),
                    fluidRow(
                        actionButton("async_est_pi", "Estimate pi, wrapped in functions"),
                        actionButton("async_pandoc", "Create html document"),
                        actionButton("async_pandoc_null", "Create html document with return NULL")
                    ),
                    hr(),
                    h3("Outputs"),
                    fluidRow(
                        tags$pre(tags$code(textOutput("messages"))),
                        uiOutput("html_report")
                    ),
                    hr(),
                    fluidRow(
                        actionButton("main_prev", "prev")
                    )
                )
            )
        )
    )
}