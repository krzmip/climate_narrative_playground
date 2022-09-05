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
                    p("version 0.6"),
                    fluidRow(
                        checkboxInput("modal_flag", "Show modal message when busy", value = TRUE),
                        textInput("text_input", "Insert any text"),
                        textOutput("msg"),
                        uiOutput("html_report")
                    ),
                    h3("Standard calculations"),
                    fluidRow(
                        actionButton("est_pi", "Estimate pi"),
                        actionButton("f_est_pi", "Estimate pi,  output saved to file"),
                        actionButton("ff_est_pi", "Estimate pi,  wrapped in functions"),
                        actionButton("pandoc", "Create html document")
                    ),
                    h3("Async calculations"),
                    fluidRow(
                        actionButton("async_est_pi", "Estimate pi"),
                        actionButton("async_f_est_pi", "Estimate pi, output saved to file"),
                        actionButton("async_ff_est_pi", "Estimate pi, wrapped in functions"),
                        actionButton("async_pandoc", "Create html document")
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