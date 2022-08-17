library(shiny)
library(future)
library(promises)

alternative_sleep <- function(time) {
  end_time <- Sys.time() + time
  while (Sys.time() < end_time) {
    # some time consuming operation, but preferably not much memory consuming
    rnorm(1)
  }
}

estimate_pi <- function(N = 2e6, digits = 4){
    sum <- 0
    for (i in 1:N){
        x <- runif(1)
        y <- runif(1)
        if (x^2 + y^2 < 1) {
            sum <- sum + 1
        }
    }
    est <- sum * 4 / N
    se <- sqrt(est * (4 - est) / N)
    return(
        paste0(
            "Pi is approximately ",
            round(est, digits),
            " +- ",
            round(se, digits)
        )
    )
}

estimate_pi_to_file <- function(filename, ...){
    file_conn <- file(filename)
    writeLines(estimate_pi(...), file_conn)
    close(file_conn)
    return(invisible(NULL))
}

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
                    actionButton("title_next", "next")
                )
            ),
            tabPanel(
                "main",
                fluidPage(
                    h2("Main page"),
                    p("version 0.1"),
                    fluidRow(
                        checkboxInput("modal_flag", "Show modal message when busy", value = TRUE),
                        textOutput("msg"),
                    ),
                    h3("Standard calculations"),
                    fluidRow(
                        actionButton("sleep", "Sys.sleep(10)"),
                        actionButton("alt_sleep", "alternative sleep for 10s"),
                        actionButton("est_pi", "Estimate pi"),
                        actionButton("fest_pi", "Estimate pi,  output saved to file"),
                        
                    ),
                    h3("Async calculations"),
                    fluidRow(
                        actionButton("async_sleep", "Sys.sleep(10)"),
                        actionButton("async_alt_sleep", "alternative sleep for 10s"),
                        actionButton("async_est_pi", "Estimate pi"),
                        actionButton("async_fest_pi", "Estimate pi, output saved to file"),
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

server <- function(input, output, session){
    plan(multisession)
    session$userData$temp_txt <- tempfile(fileext = ".txt")
    observeEvent(
        input[["title_next"]],
        updateTabsetPanel(inputId = "wizard", selected = "main")
    )
    observeEvent(
        input[["main_prev"]],
        updateTabsetPanel(inputId = "wizard", selected = "title")
    )
    observeEvent(
        input[["sleep"]],
        {
            if (input[["modal_flag"]]) {
                showModal(
                    modalDialog(
                        "Shiny is busy",
                        title = "Message",
                        footer = NULL
                    )
                )
            }
            Sys.sleep(10)
            if (input[["modal_flag"]]) removeModal()
            output[["msg"]] <- renderText(
                "Sys sleep"
            )
        }
    )
    observeEvent(
        input[["alt_sleep"]],
        {
            if (input[["modal_flag"]]) {
                showModal(
                    modalDialog(
                        "Shiny is busy",
                        title = "Message",
                        footer = NULL
                    )
                )
            }
            alternative_sleep(10)
            if (input[["modal_flag"]]) removeModal()
            output[["msg"]] <- renderText(
                "Alternative sleep"
            )
        }
    )
    observeEvent(
        input[["est_pi"]],
        {
            if (input[["modal_flag"]]) {
                showModal(
                    modalDialog(
                        "Shiny is busy",
                        title = "Message",
                        footer = NULL
                    )
                )
            }
            session$userData$pi_message <- estimate_pi()
            output[["msg"]] <- renderText(
                session$userData$pi_message
            )
            if (input[["modal_flag"]]) removeModal()
        }
    )
    observeEvent(
        input[["fest_pi"]],
        {
            if (input[["modal_flag"]]) {
                showModal(
                    modalDialog(
                        "Shiny is busy",
                        title = "Message",
                        footer = NULL
                    )
                )
            }
            estimate_pi_to_file(session$userData$temp_txt)
            file_conn <- file(session$userData$temp_txt)
            pi_msg <- readLines(file_conn)
            close(file_conn)
            output[["msg"]] <- renderText(
                pi_msg
            )
            if (input[["modal_flag"]]) removeModal()
        }
    )
    observeEvent(
        input[["async_sleep"]],
        {
            if (input[["modal_flag"]]) {
                showModal(
                    modalDialog(
                        "Shiny is busy",
                        title = "Message",
                        footer = NULL
                    )
                )
            }
            future_promise({
                Sys.sleep(10)
            }) %...>% {
                if (input[["modal_flag"]]) removeModal()
                output[["msg"]] <- renderText(
                    "Async sys sleep"
                )
            }
        }
    )
    observeEvent(
        input[["async_alt_sleep"]],
        {
            if (input[["modal_flag"]]) {
                showModal(
                    modalDialog(
                        "Shiny is busy",
                        title = "Message",
                        footer = NULL
                    )
                )
            }
            future_promise({
                alternative_sleep(10)
            }) %...>% {
                if (input[["modal_flag"]]) removeModal()
                output[["msg"]] <- renderText(
                    "Async sys sleep"
                )
            }
        }
    )
    observeEvent(
        input[["async_est_pi"]],
        {
            if (input[["modal_flag"]]) {
                showModal(
                    modalDialog(
                        "Shiny is busy",
                        title = "Message",
                        footer = NULL
                    )
                )
            }
            future_promise({
                estimate_pi()
            }, seed = TRUE) %...>% {
                if (input[["modal_flag"]]) removeModal()
                output[["msg"]] <- renderText(.)
            }
        }
    )
    observeEvent(
        input[["async_fest_pi"]],
        {
            if (input[["modal_flag"]]) {
                showModal(
                    modalDialog(
                        "Shiny is busy",
                        title = "Message",
                        footer = NULL
                    )
                )
            }
            future_promise({
                estimate_pi_to_file(session$userData$temp_txt)
            }, seed = TRUE) %...>% {
                if (input[["modal_flag"]]) removeModal()
                file_conn <- file(session$userData$temp_txt)
                pi_msg <- readLines(file_conn)
                close(file_conn)
                output[["msg"]] <- renderText(
                    pi_msg
                )
            }
        }
    )
}

shinyApp(ui = ui, server = server)