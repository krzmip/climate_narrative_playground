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

estimate_pi_to_file <- function(filename, free_text="", ...){
    file_conn <- file(filename)
    writeLines(
        paste0(
            free_text,
            ".   ",
            estimate_pi(...)
        ),
        file_conn
    )
    close(file_conn)
    return(invisible(NULL))
}

produce_report <- function(all_inputs, settings, async = FALSE, sleep = 0) {
  print("Produce report")
  if (async) {
    return(
        print("(async)")  
        promises::future_promise({
            produce_report_(all_inputs, settings, sleep)
        })
    )
  } else {
        print("(sync)")
        produce_report_(all_inputs, settings, sleep)
        return(invisible(NULL))
  }
}

produce_report_ <- function(all_inputs, settings, sleep){
    print("Sleep...")
    Sys.sleep(sleep)
    print("Actual report production")
    estimate_pi_to_file(settings$filename, all_inputs)
}

show_the_modal <- function(){
    showModal(
        modalDialog(
            "Shiny is busy",
            title = "Message",
            footer = NULL
        )
    )
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
                    hr(),
                    uiOutput("title_next_placeholder")
                )
            ),
            tabPanel(
                "main",
                fluidPage(
                    h2("Main page"),
                    p("version 0.3"),
                    fluidRow(
                        checkboxInput("modal_flag", "Show modal message when busy", value = TRUE),
                        textInput("text_input", "Insert any text"),
                        textOutput("msg")
                    ),
                    h3("Standard calculations"),
                    fluidRow(
                        actionButton("est_pi", "Estimate pi"),
                        actionButton("f_est_pi", "Estimate pi,  output saved to file"),
                        actionButton("ff_est_pi", "Estimate pi,  wrapped in functions")
                    ),
                    h3("Async calculations"),
                    fluidRow(
                        actionButton("async_est_pi", "Estimate pi"),
                        actionButton("async_f_est_pi", "Estimate pi, output saved to file"),
                        actionButton("async_ff_est_pi", "Estimate pi, wrapped in functions")
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
    output$status <- renderText("...")
    plan(multisession)
    session$userData$temp_txt <- tempfile(fileext = ".txt")
    output$title_next_placeholder <- renderUI({
        actionButton("title_next", "next")
    })
    all_inputs <- reactive(
        paste0(
            "text_input: ",
            input[["text_input"]],
            ", modal_flag: ",
            input[["modal_flag"]]
        )
    )
    observeEvent(
        input[["title_next"]],
        updateTabsetPanel(inputId = "wizard", selected = "main")
    )
    observeEvent(
        input[["main_prev"]],
        updateTabsetPanel(inputId = "wizard", selected = "title")
    )
    observeEvent(
        input[["est_pi"]],
        {
            if (input[["modal_flag"]]) show_the_modal()
            session$userData$pi_message <- estimate_pi()
            output[["msg"]] <- renderText(
                session$userData$pi_message
            )
            if (input[["modal_flag"]]) removeModal()
        }
    )
    observeEvent(
        input[["f_est_pi"]],
        {
            if (input[["modal_flag"]]) show_the_modal()
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
        input[["ff_est_pi"]],
        {
            settings <- list(filename=session$userData$temp_txt)
            if (input[["modal_flag"]]) show_the_modal()
            produce_report(all_inputs(), settings)
            if (input[["modal_flag"]]) removeModal()
            result <- includeText(session$userData$temp_txt)
            output$msg <- renderText(result)
        }
    )
    observeEvent(
        input[["async_est_pi"]],
        {
            if (input[["modal_flag"]]) show_the_modal()
            future_promise({
                estimate_pi()
            }, seed = TRUE) %...>% {
                if (input[["modal_flag"]]) removeModal()
                output[["msg"]] <- renderText(.)
            }
        }
    )
    observeEvent(
        input[["async_f_est_pi"]],
        {
            if (input[["modal_flag"]]) show_the_modal()
            future_promise({
                estimate_pi_to_file(session$userData$temp_txt)
            }, seed = TRUE) %...>% {
                if (input[["modal_flag"]]) removeModal()
                file_conn <- file(session$userData$temp_txt)
                pi_msg <- readLines(file_conn)
                close(file_conn)
                output[["msg"]] <- renderText(pi_msg)
            }
        }
    )
    observeEvent(
        input[["async_ff_est_pi"]],
        {
            settings <- list(filename=session$userData$temp_txt)
            if (input[["modal_flag"]]) show_the_modal()
            produce_report(all_inputs(), settings, TRUE) %...>% {
                if (input[["modal_flag"]]) removeModal()
                result <- includeText(session$userData$temp_txt)
                output$msg <- renderText(result)
            }
        }
    )
}

shinyApp(ui = ui, server = server)