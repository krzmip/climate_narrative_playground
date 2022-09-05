#' @importFrom promises %...>% %...!%
server <- function(input, output, session){
    output$status <- renderText("...")
    plan(multisession)
    session$userData$temp_txt <- tempfile(fileext = ".txt")
    session$userData$temp_md <- tempfile(fileext = ".md")
    session$userData$temp_html <- tempfile(fileext = ".html")
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
            settings <- list(output_file=session$userData$temp_txt, action="pi")
            if (input[["modal_flag"]]) show_the_modal()
            produce_report(all_inputs(), settings)
            if (input[["modal_flag"]]) removeModal()
            result <- includeText(session$userData$temp_txt)
            output$msg <- renderText(result)
        }
    )
    observeEvent(
        input[["pandoc"]],
        {
            settings <- list(
                action="pandoc",
                output_file=session$userData$temp_html,
                md_file=session$userData$temp_md,
                output_format=rmarkdown::html_document(self_contained=FALSE)
            )
            if (input[["modal_flag"]]) show_the_modal()
            produce_report(all_inputs(), settings, FALSE, 10)
            if (input[["modal_flag"]]) removeModal()
            result <- includeHTML(session$userData$temp_html)
            output$html_report <- renderUI(result)
            output$msg <- renderText("See the HTML below")
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
            settings <- list(output_file=session$userData$temp_txt, action="pi")
            if (input[["modal_flag"]]) show_the_modal()
            produce_report(all_inputs(), settings, TRUE) %...>% {
                if (input[["modal_flag"]]) removeModal()
                result <- includeText(session$userData$temp_txt)
                output$msg <- renderText(result)
            }
        }
    )
    observeEvent(
        input[["async_pandoc"]],
        {
            settings <- list(
                action="pandoc",
                output_file=session$userData$temp_html,
                md_file=session$userData$temp_md,
                output_format=rmarkdown::html_document(self_contained=FALSE)
            )
            if (input[["modal_flag"]]) show_the_modal()
            produce_report(all_inputs(), settings, TRUE, 10) %...>% {
                if (input[["modal_flag"]]) removeModal()
                result <- includeHTML(session$userData$temp_html)
                output$html_report <- renderUI(result)
                output$msg <- renderText("See the HTML below")
            }
        }
    )
}