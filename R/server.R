#' @importFrom promises %...>% %...!%
server <- function(input, output, session){
    output$status <- renderText("...")
    session$userData$temp_txt <- tempfile(fileext = ".txt")
    session$userData$temp_md <- tempfile(fileext = ".md")
    session$userData$temp_html <- tempfile(fileext = ".html")
    output$title_next_placeholder <- renderUI({
        actionButton("title_next", "next")
    })
    messages <- reactiveVal()
    output$messages <- renderText(
        paste0(messages(), collapse = "\n")
    )
    add_message <- function(text){
        messages(
            c(
                messages(), 
                paste0(
                    Sys.time(),
                    "   ",
                    text
                )
        ))
        return(NULL)
    }
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
        input[["future_debug"]],
        options(future.debug=input[["future_debug"]])
    )
    observeEvent(
        input[["sync_est_pi"]],
        {
            add_message("clicked button sync_est_pi")
            settings <- list(output_file=session$userData$temp_txt, action="pi")
            if (input[["modal_flag"]]) show_the_modal()
            produce_report(all_inputs(), settings)
            if (input[["modal_flag"]]) removeModal()
            result <- includeText(session$userData$temp_txt)
            add_message(result)
        }
    )
    observeEvent(
        input[["sync_pandoc"]],
        {
            add_message("clicked button sync_pandoc")
            settings <- list(
                action="pandoc",
                output_file=session$userData$temp_html,
                md_file=session$userData$temp_md,
                output_format=rmarkdown::html_document(self_contained=FALSE)
            )
            if (input[["modal_flag"]]) show_the_modal()
            produce_report(all_inputs(), settings, FALSE, 5)
            if (input[["modal_flag"]]) removeModal()
            result <- includeHTML(session$userData$temp_html)
            output$html_report <- renderUI(result)
            add_message("See the HTML below")
        }
    )
    observeEvent(
        input[["async_est_pi"]],
        {
            add_message("clicked button async_est_pi")
            settings <- list(output_file=session$userData$temp_txt, action="pi")
            if (input[["modal_flag"]]) show_the_modal()
            produce_report(all_inputs(), settings, TRUE) %...>% {
                if (input[["modal_flag"]]) removeModal()
                result <- includeText(session$userData$temp_txt)
                add_message(result)
            }
        }
    )
    observeEvent(
        input[["async_pandoc"]],
        {
            add_message("clicked button async_pandoc")
            settings <- list(
                action="pandoc",
                output_file=session$userData$temp_html,
                md_file=session$userData$temp_md,
                output_format=rmarkdown::html_document(self_contained=FALSE)
            )
            if (input[["modal_flag"]]) show_the_modal()
            temp <- produce_report(all_inputs(), settings, TRUE, 5) %...>% {
                if (input[["modal_flag"]]) removeModal()
                result <- includeHTML(session$userData$temp_html)
                output$html_report <- renderUI(result)
                add_message("See the HTML below")
            }
            return(temp)
        }
    )
    observeEvent(
        input[["async_pandoc_null"]],
        {
            add_message("clicked button async_pandoc_null")
            settings <- list(
                action="pandoc",
                output_file=session$userData$temp_html,
                md_file=session$userData$temp_md,
                output_format=rmarkdown::html_document(self_contained=FALSE)
            )
            if (input[["modal_flag"]]) show_the_modal()
            temp <- produce_report(all_inputs(), settings, TRUE, 5) %...>% {
                if (input[["modal_flag"]]) removeModal()
                result <- includeHTML(session$userData$temp_html)
                output$html_report <- renderUI(result)
                add_message("See the HTML below")
            }
            return(NULL)
        }
    )
    observeEvent(input$go, {
        add_message('starting sleep in other session...')
        p <- future({
            Sys.sleep(5)
        })
        p <- then(p, function(value) {
            add_message('completed sleep')
        })
        ret <- p
        ret
  })
}