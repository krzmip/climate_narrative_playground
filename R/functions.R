#' @importFrom stats rnorm
alternative_sleep <- function(time) {
  end_time <- Sys.time() + time
  sum <- 0
  count <- 0
  while (Sys.time() < end_time) {
    # some time consuming operation, but preferably not much memory consuming
    sum <- sum + stats::rnorm(1)
    count <- count + 1
  }
  return(sum/count)
}

#' @importFrom stats runif
estimate_pi <- function(N = 1e6, digits = 4){
    sum <- 0
    for (i in 1:N){
        x <- stats::runif(1)
        y <- stats::runif(1)
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

#' @importFrom promises future_promise
produce_report <- function(all_inputs, settings, async = FALSE, sleep = 0) {
  print("Produce report")
    if (async) {
        print("(async)")  
        return(
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

#' @importFrom rmarkdown render
produce_report_ <- function(all_inputs, settings, sleep){
    print("Sleep...")
    Sys.sleep(sleep)
    print("Actual report production")
    if (settings$action == "pi"){
        estimate_pi_to_file(settings$output_file, all_inputs)
    } else {
        report_contents <- paste(
            "# Title",
            "## Subtitle",
            "some text",
            "and other text",
            "## all inputs",
            all_inputs,
            sep="\n\n"
        )
        file_conn <- file(settings$md_file)
        writeLines(
            report_contents,
            file_conn
        )
        close(file_conn)
        rmarkdown::render(
            input = settings$md_file,
            output_file = settings$output_file,
            output_format = settings$output_format
        )
        return(invisible(NULL))
    }
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