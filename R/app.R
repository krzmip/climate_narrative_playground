#' @import shiny
#' @importFrom future plan multisession
#' @export
run_shiny_app <- function(){
    future::plan(future::multisession)
    shinyApp(ui = ui, server = server)
}