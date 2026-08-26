summary_modal_title <- function(field) {
  tags$div(
    class = "ns-summary-title",
    tags$span(class = "ns-summary-title-icon", icon("clipboard-list")),
    tags$div(
      tags$strong(paste(field, "field report")),
      tags$small("Treatment and performance summary")
    )
  )
}
