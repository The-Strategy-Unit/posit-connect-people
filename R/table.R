# Via https://glin.github.io/reactable/articles/custom-filtering.html#data-list-filter
dataListFilter <- function(tableId, style = "width: 100%; height: 28px;") {
  function(values, name) {
    dataListId <- sprintf("%s-%s-list", tableId, name)
    htmltools::tagList(
      htmltools::tags$input(
        type = "text",
        list = dataListId,
        oninput = sprintf(
          "Reactable.setFilter('%s', '%s', event.target.value || undefined)",
          tableId,
          name
        ),
        "aria-label" = sprintf("Filter %s", name),
        style = style
      ),
      htmltools::tags$datalist(
        id = dataListId,
        lapply(
          unique(values),
          function(value) htmltools::tags$option(value = value)
        )
      )
    )
  }
}
