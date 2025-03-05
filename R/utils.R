# Exit rendering if environment variables are missing (prevents error emails)
check_env_vars <- function(required_env_vars) {
  if (any(Sys.getenv(required_env_vars) == "")) {
    cat("One of the following environment variables was not set, so exiting \n\n")
    cat(paste("*", required_env_vars, collapse = "\n"), "\n\n")
    knitr::knit_exit()
  }
}

# Create a {DT} datatable with common settings
create_dt <- function(
    dat,
    type = c("users", "content"),
    ...  # pass further options
) {
  dat |>
    DT::datatable(
      filter = "top",
      rownames = FALSE,
      escape = FALSE,  # for URLs
      extensions = "Buttons",
      options = list(
        dom = "Bftipr",
        autoWidth = TRUE,
        buttons = list(
          list(
            extend = "csv",
            title = glue::glue("{Sys.Date()}_su-posit-connect_{type}-lookup")
          )
        ),
        ...
      )
    )
}
