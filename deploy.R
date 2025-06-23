deploy <- function(server_name, app_id) {
  rsconnect::deployDoc(
    server = server_name,
    appId = app_id,
    doc = "index.qmd",
    appName = "posit-connect-people",
    appTitle = "Lookup: Posit Connect People",
    lint = FALSE,
    forceUpdate = TRUE
  )
}

deploy("connect.strategyunitwm.nhs.uk", 82)
