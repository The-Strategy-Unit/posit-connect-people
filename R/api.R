# Make connection to Connect client
get_client <- function(
    server_name = Sys.getenv("CONNECT_SERVER"),
    api_key = Sys.getenv("CONNECT_API_KEY")
) {
  connectapi::connect(server_name, api_key)
}

# Return a dataframe with guid and group name
get_groups <- function(client = get_client()) {
  client |>
    connectapi::get_groups() |>
    dplyr::select(guid, name)
}

# Return a list of dataframes of user information, one named element per group
get_group_membership <- function(client = get_client()) {

  groups <- get_groups(client)

  groups_members <- groups[["guid"]] |>
    purrr::set_names(groups[["name"]]) |>
    purrr::map(\(guid) connectapi::get_group_members(client, guid))

  # Retain empty groups by giving them '[none]' as a user
  for (group in names(groups_members)) {
    df_is_empty <- nrow(groups_members[[group]]) == 0
    if (df_is_empty) {  # i.e. no users were returned
      groups_members[[group]] <- tibble::tibble(username = "[none]")
    }
  }

  groups_members

}

# Return a dataframe with one row per username and group membership
get_user_groups <- function(client = get_client()) {

  group_membership <- get_group_membership(client)

  group_membership |>
    purrr::list_rbind(names_to = "group") |>
    dplyr::select(username, group)

}

# Return a dataframe of user details (username, name, user_role, active_time)
get_all_users <- function(
    client = get_client(),
    include_guid = FALSE
) {

  users <- connectapi::get_users(client) |>
    dplyr::mutate(
      name = glue::glue("{first_name} {last_name}") |>
        stringr::str_squish(),
      email = tolower(email)
    ) |>
    dplyr::select(
      guid,
      username,
      name,
      email,
      user_role,
      active_time,
      locked
    )

  if (!include_guid) dplyr::select(users, -guid) else return(users)

}

# Returns a dataframe of user details with one row per user and group
get_all_users_groups <- function(client = get_client()) {

  all_users <- get_all_users(client)
  user_groups <- get_user_groups(client)

  all_users |>
    dplyr::full_join(
      user_groups,
      by = "username",
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(
      last_login = as.character(format(active_time, "%Y-%m-%d")),
      .after = user_role
    ) |>
    dplyr::select(-active_time) |>
    dplyr::mutate(
      dplyr::across(tidyselect::everything(), as.character),
      dplyr::across(
        tidyselect::everything(),
        \(column) tidyr::replace_na(column, "[none]")
      )
    ) |>
    dplyr::arrange(tolower(username), tolower(group))

}

# Returns a dataframe of content details, one row per content item
get_content <- function(client = get_client()) {
  client |>
    connectapi::get_content() |>
    tidyr::hoist(owner, "username") |>
    dplyr::mutate(
      has_tags = purrr::map(tags, \(x) !is.null(x)),
      has_tags = unlist(has_tags),
      app_mode = dplyr::if_else(
        content_category != "",
        glue::glue("{app_mode} ({content_category})"),
        app_mode
      )
    ) |>
    dplyr::select(
      content_id = id,
      name,
      title,
      description,
      mode = app_mode,
      has_tags,
      last_deployed_time,
      owner_username = username,
      content_url,
      dashboard_url
    ) |>
    dplyr::arrange(dplyr::desc(as.numeric(content_id))) |>
    dplyr::mutate(
      last_deployed_time = format(last_deployed_time, "%Y-%m-%d %H:%M"),
      dplyr::across(
        dplyr::where(is.character),
        \(col) dplyr::na_if(col, "")
      ),
      dplyr::across(
        tidyselect::everything(),
        \(column) tidyr::replace_na(column, "[none]")
      )
    )
}
