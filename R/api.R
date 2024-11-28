get_client <- function(
    server_name = Sys.getenv("CONNECT_SERVER"),
    api_key = Sys.getenv("CONNECT_API_KEY")
) {
  connectapi::connect(server_name, api_key)
}

get_groups <- function(client = get_client()) {
  client |>
    connectapi::get_groups() |>
    dplyr::select(guid, name)
}

get_group_membership <- function(client = get_client(), groups = get_groups()) {
  groups[["guid"]] |>
    purrr::set_names(groups[["name"]]) |>
    purrr::map(\(guid) connectapi::get_group_members(client, guid))
}

get_user_groups <- function(group_membership = get_group_membership()) {
  group_membership |>
    purrr::list_rbind(names_to = "group") |>
    dplyr::select(username, group)
}

get_all_users <- function(client = get_client()) {
  connectapi::get_users(client) |>
    dplyr::mutate(
      name = glue::glue("{first_name} {last_name}") |>
        stringr::str_squish()
    ) |>
    dplyr::select(username, name, user_role, active_time)
}

get_all_users_groups <- function(
    all_users = get_all_users(),
    user_groups = get_user_groups()
) {
  all_users |>
    dplyr::full_join(
      user_groups,
      by = "username",
      relationship = "many-to-many"
    )
}
