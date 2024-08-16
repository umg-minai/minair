#' File paths relative to the root of a directory hierarchy
#'
#' A shortcut of `rprojroot::find_root_file` for git repositories.
#'
#' @inheritParams rprojroot::find_root_file
#' @importFrom rprojroot is_git_root
#' @export
#' @seealso [`rprojroot::find_root_file`]
find_git_root_file <- rprojroot::is_git_root$find_file
