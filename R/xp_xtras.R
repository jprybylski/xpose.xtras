# These functions create and manipulate the xp_xtras object, which is an extension of the xpose_data object.
# xp_xtras objects are backwards compatible with xpose_data objects, and can be used in the same way.

#' Convert an object to an `xpose_data` and `xp_xtras` object
#'
#' @description
#' This function masks the default in `xpose` package, adding the
#' `xp_xtras` class to default `xpose_data` objects.
#'
#' @param x <`list`> object with `xpose_data` and `xp_xtras` components
#'
#' @return <[`xpose_data`][xpose::xpose_data]> and <`xp_xtras`> object
#' @export
#'
#' @examples
#'
#' c()
as_xpdb_x <- function(x) { # TODO
  # First just declare class
  new_x <- structure(
    x,
    class = c("xp_xtras", "xpose_data", 'uneval')
  )
  # Next check if it has _xtras parts already
  # If it does, just return new_x

  # If it doesn't, fill names with empty versions of true


  new_x
}

check_xpdb_x <- function(x, .skip_is=TRUE) {
  # Check for xp_xtras list elements in an xpose_data object

}
