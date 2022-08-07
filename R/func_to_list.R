

func_to_list <- function(control) {

  control <- lapply(control, function(arg) {
    if (any(c("function", "family") %in% class(arg))) {
      list(arg)
    } else {
      arg
    }
  })

  return(control)

}
