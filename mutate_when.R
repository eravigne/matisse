mutate_when <- function(data, ...) {
  dots <- eval(substitute(alist(...)))
  for (i in seq(1, length(dots), by = 2)) {
    condition <- eval(dots[[i]], envir = data)
    mutations <- eval(dots[[i + 1]], envir = data[condition, , drop = FALSE])
    data[condition, names(mutations)] <- mutations
  }
  data
}


# Exemple -----------------------------------------------------------------


# 
# mtcars
# 
# 
# mtcars %>% mutate_when(
#   mpg > 22,    list(cyl = 100,hp=6789876),
#   disp == 160, list(cyl = 200)
# )


# Source ------------------------------------------------------------------

# https://stackoverflow.com/questions/34096162/dplyr-mutate-replace-on-a-subset-of-rows
