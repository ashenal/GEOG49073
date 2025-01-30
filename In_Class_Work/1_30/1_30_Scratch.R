
Exercise <- function(a,b){
  if ((a %% 2 == 0) && (b %% 2 == 0)) return(TRUE)
  if ((a %% 2 != 0) && (b %% 2 != 0)) return(TRUE)
  return(FALSE)
  }

Exercise(8,9)
