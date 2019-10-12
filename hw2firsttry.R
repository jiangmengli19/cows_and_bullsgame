
generate_computer_vector <- function() {
  x <- c(sample(1:9, 4))
  #x is the number computer guess and convert it to vector
  return (x)
}


get_guess <- function(resultfrominp) {
  numbers_string <- resultfrominp
  
  user_choice <- as.numeric(unlist(strsplit(numbers_string, "")))
  
  return(user_choice)
}
number_bulls <- function(x, y) {
  bulls <- 0
  for (i in 1:4) {
    if (x[i] == y[i]) {
      bulls <- bulls + 1
    }
  }
  return (bulls)
}
number_cows <- function(x, y) {
  bulls <- number_bulls(x, y)
  tog <- 0
  for (i in 1:4) {
    for (j in 1:4) {
      if (x[i] == y[j]) {
        tog <- tog + 1
      }
    }
  }
  cows <- tog - bulls
  return(cows)
  
}
number_bulls_and_cows <- function(x, y) {
  bulls <- number_bulls(x, y)
  cows <- number_cows(x, y)
  result <- c(bulls, cows)
  return(result)
}
do_response <- function(x, y) {
  fina_resu <- number_bulls_and_cows(x, y)
  print(paste("bulls is", fina_resu[1], "cows is", fina_resu[2]))
}
check__numeric <- function(a, resu) {
  if (resu == FALSE) {
    return(resu)
  } else{
    suppressWarnings(for (i in 1:length(a)) {
      if (is.na(as.numeric(a[i]))) {
        print("you need numeric input")
        resu <- FALSE
        break
      }
    })
    return(resu)
    
  }
  
  
}

check__length <- function(a, resu) {
  if (resu == FALSE) {
    return(resu)
  } else{
    if (length(a) != 4) {
      print("you need a 4 digits input")
      resu <- FALSE
    }
    
    return(resu)
  }
  
  
  
}
check__input <- function() {
  gutime <- 0
  for (i in 1:3) {
    instruction_ <- TRUE
    numbers_string <-
      readline("Please enter four digit numbers without replacement from 1 to 9 > ")
    c <- unlist(strsplit(numbers_string, ""))
    b <- as.character(c)
    
    instruction_ <- check__numeric(b, instruction_)
    instruction_ <- check__length(b, instruction_)
    if (instruction_ == FALSE) {
      gutime <- gutime + 1
    }
    if (instruction_ == TRUE) {
      break
    }
    
  }
  result__ <- list(gutime, numbers_string)
  
  return(result__)
  
}
bulls_and_cows <- function() {
  com_gu <- generate_computer_vector()
  
  for (i in 0:9) {
    rematime <- 10 - i
    print(paste("you have", rematime, "times"))
    finalchec <- check__input()
    if (finalchec[[1]] == 3) {
      print("The game is stopped")
      break
    }
    resultfrominp <- finalchec[[2]]
    user_gu <- get_guess(resultfrominp)
    do_response(com_gu, user_gu)
    fina_res <- number_bulls_and_cows(com_gu, user_gu)
    if (fina_res[1] == 4) {
      print("win")
      break
    }
  }
  print(paste("The correct answer is", com_gu[1], com_gu[2], com_gu[3], com_gu[4]))
}

bulls_and_cows()