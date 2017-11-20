creat_calc <- function(age, gender, creat, weight) {
  if (gender == "мужской") {
    return((140 - age)/creat * weight/72)
  } else if ( gender == "женский" ) {
    return((140 - age)/creat * weight/72*0.85)
  }
}


kel_creat <- function(clear_creat) {
  return(0.00083 * clear_creat + 0.0044)
}

kel_conc <- function(peak, through, gap) {
  return(log(through/peak)/-gap)
}