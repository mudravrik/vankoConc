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

kel_conc <- function(peak, trough, gap) {
  return(log(trough/peak)/-gap)
}

auc_by_creat <- function(weight, height, age, gender, creat, dose, tau, inf_time) {
  
  bw = 50 + 2.3 * (height/2.53 - 60)
  
  if(height/2.53 < 60){
    bw = 0.4 * (weight - bw) + bw
  }
  
  cr_cl =  creat_calc(age = age, gender = gender, creat = creat, weight = bw)
  
  
  k_el = 0.00083 * cr_cl + 0.0044
  
  V_d = 0.7 * weight
  
  c_peak = dose * (1 - exp(-k_el * inf_time)) / (inf_time * V_d * k_el * (1 - exp(-k_el*tau)))
  
  c_trough = c_peak*exp(-k_el * (tau - inf_time)) 
  
  lin_trap = (c_peak + c_trough) * inf_time / 2
  
  log_trap = (c_peak - c_trough) * (tau - inf_time)/ log(c_peak/c_trough)
  
  auc = (log_trap + lin_trap) * 2
  
  return(auc)
}

auc_by_conc = function(peak, trough, tau, inf_time){
  
  k_el = kel_conc(peak = peak,trough = trough, gap = tau)
  
  lin_trap = (peak + trough) * inf_time / 2
  
  log_trap = (peak - trough) * (tau - inf_time)/ log(peak/trough)
  
  auc = (log_trap + lin_trap) * 2
  
  return(auc)
}


  
  