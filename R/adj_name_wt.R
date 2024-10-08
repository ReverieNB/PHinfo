#' Adjust probabilistic matching weights.
#'
#' Adjust the weight of matching in instances where one data set has a multi-part name and the other doesn't. This can be used to make up for the relative under weighting of these matches.
#' @param data a dataframe created by matching two data sets
#' @param wt the weight affected matches will be assigned if greater than the original weight.
#' @export

#Use this function after the probabilistic match using names has been completed. May need to rename the name columns if they are not already in the 'first_name' format.
adj_name_wt <- function(data, wt = 0.9){
  
  #If names have been cleaned, transform them back. Very inelegant, but a good enough fix. This isn't used much.
  if ("last_flag_1" %in% names(data)){
    data <- data |>
      rename(last_flag.1 = last_flag_1, last_flag.2 = last_flag_2, 
             last_name.1 = last_name_1, last_name.2 = last_name_2,
             first_name.1 = first_name_1, first_name.2 = first_name_2, 
             Weight = weight)
    
    clean_flag = 1
  }

  temp <- data |>
    dplyr::mutate(
      #adjust weight if one is a single name, one is multi-part, plus the single name is at either the beginning or end of the multi-part name
      weight_adj = dplyr::case_when(
        last_flag.1 == 1 & last_flag.2 == 0 & stringr::str_detect(last_name.1, paste0("^", last_name.2, "|", last_name.2, "$")) ~ as.numeric(wt),
        last_flag.1 == 0 & last_flag.2 == 1 & stringr::str_detect(last_name.2, paste0("^", last_name.1, "|", last_name.1, "$")) ~ as.numeric(wt),
        TRUE ~ 0),
      #for now, use the logic that in order for an adjustment the first names need to match perfectly, and adjusted weight > initial weight
      adj_flag = ifelse(first_name.1 == first_name.2 & weight_adj == wt & weight_adj > Weight, 1, 0), #flag for adjusted weights
      Weight = ifelse(adj_flag == 1, weight_adj, Weight)) |>
    dplyr::select(-weight_adj)

  message(paste0("Weight for ", sum(temp$adj_flag), " match", ifelse(sum(temp$adj_flag) == 1, "", "es"), " adjusted."))
  
  if (exists("clean_flag")){
    temp <- temp |>
      rename(last_flag_1 = last_flag.1, last_flag_2 = last_flag.2, 
             last_name_1 = last_name.1, last_name_2 = last_name.2,
             first_name_1 = first_name.1, first_name_2 = first_name.2,
             weight = Weight)
  }

  return(temp)
}
