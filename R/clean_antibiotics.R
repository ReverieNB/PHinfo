#' Clean STI antibiotic drugs
#'
#' Clean the names of antibiotic treatment regimens in the chlamydia, gonorrhea, and syphilis data. Some prior data cleaning may be required, such as making sure multiple drugs in the same column are separated by commas, and commas are only used for this purpose. The function can be updated as more drugs are added.
#' @param data a dataframe
#' @param input the name of the input variable
#' @param output the name assigned to the cleaned, output variable
#' @param disease specify either chlamydia, gonorrhea, or syphilis
#' @export

clean_antibiotics <- function(data, input = "antibiotic_treatment", output = "antibiotic_treatment", disease){

  if (disease == "syphilis"){

    syph <- data |>
      tidyr::separate_wider_delim(!!sym(input), names = c("ab1", "ab2"), delim = ",", too_few = "align_start", too_many = "error", cols_remove = FALSE) |>
      dplyr::mutate(dplyr::across(c(ab1, ab2), stringr::str_trim),
                    dplyr::across(c(ab1, ab2), ~dplyr::case_when(
                    is.na(.) | . == "" ~ "Unknown",
                    stringr::str_length(.) == 1 ~ "Unknown",
                    !is.na(as.numeric(.)) ~ "Unknown",
                    stringr::str_detect(stringr::str_to_lower(.), "azithromycin|axithromycin") ~ "Azithromycin",
                    stringr::str_detect(stringr::str_to_lower(.), "^doxycycline") ~ "Doxycycline",
                    stringr::str_detect(stringr::str_to_lower(.), "^cefixime") ~ "Cefixime",
                    stringr::str_detect(stringr::str_to_lower(.), "^ceftriaxone") ~ "Ceftriaxone",
                    stringr::str_detect(stringr::str_to_lower(.), "^gentamicin|^gentamycin") ~ "Gentamycin",
                    stringr::str_detect(stringr::str_to_lower(.), "^cefadroxil") ~ "Cefadroxil",
                    stringr::str_detect(stringr::str_to_lower(.), "^clindamycin") ~ "Clindamycin",
                    stringr::str_detect(stringr::str_to_lower(.), "^cipro") ~ "Cipro",
                    stringr::str_detect(stringr::str_to_lower(.), "^emtricitabine") ~ "Emtricitabine",
                    stringr::str_detect(stringr::str_to_lower(.), "^macrobid") ~ "Macrobid",
                    stringr::str_detect(stringr::str_to_lower(.), "^vibramycin") ~ "Vibramycin",
                    stringr::str_detect(stringr::str_to_lower(.), "^tenofovir") ~ "Tenofovir",
                    stringr::str_detect(stringr::str_to_lower(.), "^cefoxitin") ~ "Other",
                    stringr::str_detect(stringr::str_to_lower(.), "^vibramycin") ~ "Vibramycin",
                    stringr::str_detect(stringr::str_to_lower(.), "levaquin") ~ "Levaquin",
                    .default = .)),

             ab2 = dplyr::if_else(ab1 == ab2, "Unknown", ab2),
             interm = dplyr::if_else(ab2 != "Unknown", paste(ab1, ab2, sep = ","), ab1)) |>
      tidyr::separate_wider_delim(interm, names = c("ab12", "ab22"), delim = ",", too_few = "align_start", too_many = "error") |>
      dplyr::mutate(
        !!sym(output) := dplyr::case_when( #combine like drugs that are in a different order
          is.na(ab22) ~ ab12,
          stringr::str_trim(ab12) < stringr::str_trim(ab22) ~ paste0(stringr::str_trim(ab12), ", ", stringr::str_trim(ab22)),
          stringr::str_trim(ab12) > stringr::str_trim(ab22) ~ paste0(stringr::str_trim(ab22), ", ", stringr::str_trim(ab12)))) |>
      dplyr::select(-ab1, -ab2, -ab12, -ab22)

    return(syph)

  } else if (!disease %in% c("chlamydia", "gonorrhea", "syphilis")){
    stop("This function can be used for chlamydia, gonorrhea, or syphilis data. Please specify the intended disease.")
  }

}
