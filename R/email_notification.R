#' Send email notifications
#'
#' Send emails through the Hennepin County mail API, dependant on HCPH Key Vault access.
#' @param address either a single email address or a vector of multiple addresses.
#' @param subject email subject text
#' @param body email body text
#' @export

#Address can either be a single email address or a vector of multiple addresses
email_notification <- function(address, subject, body){
  
  message("Users must have HCPH Azure Key Vault access to execute this function.")
  
  #Check if httr2 package is installed, install if not
  if (!"httr2" %in% utils::installed.packages()){
    install.packages('httr2')
  }
  
  #Set credentials, urls
  client_id <- dbutils.secrets.get(scope = "PH-PrdMDW-dbw-PrdMDW-kv-secret-scope", key = "PHApplicationId")
  client_secret <- dbutils.secrets.get(scope = "PH-PrdMDW-dbw-PrdMDW-kv-secret-scope", key = "PHAuthenticationSecret")
  tenant_id <- dbutils.secrets.get(scope = "PH-PrdMDW-dbw-PrdMDW-kv-secret-scope", key = "tenantID")
  prod_email_resource_id <- dbutils.secrets.get(scope = "PH-PrdMDW-dbw-PrdMDW-kv-secret-scope", key = "mail-api-id")
  scope <- paste0(prod_email_resource_id, "/.default") 
  
  #Retrieve access token
  access_token <- httr2::request(paste0("https://login.microsoftonline.com/", tenant_id, "/oauth2/v2.0/token")) |>
    httr2::req_method("POST") |>
    httr2::req_headers(`Content-Type` = "application/x-www-form-urlencoded") |> 
    httr2::req_body_form(grant_type = "client_credentials", 
                         client_secret = client_secret,
                         client_id = client_id,
                         scope = scope) |>
    httr2::req_perform() |>
    httr2::resp_body_json() |>
    purrr::pluck("access_token")
  
  #Set mail information
  mail_url <- "https://api.hennepincounty.gov/hced-hennepin-mail-service-api/api/Mail?api-version=v1"
  apim_subscription_key <- dbutils.secrets.get(scope = "PH-PrdMDW-dbw-PrdMDW-kv-secret-scope", key = "apim-key")
  
  #Send email
  for (email in address){
    send_mail <- httr2::request(mail_url) |>
      httr2::req_method("POST") |>
      httr2::req_headers(`Content-Type` = "application/json", Authorization = paste0("Bearer ", access_token), `Ocp-Apim-Subscription-Key` = apim_subscription_key) |>
      httr2::req_body_json(list(To = email, 
                                Subject = subject,
                                body = body,
                                Format = "plain")) |>
      httr2::req_perform() |>
      httr2::resp_body_string()
    
    message(send_mail)
  }
}