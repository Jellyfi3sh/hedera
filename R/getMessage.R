
#' @title getMessage
#' @description Retrieves a Hedera message from the network.
#' @param messageId The ID of the message to retrieve, e.g., "1709313529.822677003".
#' @param network Character. Which Hedera network to use. Must be one of 
#'    "mainnet", "previewnet" or "testnet".
#' @export
#' 
getMessage <- function(messageId,
                       network = c("mainnet", "previewnet", "testnet")[3],
                       decode = FALSE) {
  
  baseUrl <- getNetworkBaseUrl(network)
  xURL <- sprintf("%sapi/v1/topics/messages/%s", baseUrl, messageId)
  
  res <- httr::GET(xURL)
  
  if (res$status_code != 200) {
    statCode <- res$status_code
    res <- httr::content(res, as = "parsed")
    stop(sprintf("Failed to retrieve message with id %s. Error: %s (code: %s)", 
                 messageId, res, statCode))
  }
  
  res <- httr::content(x = res, as = "parsed")
  
  if (decode) {
    res <- jsonlite::fromJSON(
      rawToChar(openssl::base64_decode(text = res$message)))
  }
  
  res$message_id <- messageId
  
  return(res)
  
}