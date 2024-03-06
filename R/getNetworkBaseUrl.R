
#' @title getNetworkBaseUrl
#' @description Returns the mirror node base URL for a given Hedera network.
#' @param network Character. The name of the Hedera network for which to return 
#'    the mirror node base URL. Must be one of "mainnet", "previewnet" or "testnet".
#' @export
#' 
getNetworkBaseUrl <- function(network = c("mainnet", "previewnet", "testnet")[3]) {
  
  return(
    switch(
      network,
      "mainnet" = "https://mainnet-public.mirrornode.hedera.com/",
      "testnet" = "https://testnet.mirrornode.hedera.com/",
      "previewnet" = "https://previewnet.mirrornode.hedera.com/"))
  
}