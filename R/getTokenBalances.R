
#' @title getTokenBalances
#' @description Gets the balances across all associated accounts for a given token.
#'    This represents the token supply distribution across the network.
#' @param tokenId Character. The Hedera ID for the token, e.g., 0.0.24315.
#' @param accountId Character. Optional. The account for which the balance
#'   should be retrieved. If not provided, the balances for all associated accounts 
#'   will be retrieved.
#' @param balanceFilter Character. Optional. The filter to apply to accounts, e.g.,
#'    'gt:1000' will only return the balances for accounts with more than 1000
#'    tokenId tokens. Defaults to 'gt:0'.
#' @param atTimeStamp Numeric. Optional. The timestamp, in seconds.nanoseconds,
#'    for which the balances should be retrieved.
#' @param limit Integer. Optional. The maximum number of results to return. 
#'    Defaults to 100.
#' @param order Character. Optional. 'asc' will sort the accounts in increasing
#'    order of balance; 'desc' will sort the accounts in decreasing order of 
#'    balance. Defaults to 'asc'.
#' @param network Character. Optional. The network to connect to. Defaults to
#'    'testnet'.
#' @param asDf Logical. Optional. If TRUE, the function will return the results
#'   as a data frame. Defaults to FALSE, which will return the results as a list.
#' @return A list of account balances for the given token.
#' @export
#' 
getTokenBalances <- function(tokenId,
                             accountId = NULL,
                             balanceFilter = "gt:0",
                             atTimeStamp = NULL,
                             limit = 100,
                             order = c("asc", "desc")[1],
                             network = c("mainnet", "previewnet", "testnet")[3],
                             asDf = FALSE) {
  
  
  # Determine network base URL.
  baseUrl <- getNetworkBaseUrl(network)
  
  # Initialise variables.
  lsRes <- NULL
  bCont <- TRUE
  
  q <- sprintf("%sapi/v1/tokens/%s/balances?limit=%s&order=%s%s%s%s",
               baseUrl, tokenId, limit, order,
               ifelse(!is.null(accountId), sprintf("&account.id=%s", accountId), ""),
               ifelse(!is.null(atTimeStamp), sprintf("&timestamp=%s", atTimeStamp), ""),
               ifelse(!is.null(balanceFilter), sprintf("&account.balance=%s", balanceFilter), ""))
  
  # Continue while there are more pages to read.
  while(bCont) {
    
    res <- httr::GET(url = q)
    
    if(!res$status_code == 200) {
      res <- httr::content(res, as = "parsed")
      stop("Failed to retrieve balances for token %s. Error: %s",
           tokenId, res)
    }
    
    res <- httr::content(res, as = "parsed")
    
    if(length(res$balances) == 0) {
      break
    }
    
    # Add the timestamp to each result.
    res$balances <- lapply(X = res$balances, FUN = function(x) {
      x$timestamp <- res$timestamp
      return(x)
    })
    
    # Append the results to the list.
    lsRes <- c(lsRes, res$balances)
    
    # Check if there are more results to retrieve.
    bCont <- FALSE
    if("next" %in% names(res$links)) {
      if(length(res$links$`next`) > 0) {
        bCont <- TRUE
        q <- paste0(
          gsub(pattern = ".com/", 
               replacement = ".com", 
               x = baseUrl, 
               fixed = TRUE), 
          res$links$`next`)
      }
    }
    
  }
  
  if (length(lsRes) == 0) {
    return(NULL)
  }
  
  if (!asDf) {
    return(lsRes)
  }
  
  lsRes <- lapply(X = lsRes, FUN = data.frame)
  
  return(do.call("rbind", lsRes))
  
}

