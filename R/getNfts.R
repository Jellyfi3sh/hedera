
#' @title getNfts
#' @description
#' Returns metadata about the instances of a given NFT, identified by token ID.
#'    Can optionally filter by Hedera account ID and/or NFT serial number.
#' @param limit Integer. Optional. The maximum number of results to return. 
#'    Defaults to 100.
#' @param order Character. Optional. 'asc' will sort the results in ascending
#'   order of serial number; 'desc' will sort the results in descending order of
#'   serial number. Defaults to 'asc'.
#' @param network Character. Optional. The network to connect to. Defaults to
#'    'testnet'.
#' @param asDf Logical. Optional. If TRUE, the function will return the results
#'   as a data frame. Defaults to FALSE, which will return the results as a list.
#' @export
#' 
getNfts <- function(tokenId,
                    accountId = NULL,
                    serialNo = NULL,
                    limit = 100,
                    order = c("asc", "desc")[1],
                    network = c("mainnet", "previewnet", "testnet")[3],
                    asDf = FALSE) {

  # Determine network base URL.
  baseUrl <- getNetworkBaseUrl(network)

  # Initialise variables.
  lsRes <- NULL
  bCont <- TRUE

  q <- sprintf("%sapi/v1/tokens/%s/nfts?limit=%s&order=%s%s%s",
               baseUrl, tokenId, limit, order,
               ifelse (!is.null(accountId), sprintf("&account.id=%s", accountId), ""),
               ifelse (!is.null(serialNo), sprintf("&serialnumber=%s", serialNo), ""))

  # Continue while there are more pages to read.
  while(bCont) {

    res <- httr::GET(url = q)

    if(!res$status_code == 200) {
      res <- httr::content(res, as = "parsed")
      stop("Failed to retrieve NFTs for token %s. Error: %s",
           tokenId, res)
    }

    res <- httr::content(res, as = "parsed")

    if(length(res$nfts) == 0) {
      break
    }

    lsRes <- c(lsRes, res$nfts)

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

  if(length(lsRes) == 0) {
    return(NULL)
  }

  if(!asDf) {
    return(lsRes)
  }

  lsRes <- lapply(X = lsRes, FUN = function(x) {

    x <- lapply(X = x, FUN = function(y) {
      if(length(y) == 0) {
        return(NA_character_)
      }
      return(y)
    })

    return(data.frame(x))

  })

  return(do.call("rbind", lsRes))
}
