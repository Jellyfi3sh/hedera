
#' @title getNfts
#' @description
#' Returns metadata about the instances of a given NFT, identified by token ID.
#'    Can optionally filter by Hedera account ID and/or NFT serial number.
#'
getNfts <- function(tokenId,
                    accountId = NULL,
                    serialNo = NULL,
                    limit = 100,
                    order = c("asc", "desc")[1],
                    network = c("mainnet", "previewnet", "testnet")[3],
                    asDf = FALSE) {

  # Determine network base URL.
  baseUrl <- switch(
    network,
    "mainnet" = "https://mainnet-public.mirrornode.hedera.com/",
    "testnet" = "https://testnet.mirrornode.hedera.com/",
    "previewnet" = "https://previewnet.mirrornode.hedera.com/")

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

    if(length(res$nfts) > 1) {
      stop("length(res$nfts) > 1") # TODO.
    }

    if(length(res$nfts) == 0) {
      break
    }

    lsRes <- c(lsRes, res$nfts)

    bCont <- FALSE
    if("next" %in% names(res$links)) {
      if(length(res$links$`next`) > 0) {
        bCont <- TRUE
        q <- res$links$`next`
      }
    }

  }

  if (length(lsRes) == 0) {
    return(NULL)
  }

  if (!asDf) {
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
