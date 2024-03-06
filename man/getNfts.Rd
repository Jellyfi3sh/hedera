% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getNfts.R
\name{getNfts}
\alias{getNfts}
\title{getNfts}
\usage{
getNfts(
  tokenId,
  accountId = NULL,
  serialNo = NULL,
  limit = 100,
  order = c("asc", "desc")[1],
  network = c("mainnet", "previewnet", "testnet")[3],
  asDf = FALSE
)
}
\description{
Returns metadata about the instances of a given NFT, identified by token ID.
   Can optionally filter by Hedera account ID and/or NFT serial number.
}