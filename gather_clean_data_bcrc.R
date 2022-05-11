# Setup --------------------------------------------------------------------------------------------
library(data.table)
library(lubridate)
library(jsonlite)
library(tidyr)
library(httr)

# Variables ----------------------------------------------------------------------------------------
policy_id <- "c364930bd612f42e14d156e1c5410511e77f64cab8f2367a9df544d1"
time_now <- as_datetime(now())


# Databases ----------------------------------------------------------------------------------------
RAR <- readRDS("data/RAR_bcrc.rds")


# Functions ----------------------------------------------------------------------------------------
extract_num <- function(x) as.numeric(gsub("[^0-9\\-]+","",as.character(x)))

loj <- function (X = NULL, Y = NULL, onCol = NULL) {
  if (truelength(X) == 0 | truelength(Y) == 0) 
    stop("setDT(X) and setDT(Y) first")
  n <- names(Y)
  X[Y, `:=`((n), mget(paste0("i.", n))), on = onCol]
}


# JPG listings -------------------------------------------------------------------------------------
JPG_list <- list()
p <- 1
while (TRUE) {
  api_link <- sprintf("https://server.jpgstoreapis.com/policy/%s/listings?page=%d", policy_id, p)
  X <- data.table(fromJSON(rawToChar(GET(api_link)$content)))
  if (nrow(X) == 0) break
  JPG_list[[p]] <- X
  p <- p + 1
}

JPG <- rbindlist(JPG_list)

JPG[, link           := paste0("https://www.jpg.store/asset/", asset_id)]
JPG[, asset          := display_name]
JPG[, price          := price_lovelace/10**6]
JPG[, sc             := "yes"]
JPG[, market         := "jpg.store"]
JPG[, asset_number   := extract_num(asset)]

JPG <- JPG[, .(asset, asset_number, type = "listing", price, last_offer = NA, sc, market, link)]


# JPG sales ----------------------------------------------------------------------------------------
JPGS_list <- lapply(1:7, function(p) {
  api_link <- sprintf("https://server.jpgstoreapis.com/policy/%s/sales?page=%d", policy_id, p)
  X <- data.table(fromJSON(rawToChar(GET(api_link)$content)))
  return(X)
})

JPGS <- rbindlist(JPGS_list)

JPGS[, asset          := display_name]
JPGS[, price          := price_lovelace/10**6]
JPGS[, market         := "jpg.store"]
JPGS[, asset_number   := extract_num(asset)]
JPGS[, sold_at        := as_datetime(confirmed_at)]
JPGS[, sold_at_hours  := difftime(time_now, sold_at, units = "hours")]
JPGS[, sold_at_days   := difftime(time_now, sold_at, units = "days")]

JPGS <- JPGS[order(-sold_at), .(asset, asset_number, price, sold_at, sold_at_hours,
                                sold_at_days, market)]
JPGS <- JPGS[sold_at_hours <= 24*3]


# Merge markets data -------------------------------------------------------------------------------
# Listings
DT <- copy(JPG)

# Sales
DTS <- copy(JPGS)

# Add data collection timestamp
DT[, data_date := time_now]
DTS[, data_date := time_now]


# Rarity and ranking -------------------------------------------------------------------------------
setDT(DT); setDT(RAR)
loj(DT, RAR, "asset_number")

setDT(DTS); setDT(RAR)
loj(DTS, RAR, "asset_number")

DT[, rank_range := fcase(asset_rank %between% c(1,    10),   "1-10",
                         asset_rank %between% c(11,   100),  "11-100",
                         asset_rank %between% c(101,  1000), "101-1000",
                         asset_rank %between% c(1001, 1500), "1001-2000",
                         asset_rank %between% c(2001, 3000), "2001-3000",
                         asset_rank %between% c(3001, 4000), "3001-4000",
                         asset_rank %between% c(4001, 5000), "4001-5000",
                         asset_rank %between% c(5001, 6000), "5001-6000",
                         asset_rank %between% c(6001, 7000), "6001-7000",
                         asset_rank %between% c(7001, 8000), "7001-8000",
                         asset_rank %between% c(8001, 9000), "8001-9000",
                         asset_rank %between% c(9001, 9999), "9001-9999") %>% 
     factor(levels = c("1-10",
                       "11-100",
                       "101-1000",
                       "1001-2000",
                       "2001-3000",
                       "3001-4000",
                       "4001-5000",
                       "5001-6000",
                       "6001-7000",
                       "7001-8000",
                       "8001-9000",
                       "9001-9999"))]

DTS[, rank_range := fcase(asset_rank %between% c(1,    10),   "1-10",
                          asset_rank %between% c(11,   100),  "11-100",
                          asset_rank %between% c(101,  1000), "101-1000",
                          asset_rank %between% c(1001, 1500), "1001-2000",
                          asset_rank %between% c(2001, 3000), "2001-3000",
                          asset_rank %between% c(3001, 4000), "3001-4000",
                          asset_rank %between% c(4001, 5000), "4001-5000",
                          asset_rank %between% c(5001, 6000), "5001-6000",
                          asset_rank %between% c(6001, 7000), "6001-7000",
                          asset_rank %between% c(7001, 8000), "7001-8000",
                          asset_rank %between% c(8001, 9000), "8001-9000",
                          asset_rank %between% c(9001, 9999), "9001-9999") %>% 
      factor(levels = c("1-10",
                        "11-100",
                        "101-1000",
                        "1001-2000",
                        "2001-3000",
                        "3001-4000",
                        "4001-5000",
                        "5001-6000",
                        "6001-7000",
                        "7001-8000",
                        "8001-9000",
                        "9001-9999"))]


# Remove BRCR for which we do not have the rank/rarity score ---------------------------------------
DT <- DT[!(asset_number %in% c(3097, 7210, 9535, 8114))]
DTS <- DTS[!(asset_number %in% c(3097, 7210, 9535, 8114))]


# Large format -------------------------------------------------------------------------------------
.cols <- names(DT)[names(DT) %like% "background_|earring_|hat_|eyes_|mouth_|clothes_|fur_"]
DTL <- data.table(gather(DT, raw_trait, has_trait, all_of(.cols)))
DTL <- DTL[has_trait == 1]

DTL[, trait_category := strsplit(raw_trait, "_")[[1]][1], 1:nrow(DTL)]
DTL[, trait          := gsub(trait_category, "", raw_trait), 1:nrow(DTL)]
DTL[, trait          := gsub("_", " ", trait), 1:nrow(DTL)]
DTL[, trait          := gsub("^ ", "", trait), 1:nrow(DTL)]

DTL[, trait_category_and_trait := paste0(trait_category, "_", trait)]


# Save ---------------------------------------------------------------------------------------------
saveRDS(DT, file = "data/DT.rds")
saveRDS(DTL, file = "data/DTL.rds")
saveRDS(DTS, file = "data/DTS.rds")


# Database evolution -------------------------------------------------------------------------------
DTE <- copy(DT)
if (file.exists("data/DTE_bcrc.rds")) {
  cat("File data/DTE exists:", file.exists("data/DTE_bcrc.rds"), "\n")
  DTE_old <- readRDS("data/DTE_bcrc.rds")
  DTE <- rbindlist(list(DTE, DTE_old))
  DTE <- DTE[difftime(time_now, data_date, units = "hours") <= 24] # Only retain last 24 hours
}
saveRDS(DTE, file = "data/DTE_bcrc.rds")
