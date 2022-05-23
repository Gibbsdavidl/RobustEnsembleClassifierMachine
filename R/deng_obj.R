
# can be quickly applied to data.table

# bin data vectors into 4 levels
data_bin_4 <- function(x) {
  p = quantile(x)
  if (any(table(p) > 1)) {
    # then we have duplicate levels
    p <- unique(p)
  }
  as.numeric(cut(x, breaks=p, include.lowest = T))
}


# bin data vectors into 3 levels
data_bin_3 <- function(x) {
  p = quantile(x, probs = c(0, 0.33333, 0.66666, 1))
  if (any(table(p) > 1)) {
    # then we have duplicate levels
    p <- unique(p)
  }
  as.numeric(cut(x, breaks=p, include.lowest = T))
}


# bin data vectors into 2 levels
data_bin_2 <- function(x) {
  p = quantile(x, probs = c(0, 0.5, 1))
  if (any(table(p) > 1)) {
    # then we have duplicate levels
    p <- unique(p)
  }
  as.numeric(cut(x, breaks=p, include.lowest = T))
}


#' deng_obj  data engineering object (deng)
#' 
#' An object that transforms data into more robust forms.
#' This includes pairs, signature-pairs, quartiles, and ranks
#'
#' @examples
#' # New object
#' ann <- Recm$new("Ann")
#' 
#' @export
Deng <- R6Class("Deng",
                public = list(
                  
                  #' @field what type of engineering to do
                  data_mode = NULL,
                  
                  #' @field if there are signatures to be used
                  signatures = NULL,
                  
                  
                  #' @description Create a new `Recm` object.
                  #' @param name The object is named.
                  #' @return A new `recm` object.
                  initialize = function(data_mode = NULL,
                                        signatures = NULL) {
                    self$data_mode <- data_mode
                    self$signatures <- signatures
                    
                    if (!all(data_mode %in% c('pairs','sigpairs','quartiles','tertiles','binary','ranks','original'))) {
                      print("ERROR:  please choose a valid collection of data modes: ")
                      print('pairs,sigpairs,quartiles,tertiles,binary,ranks,original')
                      stop(paste0('data_mode, ', self$data_mode  ,' wrong value'))
                    }
                    
                  },
                  
                  
                  # data engineering
                  #' @description Data engineering, replaces the object's data.table.
                  data_eng = function(data=NULL) {
                    
                    if (!all(self$data_mode %in% c('pairs','sigpairs','quartiles','tertiles','binary','ranks','original'))) {
                      print("ERROR:  please choose a valid collection of data modes: ")
                      print('pairs,sigpairs,quartiles,tertiles,binary,ranks,original')
                      stop(paste0('data_mode, ', self$data_mode  ,' wrong value'))
                    }
                    
                    rankdat <- NULL
                    pairdat <- NULL
                    newdat <- data.table()
                    
                    if ('original' %in% self$data_mode) {
                      newdat <- cbind(newdat, data)
                    }
                    
                    if ('binary' %in% self$data_mode) {
                      cols <- colnames(data)
                      quartdat <- data[ , (cols) := lapply(.SD, "data_bin_2"), .SDcols = cols]
                      colnames(quartdat) <- sapply(cols, function(a) paste0(a,'_binary',collapse = ''))
                      newdat <- cbind(newdat, quartdat)
                    } 
                    
                    if ('tertiles' %in% self$data_mode) {
                      cols <- colnames(data)
                      quartdat <- data[ , (cols) := lapply(.SD, "data_bin_3"), .SDcols = cols]
                      colnames(quartdat) <- sapply(cols, function(a) paste0(a,'_tertiles',collapse = ''))
                      newdat <- cbind(newdat, quartdat)
                    } 
                    
                    if ('quartiles' %in% self$data_mode) {
                      cols <- colnames(data)
                      quartdat <- data[ , (cols) := lapply(.SD, "data_bin_4"), .SDcols = cols]
                      colnames(quartdat) <- sapply(cols, function(a) paste0(a,'_quartile',collapse = ''))
                      newdat <- cbind(newdat, quartdat)
                    } 

                    if ('ranks' %in% self$data_mode) {
                      cols <- colnames(data)
                      rankdat <- data[ , (cols) := lapply(.SD, "rank", ties.method="min"), .SDcols = cols]
                      colnames(rankdat) <- sapply(cols, function(a) paste0(a,'_ranked',collapse = ''))
                      newdat <- cbind(newdat, rankdat)
                    }
                    
                    if ('pairs' %in% self$data_mode) {
                      # if mode includes 'pairs' then we need to make var-pairs
                      newcol_names <- c()
                      newcol_dat <- list()
                      cols <- colnames(data)
                      for (ci in 1:(length(cols)-1)) {
                        for (cj in (ci+1):length(cols)) {
                          res0 <- as.numeric(data[,.SD,.SDcols=ci] > data[,.SD,.SDcols=cj])
                          this_new_col <- paste0(cols[ci],'_X_', cols[cj])
                          newcol_names <- c(newcol_names, this_new_col)
                          newcol_dat[[this_new_col]] <- res0
                        }
                      }
                      pairdat <- data.table(data.frame(newcol_dat))
                      newdat <- cbind(newdat, pairdat)
                    }
                    
                    if ('sigpairs' %in% self$data_mode) {
                      if (is.null(self$signatures)) {
                        stop("Signatures is null in data eng obj. Please include signatures.")
                      }
                      newcol_names <- c()
                      newcol_dat <- list()
                      sig_names <- names(self$signatures)
                      for (ci in 1:(length(sig_names)-1)) {
                        for (cj in (ci+1):length(sig_names)) {
                          # Now we are looking at a pair of signatures
                          sn1 <- sig_names[ci]
                          sn2 <- sig_names[cj]
                          s1 <- gsub(' ', '_', self$signatures[[sn1]])
                          s2 <- gsub(' ', '_', self$signatures[[sn2]])
                          sig_pair_temp <- list()
                          # for each pair, look at each pair of genes, record gt or lt
                          for (sx1 in s1) {
                            for (sx2 in s2) {
                              res0 <- as.numeric(data[,.SD,.SDcols=sx1] > data[,.SD,.SDcols=sx2])
                              sig_pair_temp[[paste0(sx1,sx2,collapse = '_X_')]] <- res0
                            }
                          }
                          # first we make a df
                          resdf <- data.frame(sig_pair_temp)
                          findf <- apply(resdf, 1, sum, na.rm=T) / ncol(resdf)
                          #
                          this_new_col <- paste0(sig_names[ci],'_X_', sig_names[cj])
                          newcol_names <- c(newcol_names, this_new_col)
                          newcol_dat[[this_new_col]] <- findf
                        }
                      }
                      sigpairs_dat <- data.table(data.frame(newcol_dat))
                      newdat <- cbind(newdat, sigpairs_dat)
                    }
                    
                    return(newdat)
                  } # end data_eng
                ) # end public
)
                  
                  