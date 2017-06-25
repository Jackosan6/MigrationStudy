### Proxy table adjustment function


# Function to average out proxy values if there are less than 10 observations in a particular category
# The function will average over several adjoining birth cohorts with a combined observation count of 10 or more


#' @name ProxyTableAdjFunction
#' @param ProxyTable Input table of cohort averages
#' @param ThresholdCount
#' @param NumCategoryVars
#' @param NumRows
#' @return A data frame
#' @export

ProxyTableAdjFunction <- function(ProxyTable,ThresholdCount = 7,NumCategoryVars = 3,NumRows = 17){
  ProxyTable[is.na(ProxyTable)] <- 0
  NumEducLevels <- (ncol(ProxyTable)-NumCategoryVars)/2
  x <- NumCategoryVars - 1
  y <- ThresholdCount
  k <- 1
  while (k %in% 1:nrow(ProxyTable)){
    kup <- k - 1 + NumRows
    for (i in 1:NumEducLevels){
      j <- k
      while(j %in% k:(k+NumRows/2)){
        if (ProxyTable[j,x+2*i+1]>=y){
          j <- j + 1
        }else{
          if ((ProxyTable[j,x+2*i+1]+ProxyTable[j+1,x+2*i+1])>=y){
            ProxyTable[j,x+2*i] <- (ProxyTable[j,x+2*i]*ProxyTable[j,x+2*i+1]+
                                      ProxyTable[j+1,x+2*i]*ProxyTable[j+1,x+2*i+1])/
              (ProxyTable[j,x+2*i+1]+
                 ProxyTable[j+1,x+2*i+1])
            ProxyTable[j+1,x+2*i] <- ProxyTable[j,x+2*i]
            j <- j + 2
          }else{
            if ((ProxyTable[j,x+2*i+1]+ProxyTable[j+1,x+2*i+1]+ProxyTable[j+2,x+2*i+1])>=y){
              ProxyTable[j,x+2*i] <- (ProxyTable[j,x+2*i]*ProxyTable[j,x+2*i+1]+
                                        ProxyTable[j+1,x+2*i]*ProxyTable[j+1,x+2*i+1]+
                                        ProxyTable[j+2,x+2*i]*ProxyTable[j+2,x+2*i+1])/
                (ProxyTable[j,x+2*i+1]+
                   ProxyTable[j+1,x+2*i+1]+
                   ProxyTable[j+2,x+2*i+1])
              ProxyTable[j+1,x+2*i] <- ProxyTable[j,x+2*i]
              ProxyTable[j+2,x+2*i] <- ProxyTable[j,x+2*i]
              j <- j + 3
            }else{
              if ((ProxyTable[j,x+2*i+1]+ProxyTable[j+1,x+2*i+1]+ProxyTable[j+2,x+2*i+1]+ProxyTable[j+3,x+2*i+1])>=y){
                ProxyTable[j,x+2*i] <- (ProxyTable[j,x+2*i]*ProxyTable[j,x+2*i+1]+
                                          ProxyTable[j+1,x+2*i]*ProxyTable[j+1,x+2*i+1]+
                                          ProxyTable[j+2,x+2*i]*ProxyTable[j+2,x+2*i+1]+
                                          ProxyTable[j+3,x+2*i]*ProxyTable[j+3,x+2*i+1])/
                  (ProxyTable[j,x+2*i+1]+
                     ProxyTable[j+1,x+2*i+1]+
                     ProxyTable[j+2,x+2*i+1]+
                     ProxyTable[j+3,x+2*i+1])
                ProxyTable[j+1,x+2*i] <- ProxyTable[j,x+2*i]
                ProxyTable[j+2,x+2*i] <- ProxyTable[j,x+2*i]
                ProxyTable[j+3,x+2*i] <- ProxyTable[j,x+2*i]
                j <- j + 4
              }else{
                if ((ProxyTable[j,x+2*i+1]+ProxyTable[j+1,x+2*i+1]+ProxyTable[j+2,x+2*i+1]+ProxyTable[j+3,x+2*i+1]+
                     ProxyTable[j+4,x+2*i+1])>=y){
                  ProxyTable[j,x+2*i] <- (ProxyTable[j,x+2*i]*ProxyTable[j,x+2*i+1]+
                                            ProxyTable[j+1,x+2*i]*ProxyTable[j+1,x+2*i+1]+
                                            ProxyTable[j+2,x+2*i]*ProxyTable[j+2,x+2*i+1]+
                                            ProxyTable[j+3,x+2*i]*ProxyTable[j+3,x+2*i+1]+
                                            ProxyTable[j+4,x+2*i]*ProxyTable[j+4,x+2*i+1])/
                    (ProxyTable[j,x+2*i+1]+
                       ProxyTable[j+1,x+2*i+1]+
                       ProxyTable[j+2,x+2*i+1]+
                       ProxyTable[j+3,x+2*i+1]+
                       ProxyTable[j+4,x+2*i+1])
                  ProxyTable[j+1,x+2*i] <- ProxyTable[j,x+2*i]
                  ProxyTable[j+2,x+2*i] <- ProxyTable[j,x+2*i]
                  ProxyTable[j+3,x+2*i] <- ProxyTable[j,x+2*i]
                  ProxyTable[j+4,x+2*i] <- ProxyTable[j,x+2*i]
                  j <- j + 5
                }else{
                  if ((ProxyTable[j,x+2*i+1]+ProxyTable[j+1,x+2*i+1]+ProxyTable[j+2,x+2*i+1]+ProxyTable[j+3,x+2*i+1]+
                       ProxyTable[j+4,x+2*i+1]+ProxyTable[j+5,x+2*i+1])>=y){
                    ProxyTable[j,x+2*i] <- (ProxyTable[j,x+2*i]*ProxyTable[j,x+2*i+1]+
                                              ProxyTable[j+1,x+2*i]*ProxyTable[j+1,x+2*i+1]+
                                              ProxyTable[j+2,x+2*i]*ProxyTable[j+2,x+2*i+1]+
                                              ProxyTable[j+3,x+2*i]*ProxyTable[j+3,x+2*i+1]+
                                              ProxyTable[j+4,x+2*i]*ProxyTable[j+4,x+2*i+1]+
                                              ProxyTable[j+5,x+2*i]*ProxyTable[j+5,x+2*i+1])/
                      (ProxyTable[j,x+2*i+1]+
                         ProxyTable[j+1,x+2*i+1]+
                         ProxyTable[j+2,x+2*i+1]+
                         ProxyTable[j+3,x+2*i+1]+
                         ProxyTable[j+4,x+2*i+1]+
                         ProxyTable[j+5,x+2*i+1])
                    ProxyTable[j+1,x+2*i] <- ProxyTable[j,x+2*i]
                    ProxyTable[j+2,x+2*i] <- ProxyTable[j,x+2*i]
                    ProxyTable[j+3,x+2*i] <- ProxyTable[j,x+2*i]
                    ProxyTable[j+4,x+2*i] <- ProxyTable[j,x+2*i]
                    ProxyTable[j+5,x+2*i] <- ProxyTable[j,x+2*i]
                    j <- j + 6
                  }else{
                    if ((ProxyTable[j,x+2*i+1]+ProxyTable[j+1,x+2*i+1]+ProxyTable[j+2,x+2*i+1]+ProxyTable[j+3,x+2*i+1]+
                         ProxyTable[j+4,x+2*i+1]+ProxyTable[j+5,x+2*i+1]+ProxyTable[j+6,x+2*i+1])>=y){
                      ProxyTable[j,x+2*i] <- (ProxyTable[j,x+2*i]*ProxyTable[j,x+2*i+1]+
                                                ProxyTable[j+1,x+2*i]*ProxyTable[j+1,x+2*i+1]+
                                                ProxyTable[j+2,x+2*i]*ProxyTable[j+2,x+2*i+1]+
                                                ProxyTable[j+3,x+2*i]*ProxyTable[j+3,x+2*i+1]+
                                                ProxyTable[j+4,x+2*i]*ProxyTable[j+4,x+2*i+1]+
                                                ProxyTable[j+5,x+2*i]*ProxyTable[j+5,x+2*i+1]+
                                                ProxyTable[j+6,x+2*i]*ProxyTable[j+6,x+2*i+1])/
                        (ProxyTable[j,x+2*i+1]+
                           ProxyTable[j+1,x+2*i+1]+
                           ProxyTable[j+2,x+2*i+1]+
                           ProxyTable[j+3,x+2*i+1]+
                           ProxyTable[j+4,x+2*i+1]+
                           ProxyTable[j+5,x+2*i+1]+
                           ProxyTable[j+6,x+2*i+1])
                      ProxyTable[j+1,x+2*i] <- ProxyTable[j,x+2*i]
                      ProxyTable[j+2,x+2*i] <- ProxyTable[j,x+2*i]
                      ProxyTable[j+3,x+2*i] <- ProxyTable[j,x+2*i]
                      ProxyTable[j+4,x+2*i] <- ProxyTable[j,x+2*i]
                      ProxyTable[j+5,x+2*i] <- ProxyTable[j,x+2*i]
                      ProxyTable[j+6,x+2*i] <- ProxyTable[j,x+2*i]
                      j <- j + 7
                    }else{
                      if ((ProxyTable[j,x+2*i+1]+ProxyTable[j+1,x+2*i+1]+ProxyTable[j+2,x+2*i+1]+ProxyTable[j+3,x+2*i+1]+
                           ProxyTable[j+4,x+2*i+1]+ProxyTable[j+5,x+2*i+1]+ProxyTable[j+6,x+2*i+1]+ProxyTable[j+7,x+2*i+1])>=y){
                        ProxyTable[j,x+2*i] <- (ProxyTable[j,x+2*i]*ProxyTable[j,x+2*i+1]+
                                                  ProxyTable[j+1,x+2*i]*ProxyTable[j+1,x+2*i+1]+
                                                  ProxyTable[j+2,x+2*i]*ProxyTable[j+2,x+2*i+1]+
                                                  ProxyTable[j+3,x+2*i]*ProxyTable[j+3,x+2*i+1]+
                                                  ProxyTable[j+4,x+2*i]*ProxyTable[j+4,x+2*i+1]+
                                                  ProxyTable[j+5,x+2*i]*ProxyTable[j+5,x+2*i+1]+
                                                  ProxyTable[j+6,x+2*i]*ProxyTable[j+5,x+2*i+1]+
                                                  ProxyTable[j+7,x+2*i]*ProxyTable[j+5,x+2*i+1])/
                          (ProxyTable[j,x+2*i+1]+
                             ProxyTable[j+1,x+2*i+1]+
                             ProxyTable[j+2,x+2*i+1]+
                             ProxyTable[j+3,x+2*i+1]+
                             ProxyTable[j+4,x+2*i+1]+
                             ProxyTable[j+5,x+2*i+1]+
                             ProxyTable[j+6,x+2*i+1]+
                             ProxyTable[j+7,x+2*i+1])
                        ProxyTable[j+1,x+2*i] <- ProxyTable[j,x+2*i]
                        ProxyTable[j+2,x+2*i] <- ProxyTable[j,x+2*i]
                        ProxyTable[j+3,x+2*i] <- ProxyTable[j,x+2*i]
                        ProxyTable[j+4,x+2*i] <- ProxyTable[j,x+2*i]
                        ProxyTable[j+5,x+2*i] <- ProxyTable[j,x+2*i]
                        ProxyTable[j+6,x+2*i] <- ProxyTable[j,x+2*i]
                        ProxyTable[j+7,x+2*i] <- ProxyTable[j,x+2*i]
                        j <- j + 8
                      }else{
                        ProxyTable[j,x+2*i] <- (ProxyTable[j,x+2*i]*ProxyTable[j,x+2*i+1]+
                                                  ProxyTable[j+1,x+2*i]*ProxyTable[j+1,x+2*i+1]+
                                                  ProxyTable[j+2,x+2*i]*ProxyTable[j+2,x+2*i+1]+
                                                  ProxyTable[j+3,x+2*i]*ProxyTable[j+3,x+2*i+1]+
                                                  ProxyTable[j+4,x+2*i]*ProxyTable[j+4,x+2*i+1]+
                                                  ProxyTable[j+5,x+2*i]*ProxyTable[j+5,x+2*i+1]+
                                                  ProxyTable[j+6,x+2*i]*ProxyTable[j+5,x+2*i+1]+
                                                  ProxyTable[j+7,x+2*i]*ProxyTable[j+5,x+2*i+1]+
                                                  ProxyTable[j+8,x+2*i]*ProxyTable[j+5,x+2*i+1])/
                          (ProxyTable[j,x+2*i+1]+
                             ProxyTable[j+1,x+2*i+1]+
                             ProxyTable[j+2,x+2*i+1]+
                             ProxyTable[j+3,x+2*i+1]+
                             ProxyTable[j+4,x+2*i+1]+
                             ProxyTable[j+5,x+2*i+1]+
                             ProxyTable[j+6,x+2*i+1]+
                             ProxyTable[j+7,x+2*i+1]+
                             ProxyTable[j+8,x+2*i+1])
                        ProxyTable[j+1,x+2*i] <- ProxyTable[j,x+2*i]
                        ProxyTable[j+2,x+2*i] <- ProxyTable[j,x+2*i]
                        ProxyTable[j+3,x+2*i] <- ProxyTable[j,x+2*i]
                        ProxyTable[j+4,x+2*i] <- ProxyTable[j,x+2*i]
                        ProxyTable[j+5,x+2*i] <- ProxyTable[j,x+2*i]
                        ProxyTable[j+6,x+2*i] <- ProxyTable[j,x+2*i]
                        ProxyTable[j+7,x+2*i] <- ProxyTable[j,x+2*i]
                        ProxyTable[j+8,x+2*i] <- ProxyTable[j,x+2*i]
                        j <- j + 9
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
      j <- kup
      while(j %in% (kup-NumRows/2):kup){
        if (ProxyTable[j,x+2*i+1]>=y){
          j <- j - 1
        }else{
          if((ProxyTable[j,x+2*i+1]+ProxyTable[j-1,x+2*i+1])>=y){
            ProxyTable[j,x+2*i] <- (ProxyTable[j,x+2*i]*ProxyTable[j,x+2*i+1]+
                                              ProxyTable[j-1,x+2*i]*ProxyTable[j-1,x+2*i+1])/
              (ProxyTable[j,x+2*i+1]+
                 ProxyTable[j-1,x+2*i+1])
            ProxyTable[j-1,x+2*i] <- ProxyTable[j,x+2*i]
            j <- j - 2
          }else{
            if((ProxyTable[j,x+2*i+1]+ProxyTable[j-1,x+2*i+1]+ProxyTable[j-2,x+2*i+1])>=y){
              ProxyTable[j,x+2*i] <- (ProxyTable[j,x+2*i]*ProxyTable[j,x+2*i+1]+
                                                ProxyTable[j-1,x+2*i]*ProxyTable[j-1,x+2*i+1]+
                                                ProxyTable[j-2,x+2*i]*ProxyTable[j-2,x+2*i+1])/
                (ProxyTable[j,x+2*i+1]+
                   ProxyTable[j-1,x+2*i+1]+
                   ProxyTable[j-2,x+2*i+1])
              ProxyTable[j-1,x+2*i] <- ProxyTable[j,x+2*i]
              ProxyTable[j-2,x+2*i] <- ProxyTable[j,x+2*i]
              j <- j - 3
            }else{
              if((ProxyTable[j,x+2*i+1]+ProxyTable[j-1,x+2*i+1]+ProxyTable[j-2,x+2*i+1]+
                  ProxyTable[j-3,x+2*i+1])>=y){
                ProxyTable[j,x+2*i] <- (ProxyTable[j,x+2*i]*ProxyTable[j,x+2*i+1]+
                                                  ProxyTable[j-1,x+2*i]*ProxyTable[j-1,x+2*i+1]+
                                                  ProxyTable[j-2,x+2*i]*ProxyTable[j-2,x+2*i+1]+
                                                  ProxyTable[j-3,x+2*i]*ProxyTable[j-3,x+2*i+1])/
                  (ProxyTable[j,x+2*i+1]+
                     ProxyTable[j-1,x+2*i+1]+
                     ProxyTable[j-2,x+2*i+1]+
                     ProxyTable[j-3,x+2*i+1])
                ProxyTable[j-1,x+2*i] <- ProxyTable[j,x+2*i]
                ProxyTable[j-2,x+2*i] <- ProxyTable[j,x+2*i]
                ProxyTable[j-3,x+2*i] <- ProxyTable[j,x+2*i]
                j <- j - 4
              }else{
                if((ProxyTable[j,x+2*i+1]+ProxyTable[j-1,x+2*i+1]+ProxyTable[j-2,x+2*i+1]+
                    ProxyTable[j-3,x+2*i+1]+ProxyTable[j-4,x+2*i+1])>=y){
                  ProxyTable[j,x+2*i] <- (ProxyTable[j,x+2*i]*ProxyTable[j,x+2*i+1]+
                                                    ProxyTable[j-1,x+2*i]*ProxyTable[j-1,x+2*i+1]+
                                                    ProxyTable[j-2,x+2*i]*ProxyTable[j-2,x+2*i+1]+
                                                    ProxyTable[j-3,x+2*i]*ProxyTable[j-3,x+2*i+1]+
                                                    ProxyTable[j-4,x+2*i]*ProxyTable[j-4,x+2*i+1])/
                    (ProxyTable[j,x+2*i+1]+
                       ProxyTable[j-1,x+2*i+1]+
                       ProxyTable[j-2,x+2*i+1]+
                       ProxyTable[j-3,x+2*i+1]+
                       ProxyTable[j-4,x+2*i+1])

                  ProxyTable[j-1,x+2*i] <- ProxyTable[j,x+2*i]
                  ProxyTable[j-2,x+2*i] <- ProxyTable[j,x+2*i]
                  ProxyTable[j-3,x+2*i] <- ProxyTable[j,x+2*i]
                  ProxyTable[j-4,x+2*i] <- ProxyTable[j,x+2*i]
                  j <- j - 5
                }else{
                  if((ProxyTable[j,x+2*i+1]+ProxyTable[j-1,x+2*i+1]+ProxyTable[j-2,x+2*i+1]+
                      ProxyTable[j-3,x+2*i+1]+ProxyTable[j-4,x+2*i+1]+ProxyTable[j-5,x+2*i+1])>=y){
                  ProxyTable[j,x+2*i] <- (ProxyTable[j,x+2*i]*ProxyTable[j,x+2*i+1]+
                                                    ProxyTable[j-1,x+2*i]*ProxyTable[j-1,x+2*i+1]+
                                                    ProxyTable[j-2,x+2*i]*ProxyTable[j-2,x+2*i+1]+
                                                    ProxyTable[j-3,x+2*i]*ProxyTable[j-3,x+2*i+1]+
                                                    ProxyTable[j-4,x+2*i]*ProxyTable[j-4,x+2*i+1]+
                                                    ProxyTable[j-5,x+2*i]*ProxyTable[j-5,x+2*i+1])/
                    (ProxyTable[j,x+2*i+1]+
                       ProxyTable[j-1,x+2*i+1]+
                       ProxyTable[j-2,x+2*i+1]+
                       ProxyTable[j-3,x+2*i+1]+
                       ProxyTable[j-4,x+2*i+1]+
                       ProxyTable[j-5,x+2*i+1])

                  ProxyTable[j-1,x+2*i] <- ProxyTable[j,x+2*i]
                  ProxyTable[j-2,x+2*i] <- ProxyTable[j,x+2*i]
                  ProxyTable[j-3,x+2*i] <- ProxyTable[j,x+2*i]
                  ProxyTable[j-4,x+2*i] <- ProxyTable[j,x+2*i]
                  ProxyTable[j-5,x+2*i] <- ProxyTable[j,x+2*i]
                  j <- j - 6
                }else{
                  if((ProxyTable[j,x+2*i+1]+ProxyTable[j-1,x+2*i+1]+ProxyTable[j-2,x+2*i+1]+
                      ProxyTable[j-3,x+2*i+1]+ProxyTable[j-4,x+2*i+1]+ProxyTable[j-5,x+2*i+1]+
                      ProxyTable[j-6,x+2*i+1])>=y){
                  ProxyTable[j,x+2*i] <- (ProxyTable[j,x+2*i]*ProxyTable[j,x+2*i+1]+
                                            ProxyTable[j-1,x+2*i]*ProxyTable[j-1,x+2*i+1]+
                                            ProxyTable[j-2,x+2*i]*ProxyTable[j-2,x+2*i+1]+
                                            ProxyTable[j-3,x+2*i]*ProxyTable[j-3,x+2*i+1]+
                                            ProxyTable[j-4,x+2*i]*ProxyTable[j-4,x+2*i+1]+
                                            ProxyTable[j-5,x+2*i]*ProxyTable[j-5,x+2*i+1]+
                                            ProxyTable[j-6,x+2*i]*ProxyTable[j-6,x+2*i+1])/
                    (ProxyTable[j,x+2*i+1]+
                       ProxyTable[j-1,x+2*i+1]+
                       ProxyTable[j-2,x+2*i+1]+
                       ProxyTable[j-3,x+2*i+1]+
                       ProxyTable[j-4,x+2*i+1]+
                       ProxyTable[j-5,x+2*i+1]+
                       ProxyTable[j-6,x+2*i+1])

                  ProxyTable[j-1,x+2*i] <- ProxyTable[j,x+2*i]
                  ProxyTable[j-2,x+2*i] <- ProxyTable[j,x+2*i]
                  ProxyTable[j-3,x+2*i] <- ProxyTable[j,x+2*i]
                  ProxyTable[j-4,x+2*i] <- ProxyTable[j,x+2*i]
                  ProxyTable[j-5,x+2*i] <- ProxyTable[j,x+2*i]
                  ProxyTable[j-6,x+2*i] <- ProxyTable[j,x+2*i]
                  j <- j - 7
                }else{
                  if((ProxyTable[j,x+2*i+1]+ProxyTable[j-1,x+2*i+1]+ProxyTable[j-2,x+2*i+1]+
                      ProxyTable[j-3,x+2*i+1]+ProxyTable[j-4,x+2*i+1]+ProxyTable[j-5,x+2*i+1]+
                      ProxyTable[j-6,x+2*i+1]+ProxyTable[j-7,x+2*i+1])>=y){
                  ProxyTable[j,x+2*i] <- (ProxyTable[j,x+2*i]*ProxyTable[j,x+2*i+1]+
                                            ProxyTable[j-1,x+2*i]*ProxyTable[j-1,x+2*i+1]+
                                            ProxyTable[j-2,x+2*i]*ProxyTable[j-2,x+2*i+1]+
                                            ProxyTable[j-3,x+2*i]*ProxyTable[j-3,x+2*i+1]+
                                            ProxyTable[j-4,x+2*i]*ProxyTable[j-4,x+2*i+1]+
                                            ProxyTable[j-5,x+2*i]*ProxyTable[j-5,x+2*i+1]+
                                            ProxyTable[j-6,x+2*i]*ProxyTable[j-6,x+2*i+1]+
                                            ProxyTable[j-7,x+2*i]*ProxyTable[j-7,x+2*i+1])/
                    (ProxyTable[j,x+2*i+1]+
                       ProxyTable[j-1,x+2*i+1]+
                       ProxyTable[j-2,x+2*i+1]+
                       ProxyTable[j-3,x+2*i+1]+
                       ProxyTable[j-4,x+2*i+1]+
                       ProxyTable[j-5,x+2*i+1]+
                       ProxyTable[j-6,x+2*i+1]+
                       ProxyTable[j-7,x+2*i+1])

                  ProxyTable[j-1,x+2*i] <- ProxyTable[j,x+2*i]
                  ProxyTable[j-2,x+2*i] <- ProxyTable[j,x+2*i]
                  ProxyTable[j-3,x+2*i] <- ProxyTable[j,x+2*i]
                  ProxyTable[j-4,x+2*i] <- ProxyTable[j,x+2*i]
                  ProxyTable[j-5,x+2*i] <- ProxyTable[j,x+2*i]
                  ProxyTable[j-6,x+2*i] <- ProxyTable[j,x+2*i]
                  ProxyTable[j-7,x+2*i] <- ProxyTable[j,x+2*i]
                  j <- j - 8
                  }else{
                    if((ProxyTable[j,x+2*i+1]+ProxyTable[j-1,x+2*i+1]+ProxyTable[j-2,x+2*i+1]+
                        ProxyTable[j-3,x+2*i+1]+ProxyTable[j-4,x+2*i+1]+ProxyTable[j-5,x+2*i+1]+
                        ProxyTable[j-6,x+2*i+1]+ProxyTable[j-7,x+2*i+1]+ProxyTable[j-8,x+2*i+1])>=y){
                      ProxyTable[j,x+2*i] <- (ProxyTable[j,x+2*i]*ProxyTable[j,x+2*i+1]+
                                                ProxyTable[j-1,x+2*i]*ProxyTable[j-1,x+2*i+1]+
                                                ProxyTable[j-2,x+2*i]*ProxyTable[j-2,x+2*i+1]+
                                                ProxyTable[j-3,x+2*i]*ProxyTable[j-3,x+2*i+1]+
                                                ProxyTable[j-4,x+2*i]*ProxyTable[j-4,x+2*i+1]+
                                                ProxyTable[j-5,x+2*i]*ProxyTable[j-5,x+2*i+1]+
                                                ProxyTable[j-6,x+2*i]*ProxyTable[j-6,x+2*i+1]+
                                                ProxyTable[j-7,x+2*i]*ProxyTable[j-7,x+2*i+1]+
                                                ProxyTable[j-8,x+2*i]*ProxyTable[j-8,x+2*i+1])/
                        (ProxyTable[j,x+2*i+1]+
                           ProxyTable[j-1,x+2*i+1]+
                           ProxyTable[j-2,x+2*i+1]+
                           ProxyTable[j-3,x+2*i+1]+
                           ProxyTable[j-4,x+2*i+1]+
                           ProxyTable[j-5,x+2*i+1]+
                           ProxyTable[j-6,x+2*i+1]+
                           ProxyTable[j-7,x+2*i+1]+
                           ProxyTable[j-8,x+2*i+1])

                      ProxyTable[j-1,x+2*i] <- ProxyTable[j,x+2*i]
                      ProxyTable[j-2,x+2*i] <- ProxyTable[j,x+2*i]
                      ProxyTable[j-3,x+2*i] <- ProxyTable[j,x+2*i]
                      ProxyTable[j-4,x+2*i] <- ProxyTable[j,x+2*i]
                      ProxyTable[j-5,x+2*i] <- ProxyTable[j,x+2*i]
                      ProxyTable[j-6,x+2*i] <- ProxyTable[j,x+2*i]
                      ProxyTable[j-7,x+2*i] <- ProxyTable[j,x+2*i]
                      ProxyTable[j-8,x+2*i] <- ProxyTable[j,x+2*i]
                      j <- j - 9
                    }else{
                      if((ProxyTable[j,x+2*i+1]+ProxyTable[j-1,x+2*i+1]+ProxyTable[j-2,x+2*i+1]+
                          ProxyTable[j-3,x+2*i+1]+ProxyTable[j-4,x+2*i+1]+ProxyTable[j-5,x+2*i+1]+
                          ProxyTable[j-6,x+2*i+1]+ProxyTable[j-7,x+2*i+1]+ProxyTable[j-8,x+2*i+1]+
                          ProxyTable[j-9,x+2*i+1])>=y){
                        ProxyTable[j,x+2*i] <- (ProxyTable[j,x+2*i]*ProxyTable[j,x+2*i+1]+
                                                  ProxyTable[j-1,x+2*i]*ProxyTable[j-1,x+2*i+1]+
                                                  ProxyTable[j-2,x+2*i]*ProxyTable[j-2,x+2*i+1]+
                                                  ProxyTable[j-3,x+2*i]*ProxyTable[j-3,x+2*i+1]+
                                                  ProxyTable[j-4,x+2*i]*ProxyTable[j-4,x+2*i+1]+
                                                  ProxyTable[j-5,x+2*i]*ProxyTable[j-5,x+2*i+1]+
                                                  ProxyTable[j-6,x+2*i]*ProxyTable[j-6,x+2*i+1]+
                                                  ProxyTable[j-7,x+2*i]*ProxyTable[j-7,x+2*i+1]+
                                                  ProxyTable[j-8,x+2*i]*ProxyTable[j-8,x+2*i+1]+
                                                  ProxyTable[j-9,x+2*i]*ProxyTable[j-9,x+2*i+1])/
                          (ProxyTable[j,x+2*i+1]+
                             ProxyTable[j-1,x+2*i+1]+
                             ProxyTable[j-2,x+2*i+1]+
                             ProxyTable[j-3,x+2*i+1]+
                             ProxyTable[j-4,x+2*i+1]+
                             ProxyTable[j-5,x+2*i+1]+
                             ProxyTable[j-6,x+2*i+1]+
                             ProxyTable[j-7,x+2*i+1]+
                             ProxyTable[j-8,x+2*i+1]+
                             ProxyTable[j-9,x+2*i+1])

                        ProxyTable[j-1,x+2*i] <- ProxyTable[j,x+2*i]
                        ProxyTable[j-2,x+2*i] <- ProxyTable[j,x+2*i]
                        ProxyTable[j-3,x+2*i] <- ProxyTable[j,x+2*i]
                        ProxyTable[j-4,x+2*i] <- ProxyTable[j,x+2*i]
                        ProxyTable[j-5,x+2*i] <- ProxyTable[j,x+2*i]
                        ProxyTable[j-6,x+2*i] <- ProxyTable[j,x+2*i]
                        ProxyTable[j-7,x+2*i] <- ProxyTable[j,x+2*i]
                        ProxyTable[j-8,x+2*i] <- ProxyTable[j,x+2*i]
                        ProxyTable[j-9,x+2*i] <- ProxyTable[j,x+2*i]
                        j <- j - 10
                      }else{
                          ProxyTable[j,x+2*i] <- (ProxyTable[j,x+2*i]*ProxyTable[j,x+2*i+1]+
                                                    ProxyTable[j-1,x+2*i]*ProxyTable[j-1,x+2*i+1]+
                                                    ProxyTable[j-2,x+2*i]*ProxyTable[j-2,x+2*i+1]+
                                                    ProxyTable[j-3,x+2*i]*ProxyTable[j-3,x+2*i+1]+
                                                    ProxyTable[j-4,x+2*i]*ProxyTable[j-4,x+2*i+1]+
                                                    ProxyTable[j-5,x+2*i]*ProxyTable[j-5,x+2*i+1]+
                                                    ProxyTable[j-6,x+2*i]*ProxyTable[j-6,x+2*i+1]+
                                                    ProxyTable[j-7,x+2*i]*ProxyTable[j-7,x+2*i+1]+
                                                    ProxyTable[j-8,x+2*i]*ProxyTable[j-8,x+2*i+1]+
                                                    ProxyTable[j-9,x+2*i]*ProxyTable[j-9,x+2*i+1]+
                                                    ProxyTable[j-10,x+2*i]*ProxyTable[j-10,x+2*i+1])/
                            (ProxyTable[j,x+2*i+1]+
                               ProxyTable[j-1,x+2*i+1]+
                               ProxyTable[j-2,x+2*i+1]+
                               ProxyTable[j-3,x+2*i+1]+
                               ProxyTable[j-4,x+2*i+1]+
                               ProxyTable[j-5,x+2*i+1]+
                               ProxyTable[j-6,x+2*i+1]+
                               ProxyTable[j-7,x+2*i+1]+
                               ProxyTable[j-8,x+2*i+1]+
                               ProxyTable[j-9,x+2*i+1]+
                               ProxyTable[j-10,x+2*i+1])

                          ProxyTable[j-1,x+2*i] <- ProxyTable[j,x+2*i]
                          ProxyTable[j-2,x+2*i] <- ProxyTable[j,x+2*i]
                          ProxyTable[j-3,x+2*i] <- ProxyTable[j,x+2*i]
                          ProxyTable[j-4,x+2*i] <- ProxyTable[j,x+2*i]
                          ProxyTable[j-5,x+2*i] <- ProxyTable[j,x+2*i]
                          ProxyTable[j-6,x+2*i] <- ProxyTable[j,x+2*i]
                          ProxyTable[j-7,x+2*i] <- ProxyTable[j,x+2*i]
                          ProxyTable[j-8,x+2*i] <- ProxyTable[j,x+2*i]
                          ProxyTable[j-9,x+2*i] <- ProxyTable[j,x+2*i]
                          ProxyTable[j-10,x+2*i] <- ProxyTable[j,x+2*i]
                          j <- j - 11
                      }
                    }
                  }
                        }
                    }
                }
              }
            }
          }
        }
      }
    }
    k <- k + NumRows
  }
  return(ProxyTable)
}
