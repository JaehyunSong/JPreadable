#' @name JPR_multiple
#' @title Measuring Japanese readability (Multiple ver.)
#' @author Jaehyun SONG
#' @description Measuring Japanese readability (Multiple ver.)
#' @docType package
#'
#' @usage JPR_multiple(file.df)
#'
#' @param file.df information of text file. The data frame must have three columns. The First column is id, the second is description, the third is filename(include path)
#' @param result.only Show only results without likihood(default is FALSE)
#' @return data.frame
#'
#' @examples
#' setwd("/User/Username/Documents/")
#' sample.df <- data.frame(id = c(1:3), text  = c("Abe Shinzo", "Kan Naoto", "Ozawa Ichiro"), file_path = c("AS.txt", "KN.txt", "OI.txt"))
#' JPR_multiple(sample.df)
NULL


JPR_multiple <- function(file.df, result.only = FALSE){
  empty.vector <- rep(NA, nrow(file.df))
  if(result.only == FALSE){
    result.df <- data.frame(id = empty.vector,
                            text  = empty.vector,
                            max_ns = empty.vector,
                            max_ns_lik = empty.vector,
                            max_s2 = empty.vector,
                            max_s2_lik = empty.vector,
                            max_s3 = empty.vector,
                            max_s3_lik = empty.vector,
                            result = empty.vector)

    for(i in 1:nrow(file.df)){
      temp.result <- JPreadable(as.character(file.df[i, 3]))

      result.df[i, ] <- c(file.df[i, 1],
                          as.character(file.df[i, 2]),
                          temp.result[[3]][1],
                          round(temp.result[[2]][temp.result[[3]][1], 2], 3),
                          temp.result[[3]][2],
                          round(temp.result[[2]][temp.result[[3]][2], 3], 3),
                          temp.result[[3]][3],
                          round(temp.result[[2]][temp.result[[3]][2], 4], 3),
                          temp.result[[4]])
    }
  }else{
    result.df <- data.frame(id = empty.vector,
                            text  = empty.vector,
                            result = empty.vector)

    for(i in 1:nrow(file.df)){
      temp.result <- JPreadable(as.character(file.df[i, 3]))

      result.df[i, ] <- c(file.df[i, 1],
                          as.character(file.df[i, 2]),
                          temp.result[[4]])
    }
  }

  return(result.df)
}
