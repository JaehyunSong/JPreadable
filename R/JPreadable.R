#' @name JPreadable
#' @title Measuring Japanese readability
#' @author Jaehyun SONG
#' @description Measuring Japanese readability
#' @docType package
#'
#' @usage JPreadable(file)
#'
#' @param file a name of text file(include its path)
#' @return List type
#'
#' @examples
#' setwd("/User/Username/Documents/")
#' JPreadble("sample_text.txt")
NULL

JPreadable <- function(file){
  sample.text <- paste(scan(file, character(0)), collapse = "")
  raw.text <- sample.text
  sample.text <- substring(sample.text, 1:nchar(sample.text), 1:nchar(sample.text))

  text.table <- table(sample.text)
  text.df <- data.frame(text.table)
  colnames(text.df) <- c("word", "count")
  text.df$word <- as.character(text.df$word)

  na.vec <- rep(NA, length = nrow(text.df))
  lik.df <- data.frame(lg1 = na.vec, lg2 = na.vec, lg3 = na.vec, lg4 = na.vec, lg5 = na.vec,
                       lg6 = na.vec, lg7 = na.vec, lg8 = na.vec, lg9 = na.vec, lg10 = na.vec,
                       lg11 = na.vec, lg12 = na.vec, lg13 = na.vec)
  for(i in 1:nrow(text.df)){
    if(nrow(dic[dic$word == text.df[i, 1], 3:15]) == 0){
      lik.df[i, ] <- rep(0, length = 13)
    }else{
      lik.df[i, ] <- text.df[i, 2] * dic[dic$word == text.df[i, 1], 3:15]
    }
  }

  lik.vec <- c()
  for(i in 1:13){
    lik.vec[i] <- sum(lik.df[, i])
  }

  result.df <- data.frame(grade = c(1:13), ns = lik.vec, s2 = rep(NA, 13), s3 = rep(NA, 13))
  s2.coef <- lm(ns ~ poly(grade, 2, raw = TRUE), data = result.df)$coefficients
  s3.coef <- lm(ns ~ poly(grade, 3, raw = TRUE), data = result.df)$coefficients

  for(i in 1:13){
    result.df[i, 3] <- s2.coef[1] + s2.coef[2] * result.df[i, 1] + s2.coef[3] * result.df[i, 1]^2
    result.df[i, 4] <- s3.coef[1] + s3.coef[2] * result.df[i, 1] + s3.coef[3] * result.df[i, 1]^2 + s3.coef[4] * result.df[i, 1]^3
  }

  max.lik.vec <- c("ns" = result.df[result.df$ns == max(result.df$ns), 1],
                   "s2" = result.df[result.df$s2 == max(result.df$s2), 1],
                   "s3" = result.df[result.df$s3 == max(result.df$s3), 1])

  T13scale <- median(max.lik.vec)

  result <- list(raw.text,
                 result.df,
                 max.lik.vec,
                 T13scale)

  return(result)
}
