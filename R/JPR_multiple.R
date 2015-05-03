JPR_multiple <- function(file.df){
  empty.vector <- rep(NA, nrow(filelist))
  result.df <- data.frame(id = empty.vector,
                          text  = empty.vector,
                          max_ns = empty.vector,
                          max_ns_lik = empty.vector,
                          max_s2 = empty.vector,
                          max_s2_lik = empty.vector,
                          max_s3 = empty.vector,
                          max_s3_lik = empty.vector,
                          result = empty.vector)

  for(i in 1:nrow(filelist)){
    temp.result <- JPreadable(as.character(filelist$file_path[i]))

    result.df[i, ] <- c(filelist[i, 1],
                        as.character(filelist[i, 2]),
                        temp.result[[3]][1],
                        round(temp.result[[2]][temp.result[[3]][1], 2], 3),
                        temp.result[[3]][2],
                        round(temp.result[[2]][temp.result[[3]][2], 3], 3),
                        temp.result[[3]][3],
                        round(temp.result[[2]][temp.result[[3]][2], 4], 3),
                        temp.result[[4]])
  }

  return(result.df)
}
