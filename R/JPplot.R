JPplot <- function(JPresult){
  result.df <- JPresult[[1]]

  plot(result.df$grade, result.df$ns, type = "l",
       xlab = "Grade", ylab = "Liklihood")
  points(result.df$grade, result.df$ns)
  lines(result.df$grade, result.df$s2, type = "l", col = "red")
  points(result.df$grade, result.df$s2, col = "red")
  lines(result.df$grade, result.df$s3, type = "l", col = "blue")
  points(result.df$grade, result.df$s3, col = "blue")
  legend("bottomright", lty = c("solid", "solid", "solid"),
         col = c("black", "red", "blue"),
         legend = c("ns", "s2", "s3"))
}
