fit <- cor.test(mtcars$mpg, mtcars$qsec)

df_numeric  <- mtcars[, c(1,3:7)]
pairs(df_numeric)
cor(df_numeric)
fit <- corr.test(df_numeric)
?corr.ñalc
res <- numeric(2)

filtered.cor <- function(x)
{
  df <- x[,sapply(x, is.numeric)]
  cr <- NULL
  for (i in 1:ncol(df))
  {
    for (j in 1:ncol(df))
    {
      if (i != j)
      {
        cr <- c(cr, cor.test(df[[i]], df[[j]])$estimate)
      }
    }
  }
  return(cr[which.max(abs(cr))])
}

str(shapiro.test(mtcars$qsec))

smart_cor <- function(x)
{
  a <- shapiro.test(x[[1]])$p.value
  b <- shapiro.test(x[[2]])$p.value
  if (a < 0.05 | b < 0.05)
  {
    return(cor.test(x[[1]], x[[2]], method = "spearman")$estimate)
  }
  return(cor.test(x[[1]], x[[2]])$estimate)
}

smart_cor()