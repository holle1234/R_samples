
# Function with default args and basic conditional syntax
loop_func <- function(start=0, stop=1){
  stopifnot(start < stop)
  
  for (i in start:stop) {
    if (i %% 2 == 0){
       print(i)
    }
    else if (i %% 3 == 0){
      print("divisible by 3 but not by 2")
    }
    else{
      print("default behavior")
    }
  }
}

# function to showcase string manipulation
string_func <- function(a, b, sep="-"){
  comb = paste(a, b, sep=sep)
  print(format(comb, width=10, justify="left"))
  print(format(comb, width=10, justify="c"))
  print(format(comb, width=10, justify="right"))
  print(c(toupper(comb), tolower(comb)))
  print(paste("Number of chars in comb ", nchar(comb)))
  print(paste("substring of comb: ", substr(comb, 0, 2)))
  
  cs = unlist(strsplit(comb, split=sep))
  sprintf("comb splitted to %s and %s", "l", "l")
  
  tryCatch(
    expr={
      new_a <- round(as.numeric(a), digits=2)
      print(paste("rounded a from", a, "to", new_a, "by 2 digits"))
    },
    warning=function(e){
      print("try using a numerical as an argument for a!")
    }
    )

}

# function to showcase matrix  manipulation
matrix_func <- function(w, h){
  # matrix could be created first
  m = matrix(data=0, nrow=h, ncol=w)
  diag(m) <- 1
  print(m)
  
  # or the diagonal can create it for you
  m <- diag(2, h, w)
  print(m)
  
  # if you want to fill the matrix with a seq
  m = matrix(data=0, nrow=h, ncol=w)
  diag(m) <- 1:min(dim(m))
  print(m)
  
  # add new row
  m <- rbind(m, c(1:dim(m)[2]))
  print(m)
  
  # add new col
  m <- cbind(m, c(1:dim(m)[1]) * -1)
  print(m)
  
  # select a section of matrix from row to col
  fr <- 1; tr <- 3
  fc <- 1; tc <- dim(m)[2]
  m_sec <- m[fr:tr,fc:tc]
  print(m_sec)
  
  # select all but x rows and all cols
  x <- c(-1, -dim(m)[1])
  m_sec <- m[x,]
  print(m_sec)
  
  # change specific values based on condition
  m[m >= 1] <- -5
  print(m)
  
  # sum function across all rows 
  m_sum = rowSums(m)
  print(m_sum)
  
  # sum function across all cols 
  m_sum = colSums(m)
  print(m_sum)
  
  # check if item is in the matrix
  print(c(-1 %in% m, 1 %in% m))
  
  # difference between dim and lenght
  print(c(dim=dim(m), len=length(m)))
  
  # cbind
  m <- cbind(x1=c(0), x2=c(0:5))
  print(m)
  
  # rbind
  m <- rbind(x1=c(0), x2=c(0:5))
  print(m)
  
  # transpose
  m <- t(m)
  print(m)
  
  # unique (must be a vector to compute)
  uv <- unique(as.vector(m))
  print(uv)
  
  # flatten by converting to array
  fm <- as.vector(m)
  print(fm)
  
}

# random ascii lowercase (unique if n <= 25)
get_rand_ascii <- function(n=1){
  
  # assert min lenght of 1 (one)
  stopifnot(n>0)
  
  # ensure unique values if n <= 25
  rep <- if (n > 25) T else F
  
  s <- sample(letters, size=n, replace=rep)
  return(s)
}


# function to showcase dataclass  manipulation
data_frame_func <- function(w, h){
  
  # creating a dataframe
  df <- data.frame(
      id=c(0:9),
      v1=round(runif(10, min=-100, max=100), 3),
      v2=round(runif(10, min=0, max=1), 3),
      sid=get_rand_ascii(10)
  )
  print(df)
  
  # summary
  print(summary(df))
  
  # head
  print(head(df))
  
  # access row
  row1 <- df[1,]
  print(row1)
  
  # access col
  col1 <- df[dim(df)[2]]
  col2 <- df$sid
  col3 <- df["sid"]
  
  # create new from existing
  new_df <- cbind(col1, col2, col3)
  colnames(new_df) <- c("a","b","c")
  print(new_df)
  
  # conditional filtering
  fdf <- df[df$v1 > 0, ]
  print(fdf)
  
  # exclude columns by index
  fdf <- df[-c(1,2)]
  print(fdf)
    
  # exclude columns with subset
  dfd <- subset(df, select=-c(1,2))
  print(fdf)
  
}

# function to showcase factorial
factor_func <- function(){
  v <- c(get_rand_ascii(10))
  fv <- factor(v)
  print(str(fv))
  
  # get levels
  print(levels(fv))
  
  # set levels manually 
  # there are more levels than we need right now..
  # ..but our data can later have these values
  v <- append(v, "b")
  fv <- factor(v, levels=letters, exclude="b")
  print(str(fv))
  
  # we used exclude argument set to b
  # b should appear in vector and set to NA in factor
  ind <- length(v)
  print(c(v[ind], fv[ind], is.na(fv[ind])))
  
  # change value in factor
  fv[1] <- "a"
  print(fv[1])
  
  # if value is not in levels error occurs
  fv[1] <- "b"
  print(fv[1])

  
}


# loop_func(start=11, stop=10)
# string_func("12.3456", "B", sep="-")
# matrix_func(5, 4)
# data_frame_func()
factor_func()

