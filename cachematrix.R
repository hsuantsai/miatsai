makeCacheMatrix <- function(x = matrix()){
inv <- NULL #將變量初始化，用於緩存反矩陣
set <- function(y) { #創建矩陣y作為輸入值
    x <<- y #將y矩陣分配給x函數
    inv <<- NULL #將反矩陣的值初始化
  }
  get <- function() x #創建get函數，返回先前設定的x矩陣
  setInverse <- function(inverse) inv <<- inverse #將反矩陣作為輸入分配給變量
  getinverse <- function() inv #返回已緩存的反矩陣inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse) #列出可以提取或操作的四個函數
}

cacheSolve <- function(x, ...) { 
  inv <- x$getInverse() #從矩陣當中找出緩存的反矩陣
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv) #檢查是否為反矩陣，如果是的話顯示訊息，並回傳（不重新計算）
  }
  data <- x$get() #提取矩陣
  inv <- solve(data, ...) #回傳任何需計算的反矩陣
  x$setInverse(inv) #緩存逆矩陣
  inv
}
