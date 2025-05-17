# 1.a
multiprint <- function(n, c) {
  print(rep(c, n))
}

# b
ismultiply <- function(a = 1, b = 1, result = 1) {
  return(a * b == result)
}

# c
issquare <- function(x) {
  if (!is.numeric(x) || x < 0) {
    return(FALSE)
  }
  sqrt_x <- sqrt(x)
  return(sqrt_x == floor(sqrt_x))
}

# d.
generatetwice <- function() {
  seen <- integer(0)
  repeat {
    num <- sample(1:10, 1)
    cat("Generated:", num, "\n")
    if (num %in% seen) {
      return(num)
    } else {
      seen <- c(seen, num)
    }
  }
}


# 2.a
getfirstlast <- function(v) {
    return(c(head(v, 1), tail(v, 1)))
}

# b
getrows <- function(df, n) {
  return(head(df, n))
}

# c
getrowbyname <- function(df, name_row) {
  return(df[name_row, , drop = FALSE])
}

# d
getcolumnbyname <- function(df, col_name) {
  return(df[[,col_name]])
}



# 3 创建数据框
set.seed(123) # 设置随机种子以确保结果可重现
df <- data.frame(
  EID = 1:8,
  EXP1 = sample(1:100, 8, replace = TRUE),
  EXP2 = sample(1:100, 8, replace = TRUE),
  EXP3 = sample(1:100, 8, replace = TRUE)
)

# 显示创建的数据框
print("创建的数据框:")
print(df)

# a. 保存为CSV文件并计算平均值
write.csv(df, "numtest.csv", row.names = FALSE)
df_csv <- read.csv("numtest.csv")
print("\na. 从numtest.csv读取的数据:")
print(df_csv)
mean_values <- colMeans(df_csv[,c("EXP1", "EXP2", "EXP3")])
print("EXP1, EXP2和EXP3的平均值:")
print(mean_values)

# b. 使用分号分隔符保存为CSV文件并计算最小值
write.table(df, "numtest1.csv", sep = ";", row.names = FALSE)
df_csv_semicolon <- read.table("numtest1.csv", sep = ";", header = TRUE)
print("\nb. 从numtest1.csv读取的数据(使用分号分隔):")
print(df_csv_semicolon)
min_values <- sapply(df_csv_semicolon[,c("EXP1", "EXP2", "EXP3")], min)
print("EXP1, EXP2和EXP3的最小值:")
print(min_values)

# c. 保存为RDS文件并计算最大值
saveRDS(df, "numtest.rds")
df_rds <- readRDS("numtest.rds")
print("\nc. 从numtest.rds读取的数据:")
print(df_rds)
max_values <- sapply(df_rds[,c("EXP1", "EXP2", "EXP3")], max)
print("EXP1, EXP2和EXP3的最大值:")
print(max_values)

# 4
print("Enter number of rows (M): ")
M <- as.integer(readLines("stdin", 1))
print("\nEnter number of columns (N): ")
N <- as.integer(readLines("stdin", 1))

# Check if M*N is less than or equal to 99
if (M * N > 99) {
  stop("Matrix size exceeds 99 unique values. Please choose smaller M and N.")
}

random_numbers <- sample(1:99, M * N)
mat <- matrix(random_numbers, nrow = M, ncol = N)


saveRDS(mat, file = "matrix.rds")
cat("\nMatrix saved to 'matrix.rds'\n")

mat_loaded <- readRDS("matrix.rds")
cat("Matrix loaded from 'matrix.rds':\n")
print(mat_loaded)

# Find max and min values and their positions
max_val <- max(mat_loaded)
min_val <- min(mat_loaded)

max_pos <- which(mat_loaded == max_val, arr.ind = TRUE)
min_pos <- which(mat_loaded == min_val, arr.ind = TRUE)

cat(sprintf("Maximum value: %d at position (row=%d, col=%d)\n", max_val, max_pos[1], max_pos[2]))
cat(sprintf("Minimum value: %d at position (row=%d, col=%d)\n", min_val, min_pos[1], min_pos[2]))
