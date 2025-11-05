##I. Tính toán các giá trị thống kê mô tả
#1. lọc các giá trị số trong bảng
numeric_vars <- sapply(data, is.numeric)
numeric_data <- data[,numeric_vars]
head(numeric_data)
#2. Tính các giá trị thống kê mô tả các biến liên tục
summary_stats <- sapply(numeric_data, function(x){
  c(
    Mean = mean(x),
    SD = sd(x),
    Min = min(x),
    Max = max(x),
    Q1 = quantile(x,0.25),
    Q2 = quantile(x, 0.5),
    Q3 = quantile(x, 0.75)
  )
})

t(as.data.frame(summary_stats))
summary_stats

#3. Lập bảng thống kê số lượng với các biến phân loại
gendertab <-table(data$gender)
racetab <- table(data$race.ethnicity)
parenttab <- table(data$parental.level.of.education)
lunchtab <- table(data$lunch)
testtab <- table(data$test.preparation.course)

frequency_tabs <- list(
  Gender = gendertab,
  Race = racetab,
  Parental_Education = parenttab,
  Lunch = lunchtab,
  Test_Prep = testtab
)



