# Required R packages for Monetary Policy and Inflation Analysis
# 根据 coding.R 中实际使用的 library() 调用整理

required_packages <- c(
  "tidyverse",    # 数据处理与整理（包括 readr）
  "lubridate",    # 日期处理
  "zoo",          # 缺失值线性插值
  "readr",        # 读取 CSV 文件
  "lmtest",       # 回归检验和系数检验
  "sandwich",     # HAC 稳健协方差矩阵
  "car",          # Wald 联合假设检验
  "strucchange",  # Bai-Perron 结构突变分析
  "AER"           # 工具变量回归（ivreg）
)

# 只安装尚未安装的依赖包
missing_packages <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

# 提示依赖检查已完成
message("Required R packages are installed or already available.")
