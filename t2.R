# ---------
#  21-4
# ---------
# ---------------------------------------------------------- #

library(plyr)
baseball <- baseball[baseball$year >= 1990, ]
head(baseball)

# ---------------------------------------------------------- #

## 建立函數以計算打擊率
# data為原資料
# boot將傳遞不同組的索引(indices)
# 在單一次傳遞裏,有些橫排的索引會出現幾次
# 有些則完全不會出現
# 平均來說63%的橫排會出現
# boot將重複性地呼叫此函數
bat.avg <- function(data, indices=1:NROW(data), hits="h",
                    at.bats="ab")
      {
         sum(data[indices, hits], na.rm=TRUE) /
         sum(data[indices, at.bats], na.rm=TRUE)
      }

# 用原資料來測試該函數
bat.avg(baseball)

# 開始自助抽樣
# 所用的資料為baseball資料,其將呼叫bat.avg 1,200 次
# 每次會把索引傳遞到函數
avgBoot <- boot(data=baseball, statistic=bat.avg, R=1200, stype="i")

# 顯示對原資料的測量(original),估計值的偏差(bias)和標準誤差
avgBoot

# 顯示信賴區間
boot.ci(avgBoot, conf=.95, type="norm")

# ---------------------------------------------------------- #

ggplot() +
   geom_histogram(aes(x=avgBoot$t), fill="grey", color="grey") +
   geom_vline(xintercept=avgBoot$t0 + c(-1, 1)*2*sqrt(var(avgBoot$t)),
              linetype=2)

# ---------------------------------------------------------- #