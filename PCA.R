library("tidyverse")


data <- read.csv("C:\\Users\\User\\2017_financial index_163 comp.csv",header=T)
library(data.table)
#移出千位的,
data[c('op_profit_growth_rate', 'current_ratio','quick_rartio')] <- lapply(data[c('op_profit_growth_rate', 'current_ratio','quick_rartio')], function(x) as.numeric(gsub(",", "", x)))
a=data[, 2:ncol(data)]#remove ID


#敘述統計量
summary(data[,2:ncol(data)])
#ROE為負代表賠錢，MAX37是美募得一塊錢的股權就能賺0.37元
#PMR MIN 每銷售一塊賠兩塊
#debt_ratio max代表一間公司的資產有94%都是負債買來的
cor(a)
#計算彼此的相關係數

pca.model <- prcomp(a, scale = T)#=T是做資料標準化
names(pca.model)

var.exp <- tibble(
  pc = paste0("PC_", formatC(1:16, width=2, flag="0")),
  var = pca.model$sdev^2,
  prop = (pca.model$sdev)^2 / sum((pca.model$sdev)^2),
  cum_prop = cumsum((pca.model$sdev)^2 / sum((pca.model$sdev)^2)))

head(var.exp)

library(plotly)
plot_ly(
  x = var.exp$pc,
  y = var.exp$var,
  type = "bar"
) %>%
  layout(
    title = "Variance Explained by Each Principal Component",
    xaxis = list(type = 'Principal Component', tickangle = -60),
    yaxis = list(title = 'Variance'),
    margin = list(r = 30, t = 50, b = 70, l = 50)
  )

plot_ly(
  x = var.exp$pc,
  y = var.exp$cum_prop,
  type = "bar"
) %>%
  layout(
    title = "Cumulative Proportion by Each Principal Component",
    xaxis = list(type = 'Principal Component', tickangle = -60),
    yaxis = list(title = 'Proportion'),
    margin = list(r = 30, t = 50, b = 70, l = 50)
  )

#結論：取前六個主成分就能到80%
#主成分的系列矩陣
head(pca.model$rotation, 5)
ggplot(melt(pca.model$rotation[, 1:6]), aes(Var2, Var1)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient2(low = "firebrick4", high = "steelblue",
                       mid = "white", midpoint = 0) +
  guides(fill=guide_legend(title="Coefficient")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_blank())
#不希望一個主成分什麼都講，也不希望正負在一起很難表達方向性，有點難解釋，改採用非負稀疏主成分分析

set.seed(1234)
library(nsprcomp)
nspca.model <- nscumcomp(
  data[, 2:17], 
  k = 90, nneg = T,#T代表出來的細數都非負
  scale. = T)

var.exp <- tibble(
  pc = paste0("PC_", formatC(1:16, width=2, flag="0")),
  var = nspca.model$sdev^2,
  prop = (nspca.model$sdev)^2 / sum((nspca.model$sdev)^2),
  cum_prop = cumsum((nspca.model$sdev)^2 / sum((nspca.model$sdev)^2)))
#8個主成分到80%


library(plotly)

plot_ly(
  x = var.exp$pc,
  y = var.exp$var,
  type = "bar"
) %>%
  layout(
    title = "Variance Explained by Each Principal Component",
    xaxis = list(type = 'Principal Component', tickangle = -60),
    yaxis = list(title = 'Variance'),#
    margin = list(r = 30, t = 50, b = 70, l = 50)
  )

plot_ly(
  x = var.exp$pc,
  y = var.exp$cum_prop,
  type = "bar"
) %>%
  layout(
    title = "Cumulative Proportion by Each Principal Component",
    xaxis = list(type = 'Principal Component', tickangle = -60),
    yaxis = list(title = 'Proportion'),#
    margin = list(r = 30, t = 50, b = 70, l = 50)
  )


ggplot(melt(nspca.model$rotation[, 1:8]), aes(Var2, Var1)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient2(low = "white", high = "steelblue") +
  guides(fill=guide_legend(title="Coefficient")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_blank())
#主成份 1 重點為「股東權益獲利與成長能力」
#主成份 2 重點為「資產獲利能力」
#主成份 3 重點為「毛利與週轉率」


#公司個別分析,繪製「主成份分數」與「該主成份係數最大變數」的散佈圖
nspca.score <- data.frame(nspca.model$x)
row.names(nspca.score) <-data$comp_id

#縱軸股東報酬率 橫軸主成分分析分數
#6684特別突出 6291特別差
plot_ly(
  x = nspca.score[, 1],
  y = data$roe,
  text = data$comp_id,
  type = "scatter",
  mode = "markers"
) %>% layout(
  title = "ROE v.s. PC 1 Score: Scatter Plot",
  xaxis = list(title = 'Principal Component 1'),
  yaxis = list(title = 'Return on Equity'),
  margin = list(r = 30, t = 50, b = 70, l = 50)
)


#3529
plot_ly(
  x = nspca.score[, 2],
  y = nspca.score[, 3],
  text = data$comp_id,
  type = "scatter",
  mode = "markers"
) %>% layout(
  title = "PC 2 v.s. PC 3 Score: Scatter Plot",
  xaxis = list(title = 'Principal Component 2'),
  yaxis = list(title = 'Principal Component 3'),
  margin = list(r = 30, t = 50, b = 70, l = 50)
)