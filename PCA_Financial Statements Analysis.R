library("tidyverse")

library(plotly)

setwd("C:\\Users\\User")

data <- read.csv("2017_financial index_163 comp.csv",header=T)

library(data.table)

#移除千位的逗點

data[c('op_profit_growth_rate', 'current_ratio','quick_rartio')] <- lapply(data[c('op_profit_growth_rate', 'current_ratio','quick_rartio')], function(x) as.numeric(gsub(",", "", x)))

#淨收益佔淨銷售的比率

#Q1

data<-data %>%
  
  mutate(
    
    sales_margin_rate=roa/asset_turnover,
    
    profit_indicator = roa * (1 + profit_margin_rate),
    
    t_roa = exp(roa/10) / (1+exp(roa/10))
    
  )



data %>%
  
  ggplot(aes(x = roa)) +
  
  geom_histogram(bins = 30)



data %>%
  
  ggplot(aes(x = t_roa)) +
  
  geom_histogram(bins = 30)


#t_toa將集中的數據分散開，較易於觀察



#Q2

set.seed(500)

library(nsprcomp)

#為什麼解答這裡是19不是20最後一列不用看嗎~

nspca.model <- nscumcomp(
  
  data[, 2:20], 
  
  k =100, nneg = T,#T代表出來的細數都非負
  
  scale. = T)



var.exp <- tibble(
  
  pc = paste0("PC_", formatC(1:19, width=2, flag="0")),
  
  var = nspca.model$sdev^2,
  
  prop = (nspca.model$sdev)^2 / sum((nspca.model$sdev)^2),
  
  cum_prop = cumsum((nspca.model$sdev)^2 / sum((nspca.model$sdev)^2)))

##  pc       var    prop cum_prop

## 1 PC_01 2.63   0.245      0.245

##2 PC_02 1.19   0.111      0.356

## 3 PC_03 0.978  0.0912     0.448

## 4 PC_04 0.847  0.0790     0.527

## 5 PC_05 0.790  0.0737     0.600

## 6 PC_06 0.740  0.0690     0.669

## 7 PC_07 0.642  0.0599     0.729

## 8 PC_08 0.607  0.0567     0.786

## 9 PC_09 0.430  0.0401     0.826

##10 PC_10 0.381  0.0355     0.862

##11 PC_11 0.268  0.0250     0.886

##12 PC_12 0.237  0.0221     0.909

##13 PC_13 0.230  0.0214     0.930

##14 PC_14 0.206  0.0193     0.949

##15 PC_15 0.192  0.0179     0.967

##16 PC_16 0.148  0.0138     0.981

##17 PC_17 0.142  0.0133     0.994

##18 PC_18 0.0464 0.00432    0.999

##19 PC_19 0.0151 0.00141    1   

#7個主成分到70%



ggplot(melt(nspca.model$rotation[, 1:8]), aes(Var2, Var1)) +
  
  geom_tile(aes(fill = value), colour = "white") +
  
  scale_fill_gradient2(low = "white", high = "steelblue") +
  
  guides(fill=guide_legend(title="Coefficient")) +
  
  theme_bw() +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        
        axis.title = element_blank())

#PC1:sales_margin_rate

#PC2:op_profit_growth_rate





nspca.score <- data.frame(nspca.model$x)

row.names(nspca.score) <-data$comp_id

plot_ly(
  
  x = nspca.score[, 1],
  
  y = nspca.score[, 2],
  
  text = data$comp_id,
  
  type = "scatter",
  
  mode = "markers"
  
) %>% layout(
  
  title = "PC 1 v.s. PC 2 Score: Scatter Plot",
  
  xaxis = list(title = 'Principal Component 1'),
  
  yaxis = list(title = 'Principal Component 2'),
  
  margin = list(r = 30, t = 50, b = 70, l = 50)
  
)

#想請問為什麼在隨機也重置之後，算出的答案依然跟解答不一樣呢？主成分分析是有標準答案的嗎？

#表現最好的為安格6684，今年度因為新品效應年增收翻倍