library("tidyverse")


data <- read.csv("C:\\Users\\User\\2017_financial index_163 comp.csv",header=T)
library(data.table)
#���X�d�쪺,
data[c('op_profit_growth_rate', 'current_ratio','quick_rartio')] <- lapply(data[c('op_profit_growth_rate', 'current_ratio','quick_rartio')], function(x) as.numeric(gsub(",", "", x)))
a=data[, 2:ncol(data)]#remove ID


#�ԭz�έp�q
summary(data[,2:ncol(data)])
#ROE���t�N���߿��AMAX37�O���ұo�@���������v�N����0.37��
#PMR MIN �C�P��@���ߨ��
#debt_ratio max�N���@�����q���겣��94%���O�t�ŶR�Ӫ�
cor(a)
#�p�⩼���������Y��

pca.model <- prcomp(a, scale = T)#=T�O����ƼзǤ�
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

#���סG���e���ӥD�����N���80%
#�D�������t�C�x�}
head(pca.model$rotation, 5)
ggplot(melt(pca.model$rotation[, 1:6]), aes(Var2, Var1)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient2(low = "firebrick4", high = "steelblue",
                       mid = "white", midpoint = 0) +
  guides(fill=guide_legend(title="Coefficient")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_blank())
#���Ʊ�@�ӥD�����������A�]���Ʊ楿�t�b�@�_�������F��V�ʡA���I�������A��ĥΫD�t�}���D�������R

set.seed(1234)
library(nsprcomp)
nspca.model <- nscumcomp(
  data[, 2:17], 
  k = 90, nneg = T,#T�N���X�Ӫ��ӼƳ��D�t
  scale. = T)

var.exp <- tibble(
  pc = paste0("PC_", formatC(1:16, width=2, flag="0")),
  var = nspca.model$sdev^2,
  prop = (nspca.model$sdev)^2 / sum((nspca.model$sdev)^2),
  cum_prop = cumsum((nspca.model$sdev)^2 / sum((nspca.model$sdev)^2)))
#8�ӥD������80%


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
#�D���� 1 ���I���u�ѪF�v�q��Q�P������O�v
#�D���� 2 ���I���u�겣��Q��O�v
#�D���� 3 ���I���u��Q�P�g��v�v


#���q�ӧO���R,ø�s�u�D�������ơv�P�u�ӥD�����Y�Ƴ̤j�ܼơv�����G��
nspca.score <- data.frame(nspca.model$x)
row.names(nspca.score) <-data$comp_id

#�a�b�ѪF���S�v ��b�D�������R����
#6684�S�O��X 6291�S�O�t
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