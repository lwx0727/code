rm(list = ls())
##################3.28种免疫细胞箱图
load("tcga_meta.Rdata")
load("immuData.Rdata")
table(immuData$ICIcluster)
table(tcga_meta$ICIcluster)



data2=immuData[,c(5,16,18,23,25)]
identical(rownames(data2),rownames(tcga_meta))
data2=cbind(data2,tcga_meta)
data2$Group=ifelse(data2$`Activated dendritic cell`>median(data2$`Activated dendritic cell`),"High","Low")

save(data2,file = "data2.Rdata")

####生存分析
library(survival)
library(survminer)
sfit <- survfit(Surv(time, event) ~ Group,
                data = data2)
ggsurvplot(sfit,pval = T,palette = "jco",surv.median.line = "hv")



#个人版生存分析图
#要自定义legend.lab的话，需要吧group因子化固定顺序，不然是默认字母顺序的
data2$Group=factor(data2$Group,levels = c("High","Low"),ordered = F)
sfit <- survfit(Surv(time, event) ~ Group,
                data = data2)
ggsurvplot(fit=sfit,
           title="",#标题，默认在右上角，也可修改到中间，稍有点麻烦
           legend.title = "Activated dendritic cell",#定义图例的名称
           legend.labs = c("High","Low"),
           legend = c(0.8,0.8),#图例位置
           pval = T, #在图上添加log rank检验的p值
           pval.method = TRUE,#添加p值的检验方法
           #conf.int = TRUE,#添加置信区间
           #risk.table = TRUE, #在图下方添加风险表
           #risk.table.col = "strata", #根据数据分组为风险表添加颜色
           risk.table.y.text = F,#风险表Y轴是否显示分组的名称,F为以线条展示分组
           #linetype = "strata", #改变不同组别的生存曲线的线型
           surv.median.line = "hv", #标注出中位生存时间
           xlab = "Time in months", #x轴标题
           #xlim = c(0,10), #展示x轴的范围
           #break.time.by = 2, #x轴间隔
           linetype=1,#线条类型
           size = 1, #线条大小
           #ggtheme = theme_bw(), #为图形添加网格
           palette = c("orange","blue")#图形颜色风格
) 
dev.copy2pdf(file="./figure/10ActDC生存分析.pdf",height=6,width=6)
