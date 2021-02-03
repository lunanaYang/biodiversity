ls()
rm(list = ls())
gc()
mydir="D:/WorkFile/Biodiversity/Data/1. total amphibian/2.Climate"

setwd(mydir)
library(R.matlab)
library(corrplot)
library(lavaan)
library(semPlot)
library(car)

file_out="D:/WorkFile/Biodiversity/Final Files/Tables/Total/Model2"
Fig_out="D:/WorkFile/Biodiversity/Final Files/Graphics"
## 拟合指标的数据框
fitmeasuresA=data.frame(cfi=0, rmsea=0, srmr=0, chisq=0,pvalue=0)
Fig_file=paste(Fig_out,'/',"Total_Final_climate_Model2.pdf",sep = "")

pdf(file = Fig_file)
par(mfrow=c(2,4))
for (i in 1:7) {
  filepath=paste(as.character(i),'_Climate_sample_percent1.mat',sep = "")
  
  data=readMat(filepath)
  data=data.frame(data)
  
  colnames(data)=c("PIS","amp","DEM","NPP","NL","HI","LST","T","P",
                   "FT","WL","GL","WB","SB","CL","EC")
  data=data[,-16]
  normalize=function(x) {
    return ((x - min(x)) / (max(x) - min(x)))
  } 
  model<-'
                      ## Lavaan latan variables
                     
                      UI =~start(0.5)*PIS+start(0.5)*NL
                      ## Lavaan regression
                      
                      amp~ UI+DEM+HI+T+P
                      HI~UI
                      T~UI+DEM
                      P~DEM
                      P~~T
                      UI~~1*UI
          '
  
  dataNOR=as.data.frame(lapply(data, normalize))
  fit=sem(model, data=dataNOR, estimator="MLR",std.lv=T)
  semPaths(object = fit, what = "std", whatLabels = "std",
           style = "ram", layout = "tree3",intercepts = TRUE,edge.label.cex = 2,label.cex=2)
  
  sink(file=paste(file_out,'/',as.character(i),'modelfinal_Model2.txt',sep = ""))
  cat("the model:climate zone ",i, "All","\n")
  fitmeasuresA[i,]=fitMeasures(fit, c("cfi", "rmsea", "srmr","chisq","pvalue"))
  summary(fit, standardize = TRUE, modindices = F, rsquare = T, fit.measures = T)
  sink()
  rm(model,fit,data,dataNOR)
}
dev.off()
## save the firmeasures to excel.

row.names(fitmeasuresA)=c("Arctic","Boreal","Cool temperate","Warm temperate","Sub-tropical",
                          "Drylands","Tropical")
fileoutpath="D:/WorkFile/Biodiversity/Final Files/Tables/TotalClimateFitmeasures_Model2.csv"
write.csv(fitmeasuresA,fileoutpath)
