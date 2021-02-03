ls()
rm(list = ls())
gc()

mydir="D:/WorkFile/Biodiversity/Final Files/Oringinal Data/2. threatened amphibian/1. Global"
setwd(mydir)
library(R.matlab)
library(corrplot)
library(lavaan)
library(semPlot)
library(car)
filepath='GlobalData_sample_percent1.mat'
data=readMat(filepath)
data=data.frame(data)
file_out="D:/WorkFile/Biodiversity/Final Files/Tables/Threatenend/Model2"
Fig_out="D:/WorkFile/Biodiversity/Final Files/Graphics"
colnames(data)=c("PIS","amp","DEM","NPP","NL","HI","LST","T","P",
                 "FT","WL","GL","WB","SB","CL")
Fig_file=paste(Fig_out,'/',"Threatened_Final_Global_Model2.pdf",sep = "")

pdf(file = Fig_file)
normalize=function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
} 
dataNOR=as.data.frame(lapply(data, normalize))
modelG <-'
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
fit <- sem(modelG, data=dataNOR, estimator="MLR",std.lv=T)
sink(file=paste(file_out,'/','modelfinal_Global_Model2.txt',sep = ""))
cat("the model:Global ", "All","\n")
summary(fit, standardize = TRUE, modindices = F, rsquare = TRUE,fit.measures=TRUE)
fitMeasures(fit, c("cfi", "rmsea", "srmr","CFI","chisq","pvalue"))
sink()
semPaths1 = semPaths(object = fit, what = "std", whatLabels = "std", 
                     style = "ram", layout = "tree3",intercepts = TRUE,
                     edge.label.cex = 2,label.cex=2
)
dev.off()                    
