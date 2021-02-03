ls()
rm(list = ls())
library(mgcv)
library(ggplot2)
library(Rmisc)
library("cowplot")
windowsFonts(TNM = windowsFont("Times New Roman"))
## Files position
input_Total=('D:/WorkFile/Biodiversity/Final Files/Oringinal Data/1. toal amphibian/3. PatternData/')
input_Th=('D:/WorkFile/Biodiversity/Final Files/Oringinal Data/2. threatened amphibian/3.PatternData/')
input=c(input_Total,input_Th)

## out file postition
out='D:/WorkFile/Biodiversity/Final Files/Graphics'


# 
globalfile=paste(out,'/','GlobalPatter.tif',sep="")
tiff(filename = globalfile,compression="lzw",height=1250,width=2500,res = 300 )
## Total Files Name
filepath_Global=c(paste(input[1],'global.txt',sep = ""))
## immport data
data=read.table(filepath_Global,header = T,sep = ",")

p1=ggplot(data,aes(data$ISP,data$mean))+geom_point(size = 1.5)+
  xlab("Urban-rural Gradient")+ylab("Total Amphibian Species")+
  geom_smooth(data=data,method = "gam",formula = y~s(x))+
  scale_y_continuous(limits=c(min(data$mean),max(data$mean)))+
  theme(axis.title.x =element_text(size=14, family='TNM'), plot.margin=unit(x=c(0.2,0.2,0.2,0.2),units="inches"),
        axis.title.y=element_text(size=14, family='TNM'),axis.text=element_text(size=14,color = "black", family='TNM')
        ,panel.background = element_rect(fill="white"),
        panel.border =element_rect(fill = NA, colour = "black"),axis.ticks = element_line(colour = "black"))

## Threatened Files Name
filepath_Global=c(paste(input[2],'global.txt',sep = ""))
## immport data
data2=read.table(filepath_Global,header = T,sep = ",")



p2=ggplot(data2,aes(data2$ISP,data2$mean))+geom_point(size = 1.5)+
  xlab("Urban-rural Gradient")+ylab("Threatened Amphibian Species")+
  geom_smooth(data=data2,method = "gam",formula = y~s(x))+
  scale_y_continuous(limits=c(min(data2$mean),max(data2$mean)))+
  theme(axis.title.x =element_text(size=14, family='TNM'), plot.margin=unit(x=c(0.2,0.2,0.2,0.2),units="inches"),
        axis.title.y=element_text(size=14, family='TNM'),axis.text=element_text(size=14,color = "black", family='TNM')
        ,panel.background = element_rect(fill="white"),
        panel.border =element_rect(fill = NA, colour = "black"),axis.ticks = element_line(colour = "black"))
print(plot_grid(p1,p2,labels = c("(a)", "(b)"),
          label_fontfamily ='TNM',
          ncol = 2, nrow = 1)
)
multiplot(p1,p2, cols=2)
dev.off()
## Climte zone
## filepath
filepath_Climate=character()
labely=c('Total Amphibian Species','Threatened Amphibian Species')
colnum=seq(4,3)
rownum=2
for (i in 1:2) {
  
  if(i==1){
    filepath_Climate=c(paste(input[i],'Arctic_amp&ISP.txt',sep = ""),
                                          paste(input[i],'Boreal_amp&ISP.txt',sep = ""),
                                          paste(input[i],'Cooltemperate_amp&ISP.txt',sep = ""),
                                          paste(input[i],'Warmtemperate_amp&ISP.txt',sep = ""),
                                          paste(input[i],'Subtropical_amp&ISP.txt',sep = ""),
                                          paste(input[i],'Drylands_amp&ISP.txt',sep = ""),
                                          paste(input[i],'Tropical_amp&ISP.txt',sep = ""))
  
    # plot figure
    ## out file postition
    
    Figfile=paste(out,'/','ClimatePatter_Total.tif',sep="")
    
    ## immport data
    data1=read.table(filepath_Climate[1],header = T,sep = ",")
    # result1<-gam(data1$mean ~ s(data1$ISP))
    # summary(result1)
    # plot(result1,se=T,resid=T,pch=16)
    p1=ggplot(data1,aes(data1$ISP,data1$mean))+geom_point(size = 1.5)+
      xlab("Urban-rural Gradient")+ylab(labely[i])+
      geom_smooth(data=data1,method = "gam",formula = y~s(x))+
      scale_y_continuous(limits=c(min(data1$mean),max(data1$mean)))+
      theme(axis.title.x =element_text(size=14, family='TNM'), plot.margin=unit(x=c(0.2,0.2,0.2,0.2),units="inches"),
            axis.title.y=element_text(size=14, family='TNM'),axis.text=element_text(size=14,color = "black", family='TNM')
            ,panel.background = element_rect(fill="white"),
            panel.border =element_rect(fill = NA, colour = "black"),axis.ticks = element_line(colour = "black"))
    
    data2=read.table(filepath_Climate[2],header = T,sep = ",")

    p2=ggplot(data2,aes(data2$ISP,data2$mean))+geom_point(size = 1.5)+
      xlab("Urban-rural Gradient")+ylab(labely[i])+
      geom_smooth(data=data2,method = "gam",formula = y~s(x))+
      scale_y_continuous(limits=c(min(data2$mean),max(data2$mean)))+
      theme(axis.title.x =element_text(size=14, family='TNM'), plot.margin=unit(x=c(0.2,0.2,0.2,0.2),units="inches"),
            axis.title.y=element_text(size=14, family='TNM'),axis.text=element_text(size=14,color = "black",family='TNM')
            ,panel.background = element_rect(fill="white"),
            panel.border =element_rect(fill = NA, colour = "black"),axis.ticks = element_line(colour = "black"))
    data3=read.table(filepath_Climate[3],header = T,sep = ",")

    p3=ggplot(data3,aes(data3$ISP,data3$mean))+geom_point(size = 1.5)+
      xlab("Urban-rural Gradient")+ylab(labely[i])+
      geom_smooth(data=data3,method = "gam",formula = y~s(x))+
      scale_y_continuous(limits=c(min(data3$mean),max(data3$mean)))+
      theme(axis.title.x =element_text(size=14, family='TNM'), plot.margin=unit(x=c(0.2,0.2,0.2,0.2),units="inches"),
            axis.title.y=element_text(size=14, family='TNM'),axis.text=element_text(size=14,color = "black", family='TNM')
            ,panel.background = element_rect(fill="white"),
            panel.border =element_rect(fill = NA, colour = "black"),axis.ticks = element_line(colour = "black"))

    data4=read.table(filepath_Climate[4],header = T,sep = ",")

    p4=ggplot(data4,aes(data4$ISP,data4$mean))+geom_point(size = 1.5)+
      xlab("Urban-rural Gradient")+ylab(labely[i])+
      geom_smooth(data=data4,method = "gam",formula = y~s(x))+
      scale_y_continuous(limits=c(min(data4$mean),max(data4$mean)))+
      theme(axis.title.x =element_text(size=14, family='TNM'), plot.margin=unit(x=c(0.2,0.2,0.2,0.2),units="inches"),
            axis.title.y=element_text(size=14, family='TNM'),axis.text=element_text(size=14,color = "black", family='TNM')
            ,panel.background = element_rect(fill="white"),
            panel.border =element_rect(fill = NA, colour = "black"),axis.ticks = element_line(colour = "black"))

    data5=read.table(filepath_Climate[5],header = T,sep = ",")

    p5=ggplot(data5,aes(data5$ISP,data5$mean))+geom_point(size = 1.5)+
      xlab("Urban-rural Gradient")+ylab(labely[i])+
      geom_smooth(data=data5,method = "gam",formula = y~s(x))+
      scale_y_continuous(limits=c(min(data5$mean),max(data5$mean)))+
      theme(axis.title.x =element_text(size=14, family='TNM'), plot.margin=unit(x=c(0.2,0.2,0.2,0.2),units="inches"),
            axis.title.y=element_text(size=14, family='TNM'),axis.text=element_text(size=14,color = "black", family='TNM')
            ,panel.background = element_rect(fill="white"),
            panel.border =element_rect(fill = NA, colour = "black"),axis.ticks = element_line(colour = "black"))

    data6=read.table(filepath_Climate[6],header = T,sep = ",")

    p6=ggplot(data6,aes(data6$ISP,data6$mean))+geom_point(size = 1.5)+
      xlab("Urban-rural Gradient")+ylab(labely[i])+
      geom_smooth(data=data6,method = "gam",formula = y~s(x))+
      scale_y_continuous(limits=c(min(data6$mean),max(data6$mean)))+
      theme(axis.title.x =element_text(size=14, family='TNM'), plot.margin=unit(x=c(0.2,0.2,0.2,0.2),units="inches"),
            axis.title.y=element_text(size=14, family='TNM'),axis.text=element_text(size=14,color = "black", family='TNM')
            ,panel.background = element_rect(fill="white"),
            panel.border =element_rect(fill = NA, colour = "black"),axis.ticks = element_line(colour = "black"))

    data7=read.table(filepath_Climate[7],header = T,sep = ",")

    p7=ggplot(data7,aes(data7$ISP,data7$mean))+geom_point(size = 1.5)+
      xlab("Urban-rural Gradient")+ylab(labely[i])+
      geom_smooth(data=data7,method = "gam",formula = y~s(x))+
      scale_y_continuous(limits=c(min(data7$mean),max(data7$mean)))+
      theme(axis.title.x =element_text(size=14, family='TNM'), plot.margin=unit(x=c(0.2,0.2,0.2,0.2),units="inches"),
            axis.title.y=element_text(size=14, family='TNM'),axis.text=element_text(size=14,color = "black", family='TNM')
            ,panel.background = element_rect(fill="white"),
            panel.border =element_rect(fill = NA, colour = "black"),axis.ticks = element_line(colour = "black"))

    tiff(filename = Figfile,compression="lzw",height=2500,width=5000,res = 300 )

    print(plot_grid(p1,p2,p3,p4,p5,p6,p7,
                    labels = c("(a)", "(b)", "(c)","(d)","(e)","(f)","(g)"),
                    label_fontfamily ='TNM',
                    ncol = colnum[i], nrow = rownum))
    dev.off()
    rm(data1,data2,data3,data4,data5,data6,data7)
  }
  if(i==2){
    filepath_Climate=c(
      paste(input[i],'Boreal_amp&ISP.txt',sep = ""),
      paste(input[i],'Cooltemperate_amp&ISP.txt',sep = ""),
      paste(input[i],'Warmtemperate_amp&ISP.txt',sep = ""),
      paste(input[i],'Subtropical_amp&ISP.txt',sep = ""),
      paste(input[i],'Drylands_amp&ISP.txt',sep = ""),
      paste(input[i],'Tropical_amp&ISP.txt',sep = ""))
    # plot figure
    ## out file postition
    
    Figfile=paste(out,'/','ClimatePatter_Threatened.tif',sep="")
    
    ## immport data
    data1=read.table(filepath_Climate[1],header = T,sep = ",")
    
    p1=ggplot(data1,aes(data1$ISP,data1$mean))+geom_point(size = 1.5)+
      xlab("Urban-rural Gradient")+ylab(labely[i])+
      geom_smooth(data=data1,method = "gam",formula = y~s(x),colour="red")+
      scale_y_continuous(limits=c(min(data1$mean-0.01),max(data1$mean)))+
      theme(axis.title.x =element_text(size=14, family='TNM'), plot.margin=unit(x=c(0.2,0.2,0.2,0.2),units="inches"),
            axis.title.y=element_text(size=14, family='TNM'),axis.text=element_text(size=14,color = "black", family='TNM')
            ,panel.background = element_rect(fill="white"),
            panel.border =element_rect(fill = NA, colour = "black"),axis.ticks = element_line(colour = "black"))
    
    data2=read.table(filepath_Climate[2],header = T,sep = ",")
    
    p2=ggplot(data2,aes(data2$ISP,data2$mean))+geom_point(size = 1.5)+
      xlab("Urban-rural Gradient")+ylab(labely[i])+
      geom_smooth(data=data2,method = "gam",formula = y~s(x))+
      scale_y_continuous(limits=c(min(data2$mean-0.02),max(data2$mean)))+
      theme(axis.title.x =element_text(size=14, family='TNM'), plot.margin=unit(x=c(0.2,0.2,0.2,0.2),units="inches"),
            axis.title.y=element_text(size=14, family='TNM'),axis.text=element_text(size=14,color = "black", family='TNM')
            ,panel.background = element_rect(fill="white"),
            panel.border =element_rect(fill = NA, colour = "black"),axis.ticks = element_line(colour = "black"))
    data3=read.table(filepath_Climate[3],header = T,sep = ",")
    
    p3=ggplot(data3,aes(data3$ISP,data3$mean))+geom_point(size = 1.5)+
      xlab("Urban-rural Gradient")+ylab(labely[i])+
      geom_smooth(data=data3,method = "gam",formula = y~s(x))+
      scale_y_continuous(limits=c(min(data3$mean),max(data3$mean)))+
      theme(axis.title.x =element_text(size=14, family='TNM'), plot.margin=unit(x=c(0.2,0.2,0.2,0.2),units="inches"),
            axis.title.y=element_text(size=14, family='TNM'),axis.text=element_text(size=14,color = "black", family='TNM')
            ,panel.background = element_rect(fill="white"),
            panel.border =element_rect(fill = NA, colour = "black"),axis.ticks = element_line(colour = "black"))
    
    data4=read.table(filepath_Climate[4],header = T,sep = ",")
    
    p4=ggplot(data4,aes(data4$ISP,data4$mean))+geom_point(size = 1.5)+
      xlab("Urban-rural Gradient")+ylab(labely[i])+
      geom_smooth(data=data4,method = "gam",formula = y~s(x))+
      scale_y_continuous(limits=c(min(data4$mean),max(data4$mean)))+
      theme(axis.title.x =element_text(size=14, family='TNM'), plot.margin=unit(x=c(0.2,0.2,0.2,0.2),units="inches"),
            axis.title.y=element_text(size=14, family='TNM'),axis.text=element_text(size=14,color = "black", family='TNM')
            ,panel.background = element_rect(fill="white"),
            panel.border =element_rect(fill = NA, colour = "black"),axis.ticks = element_line(colour = "black"))
    
    data5=read.table(filepath_Climate[5],header = T,sep = ",")
    
    p5=ggplot(data5,aes(data5$ISP,data5$mean))+geom_point(size = 1.5)+
      xlab("Urban-rural Gradient")+ylab(labely[i])+
      geom_smooth(data=data5,method = "gam",formula = y~s(x),colour="red")+
      scale_y_continuous(limits=c(min(data5$mean),max(data5$mean)))+
      theme(axis.title.x =element_text(size=14, family='TNM'), plot.margin=unit(x=c(0.2,0.2,0.2,0.2),units="inches"),
            axis.title.y=element_text(size=14, family='TNM'),axis.text=element_text(size=14,color = "black", family='TNM')
            ,panel.background = element_rect(fill="white"),
            panel.border =element_rect(fill = NA, colour = "black"),axis.ticks = element_line(colour = "black"))
    
    data6=read.table(filepath_Climate[6],header = T,sep = ",")
    
    p6=ggplot(data6,aes(data6$ISP,data6$mean))+geom_point(size = 1.5)+
      xlab("Urban-rural Gradient")+ylab(labely[i])+
      geom_smooth(data=data6,method = "gam",formula = y~s(x))+
      scale_y_continuous(limits=c(min(data6$mean),max(data6$mean)))+
      theme(axis.title.x =element_text(size=14, family='TNM'), plot.margin=unit(x=c(0.2,0.2,0.2,0.2),units="inches"),
            axis.title.y=element_text(size=14, family='TNM'),axis.text=element_text(size=14,color = "black", family='TNM')
            ,panel.background = element_rect(fill="white"),
            panel.border =element_rect(fill = NA, colour = "black"),axis.ticks = element_line(colour = "black"))
    
    
    tiff(filename = Figfile,compression="lzw",height=2500,width=3750,res = 300 )
    
    print(plot_grid(p1,p2,p3,p4,p5,p6,   
              labels = c("(a)", "(b)", "(c)","(d)","(e)","(f)"),
              label_fontfamily ='TNM', 
              ncol = colnum[i], nrow = rownum))
    dev.off()
    rm(data1,data2,data3,data4,data5,data6)
  }
  
  
}

