#install.packages("ggplot2")
#install.packages("ggthemes")
#install.packages("extrafont")
#install.packages("tidyverse")
#install.packages("ggbeeswarm")
#install.packages("reshape")
#install.packages("foreach")
#install.packages("zoo")
#install.packages("smoother")
#install.packages("TTR")
#install.packages(("mltools"))
#install.packages("https://CRAN.R-project.org/package=TTR")
#install.packages("data.table")
library(ggplot2)
library(ggthemes)
library(extrafont)
library(tidyverse)
library(ggbeeswarm)
library(beeswarm)
library(reshape)
library(data.table)

library(ggplot2)
library(reshape2)
library(foreach)
##########Data Cut with Box Plots
###############################################################################################
print ("Starting")

genotype_name = readline(prompt = "Enter the genotype name: ") 
dose = readline(prompt = "Enter the dose with a colon: ")

# this doesn't do anything -- just makes it more intuitive to enter data
temperature = readline(prompt = "Enter the temperature: ")

experimenter = readline(prompt = "Enter the initials of the experimenter: ")

# Different prompts for different sample sizes 
diff_sample_sizes = readline(prompt = "Are the sample sizes different? [Y/N]: ")

if (diff_sample_sizes == "Y") { 
  sample_int_1 = as.integer(readline(prompt = "Enter the sample size for yw/Gal4 (Gal4/+): "))
  sample_int_2 = as.integer(readline(prompt = "Enter the sample size for yw/shi (UAS/+): "))
  sample_int_3 = as.integer(readline(prompt = "Enter the sample_size for Gal4/shi (Gal4/UAS): "))
  sample_size = "diff n" 
  
} else { 
    sample_size = readline (prompt = "Enter the sammple size (n): ") 
    sample_size_int = as.integer(sample_size)
    sample_int_1 = sample_size_int
    sample_int_2 = sample_size_int
    sample_int_3 = sample_size_int
    
  }

temperature = "30°C"

# This is the path name convention followed: genotype_dose_temperature. Example: 
# 315C_6055_30C
fly_information = paste(genotype_name, gsub(":", "", dose), gsub("°", "", temperature), sep = "_")

# Sets the working directory to the folder with the matching genotype and dose. 
setwd(paste("~/Documents/KaunLab/flygram/flygram_analysis/csv_flygram_data", fly_information, sep = "/"))

# Sets the output path 
output_path = ("~/Documents/KaunLab/flygram/flygram_plots/")


# Data Loading and Analysis -----------------------------------------------
# dataframe 1 = yw/Gal4 (Gal4/+)
# dataframe 2 = yw/shi (UAS/+)
# dataframe 3 = Gal4/shi (Gal4/UAS)

data_frame1=read.csv(paste("yw.", fly_information, "_alldata", ".csv", sep = ""), row.names=1)        
data_frame2=read.csv(paste("yw.shi", fly_information, "_alldata", ".csv", sep = ""), row.names=1)      
data_frame3 = read.csv(paste("shi.", fly_information, "_alldata", ".csv", sep = ""), row.names=1)  

colnames(data_frame1) [1:sample_int_1]=paste("yw.", genotype_name, sep = "")
colnames(data_frame2) [1:sample_int_2]="yw.shibire"
colnames(data_frame3) [1:sample_int_3]=paste(genotype_name, ".shibire", sep = "")

data<-cbind(data_frame1, data_frame2, data_frame3)

genotype <- c(rep("A",sample_int_1),rep("B",sample_int_2),rep("C",sample_int_3))
colnames(data)=genotype

# make avg variable
A<-rowMeans(data[,(1:sample_int_1)]) # 1 - 15
B<-rowMeans(data[,(sample_int_1+1):(sample_int_1 + sample_int_2)]) # 16 - 
C<-rowMeans(data[,(sample_int_1 + sample_int_2 + 1):(sample_int_1 + sample_int_2 + sample_int_3)])

avgdata<-data.frame(A=A,B=B,C=C)
avgdata2<-avgdata*100
# make sem variable
x<- function(x) sd(x)/sqrt(length(x))
stderrors <- foreach(i = 1:dim(data)[1], .combine = rbind) %do% c(x(data[i, 1:sample_int_1]), x(data[i, (sample_int_1+1):(sample_int_1 + sample_int_2)]), x(data[i, (sample_int_1 + sample_int_2 +1):(sample_int_1 + sample_int_2 + sample_int_3)]))
se<-c(stderrors[,1],stderrors[,2],stderrors[,3])
se<-se*100

time<-c(rep(seq(10,1190,by=10), 3))

mdata<-melt(avgdata2)
# make graphable matrix
mdata<- data.frame(mdata,time=time, se=se)

# White Plot --------------------------------------------------------------
Plot1<-ggplot(mdata,aes(x=time, y=value, color=variable, group=variable, ymin= 0, ymax = 100, xmin=10, xmax=1300))+
  annotate("rect", xmin=300, xmax=900, ymin=-Inf, ymax=Inf, alpha=0.3, fill="grey")+
  annotate("text", x=600, y=95, label=paste(dose, "Ethanol,", temperature, sep = " "), color= "black", size=11)+
  #annotate("text", x=600, y=90, label=temperature, color= "black", size=11)+
  geom_line(size=2)+
  geom_ribbon(aes(ymin=value - se, ymax=value + se, fill=variable, linetype=NA), alpha=0.4, show.legend = FALSE)+

  scale_alpha(guide = 'none') +

  scale_color_manual(name= "", values=c("#828282", "#a9a9a9", "#663399"), labels = c(bquote(italic(.(paste0('+/', genotype_name)))), expression(italic('shi'^ts*'/+')), bquote(italic('shi'^ts*'\n/')*italic(.(genotype_name))))) +
  scale_fill_manual(name= "", values=c("#828282", "#a9a9a9", "#663399"), labels = c(bquote(italic(.(paste0('+/', genotype_name)))), expression(italic('shi'^ts*'/+')), bquote(italic('shi'^ts*'\n/')*italic(.(genotype_name))))) +


  #
  #c(m, a) to c(m_lower, a_lower, m_uppper, a_upper)
  scale_x_continuous(breaks=seq(0, 1300, 200), expand = c(0, 10, 0, 0))+
  #scale_x_continuous(breaks=seq(0, 1200, 200))+

  #scale_y_continuous(breaks=seq(-90, 40, 20), expand = c(0.075, 0, 0.10, 0))+
  #scale_y_continuous(breaks=seq(0, 100, 20), expand = c(0.1, 0, 0, 0))+
  scale_y_continuous(breaks=seq(0, 100, 20))+

  ylab("Percent Activity")+
  xlab("Time Elapsed (sec)")+
  theme_classic()+
  #margin(t = 0, r = 2, b = 0, l = 0, unit = "pt")+
  theme(axis.title.x = element_text(size=28), axis.title.y= element_text(size=40), axis.ticks.x = element_blank(),axis.ticks.y = element_blank())+

  theme(axis.text.x = element_text(size=28, color="black"), axis.text.y = element_text(size=28, color="black"))+

  #Normally: legend position = c(.75, .15)
  #Exceptions: 60B 70:45 (0.75, 0.75), 25B 70:45 (0.75, 0.75), 99c 70:45 (0.75, 0.75), 308B 70:45 (0.75, 0.75), R58E02 40:75 (0.55, 0.15)
  theme(legend.text = element_text(size = 26), legend.position = c(.75,.15), legend.key.width=unit(2.5,"line"), legend.key = element_rect(colour = "transparent", fill="transparent"),legend.text.align = 0, legend.background = element_rect(colour="transparent", fill= "transparent"))+

  #guides(colour = guide_legend(override.aes = list(size=3)), cex=.80)+

  #theme(legend.background=element_blank()) +

  guides(colour = guide_legend(override.aes = list(size=10))) +
  
  theme(plot.margin = margin(0.4, 0.4, 0.4, 0.4, "cm")) 

  #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black", size = 0.5)) +

  #theme(legend.text.align = 0) +

  #theme(legend.text=element_text(size=24)) +
  #theme(legend.key = element_blank())

  png(paste(output_path, "WhiteBoxLegendPlots_BigFont/", paste("thesis", genotype_name, gsub(":", "", dose), gsub("°", "", temperature), experimenter, sample_size, "white.png", sep = "_"), sep = ""), width=8, height=8, units ="in", res= 120)
plot(Plot1)
dev.off()


# Plot1<-ggplot(mdata,aes(x=time, y=value, color=variable, group=variable, ymin= 0, ymax = 100, xmin=10, xmax=1300))+
#   annotate("rect", xmin=300, xmax=900, ymin=-Inf, ymax=Inf, alpha=0.3, fill="grey")+
#   annotate("text", x=600, y=95, label=paste(dose, "Ethanol", sep = " "), color= "black", size=11)+
#   annotate("text", x=600, y=85, label=temperature, color= "black", size=11)+
#   geom_line(size=2)+
#   geom_ribbon(aes(ymin=value - se, ymax=value + se, fill=variable, linetype=NA), alpha=0.4, show.legend = FALSE)+
# 
#   scale_color_manual(name= "", values=c("#828282", "#a9a9a9", "#663399"), labels = (bquote(italic(.(paste0('+/', genotype_name)))(italic('shi'^ts*'/+'),italic('shi'^ts*'\n/' ~ .(genotype_name))))))+
#   scale_fill_manual(name= "", values=c("#828282", "#a9a9a9", "#663399"), labels = c(bquote(italic(.(paste0('+/', genotype_name)))(italic('shi'^ts*'/+'),italic('shi'^ts*'\n/' ~ .(genotype_name))))))+
#   #
#   #c(m, a) to c(m_lower, a_lower, m_uppper, a_upper)
#   scale_x_continuous(breaks=seq(0, 1300, 200), expand = c(0, 10, 0, 0))+
#   #scale_x_continuous(breaks=seq(0, 1200, 200))+
# 
#   #scale_y_continuous(breaks=seq(-90, 40, 20), expand = c(0.075, 0, 0.10, 0))+
#   #scale_y_continuous(breaks=seq(0, 100, 20), expand = c(0.1, 0, 0, 0))+
#   scale_y_continuous(breaks=seq(0, 100, 20))+
# 
#   ylab("Percent Activity")+
#   xlab("Time Elapsed (sec)")+
#   theme_classic()+
#   #margin(t = 0, r = 2, b = 0, l = 0, unit = "pt")+
#   theme(axis.title.x = element_text(size=26), axis.title.y= element_text(size=26), axis.ticks.x = element_blank(),axis.ticks.y = element_blank())+
#   theme(axis.text.x = element_text(size=26, color="black"), axis.text.y = element_text(size=26, color="black"))+
#   theme(legend.text = element_text(size = 24), legend.position = c(.85,.85), legend.key.width=unit(2.5,"line"), legend.key = element_rect(colour = "transparent", fill="transparent"),legend.text.align = 0, legend.background = element_rect(colour="transparent", fill= "transparent"))+
#   guides(colour = guide_legend(override.aes = list(size=3)), cex=.80)+
# 
# png(paste(output_path, "WhitePlots/", paste(genotype_name, gsub(":", "", dose), gsub("°", "", temperature), experimenter, sample_size, "white.png", sep = "_"), sep = ""), width=8, height=8, units ="in", res= 120)
# plot(Plot1)
# dev.off()
# # Transparent Plot --------------------------------------------------------
# 
# Plot2<-ggplot(mdata,aes(x=time, y=value, color=variable, group=variable, ymin= 0, ymax = 100, xmin=10, xmax=1300))+
#   annotate("rect", xmin=300, xmax=900, ymin=-Inf, ymax=Inf, alpha=0.3, fill="grey")+
#   annotate("text", x=600, y=95, label=paste(dose, "Ethanol", sep = " "), color= "white", size=11)+
#   annotate("text", x=600, y=85, label=temperature, color= "white", size=11)+
#   geom_line(size=2)+
#   geom_ribbon(aes(ymin=value - se, ymax=value + se, fill=variable, linetype=NA), alpha=0.4, show.legend = FALSE)+
# 
#   scale_color_manual(name= "", values=c("#e5e5e5", "#b2b2b2", "#8932D9"), labels = (bquote(italic(.(paste0('+/', genotype_name)))(italic('shi'^ts*'/+'),italic('shi'^ts*'\n/' ~ .(genotype_name))))))+
#   
#   scale_fill_manual(name= "", values=c("#e5e5e5", "#b2b2b2", "#8932D9"), labels = c(bquote(italic(.(paste0('+/', genotype_name)))(italic('shi'^ts*'/+'),italic('shi'^ts*'\n/' ~ .(genotype_name))))))+
#   #c(m, a) to c(m_lower, a_lower, m_uppper, a_upper)
#   scale_x_continuous(breaks=seq(0, 1300, 200), expand = c(0, 10, 0, 0))+
#   #scale_x_continuous(breaks=seq(0, 1200, 200))+
# 
#   #scale_y_continuous(breaks=seq(-90, 40, 20), expand = c(0.075, 0, 0.10, 0))+
#   #scale_y_continuous(breaks=seq(0, 100, 20), expand = c(0.1, 0, 0, 0))+
#   scale_y_continuous(breaks=seq(0, 100, 20))+
# 
#   ylab("Percent Activity")+
#   xlab("Time Elapsed (sec)")+
#   theme_classic()+
#   #margin(t = 0, r = 2, b = 0, l = 0, unit = "pt")+
# 
#   theme(axis.title.x = element_text(size=26, color="white"), axis.title.y= element_text(size=26, color="white"), axis.ticks.x = element_blank(),axis.ticks.y = element_blank())+
#   theme(axis.text.x = element_text(size=26, color="white"), axis.text.y = element_text(size=26, color="white"))+
#   theme(axis.line= element_line(color="white"))+
#   theme(legend.text = element_text(size = 24, color="white"), legend.position = c(.85,.85), legend.key.width=unit(2.5,"line"), legend.key = element_rect(colour = "transparent", fill="transparent"),legend.text.align = 0, legend.background = element_rect(colour="transparent", fill= "transparent"))+
#   guides(colour = guide_legend(override.aes = list(size=3)), cex=.80)+
#   theme(panel.background = element_rect(fill = "transparent", colour = NA), plot.background = element_rect(fill = "transparent", colour = NA))+
# 
#   png(paste(output_path, "TransparentPlots/", paste(genotype_name, gsub(":", "", dose), gsub("°", "", temperature), experimenter, sample_size,"transparent.png", sep = "_"), sep = ""), width=8, height=8, units ="in", bg = "transparent", res= 120)
# plot(Plot2)
# dev.off()
