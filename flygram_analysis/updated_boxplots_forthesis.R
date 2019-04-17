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
library(gridExtra)
library(grid)


###############################################################################
# No legend or labels

process_df = function (df) {
  row_names = rownames(df)
  new_df = data.frame(row.names = row_names)
  new_df[[1]] = df[[1]]
 
  for (row in 1:nrow(df)) { 
   
    baseline_vals = df[row, which(colnames(df)=="50.0"):which(colnames(df)=="240.0")]
    early_vals = df[row, which(colnames(df)=="300.0"):which(colnames(df)=="490.0")]
    mid_vals = df[row, which(colnames(df)=="500.0"):which(colnames(df)=="690.0")]
    late_vals = df[row, which(colnames(df)=="700.0"):which(colnames(df)=="890.0")]
    offset_vals = df[row, which(colnames(df)=="940.0"):which(colnames(df)=="1130.0")]
    
    new_df[row, "baseline"] = (sum(baseline_vals)/length(baseline_vals))*100
    new_df[row, "early"] = (sum(early_vals)/length(early_vals))*100
    new_df[row, "mid"] = (sum(mid_vals)/length(mid_vals))*100
    new_df[row, "late"] = (sum(late_vals)/length(late_vals))*100
    new_df[row, "recovery"] = (sum(offset_vals)/length(offset_vals))*100
   # average = (sum(vals)/length(vals))*100
  }
  return (new_df)
}

make_plot = function (fly_information, df, timepoint, timepoint_title, num) {

  #gets the genotype (e.g. 109B)   
  genotype = str_split(fly_information, "_")[[1]][1]
  
  # gets the dose (e.g. 70:45)
  dose = str_split(fly_information, "_")[[1]][2]
  dose1 = strsplit(dose, "(?<=.{2})", perl = TRUE)[[1]][1]
  dose2 = strsplit(dose, "(?<=.{2})", perl = TRUE)[[1]][2]
  dose_colon = paste(dose1, dose2, sep = ":")

  #gets the temperature (e.g. 30C) 
  temp = str_split(fly_information, "_")[[1]][3]

  #creates factors for each genotype
  df[[1]] <- factor(df[[1]], levels = c(paste0("yw.", genotype), paste0("yw.shi", genotype), paste0("shi.", genotype)))
  p = ggplot(df, aes(x=df[[1]], y=(df[[which(colnames(df)==timepoint)]]), color = df[[1]], fill = df[[1]], ymin= 0, ymax = 100)) +
    geom_boxplot(outlier.shape = NA, lwd = 0.7, width= .8, alpha = .5, show.legend=FALSE) +
    geom_jitter(aes(color = df[[1]], fill = df[[1]]), position = position_jitter(width = 0.1), size=4,  show.legend = FALSE, alpha = .8) +

    #scale_x_discrete(labels=c("      ", "", "     ")) +

    #ylab("Percent Activity") +

    scale_alpha(guide = 'none') +

    scale_color_manual(name = "", values=alpha(c("#828282", "#a9a9a9", "#663399"), 0.75), guide = guide_legend(override.aes = list(linetype = c(0, 0, 0))),  labels = c(bquote(italic(.(paste0('+/', genotype)))), expression(italic('shi'^ts*'/+')), bquote(italic('shi'^ts*'\n/')*italic(.(genotype))))) +

    scale_fill_manual(name = "", values=alpha(c("#828282", "#a9a9a9", "#663399"), 0.75), guide = guide_legend(override.aes = list(linetype = c(0, 0, 0))),  labels = c(bquote(italic(.(paste0('+/', genotype)))), expression(italic('shi'^ts*'/+')), bquote(italic('shi'^ts*'\n/')*italic(.(genotype))))) +

    theme(axis.title.x = element_blank(),
          #axis.text.x = element_text(size = 26, colour = "black", margin = margin(t = 10, r = 0, b = 0, l = 0)),
          axis.ticks.x=element_blank(),
          axis.text.x = element_blank(), 
          axis.ticks.y = element_blank(),
          axis.text.y = element_text (size = 26),
          axis.title.y = element_blank(), 
          axis.title=element_text(size=26)) +

    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black", size = 0.5)) +

    theme(plot.margin = margin(0.4, 0.4, 0.4, 0.4, "cm")) 
  
  png(paste("~/Documents/KaunLab/flygram/flygram_analysis/processed_flygram_data/", fly_information, paste0(paste("new", fly_information, timepoint_title, num, sep = "_"), ".png"), sep = "/"), width=2.6, height=6, units = "in", res=220) 

  plot (p)
  
  print (paste("Plotted", timepoint_title, sep = " "))

  dev.off()

}


setwd("~/Documents/KaunLab/flygram/flygram_analysis/processed_flygram_data/")
files <- list.files("~/Documents/KaunLab/flygram/flygram_analysis/processed_flygram_data", pattern = ".csv", recursive = TRUE)

for (file in files) { 
  #if (file == "195B_7045_30C/processed_195B_7045_30C.csv") { 
  
  fly_info = str_split(file, "/")[[1]][1]
  print (fly_info)
  
  df = read.csv(paste("~/Documents/KaunLab/flygram/flygram_analysis/processed_flygram_data/", file, sep = "/"), header = TRUE, sep = ",", check.names=F) 
  
  new_df = process_df(df) 
  #print (new_df)
  make_plot(fly_info, new_df, "baseline", "Baseline", "1")
  make_plot(fly_info, new_df, "early", "Early", "2")
  make_plot(fly_info, new_df, "mid", "Middle", "3")
  make_plot(fly_info, new_df, "late", "Late", "4")
  make_plot(fly_info, new_df, "recovery", "Post", "5")

 
  #}
}