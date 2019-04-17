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

mround = function(x,base) { 
  base*round(x/base) 
} 


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
    new_df[row, "post"] = (sum(offset_vals)/length(offset_vals))*100
    # average = (sum(vals)/length(vals))*100
  }
  return (new_df)
}


setwd("~/Documents/KaunLab/flygram/flygram_analysis/processed_flygram_data/")
files <- list.files("~/Documents/KaunLab/flygram/flygram_analysis/processed_flygram_data", pattern = ".csv", recursive = TRUE)


diff_df = data.frame(matrix(ncol = 23, nrow = 0))
cn =  c("genotype", "dose", "temp", "c_baseline", "e_baseline", "baseline_diff", "baseline_color", "c_early", "e_early", "early_diff", "early_color", "c_mid", "e_mid", "mid_diff", "mid_color", "c_late", "e_late", "late_diff", "late_color", "c_post", "e_post", "post_diff", "post color")
colnames(diff_df) = cn

for (file in files) { 
  #if (file == "109B_6055_30C/processed_109B_6055_30C.csv") {
  if (file != "299B_7045_30C/processed_299B_7045_30C.csv" & file != "R58E02_6055_30C/processed_R58E02_6055_30C.csv") { 
    
    fly_info = str_split(file, "/")[[1]][1]
    genotype = str_split(fly_info, "_")[[1]][1]
    dose = str_split(fly_info, "_")[[1]][2]
    dose1 = strsplit(dose, "(?<=.{2})", perl = TRUE)[[1]][1]
    dose2 = strsplit(dose, "(?<=.{2})", perl = TRUE)[[1]][2]
    dose_colon = paste(dose1, dose2, sep = ":")
    
    #gets the temperature (e.g. 30C) 
    temp = str_split(fly_info, "_")[[1]][3]
    
    print (fly_info)
    
    df = read.csv(paste("~/Documents/KaunLab/flygram/flygram_analysis/processed_flygram_data/", file, sep = "/"), header = TRUE, sep = ",", check.names=F) 
    
    new_df = process_df(df) 
    
    yw_genotype = new_df[which(new_df$V1 == paste0("yw.", genotype)),2:6]
    yw_shi = new_df[which(new_df$V1 == paste0("yw.shi", genotype)), 2:6]
    
    c_output = c()
    
    controls = (yw_genotype + yw_shi)/2
    print ("control")
    for (col in 1:ncol(controls)){
      a = colMeans(controls[col])
      #print (a)
      c_output[col] = a
    }
    
    e_output = c()
    experimentals = new_df[which(new_df$V1 == paste0("shi.", genotype)), 2:6]
    print ("experimental")
    for (col in 1:ncol(experimentals)){
      a = colMeans(experimentals[col])
      #print (a)
      e_output[col]=a
    }
    
    #For positive difference scores (experimental < control)
    colfunc_blue =  colorRampPalette(c("#e6ecff", "#0040ff"))
    blue_cols = colfunc_blue(15)
    
    #TODO: Fix the scale for red
    # min = - 28
    #For negative difference scores (experimental > control)
    colfunc_red = colorRampPalette(c("#ffe6e6", "#ff0000"))
    red_cols = colfunc_red(15)
    
    diff = c_output - e_output 
    
    genotype_df = data.frame("genotype" = genotype, "dose" = dose, "temp" = temp, "c_baseline" = c_output[1], "e_baseline" = e_output[1], "baseline_diff" = diff[1], "c_early" = c_output[2], "e_early" = e_output[2], "early_diff" = diff[2], "c_mid" = c_output[3], "e_mid" = e_output[3], "mid_diff" = diff[3], "c_late" = c_output[4], "e_late" = e_output[4], "late_diff" = diff[4], "c_post" = c_output[5], "e_post" = e_output[5], "post_diff" = diff[5])
    #For baseline
    if (diff[1] < 0) { 
      genotype_df["baseline_color"] = red_cols[round(abs(mround(diff[1], 2))/2)+1]
    } 
    if (diff[1] > 0){
      genotype_df["baseline_color"] = blue_cols[round(abs(mround(diff[1], 2))/2)+1]
    }
    
    #For early EtOH
    if (diff[2] < 0) { 
      genotype_df["early_color"] = red_cols[round(abs(mround(diff[2], 2))/2)+1]
    } 
    
    if (diff[2] > 0){
      genotype_df["early_color"] = blue_cols[round(abs(mround(diff[2], 2))/2)+1]
    }
    
    #For mid EtOH
    if (diff[3] < 0) { 
      genotype_df["mid_color"] = red_cols[round(abs(mround(diff[3], 2))/2)+1]
    } 
    
    if (diff[3] > 0){
      genotype_df["mid_color"] = blue_cols[round(abs(mround(diff[3], 2))/2)+1]
    }
    
    #For late EtOH
    if (diff[4] < 0) { 
      genotype_df["late_color"] = red_cols[round(abs(mround(diff[4], 2))/2)+1]
    } 
    
    if (diff[4] > 0){
      genotype_df["late_color"] = blue_cols[round(abs(mround(diff[4], 2))/2)+1]
    }
    
    #For Post EtOH
    if (diff[5] < 0) { 
      genotype_df["post_color"] = red_cols[round(abs(mround(diff[5], 2))/2)+1]
    } 
    
    if (diff[5] > 0){
      genotype_df["post_color"] = blue_cols[round(abs(mround(diff[5], 2))/2)+1]
    }
  
    
    diff_df = rbind(diff_df, genotype_df)
  
  } 
}

diff_only_df = diff_df[c("genotype", "dose", "temp", "baseline_diff", "baseline_color", "early_diff", "early_color", "mid_diff", "mid_color", "late_diff", "late_color", "post_diff", "post_color")]

###

#write.csv(diff_df, file = "~/Documents/diff_df.csv")
#write.csv(diff_only_df, file = "~/Documents/diff_only_df.csv")