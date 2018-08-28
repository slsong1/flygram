rm(list=ls())

library(car)

test_for_hsc = function(df){
  levene = leveneTest(activity ~ group, data = df)
  p_value = levene[[3]][1]
  return (p_value)
}

get_anova_stats = function(file_path) {
  df = read.csv(file_path, sep=',', header=T)
  l_p_value = test_for_hsc(df)
  if (l_p_value < 0.05) {
    print ("Variances are not homogeneous.")
    print(paste0("Levene's p_value: ", l_p_value))
  }
  else {
    print ("Variances are homogeneous by Levene's test")
    print(paste0("Levene's p_value: ", l_p_value))
  }

  #Do a one-way ANOVA 
  anov = aov(activity ~ group, data = df)
  
  summary = summary(anov)
  p_value = summary(anov)[[1]][["Pr(>F)"]][1]
  print(paste0("ANOVA p_value: ", p_value))
  # Do a post-hoc test
  if (p_value < 0.05) {
    tukey = TukeyHSD(anov)
    tukey_df = as.data.frame(tukey$group)
    print (tukey_df[tukey_df["p adj"] < 0.01,,drop=FALSE])
  }
  else {
    print ("ANOVA p value is not significant.")
  }
  return (df)
  
}
print ("********")
print ("Baseline")
print ("********")
baseline_df = get_anova_stats('~/Documents/KaunLab/flygram/flygram_analysis/processed_flygram_data/109B_baseline.csv')
print ("********")
print ("Ethanol")
print ("********")
ethanol_df = get_anova_stats('~/Documents/KaunLab/flygram/flygram_analysis/processed_flygram_data/109B_ethanol.csv')
print ("********")
print ("Recovery")
print ("********")
recovery_df = get_anova_stats('~/Documents/KaunLab/flygram/flygram_analysis/processed_flygram_data/109B_recovery.csv')

#Make a boxplot of the data
# p = ggplot(baseline_df, aes(x=group, y=activity, color=group)) +geom_boxplot()
# p + geom_jitter(shape=16, position=position_jitter(0.2))

a = ggplot(recovery_df, aes(x=group, y=activity, color=group)) +
  geom_boxplot()
a + geom_jitter(shape=16, position=position_jitter(0.2))

