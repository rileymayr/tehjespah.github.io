simpleEffectsAOV <- function(omnibus.aov,simple.aov){
  # get important values from omnibus:
  omnibus_mse <- omnibus.aov$anova_table$MSE[1]
  omnibus_df <- omnibus.aov$anova_table$'den Df'[1]
  omnibus_ss_error <- omnibus_mse * omnibus_df
  
  # get important values from simple effects aov
  simple_f <- simple.aov$anova_table$F
  simple_df <- simple.aov$anova_table$`num Df`
  simple_mse <- simple.aov$anova_table$MSE[1]
  simple_ms <- simple_mse*simple_f
  simple_ss <- simple_ms * simple_df
  
  # get corrected F
  corrected_f <- simple_ms/omnibus_mse
  
  # get corrected P
  p.value <- (1-pf(corrected_f,df1 = simple_df,df2 = omnibus_df))
  
  p.value <- ifelse(p.value>.001,round(p.value,3),"<.001")
  
  # get simple effects pes:
  pes <- (simple_ss/(simple_ss+omnibus_ss_error))
  
  output <- paste0("F(",simple_df, ",", omnibus_df,") = ", round(corrected_f,digits = 3), ", p=", p.value, ", pes=", round(pes,3))
  
  cbind(row.names(simple.aov[[1]]),omnibus_mse,output)
} 