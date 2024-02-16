#' Contingency table
#'
#' @param ctab_list List of contingency tables
#' @return Prints contingency table(s) for all forecast candidates.
#' @export

print_contingency_table <- function(ctab_list){
  
  contingency_table_all <- NULL
  for(i in 1:length(ctab_list)){
    
    contingency_table_all <- rbind(contingency_table_all,ctab_list[[i]])
    
  }
  contingency_table_all$forecast <- names(ctab_list)
  contingency_table_all <- contingency_table_all[,c(7,1:6)]
  
  cat("CONTINGENCY TABLE\n")
  cat("-----------------\n")
  print(contingency_table_all)
  cat("\n")
  
}
