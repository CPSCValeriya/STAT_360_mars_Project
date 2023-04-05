library(crayon)
print_basis = function(mars){

  num_Bfuncs = length(mars$Bfuncs)
  cat(cyan$bold("Printing mars object basis functions...\n\n"));
  cat(red("B0:\n"));
  cat(cyan(" NULL (intercept)\n"));

  for(i in 2:num_Bfuncs){

    num_products = nrow(data.frame(mars$Bfuncs[i]))
    for(j in 1:num_products){

      #Retrieve hinge function information from corresponding Bfuncs
      curr_hinge = mars$Bfuncs[[i]][j,]
      v = curr_hinge[["v"]]
      s = curr_hinge[["s"]]
      t = curr_hinge[["t"]]

      if(s == 1){
        s = "+1";
      }

      #cat(red(names(mars$B[i])))
      #cat(cyan("B",i-1,":",sep="")

      if(j == 1){
        cat(red(names(mars$B[i]),":\n", sep=""));
      }

      cat(cyan(" knot =", round(t,3), "| variable =", v, "| sign =", s,"\n"))

    }

  }

}

print_product = function(mars){


  num_Bfuncs = length(mars$Bfuncs)
  cat(red$bold("Printing mars object product...\n\n"));

  formula = paste0(sprintf("%.2f",mars$coefficients[1]), "+ \n")

  for(i in 2:num_Bfuncs){

    num_products = nrow(data.frame(mars$Bfuncs[i]))

    coeff = mars$coefficients[i];
    if(coeff > 0){
      coeff = paste0("+",sprintf("%.2f",coeff),sep="");
    }else{
      coeff = sprintf("%.2f",coeff);
    }

    formula = paste0(formula, coeff, " * ", sep="");
    for(j in 1:num_products){

      #Retrieve hinge function information from corresponding Bfuncs
      curr_hinge = mars$Bfuncs[[i]][j,]
      v = curr_hinge[["v"]]
      s = curr_hinge[["s"]]
      t = curr_hinge[["t"]]

      if(s == 1){
        s = "+1";
      }

      hinge_string = paste0("h(v = ", v, ", s = ", s,", t = ",round(t,2),")", sep="");

      if(j < num_products){

        formula = paste0(formula,hinge_string," * ",sep="");

      }else{

        if(i != num_Bfuncs){
          formula = paste0(formula,hinge_string," + \n",sep="");
        }else{
          formula = paste0(formula,hinge_string,"\n",sep="");
        }

      }

    }

  }

  cat(red(formula));

}
