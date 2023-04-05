library(crayon)
print_basis = function(mars){

  num_Bfuncs = length(mars$Bfuncs)
  cat(cyan$bold("Printing mars object basis functions...\n\n"));
  cat(red("B0:\n"));
  cat(cyan(" NULL (intercept)\n"));

  for(i in 2:num_Bfuncs){

    num_products = nrow(data.frame(mars$Bfuncs[i]))
    num = 1
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
