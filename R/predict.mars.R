#' Predict method for mars Objects
#'
#' @description Implementation of predict method for mars object. Outputs a prediction, obtaining estimated values of the response variable given a different dataset and mars model object. The newdata argument is optional.
#' @title Predict method for mars objects
#' @param object a mars object to use for prediction, usually created by mars
#' @param newdata optional data to use for prediction
#' @return a vector of prediction values
#' @family methods
#' @export
#' @import crayon
#' @examples
#' ## Predict values for iris data
#' m <-mars(Sepal.Length~Sepal.Width+Petal.Length+Petal.Width,data=iris,control=mars.control())
#' predict(m)
predict.mars <- function(object,newdata) {

  cat(cyan$bold("Printing mars object prediction...\n\n"));

  if(missing(newdata) || is.null(newdata)) {

    B <- as.matrix(object$B)

  }else{

    tt <- terms(object$formula,data=newdata)
    tt <- delete.response(tt)
    mf <- model.frame(tt,newdata)
    mt <- attr(mf, "terms")
    X <- model.matrix(mt, mf)[,-1]
    B <- make_B(X, object$Bfuncs)

  }

  beta <- object$coefficients
  drop(B %*% beta) # Performs scalar product

}

#' Make B matrix
#'
#' Create set of basis functions using the Bfuncs list
#'
#' @title Make B matrix
#' @param model a model to create basis functions for
#' @param Bfuncs a list of all basis functions
#' @return The corresponding matrix of basis functions
#' @export
make_B <- function(model, Bfuncs){

  num_Bfuncs = length(Bfuncs)
  B <- data.frame(matrix(1,nrow=nrow(model),ncol=num_Bfuncs))

  for(i in 2:num_Bfuncs){

    num_products = nrow(data.frame(Bfuncs[i]))

    for(j in 1:num_products){

      #Retrieve hinge function information from corresponding Bfuncs
      curr_hinge = Bfuncs[[i]][j,]
      v = curr_hinge[["v"]]
      s = curr_hinge[["s"]]
      t = curr_hinge[["t"]]
      B[,i] = B[,i] * h(model[,v],s,t)

    }

  }

  invisible(as.matrix(B))

}
