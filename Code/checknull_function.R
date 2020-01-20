check.null <- function(x){
  if(is.null(x)){
    TRUE
  }else if(x != "Categorical"){
    TRUE
  }else{
    FALSE
  }
}



