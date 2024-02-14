# create expandable content for reactable
row_details <- function(index){
  
  action <- act[index, ] %>% 
    dplyr::select(-Activity)
  
  # make names in actino an h3 header
  hds <- names(action) %>% 
    lapply(htmltools::h3)
  
  ps <- action %>% 
    lapply(htmltools::p)
  
  out <- NULL
  for(i in 1:ncol(action)){
    out <- c(out, hds[i], ps[i])
  }
  
  htmltools::div(out)
  
}