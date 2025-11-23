################################################################################
########## Write Algorithm to Assign Titles and Abstracts to Countries #########
################################################################################

### Load Keywords
keywords <- readxl::read_excel(here::here(data, "keywords/countries.xlsx"), col_names = TRUE)

### Write String-Searching Algorithm
assign_country <- function(title, abstract){
  
  title_country <- rep(0, length(keywords))
  
  ### Count Country Keywords in Titles
  for(i in 1:length(keywords)){
    title_country[i] <- stringi::stri_count_regex(title, keywords[[i]]) %>%
      sum(na.rm = TRUE)}
  
  if(max(title_country) > 0 & length(which(title_country == max(title_country))) == 1){
    return(colnames(keywords[which.is.max(title_country)]))}
  
  abstract_country <- rep(0, length(keywords))
  
  ### Count Country Keywords in Abstracts
  if(max(title_country) == 0 || max(title_country) != 0 & length(which(title_country == max(title_country))) > 1){
  for(i in 1:length(keywords)){
    abstract_country[i] <- stringi::stri_count_regex(abstract, keywords[[i]]) %>%
      sum(na.rm = TRUE)}
    
    if(max(abstract_country) > 0){
      return(colnames(keywords[which.is.max(abstract_country)]))}
    
    if(max(abstract_country) == 0){
      return(NA)}}}

################################################################################

### UNIT TESTS - IF FAILED, ERROR IN CODE!

if(assign_country(title = "China", abstract = "") != "China"){
  stop("ALGORITHM YIELDING INCORRECT OUTPUT!")}

if(assign_country(title = "NA", abstract = "India") != "India"){
  stop("ALGORITHM YIELDING INCORRECT OUTPUT!")}

if(assign_country(title = "China", abstract = "India") != "China"){
  stop("ALGORITHM YIELDING INCORRECT OUTPUT!")}

if(assign_country(title = "China and India", abstract = "Brazil") != "Brazil"){
 stop("ALGORITHM YIELDING INCORRECT OUTPUT!")}

if(assign_country(title = "Democratic Republic of Congo", abstract = "") != "Congo, Dem. Rep."){
  stop("ALGORITHM YIELDING INCORRECT OUTPUT")}

if(assign_country(title = "Dominican-Republic", abstract = "") != "Dominican Republic"){
  stop("ALGORITHM YIELDING INCORRECT OUTPUT")}

