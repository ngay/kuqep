#' Generate a list of KU terms.
#'
#' @param start The starting term entered as text (in quotes), two-digit year, semester character, and term character.
#' @param stop The stopping term entered as text (in quotes), two-digit year, semester character, and term character.
#' @return A sequence of ordered term codes.
#' @export keiser_terms

keiser_terms = function(start,stop){
  #Note: The start and stop must be in quotes
  report = c(start,stop)
  #Create the sequence of semesters and terms
  sem_seq = rep(c("W","S","F"),each = 4)
  term_seq = rep(c("A","B","C","D"),3)
  year_seq = paste(sem_seq, term_seq, sep="")
  #Create the sequence of years, semesters, and terms
  num_terms = (12-match(substr(report,3,4)[1],year_seq)+1) + match(substr(report,3,4)[2],year_seq) + ((as.integer(substr(report,1,2)[2]))-(as.integer(substr(report,1,2)[1]))-1)*12
  loop_length =  ((as.integer(substr(report,1,2)[2]))-(as.integer(substr(report,1,2)[1]))+1)
  loop_year = as.integer(substr(report,1,2)[1])
  start_term = match(substr(report,3,4)[1],year_seq)
  stop_term = 12
  report_seq = NULL
  index = 1
  for (i in 1:loop_length) {
    for (j in start_term:stop_term) {
      report_seq[index] = paste(loop_year,year_seq[j],sep="")
      index = index + 1
    }
    loop_year = loop_year + 1
    start_term = 1
    stop_term = ifelse((num_terms-length(report_seq))<=12,match(substr(report,3,4)[2],year_seq),12)
  }
  report_seq = factor(report_seq,levels=report_seq,ordered=TRUE)
  return(report_seq)
}
