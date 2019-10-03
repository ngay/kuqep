#' Clean Final Grades for the QEP.
#'
#' @param data The dataset to clean.
#' @param loc The dataframe that contains Keiser locations and starting terms.
#' @return A dataset that has: (1) abbreviated campus names, (2) ordered terms, (3) separate variables
#'   for term, semester, and year, (3) no WNAs, (4) class size, (5) attempt number, and
#'   (6) defines success.
#' @import dplyr
#' @export

fnlgrd_clean <- function(data,loc){

  #Replace full campus names with their abbreviations
  data$Campus_old=data$Campus
  data$Campus=loc$Abbreviation[match(data$Campus,loc$Full)]

  #Class-size
  class_size = as.data.frame(table(data$adClassSchedID))
  data$ClassSize = class_size$Freq[match(data$adClassSchedID,class_size$Var1)]

  #Create new columns in the data designating the year, semester, and term
  data$year = as.integer(substr(data$TermCode,2,3))
  data$semester = factor(substr(data$TermCode,4,4),levels=c("W","S","F"), ordered = TRUE)
  data$term = factor(substr(data$TermCode,5,5),levels=c("A","B","C","D"), ordered = TRUE)

  #Create the sequence of years, semesters, and terms
  report_seq = keiser_terms(report[1],report[2])

  #Force the terms in the locations df to respect the order
  loc$Start_Term = factor(loc$Start_Term,ordered = TRUE, levels=report_seq)

  #Create new column that contains the year, semester, and term
  data$termcode_new = factor(substr(data$TermCode,2,5),levels=report_seq, ordered = TRUE)

  #Define levels of grade
  LtrGr = c("A","B","C","D","F","W","WF","I")
  data$LetterGrade = factor(data$LetterGrade,levels=LtrGr,ordered=TRUE)

  #Define Success
  data$Success = factor(ifelse(data$LetterGrade<"C","Successful","Not Successful"), levels=c("Successful","Not Successful"),ordered=TRUE)

  #Attempt Number
  data = data %>%
    arrange(termcode_new) %>%
    group_by(stunum) %>%
    mutate(attempt = row_number())
}
