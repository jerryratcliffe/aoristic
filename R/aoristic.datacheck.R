#' Check aoristic input data
#' 
#' A function that flags datetime errors with aoristic input data. 
#' 
#' The aoristic.df function handles data with Start (or From) and End (or To) datetime objects. Sometimes
#' these data can be messy or inaccurately recorded (such as crime data from some police departments). This
#' function checks for common problems and reports the result in a column labeled 'aoristic_datacheck'.
#' 
#' Rows with missing 'End' information are flagged with '1' and counted in the console.
#' Rows where the 'End' datetime occurs before the 'Start' datetime are flagged '2'. 
#' 
#' Note that aoristic.df() will still run even with the data issues. Where the 'End'
#' datetime is missing, the aoristic value will be assigned to the 'Start' datetime hour.
#' When the 'End' datetime comes before the 'Start' datetime, the 'Start' datetime is only used
#' and again, the aoristic value is assigned to the 'Start' datetime hour. 
#'
#' @param data1 data.frame with a minimum of 4 columns with X, Y coords, Start and End date/time
#' @param Xcoord a vector of the X coordinate or latitude (numeric object)
#' @param Ycoord a vector of the Y coordinate or longitude (numeric object)
#' @param DateTimeFrom a  vector of the column name for FromDateTime (POSIXct date-time object)
#' @param DateTimeTo a vector of the column name for ToDateTime (POSIXct date-time object)
#' @return A data frame flagging any problems or logical errors from an aoristic data check
#' @examples 
#' datacheck.df <- aoristic.datacheck(dcburglaries, 'X', 'Y', 'StartDateTime', 'EndDateTime')
#' @import lubridate
#' @export
#' @references Ratcliffe, J. H. (2002). Aoristic signatures and the spatio-temporal analysis of high volume crime patterns. Journal of Quantitative Criminology, 18(1), 23-43.


aoristic.datacheck <- function(data1, Xcoord, Ycoord, DateTimeFrom, DateTimeTo) {
    
  # BUGHUNT 
  bughunt <- F
  DCburg <- NYburg <- NULL
  if (bughunt){ # Only used if I am debugging this function
    testset <- 2
    if (testset == 1)
    {
      data1 <- DCburg
      Xcoord <- 'XCOORD'
      Ycoord <- 'YCOORD'
      DateTimeFrom <- 'STARTDateTime'
      DateTimeTo <- 'ENDDateTime'
    } else {
      data1 <- NYburg
      Xcoord <- 'X_COORD_CD'
      Ycoord <- 'Y_COORD_CD'
      DateTimeFrom <- 'STARTDateTime'
      DateTimeTo <- 'ENDDateTime'
    }
  } #/BUGHUNT
  
  
  
  
  
    if (!is.data.frame(data1)) {
        stop("The input data frame specified is not a data.frame object")
    }
    
    x_lon <- y_lat <- NULL
    
    # Build a local data.frame and populate with passed arguments
    df1 <- data.frame(matrix(ncol = 4, nrow = nrow(data1)))
    colnames(df1) <- c("x_lon", "y_lat", "datetime_from", "datetime_to")
    df1$x_lon <- data1[, Xcoord]
    df1$y_lat <- data1[, Ycoord]
    df1$datetime_from <- data1[, DateTimeFrom]
    df1$datetime_to <- data1[, DateTimeTo]
    
    if (!class(df1$datetime_from)[1] == "POSIXct") {
        stop("The DateTimeFrom field is not POSIXct object. Use the lubridate package before using this function")
    }
    if (!class(df1$datetime_to)[1] == "POSIXct") {
        stop("The DateTimeTo field is not POSIXct object. Use the lubridate package before using this function")
    }
    if  (is.numeric(df1$x_lon)[1] == FALSE) {
        stop("The X coordinate field is not a numeric object. Change the variable format before continuing")
    }
    if  (is.numeric(df1$y_lat)[1] == FALSE) {
        stop("The Y coordinate field is not a numeric object. Change the variable format before continuing")
    }
    
    suppressWarnings (duration <- as.duration(ymd_hms(df1$datetime_from) %--% ymd_hms(df1$datetime_to)))
    
    # Take the duration variable and add to the data.frame
    df1$duration <- duration 
    
    
    df1["aoristic_datacheck"] <- 0  #Create a datacheck column to hold results
    
    df1$aoristic_datacheck[is.na(df1$datetime_to)] <- 1  # Mark no *TO* datetime values with 1
    df1$aoristic_datacheck[df1$duration < 0] <- 2  # Mark illogical from/to combinations with 2
    rowsWith_NA_error <- sum(df1$aoristic_datacheck == 1)
    rowsWith_DT_error <- sum(df1$aoristic_datacheck == 2)
    
    errors.coord <- sum(is.na(df1$x_lon) | is.na(df1$y_lat))
    errors.zero <- nrow(subset(df1, x_lon == 0 | y_lat == 0))
    
    txt <- "\n---- Aoristic data check -------------------------------------------\n"
    if (rowsWith_NA_error > 0 || rowsWith_DT_error > 0) {
        txt <- paste(txt, "     ", rowsWith_NA_error, " rows were missing END/TO datetime values.", "\n", sep = "")
        txt <- paste(txt, "     ", rowsWith_DT_error, " rows had END/TO datetimes before START/FROM datetimes.", "\n", sep = "")
        
        txt <- paste(txt, "     In the aoristic.datacheck data frame these rows are indicated", 
                     "\n", sep = "")
        txt <- paste(txt, "     with missing end datetimes = 1 and start/end logical errors = 2", "\n", sep = "")
        txt <- paste(txt, "     See the aoristic.datacheck column. Also see ?aoristic.datacheck", "\n", sep = "")
        
    } else {
        txt <- paste(txt, "     Congratulations!", "\n", sep = "")
        txt <- paste(txt, "     Data check did not find any missing data in the END/TO column,", "\n", sep = "")
        txt <- paste(txt, "     and did not find any logical errors in the date sequence.", "\n", sep = "")
    }
    message(txt)
    txt <- ""
    
    message("     Coordinates check:")

    if (errors.coord > 0 || errors.zero > 0) {
        if (errors.coord > 0) {
            txt <- paste(txt, "     ", errors.coord, " rows missing spatial coordinates.", "\n", sep = "")
        }
        if (errors.zero > 0) {
            txt <- paste(txt, "     ", errors.zero, " rows had a zero spatial coordinate.", "\n", sep = "")
        }
    } else{
        txt <- "     No missing or zero coordinates.\n"
    }
    message(txt)

    
    aoristic.datacheck <- df1
    return(aoristic.datacheck)
}