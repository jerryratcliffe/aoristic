#' Checks a data set for aoristic logical errors
#'
#' @param data1 data.frame with a minimum of 4 columns with X, Y coords, Start and End date/time
#' @param Xcoord a vector of the X coordinate or latitude (passed through for user)
#' @param Ycoord a vector of the Y coordinate or longitude (passed through for user)
#' @param DateTimeFrom a  vector of the column name for FromDateTime (POSIXct date-time object)
#' @param DateTimeTo a vector of the column name for ToDateTime (POSIXct date-time object). If missing, one hour duration assigned.
#' @return A data frame with logical errors from an aoristic data check
#' @import lubridate
#' @export
#' @references Ratcliffe, J. H. (2002). Aoristic Signatures and the Spatio-Temporal Analysis of High Volume Crime Patterns. Journal of Quantitative Criminology, 18(1), 23-43.


aoristic2.datacheck <- function(data1, Xcoord, Ycoord, DateTimeFrom, DateTimeTo) {
    
    if (!is.data.frame(data1)) {
        stop("The input data frame specified is not a data.frame object")
    }
    
    # Build a local data.frame and populate with passed arguments
    df1 <- data.frame(matrix(ncol = 4, nrow = nrow(data1)))
    colnames(df1) <- c("x_lon", "y_lat", "datetime_from", "datetime_to")
    df1$x_lon <- data1[, Xcoord]
    df1$y_lat <- data1[, Ycoord]
    df1$datetime_from <- data1[, DateTimeFrom]
    df1$datetime_to <- data1[, DateTimeTo]
    
    if (!class(df1$datetime_from)[1] == "POSIXct") {
        stop("The DateTimeFrom field is not POSIXct object.  Use the lubridate package before using this function")
    }
    if (!class(df1$datetime_to)[1] == "POSIXct") {
        stop("The DateTimeTo field is not POSIXct object.  Use the lubridate package before using this function")
    }
    
    duration <- as.duration(ymd_hms(df1$datetime_from) %--% ymd_hms(df1$datetime_to))
    df1$duration <- duration/dhours(1)  # This is the exact duration in fractions of hours
    
    df1["aoristic_datacheck"] <- 0  #Create a datacheck column to hold results
    
    df1$aoristic_datacheck[is.na(df1$datetime_to)] <- 1  # Mark no *TO* datetime values with 1
    df1$aoristic_datacheck[df1$duration < 0] <- 2  # Mark illogical from/to combinations with 2
    
    rowsWith_NA_error <- sum(df1$aoristic_datacheck == 1)
    rowsWith_DT_error <- sum(df1$aoristic_datacheck == 2)
    
    if (rowsWith_NA_error > 0 || rowsWith_DT_error > 0) {
        
        txt <- paste("\n", " ****** Aoristic data check:", "\n", sep = "")
        txt <- paste(txt, " ****** ", rowsWith_NA_error, " row(s) had no END/TO datetime value. In an aoristic analysis", 
            "\n", sep = "")
        txt <- paste(txt, " ******    the START/FROM datetime will be passed to the END/TO column, and", 
            "\n", sep = "")
        txt <- paste(txt, " ******    the event duration will default to one hour.", "\n", sep = "")
        txt <- paste(txt, " ****** ", rowsWith_DT_error, " row(s) had a logical error where the START/FROM datetime was *after*", 
            "\n", sep = "")
        txt <- paste(txt, " ******    the END/TO value. In an aoristic analysis this will default to a", 
            "\n", sep = "")
        txt <- paste(txt, " ******    duration of zero hours and be effectively ignored in the analysis.", 
            "\n", sep = "")
        txt <- paste(txt, " ******    ", "\n", sep = "")
        txt <- paste(txt, " ******    In the aoristic.datacheck data frame these rows are marked in a column", 
            "\n", sep = "")
        txt <- paste(txt, " ******    with NA values = 1 and logical error values = 2", "\n", sep = "")
        txt <- paste(txt, " ******    See the aoristic.datacheck column.", "\n", sep = "")
        
    } else {
        txt <- paste("\n", " ****** Aoristic data check:", "\n", sep = "")
        txt <- paste(txt, " ******    Congratulations!", "\n", sep = "")
        txt <- paste(txt, " ******    The data check did not find any missing data in the END/TO column,", 
            "\n", sep = "")
        txt <- paste(txt, " ******    and did not find any logical errors in the date sequence.", "\n", 
            sep = "")
        
    }
    aoristic.datacheck <- df1
    message(txt)
    return(aoristic.datacheck)
}

