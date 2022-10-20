#' Calculate aoristic weights
#' 
#' Calculates aoristic proportional weights across 168 units representing each hour of the week (24 hours x 7 days). 
#' It is designed for situations when an event time is not know but could be spread across numerous
#' hours or days, and is represented by a Start (or From) date and time, and an End (or To) date and time.
#' The output retains the source data, and can be reimported into a GIS for spatial analysis. The output 
#' from this function is used in other aoristic library functions. 
#' 
#' NOTE: If an observation is missing the End/To datetime, the entire aoristic weight (1.0) will be assigned to the
#' hour block containing the Start/From datetime. Events with start datetime events after the end datetime will also 
#' be assigned to the hour block containing the Start/From datetime. Events with time spans lasting more than one 
#' week (>168 hours) will default to a time span of 168 hours and a value of ~ 0.0059 (1/168) assigned to each day/hour.
#' 
#'
#' @param data1 data frame with a minimum of 4 columns with X, Y coords, Start and End date/time
#' @param Xcoord a vector of the event X coordinate or latitude (numeric object)
#' @param Ycoord a vector of the event Y coordinate or longitude (numeric object)
#' @param DateTimeFrom a vector of the column name for FromDateTime (POSIXct date-time object)
#' @param DateTimeTo a vector of the column name for ToDateTime (POSIXct date-time object) 
#' @return A data frame with aoristic values for each hour of the week for each observation
#' @import lubridate
#' @examples 
#' df <- aoristic.df(dcburglaries, 'X', 'Y', 'StartDateTime', 'EndDateTime')
#' @export
#' @references Ratcliffe, J. H. (2002). Aoristic signatures and the spatio-temporal analysis of high volume crime patterns. Journal of Quantitative Criminology, 18(1), 23-43.
#'
#'

aoristic.df <- function
(data1, Xcoord, Ycoord, DateTimeFrom, DateTimeTo) {
  
  
  # BUGHUNT
  bughunt <- F
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
    
    # Build a local data.frame and populate with passed arguments
    x_lon <- y_lat <- x <- NULL
    df1 <- data.frame(matrix(ncol = 4, nrow = nrow(data1)))
    colnames(df1) <- c("x_lon", "y_lat", "datetime_from", "datetime_to")
    df1$x_lon <- data1[, Xcoord]
    df1$y_lat <- data1[, Ycoord]
    df1$datetime_from <- data1[, DateTimeFrom]
    df1$datetime_to <- data1[, DateTimeTo]
    myMessages <- "T"
    
    
    if (!class(df1$datetime_from)[1] == "POSIXct") {
        stop("The DateTimeFrom field is not a POSIXct object. Convert with the lubridate package before using this function")
    }
    if (!class(df1$datetime_to)[1] == "POSIXct") {
        stop("The DateTimeTo field is not a POSIXct object. Convert with the lubridate package before using this function")
    }
    
    duration <- as.duration(ymd_hms(df1$datetime_from) %--% ymd_hms(df1$datetime_to))
    df1$duration <- duration %/% dminutes(1)  # This is the modelo exact duration in minutes, rounded down
    
    # DATA ERROR CHECKING:
    # This catches where the user only has a from/start date. This can occur when the event time is known.
    errors.missing.df <- plyr::count(is.na(df1[, 4]))
    if (nrow(subset(errors.missing.df, x == TRUE )) > 0) {
        errors.missing <- subset(errors.missing.df, x == TRUE)$freq
        df1$duration[is.na(df1$datetime_to)] <- 1  # If duration missing default to one minute
    } else {
        errors.missing <- 0
    }
    
    # DATA ERROR CHECKING:
    #This catches when FROM datetimes are later than the TO datetime. 
    errors.logic.df <- plyr::count(df1[, 5] < 0)
    if (nrow(subset(errors.logic.df, x== TRUE))>0) {
        errors.logic <- subset(errors.logic.df, x == TRUE)$freq
        # At this point just noting the number. Use start datetime only for analysis
    } else {
        errors.logic <- 0
    }
    
    
    # Create a new dataframe to hold aoristic value for each hour of the week. 
    df2 <- data.frame(matrix(0, ncol = 168, nrow = nrow(df1)))
    
    for (i in 1:168){  
        names(df2)[i] <- paste("hour", i, sep = "") 
    }
    df1 <- cbind(df1, df2)  # Bind the source data to the hours matrix
    rm(df2)                 # Tidy up data.frame

    
    # Loop each data row and allocate aoristic probability --------------------
    
    for (i in seq_len(nrow(df1))) {
        from.day <- wday(df1[i, "datetime_from"])               # The day number for the start date
        from.hour <- hour(df1[i, "datetime_from"])              # The hour number for the start hour
        time.span <- df1[i, "duration"]                         # The event time span
        hour.position <- ((24 * (from.day - 1)) + from.hour) + 1
        cur.column.name <- paste("hour", hour.position, sep = "")
        left.in.hour <- 60 - minute(df1[i, "datetime_from"])    # For when start hour begins > :00
        aor.minute <- 1 / time.span                             # Aoristic weight per minute
        
        # Catch the rare occurrence when there is no START date-time
        if (is.na(from.day)) {
            txt <- paste("Warning message: No START date-time found in row ", i, ". Row will be ignored.", sep = '')
            message(txt)
            next
        }
        
        if (time.span >= 10080) {                       
            # Event duration > one week. Increments each day/hour equally.
            for (j in 1:168) {
                cur.column.name <- paste("hour", j, sep = "")
                df1[i, cur.column.name] <- df1[i, cur.column.name] + 1 / 168
            }
        }
        
        
        if (is.na(df1[i, 'datetime_to'])) {
            # The End date is missing, in crime data often when the event time is precisely known. 
            # In these cases, aoristic.df assigns the time.span to the containing hour block (+1)
            df1[i, cur.column.name] <- df1[i, cur.column.name] + time.span 
        }   
        
        
        if (time.span >= 0 && time.span <= 1 && !is.na(df1[i, 'datetime_to'])) {
            # Event duration is known precisely, with duration 0 or 1 and the TO datetime
            # field exists. In these cases, aoristic.df assigns the containing hour block +1
            df1[i, cur.column.name] <- df1[i, cur.column.name] + 1
        }
        
        
        if (time.span < 0) {
            # Event time span is illogical in that End datetime is before Start datetime.
            # Some options here. Either -1-, ignore this row, or -2- use the Start datetime
            # and proceed as if the End datetime did not exist [as above].
            # next                                                  #1 (ignore)
            df1[i, cur.column.name] <- df1[i, cur.column.name] + 1  #2 (use start date)
            # A third future option is to swap start and end datetimes
        }
        
        
        if (time.span > 1 && time.span < 10080) {
            # We have an event with a time span that has to be distributed appropriately.
            # Assign aoristic weights until the remaining minutes in the time span are exhausted.
            rmg.mins <- time.span   
            
            while (rmg.mins > 0) {
                if (rmg.mins <= left.in.hour) {
                    # then the current hour can be assigned the remaining aoristic weight
                    df1[i, cur.column.name] <- df1[i, cur.column.name] + (rmg.mins * aor.minute)
                    rmg.mins <- 0
                }
                if (rmg.mins > left.in.hour) {
                    df1[i, cur.column.name] <- df1[i, cur.column.name] + (left.in.hour * aor.minute)
                    rmg.mins <- rmg.mins - left.in.hour             # decrease rmg.mins
                    left.in.hour <- 60                              # reset so the next time period is a full hour
                    ifelse (hour.position >= 168, hour.position <- 1, hour.position <- hour.position + 1)
                    cur.column.name <- paste("hour", hour.position, sep = "")
                }
            }
        }
    }  
    
    
    if (myMessages) {
        message("\nAoristic data frame created.")
        
        # Report how many rows only had a start datetime
        if (errors.missing > 0) {
            message(paste("  ", errors.missing, " row(s) were missing END/TO datetime values.", sep = ""))
        }
      # Report how many rows only start datetimes after end datetimes
      if (errors.logic > 0) {
        message(paste("  ", errors.logic, " row(s) had END/TO datetimes before START/FROM datetimes.", sep = ""))
      }
      # Report rogue errors (miscellaneous wtfs)
      if (errors.rogue > 0) {
        message(paste("  ", errors.rogue, " row(s) had miscellaneous errors that this package repaired.", "\n", sep = ""))
      }
      if (errors.missing > 0 || errors.logic > 0){
            txt <- paste("  Use 'aoristic.datacheck()' to identify these rows.", "\n", sep = "")
            txt <- paste(txt, "  '?aoristic.datacheck' explains how aoristic.df handles these data.", "\n", sep = "")
      }
      txt <- paste(txt, "  Any warnings appearing below indicate miscellaneous data errors that could not", "\n", sep = "")
      txt <- paste(txt, "  diagnosed and corrected by the aoristic package. They have not be used in the analysis.","\n", sep = "")
      message(txt)
    }  
    
    return(df1)
}

