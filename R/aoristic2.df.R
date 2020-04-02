#' Create a data.frame with hourly aoristic values
#'
#' @param data1 data.frame with a minimum of 4 columns with X, Y coords, Start and End date/time
#' @param Xcoord a vector of the X coordinate or latitude (passed through for user)
#' @param Ycoord a vector of the Y coordinate or longitude (passed through for user)
#' @param DateTimeFrom a  vector of the column name for FromDateTime (POSIXct date-time object)
#' @param DateTimeTo a vector of the column name for ToDateTime (POSIXct date-time object). If missing, one hour duration assigned.
#' @return A data frame with aoristic values for each hour of the week for each input row
#' @import lubridate
#' @export
#' @references Ratcliffe, J. H. (2002). Aoristic Signatures and the Spatio-Temporal Analysis of High Volume Crime Patterns. Journal of Quantitative Criminology, 18(1), 23-43.
#'
#'
# Some useful keyboard shortcuts for package authoring: Install Package: 'Ctrl + Shift + B' Check
# Package: 'Ctrl + Shift + E' Test Package: 'Ctrl + Shift + T'


aoristic2.df <- function(data1, Xcoord, Ycoord, DateTimeFrom, DateTimeTo) {
    
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
    myMessages = "T"
    
    
    if (!class(df1$datetime_from)[1] == "POSIXct") {
        stop("The DateTimeFrom field is not POSIXct object.  Use the lubridate package before using this function")
    }
    if (!class(df1$datetime_to)[1] == "POSIXct") {
        stop("The DateTimeTo field is not POSIXct object.  Use the lubridate package before using this function")
    }
    
    duration <- as.duration(ymd_hms(df1$datetime_from) %--% ymd_hms(df1$datetime_to))
    df1$duration <- duration/dhours(1)  # This is the exact duration in fractions of hours
    
    # USER DATA ERROR CHECKING Adjust the dataset where the user does not have a datetime_to value ( = NA)
    # This catches where the user only has a from or start date With crime this can be when the event time
    # is actually known.  I dont actually adjust the from datetime, but the duration becomes one hour.
    errors_missing <- sum(is.na(df1[, 4]))
    
    if (errors_missing > 0) {
        {
            txt <- paste(" **** WARNING: ", errors_missing, " row(s) were missing END/TO datetime values.", 
                "\n", sep = "")
            txt <- paste(txt, "               The START/FROM datetime will be used in isolation and ", "\n", 
                sep = "")
            txt <- paste(txt, "               the duration of the event will default to one hour. ", "\n", 
                sep = "")
            txt <- paste(txt, "               Use `aoristic2.datacheck` to identify problematic rows.", 
                "\n", sep = "")
            message(txt)
            df1$duration[is.na(df1$datetime_to)] <- 1  # If duration missing default to one hour
            # df1$datetime_to[is.na(df1$datetime_to)] <- df1$datetime_from[is.na(df1$datetime_to)] # Not used
        }
    }
    
    # USER DATA ERROR CHECKING I will adjust the dataset where the real duration is < 0 This catches when
    # the user inadvertently has FROM datetimes that are later than the TO datetime. This is deemed to be an
    # error and the event is set with a zero duration and is effectively ignored. Warn the user here.
    errors_logic <- sum(df1[, 5] < 0)
    
    if (errors_logic > 0) {
        {
            txt <- paste(" **** WARNING: ", errors_logic, " row(s) have illogical FROM and TO row values.", 
                "\n", sep = "")
            txt <- paste(txt, "               This means the FROM datetime is later than the TO datetime.", 
                "\n", sep = "")
            txt <- paste(txt, "               Problem rows will be kept but duration_rounded set to zero hours and they will", 
                "\n", sep = "")
            txt <- paste(txt, "               be effectively ignored. Use `aoristic2.datacheck` to identify the error rows.", 
                "\n", sep = "")
            message(txt)
        }
    }
    
    # Now work with rounding to whole hours.
    df1$rounded_from <- floor_date(df1$datetime_from, "hour")
    df1$duration_rounded <- ceiling(as.numeric(duration, "hour"))
    df1$duration_rounded[is.na(df1$duration_rounded)] <- 1  # recode duration as 1 hour if timeTo is missing
    df1$rounded_to <- df1$rounded_from + hours(df1$duration_rounded)
    
    
    # Next stage is to do the aoristic hour-by-hour Create a new dataframe with 168 columns for day/hour
    df2 <- data.frame(matrix(0, ncol = 168, nrow = nrow(df1)))
    if (myMessages) {
        message("     Please wait.... aoristic data frame can take a while to create...")
    }
    
    
    for (i in 1:168) {
        # separated in case I want to add zeros to level variable name length in future
        if (i < 10) {
            names(df2)[i] <- paste("hour", i, sep = "")
        }
        if (i >= 10 && i < 100) {
            names(df2)[i] <- paste("hour", i, sep = "")
        }
        if (i >= 100) {
            names(df2)[i] <- paste("hour", i, sep = "")
        }
    }
    df1 <- cbind(df1, df2)  # Bind the source data to the hours matrix
    rm(df2)  # Tidy up data.frame
    
    # Populate the hourX data.frame columns with aoristic value Cycle each row Only change the row if there
    # is no logical error
    for (i in 1:nrow(df1)) {
        if (df1[i, "duration"] > 0) {
            from.day <- wday(df1[i, "datetime_from"])  # The day number for the start date
            from.hour <- hour(df1[i, "datetime_from"])  # The hour number for the start hour
            start.position <- ((24 * (from.day - 1)) + from.hour) + 1  # Where we start hour increments
            num.increments <- df1[i, "duration_rounded"]  # How many hour increments
            num.increments <- ifelse(num.increments > 168, 168, num.increments)  # If duration is more than a week, ever hour get same value
            val.increments <- 1/num.increments
            cur.column.val <- start.position
            
            for (j in 1:num.increments) {
                if (cur.column.val > 168) 
                  {
                    cur.column.val <- 1
                  }  # Reset to hour 1 if over Saturday 2300 hrs
                cur.column.name <- paste("hour", cur.column.val, sep = "")
                df1[i, cur.column.name] <- df1[i, cur.column.name] + val.increments
                cur.column.val <- cur.column.val + 1
            }
        }
    }
    
    if (myMessages) {
        message("     Aoristic data frame created.")
    }
    return(df1)
}

