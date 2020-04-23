#' Summarize weekly aoristic weights
#'
#' Summarizes the sum of aoristic weights for each hour of the week, based on output from an aoristic data 
#' frame (created by aoristic.df). The function returns a data frame, with optional outputs. 
#' Option 'xlsx' sends the data frame to an Excel spreadsheet in the same folder as the source 
#' data. Filenames increment to prevent overwriting previous analyses. Using option 'jpg' 
#' creates a color coded summary table in jpg format in the same folder as the source data.
#' The filename is aoristic_distribution.jpg and it will overwrite previous files of the same name.
#' The function will attempt to open the jpg file for the user. 
#'  
#' NOTE: Be aware that the distribution of values is NOT the same as the aoristic.ref() output, because
#' the summary charts and graphs move Sunday to the end of the week to keep the weekend together.
#' 
#' @param data1 a data frame output from the aoristic.df function
#' @param output output ='xlsx' for an Excel format output
#' #' output ='jpg' for JPG grid, blank otherwise
#' @return A data frame with aoristic values summed for each hour of the week
#' @examples 
#' \dontrun{
#' 
#' aor.summary <- aoristic.summary(aor.df)
#' aor.summary <- aoristic.summary(aor.df, 'xlsx')
#' aor.summary <- aoristic.summary(aor.df, 'jpg')
#' }
#' @import formattable htmltools
#' @export
#' @references Ratcliffe, J. H. (2002). Aoristic signatures and the spatio-temporal analysis of high volume crime patterns. Journal of Quantitative Criminology, 18(1), 23-43.


aoristic.summary <- function (data1, output = ""){

    df3 <- data.frame(matrix(0, ncol = 7, nrow = 24))
    output.row <- 1
    output.col <- 1

    for (k in 1:168)  # Sum the column values for each hour of the week
    {
      cur.column.name <- paste("hour", k, sep = "")
      z <- sum(as.numeric(data1[ ,cur.column.name]), na.rm = TRUE)

      df3[output.row,output.col] <-  trimws(format(round(z, 3), nsmall=3)) # Assign value to cell

          output.row <- output.row + 1
      if (output.row == 25) {
        output.row <- 1
        output.col <- output.col + 1
      }
    }

    colnames(df3) <- c('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat')
    Range <- c('0000-0059',  '0000-0159',  '0200-0259',  '0300-0359',  '0400-0459',  '0500-0559',
                     '0600-0659',  '0700-0759',  '0800-0859',  '0900-0959',  '1000-1059',  '1100-1159',
                     '1200-1259',  '1300-1359',  '1400-1459',  '1500-1559',  '1600-1659',  '1700-1759',
                     '1800-1859',  '1900-1959',  '2000-2059',  '2100-2159',  '2200-2259',  '2300-2359' )
    df4 <- data.frame(Range)
    df4 <- data.frame(df4,df3)
    df4 <- df4[, c(1, 3, 4, 5, 6, 7, 8, 2)] # Reorder columns to put weekend at the end
    rm(df3)
    for (j in 2:8){# recode hours as numeric
      df4[ ,j] <- as.numeric(df4[, j])
    }

    

# Optional outputs --------------------------------------------------------
    
    # EXCEL OUTPUT: Switch output='xlsx'
    if (output == "xlsx")
    {
        current.folder <- getwd()
        filenum.inc <- 1
        output.file <- paste(current.folder, '/Aoristic_summary_', filenum.inc, '.xlsx',sep='')

        # If user already has a _X file, increment until we have a free _X filename available
        while (file.exists(output.file)) {
          filenum.inc <- filenum.inc + 1
          output.file <- paste(current.folder, '/Aoristic_summary_', filenum.inc, '.xlsx',sep='')
        }

        openxlsx::write.xlsx(df4, output.file, sheetName = "Aoristic",
                   col.names = TRUE, row.names = TRUE, append = FALSE)

        txt1 <- paste('\n****** Aoristic summary file for Excel written to: \n',output.file, sep='       ')
        message(txt1)
    }

    # JPG OUTPUT: Switch output='jpg'
    if (output == "jpg"){
      
      export_formattable <- function(f, file, width = "600px", height = NULL,
                                     background = "white", delay = 0.2)
      {
        w <- as.htmlwidget(f, width = width, height = height)
        path <- htmltools::html_print(w, background = background, viewer = NULL)
        url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
        webshot::webshot(url,
                file = file,
                selector = ".formattable_widget",
                delay = delay)
      }
  
  
      custom_color_tile <- function (...)
      {
        formatter("span",
  
                  style = function(x) style(display = "block",
                                            padding = "0 1px",
                                            `color` = "white",
                                            #`width`  = "10px",
                                            `border-radius` = "1px",
                                            `background-color` = csscolor(gradient(as.numeric(x),
                                                                                   ...))))
      }
      
      format.table  <- formattable(df4,
                                   align = c("l",rep("c", ncol(df4) - 1)),
                                   list(
                                     `Range` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
                                     area(col = 2:8) ~ custom_color_tile("#CCE5FF", '#A90303'))
                        )
  
            export_formattable(format.table,"aoristic_distribution.jpg")
            # webshot::install_phantomjs()
            viewer <- getOption("viewer")
            htmlFile <- paste(getwd(), "/aoristic_distribution.jpg", sep='')
            viewer(htmlFile)
            txt1 <- paste('\n****** Aoristic summary grid in jpg format written to: \n',htmlFile, sep='       ')
            message(txt1)
    }
  return(df4)
}
