# https://stackoverflow.com/questions/12193779/how-to-use-the-trycatch-function

urls <- c(
  "http://stat.ethz.ch/R-manual/R-devel/library/base/html/connections.html",
  "http://en.wikipedia.org/wiki/Xz",
  "xxxxx"
)

readUrl <- function(url) {
  tryCatch(
    {
      # Just to highlight: if you want to use more than one
      # R expression in the "try" part then you'll have to
      # use curly brackets.
      # 'tryCatch()' will return the last evaluated expression
      # in case the "try" part was completed successfully
      
      message("This is the 'try' part")
      
      suppressWarnings(readLines(url))
      # The return value of `readLines()` is the actual value
      # that will be returned in case there is no condition
      # (e.g. warning or error).
    },
    error = function(cond) {
      message(paste("URL does not seem to exist:", url))
      message("Here's the original error message:")
      message(conditionMessage(cond))
      # Choose a return value in case of error
      NA
    },
    warning = function(cond) {
      message(paste("URL caused a warning:", url))
      message("Here's the original warning message:")
      message(conditionMessage(cond))
      # Choose a return value in case of warning
      NULL
    },
    finally = {
      # NOTE:
      # Here goes everything that should be executed at the end,
      # regardless of success or error.
      # If you want more than one expression to be executed, then you
      # need to wrap them in curly brackets ({...}); otherwise you could
      # just have written 'finally = <expression>' 
      message(paste("Processed URL:", url))
      message("Some other message at the end")
    }
  )
}

y <- lapply(urls, readUrl)
# This is the 'try' part
# Processed URL: http://stat.ethz.ch/R-manual/R-devel/library/base/html/connections.html
# Some other message at the end
# This is the 'try' part
# Processed URL: http://en.wikipedia.org/wiki/Xz
# Some other message at the end
# This is the 'try' part
# URL does not seem to exist: xxxxx
# Here's the original error message:
# cannot open the connection
# Processed URL: xxxxx
# Some other message at the end

