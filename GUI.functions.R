process.data <- function(h, ...) {
  where <- switch(svalue(analysis.output),
                  "Screen"="screen","PDF"="pdf","Metafile"="emf")
  
  new.data <- svalue(num.lots)
  analysis.level <- svalue(analysis.tier)
  
  if (is.na(new.data))
    new.data <- NULL
  
  parse.PMDB.data(df = read.csv(data.file),new.lots = new.data,
                  analysis.level = analysis.level,where = where)
}

get.CSV <- function(h, ...) {
  data.file <<- gfile(filter=list("CSV files"=list(patterns=c("*.csv"))))
  svalue(csv.text) <- basename(data.file)
  setwd(dirname(data.file))

}

clear.all <- function(...) {
  svalue(analysis.tier) <- ""
  data.file <- NULL
  svalue(csv.text) <- "Select file for upload"
  svalue(analysis.output) <- "Screen"
  svalue(num.lots) <- ""
}

cancel.button <- function(h, ...) {
  gconfirm("This will close your session. Continue?", title="Quit R session?",
           icon="warning", handler = function (h, ...) q())
}
