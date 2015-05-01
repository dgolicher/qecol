
get_cet<-function()
{
  require(reshape)
  CET <- url("http://www.metoffice.gov.uk/hadobs/hadcet/cetml1659on.dat")
  library(date)
  writeLines(readLines(CET, n = 10))
  cet <- read.table(CET, sep = "", skip = 6, header = TRUE,
                    fill = TRUE, na.string = c(-99.99, -99.9))
  names(cet) <- c(month.abb, "Annual")
  ## remove last row of incomplete data
  cet <- cet[-nrow(cet), ]
  cet <- cet[, -ncol(cet)]
  ## get rid of the annual too - store for plotting
  cet$Year <- as.numeric(rownames(cet))
  ccet<-melt(cet,id="Year")
  ccet$month<-match(ccet$variable,month.abb)
  ccet$Date<- paste(ccet$Year, ccet$month, "15", sep = "-")
  ccet$Date<-as.Date(ccet$Date, format = "%Y-%m-%d") 
  names(ccet)<-c("Year","Month","Temperature","Month_n","Date")
  ccet <- ccet[with(ccet, order(Date)), ]
  
  ## Add in a Time variable
  ccet <- transform(ccet, Time = as.numeric(Date) / 1000)
  head(ccet)
  cet<-ccet
  save(cet,file="data/cet.rda")
}


load_custom_functions<-function(){
  tmp <- tempfile()
  download.file("https://github.com/gavinsimpson/random_code/raw/master/derivFun.R", "deriv.R", method = "wget")
  source(tmp)
  tmp <- tempfile()
  download.file("https://github.com/gavinsimpson/random_code/raw/master/tsDiagGamm.R", "R/tsDIagGamm.R", method = "wget")
  source(tmp)
}

get_cru<-function(){
  URL <- url("http://www.cru.uea.ac.uk/cru/data/temperature/HadCRUT4-gl.dat")
  gtemp <- read.table(URL, fill = TRUE)
  ## Don't need the even rows
  gtemp <- gtemp[-seq(2, nrow(gtemp), by = 2), ]
  ## set the Year as rownames
  rownames(gtemp) <- gtemp[,1]
  ## Add colnames
  colnames(gtemp) <- c("Year", month.abb, "Annual")
  ## Data for 2011 incomplete so work only with 1850-2010 data series
  gtemp <- gtemp[-nrow(gtemp), ]
  ## Plot the data
  ylab <- expression(Temperature~Anomaly~(1961-1990)~degree*C)
  plot(Annual ~ Year, data = gtemp, type = "o", ylab = ylab)
  hadcrut4<-gtemp
  save(hadcrut4,file="data/hadcrut4.rda")
}