#Getting Zip
#http://www.markit.com/news/InterestRates_USD_20160102.zip

temp <- tempfile()
download.file("http://www.markit.com/news/InterestRates_USD_20160102.zip",temp)
temp_xml <- unz(temp, "InterestRates_USD_20160102.xml")
xmldoc<- readChar(temp_xml, file.info(temp_xml$size))
unlink(temp)
