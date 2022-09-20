#' get.country
#'
#' Extracts country tag from NISO-JATS coded XML file or text as vector of unique countries.
#' @param x a NISO-JATS coded XML file or text.
#' @param unifyCountry Logical. If TRUE replaces country name with standardised country name.
#' @seealso \code{\link[JATSdecoder]{JATSdecoder}} for simultaneous extraction of meta-tags, abstract, sectioned text and reference list.
#' @return Character vector with the extracted country name/s.
#' @export
#' @examples
#' x<-"Some text <country>UK</country> some text <country>England</country>
#'     Text<country>Berlin, Germany</country>"
#' get.country(x)

get.country<-function(x,unifyCountry=TRUE){
# readLines if x is file
if(file.exists(x[1])) x<-readLines(x,warn=FALSE,encoding="UTF-8")
 t<-x

if(length(grep("<country",t))>0){
 t<-paste(t,collapse="")
# split t to vector at <country
 t<-unlist(strsplit2(grep("<country",t,value=TRUE),"<country","before"))
# extract country tag content
 t<-gsub(".*>","",gsub(".*>","",gsub("</count.*","",grep("<country",t,value=TRUE))))
} else if(length(grep("<country",t))==0){
 t<-gsub(".*[0-9] |.*, ","",t)
}else t<-NA
# remove till ,
  t<-gsub(".*, ","",t)
  #clean up
  t<-gsub("[[:punct:]]","",gsub("\\\\t","",t))
  t<-gsub("addrline.*|email.*| EMai.*|Emai.*|phone|institution|Institution| tel.*|country|idalabel|label|Equal|italic| contributions| contribution","",t)
  t<-gsub("University|Research|Institute|Hospital|Department|School","",t)
  # remove numbers
  t<-gsub("[0-9]","",t)
  # if starts with only two capital letters remove them
  if(length(grep("^[A-Z][A-Z] ",t))>0)  t[grep("^[A-Z][A-Z] ",t)]<-gsub("^[A-Z][A-Z] ","",t[grep("^[A-Z][A-Z] ",t)])
  # if ends with only two capital letters remove till last space
  if(length(grep("[A-Z][A-Z]$",t))>0)  t[grep(" [A-Z][A-Z]$",t)]<-gsub(".* ","",t[grep(" [A-Z][A-Z]$",t)])
  # clean up whitespaces
  t<-gsub("^ *|(?<= ) | *$", "", t, perl = TRUE)
  #select lines with at least one capital letter
  t<-grep("[A-Z]",t,value=TRUE)
  # remove text with 2 or less characters (exept UK)
  t<-t[t=="UK"|nchar(t)>2]
  if(length(t)==0) t<-NA
  # replace country names with standardized names
  if(unifyCountry==TRUE) t<-.unifyCountry(t)
  # remove doubles
  t<-unique(t)
  return(t)
  }

  

.unifyCountry<-function(x){
x<-unlist(x)
if(length(grep("England|UK$|United Kingdom|London|KINGDOM",x))>0) x[grep("England|UK$|United Kingdom|KINGDOM|London",x)]<-"United Kingdom"
if(length(grep("USA|United States of America|UNITED STATES|United States$",x))>0) x[grep("USA|United States of America|UNITED STATES|United States$",x)]<-"United States"
if(length(grep("Peru$",x))>0) x[grep("Peru$",x)]<-"Peru"
if(length(grep("India|INDIA",x))>0) x[grep("India|INDIA",x)]<-"India"
if(length(grep("china",tolower(x)))>0) x[grep("china",tolower(x))]<-"China"
if(length(grep("japan",tolower(x)))>0) x[grep("japan",tolower(x))]<-"Japan"
if(length(grep("indonesia",tolower(x)))>0) x[grep("indonesia",tolower(x))]<-"Indonesia"
if(length(grep("russia",tolower(x)))>0) x[grep("russia",tolower(x))]<-"Russia"
if(length(grep("netherlands|amsterdam",tolower(x)))>0) x[grep("netherlands|amsterdam",tolower(x))]<-"Netherlands"
if(length(grep("austria",tolower(x)))>0) x[grep("austria",tolower(x))]<-"Austria"
if(length(grep("canada",tolower(x)))>0) x[grep("canada",tolower(x))]<-"Canada"
if(length(grep("australia",tolower(x)))>0) x[grep("australia",tolower(x))]<-"Australia"
if(length(grep("nigeria",tolower(x)))>0) x[grep("nigeria",tolower(x))]<-"Nigeria"
if(length(grep("xico",tolower(x)))>0) x[grep("xico",tolower(x))]<-"Mexico"
if(length(grep("thailand",tolower(x)))>0) x[grep("thailand",tolower(x))]<-"Thailand"
if(length(grep("singapore",tolower(x)))>0) x[grep("singapore",tolower(x))]<-"Singapore"
if(length(grep("kazakhstan|kazachstan",tolower(x)))>0) x[grep("kazakhstan|kazachstan",tolower(x))]<-"Kazakhstan"
if(length(grep("nepal",tolower(x)))>0) x[grep("nepal",tolower(x))]<-"Nepal"
if(length(grep("portugal",tolower(x)))>0) x[grep("portugal",tolower(x))]<-"Portugal"
if(length(grep("france",tolower(x)))>0) x[grep("france",tolower(x))]<-"France"
if(length(grep("spain",tolower(x)))>0) x[grep("spain",tolower(x))]<-"Spain"
if(length(grep("ireland",tolower(x)))>0) x[grep("ireland",tolower(x))]<-"Ireland"
if(length(grep("ivoire|ivory",tolower(x)))>0) x[grep("ivoire|ivory",tolower(x))]<-"Ivory Cost"
if(length(grep("switzerland",tolower(x)))>0) x[grep("switzerland",tolower(x))]<-"Switzerland"
if(length(grep("brunei",tolower(x)))>0) x[grep("brunei",tolower(x))]<-"Brunei"
if(length(grep("iran",tolower(x)))>0) x[grep("iran",tolower(x))]<-"Iran"
if(length(grep("maroc|moroc",tolower(x)))>0) x[grep("maroc|moroc",tolower(x))]<-"Morocco"
if(length(grep("italy",tolower(x)))>0) x[grep("italy",tolower(x))]<-"Italy"
if(length(grep("arab ",tolower(x)))>0) x[grep("arab ",tolower(x))]<-"United Arab Emirates"
if(length(grep("germany",tolower(x)))>0) x[grep("germany",tolower(x))]<-"Germany"
if(length(grep("south africa|afrique du sud",tolower(x)))>0) x[grep("south africa|afrique du sud",tolower(x))]<-"South Africa"
if(length(grep("brazil|brasil",tolower(x)))>0) x[grep("brazil|brasil",tolower(x))]<-"Brazil"
if(length(grep("scotland|edinburgh",tolower(x)))>0) x[grep("scotland|edinburgh",tolower(x))]<-"Scotland"
if(length(grep("sweden",tolower(x)))>0) x[grep("sweden",tolower(x))]<-"Sweden"
if(length(grep("tunis",tolower(x)))>0) x[grep("tunis",tolower(x))]<-"Tunisia"
if(length(grep("new zealand",tolower(x)))>0) x[grep("new zealand",tolower(x))]<-"New Zealand"
if(length(grep("venezuela",tolower(x)))>0) x[grep("venezuela",tolower(x))]<-"Venezuela"
if(length(grep("belgium",tolower(x)))>0) x[grep("belgium",tolower(x))]<-"Belgium"
if(length(grep("turkey",tolower(x)))>0) x[grep("turkey",tolower(x))]<-"Turkey"
if(length(grep("taiwan",tolower(x)))>0) x[grep("taiwan",tolower(x))]<-"Taiwan"
if(length(grep("philippines",tolower(x)))>0) x[grep("taiwan",tolower(x))]<-"Philippines"
if(length(grep("saudi arabia",tolower(x)))>0) x[grep("saudi arabia",tolower(x))]<-"Saudi Arabia"
if(length(grep("outh korea",tolower(x)))>0) x[grep("outh korea",tolower(x))]<-"S. Korea"
if(length(grep("orth korea",tolower(x)))>0) x[grep("orth korea",tolower(x))]<-"N. Korea"
if(length(grep("denmark",tolower(x)))>0) x[grep("denmark",tolower(x))]<-"Denmark"
if(length(grep("egypt",tolower(x)))>0) x[grep("egypt",tolower(x))]<-"Egypt"
if(length(grep("argentina",tolower(x)))>0) x[grep("argentina",tolower(x))]<-"Argentina"
if(length(grep("ethiopia",tolower(x)))>0) x[grep("ethiopia",tolower(x))]<-"Ethiopia"
if(length(grep("cameroon",tolower(x)))>0) x[grep("cameroon",tolower(x))]<-"Cameroon"
if(length(grep("ghana",tolower(x)))>0) x[grep("ghana",tolower(x))]<-"Ghana"
if(length(grep("czech",tolower(x)))>0) x[grep("czech",tolower(x))]<-"Czech Rep."
if(length(grep("slovak|slowak",tolower(x)))>0) x[grep("slovak|slowak",tolower(x))]<-"Slovakia"
if(length(grep("sloveni",tolower(x)))>0) x[grep("sloveni",tolower(x))]<-"Slovenia"
if(length(grep("finland",tolower(x)))>0) x[grep("finland",tolower(x))]<-"Finland"
if(length(grep("estonia",tolower(x)))>0) x[grep("estonia",tolower(x))]<-"Estonia"
if(length(grep("senegal",tolower(x)))>0) x[grep("senegal",tolower(x))]<-"Senegal"
if(length(grep("romania",tolower(x)))>0) x[grep("romania",tolower(x))]<-"Romania"
if(length(grep("bosnia|herzegovina",tolower(x)))>0) x[grep("bosnia|herzegovina",tolower(x))]<-"Bosnia and Herz."
if(length(grep("algerie|algeria",tolower(x)))>0) x[grep("algerie|algeria",tolower(x))]<-"Algeria"
if(length(grep("greece",tolower(x)))>0) x[grep("greece",tolower(x))]<-"Greece"
if(length(grep("bangladesh",tolower(x)))>0) x[grep("bangladesh",tolower(x))]<-"Bangladesh"
if(length(grep("mongolia",tolower(x)))>0) x[grep("mongolia",tolower(x))]<-"Mongolia"
if(length(grep("afghan",tolower(x)))>0) x[grep("afghan",tolower(x))]<-"Afghanistan"
if(length(grep("costa rica",tolower(x)))>0) x[grep("costa rica",tolower(x))]<-"Costa Rica"
if(length(grep("bolivia$",tolower(x)))>0) x[grep("bolivia$",tolower(x))]<-"Bolivia"
if(length(grep("guatemala",tolower(x)))>0) x[grep("guatemala",tolower(x))]<-"Guatemala"
if(length(grep("el salvador",tolower(x)))>0) x[grep("el salvador",tolower(x))]<-"El Salvador"
if(length(grep("equador|ecuador",tolower(x)))>0) x[grep("equador|ecuador",tolower(x))]<-"Ecuador"
if(length(grep("equatorial guinea",tolower(x)))>0) x[grep("equatorial guinea",tolower(x))]<-"Eq. Guinea"
if(length(grep("luxembourg",tolower(x)))>0) x[grep("luxembourg",tolower(x))]<-"Luxembourg"
if(length(grep("polynesia",tolower(x)))>0) x[grep("polynesia",tolower(x))]<-"Fr. Polynesia"
if(length(grep("french guiana|french guyana|guyane franxeaise",tolower(x)))>0) x[grep("french guiana|french guyana|guyane franxeaise",tolower(x))]<-"Fr. Guiana"
if(length(grep("guyane$",tolower(x)))>0) x[grep("guyane$",tolower(x))]<-"Guyane"
if(length(grep("Guam",x))>0) x[grep("Guam",x)]<-"Guam"
if(length(grep("guineabissau|guinea bissau",tolower(x)))>0) x[grep("guineabissau|guinea bissau",tolower(x))]<-"Guinea Bissau"
if(length(grep("Papua New Guinea",tolower(x)))>0) x[grep("Papua New Guinea",tolower(x))]<-"Papua New Guinea"
if(length(grep("papua$",tolower(x)))>0) x[grep("papua$",tolower(x))]<-"Papua"
if(length(grep("andorra",tolower(x)))>0) x[grep("andorra",tolower(x))]<-"Andorra"
if(length(grep("hungary",tolower(x)))>0) x[grep("hungary",tolower(x))]<-"Hungary"
if(length(grep("poland",tolower(x)))>0) x[grep("poland",tolower(x))]<-"Poland"
if(length(grep("Chile",x))>0) x[grep("Chile",x)]<-"Chile"
if(length(grep("Croatia",x))>0) x[grep("Croatia",x)]<-"Croatia"
if(length(grep("Cyprus",x))>0) x[grep("Cyprus",x)]<-"Cyprus"
if(length(grep("Cuba",x))>0) x[grep("Cuba",x)]<-"Cuba"
if(length(grep("caledonia",tolower(x)))>0) x[grep("caledonia",tolower(x))]<-"New Caledonia"
if(length(grep("curaxeao|curacao",tolower(x)))>0) x[grep("curaxeao|curacao",tolower(x))]<-"Curacao"
if(length(grep("burkina faso",tolower(x)))>0) x[grep("burkina faso",tolower(x))]<-"Burkina Faso"
if(length(grep("djibouti",tolower(x)))>0) x[grep("djibouti",tolower(x))]<-"Djibouti"
if(length(grep("dominican rep|dominicana",tolower(x)))>0) x[grep("dominican rep|dominicana",tolower(x))]<-"Dominican Rep."
if(length(grep("dominica$",tolower(x)))>0) x[grep("dominica$",tolower(x))]<-"Dominica"
if(length(grep("norway",tolower(x)))>0) x[grep("norway",tolower(x))]<-"Norway"
if(length(grep("sri lanka|srilanka",tolower(x)))>0) x[grep("sri lanka|srilanka",tolower(x))]<-"Sri Lanka"
if(length(grep("West Bank",x))>0) x[grep("West Bank",x)]<-"West Bank"
if(length(grep("barbados",tolower(x)))>0) x[grep("barbados",tolower(x))]<-"Barbados"
if(length(grep("colombia",tolower(x)))>0) x[grep("colombia",tolower(x))]<-"Colombia"
if(length(grep("uruguay",tolower(x)))>0) x[grep("uruguay",tolower(x))]<-"Uruguay"
if(length(grep("uzbekistan",tolower(x)))>0) x[grep("uzbekistan",tolower(x))]<-"Uzbekistan"
if(length(grep("estonia",tolower(x)))>0) x[grep("estonia",tolower(x))]<-"estonia"
if(length(grep("nepal",tolower(x)))>0) x[grep("nepal",tolower(x))]<-"Nepal"

x<-x[nchar(x)<=26]
# remove all non country only values
x<-x[is.element(x,.allCountries)]
return(x)
}

# vector with all country names from worldmap function
.allCountries<-c("Afghanistan","Angola","Albania","United Arab Emirates","Argentina","Armenia","Antarctica","Fr. S. and Antarctic Lands","Australia","Austria","Azerbaijan","Burundi","Belgium","Benin","Burkina Faso","Bangladesh","Bulgaria","Bahamas","Bosnia and Herz.","Belarus","Belize","Bolivia","Brazil","Brunei","Bhutan","Botswana","Central African Rep.","Canada","Switzerland","Chile","China","Ivory Coast","Cameroon","Congo (Kinshasa)","Congo (Brazzaville)","Colombia","Costa Rica","Cuba","N. Cyprus","Cyprus","Czech Rep.","Germany","Djibouti","Denmark","Dominican Rep.","Algeria","Ecuador","Egypt","Eritrea","Spain","Estonia","Ethiopia","Finland","Fiji","Falkland Is.","France","Gabon","United Kingdom","Georgia","Ghana","Guinea","Gambia","Guinea Bissau","Eq. Guinea","Greece","Greenland","Guatemala","Guyana","Honduras","Croatia","Haiti","Hungary","Indonesia","India","Ireland","Iran","Iraq","Iceland","Israel","Italy","Jamaica","Jordan","Japan","Kazakhstan","Kenya","Kyrgyzstan","Cambodia","S. Korea","Kosovo","Kuwait","Laos","Lebanon","Liberia","Libya","Sri Lanka","Lesotho","Lithuania","Luxembourg","Latvia","Morocco","Moldova","Madagascar","Mexico","Macedonia","Mali","Myanmar","Montenegro","Mongolia","Mozambique","Mauritania","Malawi","Malaysia","Namibia","New Caledonia","Niger","Nigeria","Nicaragua","Netherlands","Norway","Nepal","New Zealand","Oman","Pakistan","Panama","Peru","Philippines","Papua New Guinea","Poland","Puerto Rico","N. Korea","Portugal","Paraguay","Qatar","Romania","Russia","Rwanda","W. Sahara","Saudi Arabia","Sudan","S. Sudan","Senegal","Solomon Is.","Sierra Leone","El Salvador","Somalia","Serbia","Suriname","Slovakia","Slovenia","Sweden","Swaziland","Syria","Chad","Togo","Thailand","Tajikistan","Turkmenistan","East Timor","Trinidad and Tobago","Tunisia","Turkey","Taiwan","Tanzania","Uganda","Ukraine","Uruguay","United States","Uzbekistan","Venezuela","Vietnam","Vanuatu","West Bank","Yemen","South Africa","Zambia","Zimbabwe","Aruba","Anguilla","Aland","Andorra","American Samoa","Ashmore and Cartier Is.","Antigua and Barb.","Bahrain","St. Barthelemy","Bermuda","Barbados","Cook Is.","Comoros","Cape Verde","Curacao","Cayman Is.","Dominica","Faroe Is.","Micronesia","Gaza","Guernsey","Grenada","Guam","Hong Kong","Heard I. and McDonald Is.","Isle of Man","Indian Ocean Ter.","Br. Indian Ocean Ter.","Jersey","NA","Kiribati","St. Kitts and Nevis","Saint Lucia","Liechtenstein","Macau","St. Martin","Monaco","Maldives","Marshall Is.","Malta","N. Mariana Is.","Montserrat","Mauritius","Norfolk Island","Niue","Nauru","Pitcairn Is.","Palau","Fr. Polynesia","Singapore","S. Geo. and S. Sandw. Is.","Saint Helena","San Marino","St. Pierre and Miquelon","Sao Tome and Principe","Sint Maarten","Seychelles","Turks and Caicos Is.","Tonga","Vatican","St. Vin. and Gren.","British Virgin Is.","U.S. Virgin Is.","Wallis and Futuna","Samoa","Tuvalu","French Guiana")  

