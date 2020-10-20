#' letter.convert
#'
#' Convert and unify most hex and some html coded letters in text to unicode characters and correct CERMINE specific errors in captured statistical results.
#' @param x text to process
#' @param cermine Logical. If TRUE CERMINE specific error handling and letter conversion will be applied
#' @param greek2text Logical. If TRUE some greek letters and special characters will be unified to textual representation. (important to extract stats)
#' @param warning Logical. If TRUE prints warning massage if CERMINE specific letter conversion was performed
#' @export
#' @examples
#' x<-c("five &#x0003c; ten","five &lt; ten")
#' letter.convert(x)

# letter conversion
letter.convert<-function(x,cermine=FALSE,greek2text=FALSE,warning=TRUE){
# clean up white spaces
x<-gsub("^ *|(?<= ) | *$", "", x, perl = TRUE)
# unify -
x<-gsub("\u2013|\u2014|\u2015|\u2212|\u2010|\u2011","-",x)

# if has hex letters convert to simplified letter
if(length(grep("\\&#",x))>0){
## prepare
# add 0 behind &#x if has only 4 characters between "\\&#x" and ";" and lowerize capture
x<-gsub("\\&#[Xx](....);","\\&#x0\\L\\1;",x,perl=T)
# convert further upper to lower case letter in hex code
x<-gsub("\\&#[Xx](.....);","\\&#x\\L\\1;",x,perl=T)

## start conversion
# letters
if(length(grep("&#",x))>0){
x<-gsub("\\&#x00102;","\u0102",x) # Ă, LATIN CAPITAL LETTER A WITH BREVE 
x<-gsub("\\&#x000C1;","\u00C1",x) # Á 
x<-gsub("\\&#x000c1;","\u00c1",x) # Á 
x<-gsub("\\&#x000c4;","\u00c4",x) # Ä
x<-gsub("\\&#x000c6;","\u00c6",x) # AE &AElig;
x<-gsub("\\&#x00119;","\u0119",x) # e with little thingy: &eogon;
x<-gsub("\\&#x0015f;","\u015f",x) # s with little thingy: &scedil;
x<-gsub("\\&#x00105;","\u0105",x) # a with little thingy: &aogon;
x<-gsub("\\&#x000e4;","\u00e4",x) # ä
x<-gsub("\\&#x000dc;","\u00dc",x) # ü
x<-gsub("\\&#x000fc;","\u00fc",x) # ü
x<-gsub("\\&#x000f9;","\u00f9",x) # ù
x<-gsub("\\&#x000df;","\u00df",x) # ß
x<-gsub("\\&#x000e1;","\u00e1",x) # á
x<-gsub("\\&#x000e0;","\u00e0",x) # à
x<-gsub("\\&#x000e2;","\u00e2",x) # â a with hat
x<-gsub("\\&#x00103;","\u0103",x) # ă a with little u on top
x<-gsub("\\&#x00101;","\u0101",x) # a with bar
x<-gsub("\\&#x000e5;","\u00e5",x) # a with ° on top
x<-gsub("\\&#x000c3;","\u00C3",x) # a with ~ on top
x<-gsub("\\&#x000e6;","\u00E6",x) # a with e connected: &aelig;
x<-gsub("\\&#x0017C;","\u017C",x) # z with dot on top
x<-gsub("\\&#x0017c;","\u017C",x) # z with dot on top
x<-gsub("\\&#x0015b;","\u015B",x) # ś
x<-gsub("\\&#x00161;","\u0161",x) # s with turned ^ on top: &scaron;
x<-gsub("\\&#x0010d;","\u010d",x) # c with turned ^ on top: &ccaron;
x<-gsub("\\&#x000c7;","\u00C7",x) # C with little thingy: &Ccedil;
x<-gsub("\\&#x0010c;","\u010C",x) # Č, C with turned ^ on
x<-gsub("\\&#x00107;","\u0107",x) # ć
x<-gsub("\\&#x00108;","\u0108",x) # Ĉ, C with two dots
x<-gsub("\\&#x00109;","\u0109",x) # ĉ, c with hat
x<-gsub("\\&#x000e7;","\u00E7",x) # french c: &ccedil;
x<-gsub("\\&#x000c5;","\u00C5",x) # Angstrom letter: capital A with °
x<-gsub("\\&#x000e3;","\u00E3",x) # a with ~
x<-gsub("\\&#x000e8;","\u00E8",x) # è
x<-gsub("\\&#x000e9;","\u00E9",x) # é
x<-gsub("\\&#x0012b;","\u011B",x) # ě e with turned ^ on
x<-gsub("\\&#x0017b;","\u017B",x) # Ż, Z with dot on top
x<-gsub("\\&#x000d3;","\u00D3",x) # Ó O acute
x<-gsub("\\&#x00117;","\u0117",x) # ė e with point on top
x<-gsub("\\&#x000c9;","\u00C9",x) # É, e acute
x<-gsub("\\&#x000ea;","\u00EA",x) # ê
x<-gsub("\\&#x000ed;","\u00ED",x) # í i with '
x<-gsub("\\&#x000ec;","\u00EC",x) # ì i with `
x<-gsub("\\&#x000ee;","\u00EE",x) # î i with hat
x<-gsub("\\&#x000cd;","\u00CD",x) # Í 
x<-gsub("\\&#x00130;","\u0130",x) # I with a dot on top
x<-gsub("\\&#x00159;","\u0159",x) # r with turned ^on top ;  &rcaron;
x<-gsub("\\&#x00281;","\u02B6",x) # LATIN LETTER SMALL CAPITAL INVERTED R
x<-gsub("\\&#x0017e;","\u017E",x) # z with turned ^ on top: &zcaron;
x<-gsub("\\&#x0015a;","\u015A",x) # Ś with '
x<-gsub("\\&#x00160;","\u0160",x) # S with turned ^ on top: &Scaron;
x<-gsub("\\&#x0015e;","\u015E",x) # S with little thingy below: &Scedil;
x<-gsub("\\&#x01e6a;","\u1E6A",x) # Ṫ LATIN CAPITAL LETTER T WITH DOT ABOVE
x<-gsub("\\&#x0021b;","\u021B",x) # t with coma below ț
x<-gsub("\\&#x0016b;","\u016B",x) # u with bar
x<-gsub("\\&#x00219;","\u0219",x) # ș s with 'below
x<-gsub("\\&#x00141;","\u0141",x) # Ł: L with little / &Lstrok; Ł
x<-gsub("\\&#x00142;","\u0141",x) # ł: l with little / &lstrok;
x<-gsub("\\&#x000ef;","\u00EF",x) # iuml - ï - i with two dots on top
x<-gsub("\\&#x0feff;","",x) # empty seperator
}

if(length(grep("&#",x))>0){
x<-gsub("\\&#x00131;","\u0131",x) # &imath;
x<-gsub("\\&#x0011f;","\u011F",x) # g with small u on top: &gbreve;
x<-gsub("\\&#x000d6;","\u00D6",x) # Ö
x<-gsub("\\&#x000f6;","\u00F6",x) # ö
x<-gsub("\\&#x00151;","\u00F6 ",x) # small ö &odblac;
x<-gsub("\\&#x000f8;","\u00F8",x) # ø: danish ö
x<-gsub("\\&#x00153;","\u0153",x) # oe: &oelig;
x<-gsub("\\&#x00152;","\u0152",x) # OE: &OElig;
x<-gsub("\\&#x000d8;","\u00D8",x) # Ø: danish Ö
x<-gsub("\\&#x000f3;","\u00F3",x) # ó
x<-gsub("\\&#x000f5;","\u00F5",x) # o tilde on top
x<-gsub("\\&#x000f2;","\u00F2",x) # ò
x<-gsub("\\&#x000f4;","\u00F4",x) # ô
x<-gsub("\\&#x000fd;","\u00FD",x) # ý
x<-gsub("\\&#x00176;","\u0176",x) # Y hat
x<-gsub("\\&#x003a5;","\u03a5",x) # Y 
x<-gsub("\\&#x0017a;","\u017A",x) # ź
x<-gsub("\\&#x000fa;","\u00FA",x) # ú
x<-gsub("\\&#x0016f;","\u016F",x) # u with ° on top
x<-gsub("\\&#x000f1;","\u00F1",x) # n-je ñ
x<-gsub("\\&#x05358;","\u00F1",x) # n-je ñ
x<-gsub("\\&#x00144;","\u0144",x) # ń
x<-gsub("\\&#x000eb;","\u00EB",x) # ë
# quotes
x<-gsub("\\&#x002bc;","'",x) # '
x<-gsub("\\&#x00027;","'",x) # '
x<-gsub("\\&#x02018;","'",x) # quotation start
x<-gsub("\\&#x02019;","'",x) # quotation end
x<-gsub("\\&#x0201c;","'",x) # quotation start
x<-gsub("\\&#x0201d;","'",x) # quotation end
x<-gsub("\\&#x02019;","'",x) # '
x<-gsub("\\&#x000b4;","'",x) # '
x<-gsub("\\&#x000bb;","'",x) # >>
x<-gsub("\\&#x000ab;","'",x) # <<
x<-gsub("\\&#x0201e;","'",x) # lower "
x<-gsub("\\&#x02019;","'",x) # right single quotation mark
x<-gsub("\\&#x02018;","'",x) # left single quotation mark
# spaces
x<-gsub("\\&#x000a0;"," ",x) # non breaking space
x<-gsub("\\&#x02011;"," ",x) # non breaking hyphen
x<-gsub("\\&#x02009;"," ",x) # thin space
x<-gsub("\\&#x02029;"," ",x) # thin space
x<-gsub("\\&#x0200a;"," ",x) # space
x<-gsub("\\&#x02002;"," ",x) # space
x<-gsub("\\&#x0200e;"," ",x) # space
x<-gsub("\\&#x02005;"," ",x) # space
x<-gsub("\\&#x02008;"," ",x) # space
x<-gsub("\\&#x02007;"," ",x) # space
x<-gsub("\\&#x02006;"," ",x) # space
x<-gsub("\\&#x0202f;"," ",x) # space
x<-gsub("\\&#x02003;"," ",x) # em space
x<-gsub("\\&#x02001;"," ",x) # space
x<-gsub("\\&#x02000;"," ",x) # space
x<-gsub("\\&#x02004;"," ",x) # space
x<-gsub("\\&#x02028;"," ",x) # line seperator
x<-gsub("\\&#x0200b;"," ",x) #  ZERO WIDTH SPACE
# special signs
x<-gsub("\\&#x000ae;","\u00ae",x) #  REG-sign
x<-gsub("\\&#x000a9;","\u00a9",x) #  COPYRIGHT-sign
x<-gsub("\\&#x02122;","\u2122",x) #  TradeMark-sign
x<-gsub("\\&#x020ac;","\u20ac",x) #  Euro-sign
x<-gsub("\\&#x000a3;","\u00a3",x) #  Pound-sign
x<-gsub("\\&#x000a3;","\u00a5",x) #  Yen-sign
x<-gsub("\\&#x00024;","\u0024",x) #  Dollar-sign
x<-gsub("\\&#x02248;","\u2248",x) # approximate sign
x<-gsub("\\&#x0221e;","\u221e",x) # infinite sign
x<-gsub("\\&#x0003f;","?",x) # question mark
x<-gsub("\\&#x0002b;","+",x) # high +
x<-gsub("\\&#x000b1;","+-",x) # plus minus
x<-gsub("\\&#x02212;","-",x) # minus
# seperators
x<-gsub("\\&#x02013;","-",x) # until/dash
x<-gsub("\\&#x02010;","-",x) # hyphen/dash
x<-gsub("\\&#x000ad;","-",x) # soft hyphen
x<-gsub("\\&#x02010;","-",x) # hyphen
x<-gsub("\\&#x02014;","-",x) # mdash -
x<-gsub("\\&#x02022;"," - ",x) # bullet point
x<-gsub("\\&#x025e6;"," - ",x) # white bullet
x<-gsub("\\&#x025aa;"," - ",x) # bullet square
x<-gsub("\\&#x02012;"," - ",x) # FIGURE DASH
x<-gsub("\\&#x02219;"," - ",x) # BULLET OPERATOR (in categories)
x<-gsub("\\&#x025cf;"," - ",x) # BLACK CIRCLE (in categories)
x<-gsub("\\&#x025cb;"," - ",x) # circle
x<-gsub("\\&#x000b7;"," - ",x) #  mitdot
x<-gsub("\\&#x022c5;"," - ",x) #  sdot
# special signs
x<-gsub("\\&#x0fb01;","\ufb01",x) # fi
x<-gsub("\\&#x0fb04;","\ufb04",x) # ffl
x<-gsub("\\&#x02460;","\u2460",x) # 1 in circle
x<-gsub("\\&#x02461;","\u2461",x) # 2 in circle
x<-gsub("\\&#x02462;","\u2462",x) # 3 in circle
x<-gsub("\\&#x02463;","\u2463",x) # 4 in circle
x<-gsub("\\&#x02464;","\u2464",x) # 5 in circle
x<-gsub("\\&#x0223c;","~",x) # tilde
x<-gsub("\\&#x002dc;","~",x) # tilde
x<-gsub("\\&#x0003d;","=",x) # equals
x<-gsub("\\&#x02261;","=",x) # identical to
x<-gsub("\\&#x02245;","=~",x) # tilde full equal
x<-gsub("\\&#x00025;","\u0025",x) # percent
x<-gsub("\\&#x02030;","\u2030",x) # per mille
x<-gsub("\\&#x00026;","& ",x) #  &
x<-gsub("\\&#x00023;","# ",x) #  # , num
x<-gsub("\\&#x02026;","...",x) # ...
x<-gsub("\\&#x02192;","->",x)# rightwards arrow
x<-gsub("\\&#x025b8;","->",x)# rightwards arrow
x<-gsub("\\&#x02190;","<-",x)# leftwards arrow

x<-gsub("\\&#x02297;","\u2297",x) # circle with cross
x<-gsub("\\&#x02236;",":",x) # ratio
x<-gsub("\\&#x0003d;","=",x) # equal sign
x<-gsub("\\&#x0003c;","<",x) # less than
x<-gsub("\\&#x03008;","<",x) # less than
x<-gsub("\\&#x02329;","<",x) # less than
x<-gsub("\\&#x0226a;","<<",x) # much less than
x<-gsub("\\&#x02264;","<=",x) # less equal than
x<-gsub("\\&#x02a7d;","<=",x) # less equal than
x<-gsub("\\&#x0003e;",">",x) # greater than
x<-gsub("\\&#x0232a;",">",x) # greater than
x<-gsub("\\&#x03009;",">",x) # greater than
x<-gsub("\\&#x02a7e;",">=",x) # greater equal than
x<-gsub("\\&#x02267;",">=",x) # greater equal than
x<-gsub("\\&#x0226b;",">>",x) # much greater than
x<-gsub("\\&#x02260;","!=",x) # not greater 
x<-gsub("\\&#x02265;","<",x) # not greater equal than
x<-gsub("\\&#x00026;","&",x) # and
x<-gsub("\\&#x025ba;","->",x) # pointer right: ->
x<-gsub("\\&#x0002f;","/",x) # solidus /
x<-gsub("\\&#x000b2;","^2",x) # superscript 2
x<-gsub("\\&#x02033;","'",x) # double prime
x<-gsub("\\&#x02034;","'",x) # double prime end

}

if(length(grep("&#",x))>0){
# greek/latin/cyrillic characters
# alpha 
x<-gsub("\\&#x003b1;","\u03b1",x) # small greek alpha
x<-gsub("\\&#x00251;","\u0251",x) # small latin letter alpha
x<-gsub("\\&#x0221d;","\u221d",x) # PROPORTIONAL TO
# beta
x<-gsub("\\&#x003b2;","\u03b2",x) # small greek beta
x<-gsub("\\&#x00392;","\u0392",x) # capital greek beta
# chi
x<-gsub("\\&#x003c7;","\u03c7",x) # small greek chi
x<-gsub("\\&#x1d74c;","\u03c7",x) # bold italic small greek chi -> small greek chi
x<-gsub("\\&#x003a7;","\u03a7",x) # capital greek Chi
# phi etc
x<-gsub("\\&#x003c6;","\u03c6",x) # small greek phi
x<-gsub("\\&#x003a6;","\u03a6",x) # capital greek Phi
x<-gsub("\\&#x00424;","\u0424",x) # CYRILLIC CAPITAL LETTER EF -> looks like Phi
x<-gsub("\\&#x003d5;","\u03d5",x) # capital greek Phi
x<-gsub("\\&#x00278;","\u0278",x) # capital greek Phi
x<-gsub("\\&#x003a8;","\u03a8",x) # capital greek Psi
x<-gsub("\\&#x003c8;","\u03c8",x) # small greek Psi
x<-gsub("\\&#x00393;","\u0393",x) # capital greek Gamma
x<-gsub("\\&#x003c0;","\u03c0",x) # small greek pi
x<-gsub("\\&#x003af;","\u03af",x) # GREEK SMALL LETTER IOTA WITH TONOS
x<-gsub("\\&#x003bc;","\u03bc",x) # small greek mu
x<-gsub("\\&#x000b5;","\u00b5",x) # micro/small greek mu
x<-gsub("\\&#x003a3;","\u03a3",x) # Sum/capital greek sigma
x<-gsub("\\&#x02211;","\u2211",x) # Sum/capital greek sigma
x<-gsub("\\&#x0028a;","\u028a",x) # Latin upsilon
x<-gsub("\\&#x001b1;","\u01b1",x) #  LATIN CAPITAL LETTER UPSILON

x<-gsub("\\&#x003c5;","\u03c5",x) # GREEK SMALL LETTER UPSILON
x<-gsub("\\&#x00254;","\u0254",x) # LATIN SMALL LETTER OPEN O
x<-gsub("\\&#x003c3;","\u03c3",x) # small greek sigma
x<-gsub("\\&#x003c2;","\u03c2",x) # GREEK SMALL LETTER FINAL SIGMA
x<-gsub("\\&#x003c9;","\u03c9",x) # small greek omega
x<-gsub("\\&#x003ce;","\u03ce",x) # GREEK SMALL LETTER OMEGA WITH TONOS
x<-gsub("\\&#x003bb;","\u03bb",x) # small greek lamda
x<-gsub("\\&#x003b4;","\u03b4",x) # small greek delta
x<-gsub("\\&#x02206;","\u2206",x) # capital greek delta (increment) triangle
x<-gsub("\\&#x025b5;","\u25b5",x) # utri (triangle)
x<-gsub("\\&#x025b3;","\u25b3",x) # WHITE UP-POINTING TRIANGLE
x<-gsub("\\&#x022bf;","\u22bf",x) # italic triangle
x<-gsub("\\&#x00394;","\u0394",x) #  delta-sign

x<-gsub("\\&#x003b5;","\u03b5",x) # small greek epsilon (epsi)
x<-gsub("\\&#x0025b;","\u025b",x) # LATIN SMALL LETTER OPEN E
x<-gsub("\\&#x1d700;","\u03b5",x) # MATHEMATICAL ITALIC SMALL EPSILON 
x<-gsub("\\&#x003ad;","\u03ad",x) # GREEK SMALL LETTER EPSILON WITH TONOS 
x<-gsub("\\&#x003ba;","\u03ba",x) # small greek kappa
x<-gsub("\\&#x003f0;","\u03f0",x) # cursive kappa -> used as Chi
x<-gsub("\\&#x003b3;","\u03b3",x)# small greek gamma
x<-gsub("\\&#x003be;","\u03be",x) # small greek Xi 
x<-gsub("\\&#x0039e;","\u039e",x) # GREEK CAPITAL LETTER XI 
x<-gsub("\\&#x003A4;","\u03A4",x) # capital greek Tau
x<-gsub("\\&#x003c4;","\u03c4",x) # small greek tau 
x<-gsub("\\&#x0028c;","\u028c",x) # LATIN SMALL LETTER TURNED V
x<-gsub("\\&#x003c1;","\u03c1",x) # small greek rho
x<-gsub("\\&#x003b8;","\u03b8",x) # small greek theta
x<-gsub("\\&#x003d1;","\u03d1",x) # small greek theta
x<-gsub("\\&#x00398;","\u0398",x) # capital greek Theta
x<-gsub("\\&#x1d703;","\ud703",x) # capital greek Theta
x<-gsub("\\&#x003b6;","\u03b6",x) # capital greek Zeta
x<-gsub("\\&#x003a9;","\u03a9",x) # capital greek Omega (Ohm)
x<-gsub("\\&#x02126;","\u2126",x) # capital greek Omega (Ohm)
x<-gsub("\\&#x003bd;","\u03bd",x) # GREEK SMALL LETTER NU 
x<-gsub("\\&#x0039b;","\u039b",x) #  CAPITAL GREEK Lambda

x<-gsub("\\&#x003b7;","\u03b7",x) # eta
x<-gsub("\\&#x0014b;","\u014b",x) # engma for eta
x<-gsub("\\&#x00273;","\u0273",x) # eta
x<-gsub("\\&#x00220;","\u0220",x) # eta
x<-gsub("\\&#x0019e;","\u019e",x) # eta

x<-gsub("\\#&#x001b1;","\u01b1",x) # latin upsilon

x<-gsub("\\&#x02032;","'",x) # prime
x<-gsub("\\&#x000b0;","\u00B0",x) # degree sign
x<-gsub("\\&#x02103;","\u00B0",x) # degree Celsius

x<-gsub("\\&#x000bd;","\u00bd",x) # one half
x<-gsub("\\&#x000bc;","\u00bc",x) # one fourth
x<-gsub("\\&#x000be;","\u00be",x) # Three quarter

x<-gsub("\\&#x000d7;","*",x) # multiplied by
x<-gsub("\\&#x02217;","*",x) # times
x<-gsub("\\&#x02606;","*",x) # white star
x<-gsub("\\&#x02605;","*",x) # black star
x<-gsub("\\&#x0002a;","*",x) # asterix

x<-gsub("\\&#x000ba;","\u00B0",x) # MASCULINE ORDINAL INDICATOR 
x<-gsub("\\&#x02218;","\u00B0",x) # RING OPERATOR
x<-gsub("\\&#x02020;","\u2020",x) # daggar (died)
x<-gsub("\\&#x02021;","\u2021",x) # double daggar (died)
x<-gsub("\\&#x020de;"," ",x) # square
x<-gsub("\\&#x025a1;"," ",x) # white square
x<-gsub("\\&#x02225;","\u2225",x) # parallel to
x<-gsub("\\&#x02228;","\u2228",x) # &vee;
x<-gsub("\\&#x0221a;","\u221a",x) # square root
x<-gsub("\\&#x000af;","\u00af",x) # superscripted ¯
x<-gsub("\\&#x02202;","\u2202",x) # &PartialD;
x<-gsub("\\&#x022ef;","\u22EF",x) # MIDLINE HORIZONTAL ELLIPSIS 
x<-gsub("\\&#x02113;","l",x) # &ell;
x<-gsub("\\&#x02016;","\u2016",x) # Verbar
x<-gsub("\\&#x002d9;","'",x) # &DiacriticalDot;
x<-gsub("\\&#x005f3;","'",x) # HEBREW PUNCTUATION GERESH

x<-gsub("\\&#x0210b;","\u210b",x) # HilbertSpace
x<-gsub("\\&#x02208;","\u2208",x) #  Element of
x<-gsub("\\&#x02229;","\u2229",x) #  INTERSECTION
x<-gsub("\\&#x0211d;","\u211d",x) #  DOUBLE-STRUCK CAPITAL R

x<-gsub("\\&#x02205;","\u2205",x) #  emptyset
x<-gsub("\\&#x0220f;","\u220f",x) #  Sum Product 
x<-gsub("\\&#x0222b;","\u222b",x) #  Integral
x<-gsub("\\&#x00283;","\u0283",x) #  LATIN SMALL LETTER ESH/Integral
                      
x<-gsub("\\&#x00138;","\u0138",x) # &kgreen;
x<-gsub("\\&#x02061;","",x) # FUNCTION APPLICATION 
x<-gsub("\\&#x02131;","F",x) # SCRIPT CAPITAL F

x<-gsub("\\&#x02062;"," ",x) # INVISIBLE TIMES 
x<-gsub("\\&#x003f5;","\u03f5",x) # &varepsilon; (Element of)
x<-gsub("\\&#x02223;","\u2223",x) # &mid;
x<-gsub("\\&#x002c6;","^",x) # circ
x<-gsub("\\&#x02208;","\u2208",x) # element of
x<-gsub("\\&#x02209;","\u2209",x) # not element of
x<-gsub("\\&#x02215;","/",x) # divided by
x<-gsub("\\&#x000f7;","/",x) # DIVISION SIGN
x<-gsub("\\&#x02227;","\u2227",x) # &and;
x<-gsub("\\&#x000ac;","\u00ac",x) # NOT-sign
x<-gsub("\\&#x02200;","\u2200",x) # ∀ for all-sign
x<-gsub("\\&#x02194;","\u2194",x) # Left right arrow
x<-gsub("\\&#x02193;","\u2193",x) # downwards arrow
x<-gsub("\\&#x02191;","\u2191",x) # upwards arrow
x<-gsub("\\&#x0025c;","\u025c",x) # LATIN SMALL LETTER REVERSED OPEN E
x<-gsub("\\&#x02713;","\u2713",x) # Hook
x<-gsub("\\&#x02670;","\u2670",x) # WEST SYRIAC CROSS 

x<-gsub("\\&#x000a5;","\u00a5",x) # Yen sign
x<-gsub("\\&#x000b6;","\u00b6",x) # English name pilcrow or paragraph mark
x<-gsub("\\&#x02642;"," MALE ",x) # Male sign
x<-gsub("\\&#x02640;"," FEMALE ",x) # Female sign
}


if(length(grep("&#",x))>0){
x<-gsub("\\&#x00053;","'*S?",x) # superscript *S
x<-gsub("\\&#x005f3;","'",x) #
x<-gsub("\\&#x02033;","'",x) # double quote
x<-gsub("\\&#x00384;","'",x) #
x<-gsub("\\&#x0002a;","*",x) #
x<-gsub("\\&#x00391;","\u0391",x) #  A
x<-gsub("\\&#x000c4;","\u00C4",x) # Ä
x<-gsub("\\&#x00392;","B",x) # Latin B
x<-gsub("\\&#x02113;","L",x) # Latin L
x<-gsub("\\&#x02011;","-",x) #
x<-gsub("\\&#x000af;","-",x) #
x<-gsub("\\&#x000ad;","-",x) #
x<-gsub("\\&#x02666;","-",x) # bullet point like hyper reference
x<-gsub("\\&#x02012;","-",x) #
x<-gsub("\\&#x02015;","-",x) #
x<-gsub("\\&#x002d9;","-",x) #
x<-gsub("\\&#x000b6;","\u00A7",x) # § section sign
x<-gsub("\\&#x000ab;","<<",x) #
x<-gsub("\\&#x000bb;",">>",x) #
x<-gsub("\\&#x000bd;"," 1/2",x) # a half
x<-gsub("\\&#x0043a;","\u043a",x) # kyrillik letter ka

x<-gsub("\\&#x0025b;","\u025b",x) # greek letter epsilon 
x<-gsub("\\&#x003c4;","\u03c4",x) # greek small letter tau
x<-gsub("\\&#x003a9;","\u03a9",x) # greek captial leter Omega
x<-gsub("\\&#x003b9;","\u03b9",x) # greek letter iota
x<-gsub("\\&#x003bd;","\u03bd",x) # greek neutrum
x<-gsub("\\&#x00029;",")",x) # )
x<-gsub("\\&#x00028;","(",x) # (
x<-gsub("\\&#x000c3;","\u00c3",x) # a with ^
x<-gsub("\\&#x000ee;","\u00ee",x) # i with ^
x<-gsub("\\&#x0014d;","\u014d",x) # o with bar 
x<-gsub("\\&#x000d4;","\u00d4",x) # o with ^
x<-gsub("\\&#x000bf;","\u00BF",x) # spanish start of question mark ¿
x<-gsub("\\&#x00399;","I",x) # Capital I
x<-gsub("\\&#x02912;","->",x) # rightwards arrow ->
x<-gsub("\\&#x000A0;","\u00A0",x) # no break space
x<-gsub("\\&#x0007c;","\u007c",x) # vertical line

}

# newly detected
if(length(grep("&#",x))>0){
x<-gsub("\\&#x0005f;","\u005f",x) # low line
x<-gsub("\\&#x000a2;","\u00a2",x) # cent sign
x<-gsub("\\&#x000a8;","\u00a8",x) # DIAERESIS
x<-gsub("\\&#x00112;","\u0112",x) # LATIN CAPITAL LETTER E WITH MACRON
x<-gsub("\\&#x00192;","\u0192",x) #  LATIN SMALL LETTER F WITH HOOK
x<-gsub("\\&#x00194;","\u0194",x) # LATIN CAPITAL LETTER GAMMA
x<-gsub("\\&#x001a9;","\u01a9",x) # LATIN CAPITAL LETTER ESH
x<-gsub("\\&#x001bf;","\u01bf",x) # LATIN LETTER WYNN
x<-gsub("\\&#x00282;","\u0282",x) # LATIN SMALL LETTER S WITH HOOK
x<-gsub("\\&#x002c2;","<",x) # MODIFIER LETTER LEFT ARROWHEAD
x<-gsub("\\&#x002c7;","\u02c7",x) # CARON
x<-gsub("\\&#x00305;","\u0305",x) # COMBINING OVERLINE
x<-gsub("\\&#x00307;","\u0307",x) # COMBINING DOT ABOVE
x<-gsub("\\&#x00395;","\u0395",x) #  GREEK CAPITAL LETTER EPSILON
x<-gsub("\\&#x0039a;","\u039a",x) # GREEK CAPITAL LETTER KAPPA
x<-gsub("\\&#x003a0;","\u03a0",x) # GREEK CAPITAL LETTER PI
x<-gsub("\\&#x003a4;","\u03a4",x) # GREEK CAPITAL LETTER TAU
x<-gsub("\\&#x003ae;","\u03ae",x) # GREEK SMALL LETTER ETA WITH TONOS
x<-gsub("\\&#x003d0;","\u03d0",x) # GREEK BETA SYMBOL
x<-gsub("\\&#x003d2;","\u03d2",x) # GREEK UPSILON WITH HOOK SYMBOL
x<-gsub("\\&#x003d6;","\u03d6",x) # GREEK PI SYMBOL
x<-gsub("\\&#x01d68;","\u1d68",x) # GREEK SUBSCRIPT SMALL LETTER RHO
x<-gsub("\\&#x01e57;","\u1e57",x) # LATIN SMALL LETTER P WITH DOT ABOVE
x<-gsub("\\&#x01f75;","\u1f75",x) # GREEK SMALL LETTER ETA WITH OXIA
x<-gsub("\\&#x02025;","\u2025",x) # TWO DOT LEADER
x<-gsub("\\&#x0203e;","\u203e",x) # OVERLINE
x<-gsub("\\&#x02044;","/",x) # FRACTION SLASH
x<-gsub("\\&#x0205f;"," ",x) # MEDIUM MATHEMATICAL SPACE
x<-gsub("\\&#x02115;","\u2115",x) # DOUBLE-STRUCK CAPITAL N
x<-gsub("\\&#x02119;","\u2119",x) # DOUBLE-STRUCK CAPITAL P
x<-gsub("\\&#x0211c;","R",x) # BLACK-LETTER CAPITAL R
x<-gsub("\\&#x02124;","\u2124",x) # DOUBLE-STRUCK CAPITAL Z
x<-gsub("\\&#x021a6;","\u21a6",x) # RIGHTWARDS ARROW FROM BAR
x<-gsub("\\&#x021d2;","\u21d2",x) # RIGHTWARDS DOUBLE ARROW
x<-gsub("\\&#x021d4;","\u21d4",x) # LEFT RIGHT DOUBLE ARROW
x<-gsub("\\&#x02203;","\u2203",x) # THERE EXISTS
x<-gsub("\\&#x02207;","\u2207",x) # NABLA
x<-gsub("\\&#x0222a;","\u222a",x) # union set operator
x<-gsub("\\&#x02243;","\u2243",x) # ASYMPTOTICALLY EQUAL TO
x<-gsub("\\&#x02250;","\u2250",x) # APPROACHES THE LIMIT
x<-gsub("\\&#x02254;","\u2254",x) # COLON EQUALS
x<-gsub("\\&#x0225c;","\u225c",x) # DELTA EQUAL TO
x<-gsub("\\&#x02266;","\u2266",x) # LESS-THAN OVER EQUAL TO
x<-gsub("\\&#x0227a;","\u227a",x) # PRECEDES
x<-gsub("\\&#x0227b;","\u227b",x) # SUCCEEDS
x<-gsub("\\&#x02282;","\u2282",x) # SUBSET OF
x<-gsub("\\&#x02286;","\u2286",x) # SUBSET OF OR EQUAL TO
x<-gsub("\\&#x02295;","\u2295",x) # CIRCLED PLUS
x<-gsub("\\&#x02297;","\u2297",x) # CIRCLED TIMES
x<-gsub("\\&#x02299;","\u2299",x) # CIRCLED DOT OPERATOR
x<-gsub("\\&#x022a4;","\u22a4",x) # DOWN TACK
x<-gsub("\\&#x022a5;","\u22a5",x) # UP TACK
x<-gsub("\\&#x022b3;","\u22b3",x) # CONTAINS AS NORMAL SUBGROUP
x<-gsub("\\&#x022ba;","\u22ba",x) # INTERCALATE
x<-gsub("\\&#x022c0;","\u22c0",x) # N-ARY LOGICAL AND
x<-gsub("\\&#x022c3;","\u22c3",x) # N-ARY UNION
x<-gsub("\\&#x022c6;","*",x) # STAR OPERATOR
x<-gsub("\\&#x022ee;","\u22ee",x) # VERTICAL ELLIPSIS
x<-gsub("\\&#x022f1;","\u22f1",x) # DOWN RIGHT DIAGONAL ELLIPSIS
x<-gsub("\\&#x0230b;","\u230b",x) # RIGHT FLOOR
x<-gsub("\\&#x0231d;","\u231d",x) # TOP RIGHT CORNER
x<-gsub("\\&#x02322;","\u2322",x) # FROWN
x<-gsub("\\&#x025b7;","\u25b7",x) # WHITE RIGHT-POINTING TRIANGLE
x<-gsub("\\&#x025b9;","\u25b9",x) # WHITE RIGHT-POINTING SMALL TRIANGLE
x<-gsub("\\&#x02661;","\u2661",x) # WHITE HEART SUIT
x<-gsub("\\&#x0266f;","#",x) #  MUSIC SHARP SIGN
x<-gsub("\\&#x027e9;","\u27e9",x) # MATHEMATICAL RIGHT ANGLE BRACKET
x<-gsub("\\&#x027f6;","\u27f6",x) # LONG RIGHTWARDS ARROW
x<-gsub("\\&#x0a64d;","\ua64d",x) # CYRILLIC SMALL LETTER BROAD OMEGA
x<-gsub("\\&#x0fe37;","\ufe37",x) # PRESENTATION FORM FOR VERTICAL LEFT CURLY BRACKET
x<-gsub("\\&#x0fe38;","\ufe38",x) # PRESENTATION FORM FOR VERTICAL RIGHT CURLY BRACKET
}

# newly detected 2
if(length(grep("&#",x))>0){
x<-gsub("\\&#x00049;","I",x) # LATIN CAPITAL LETTER I
x<-gsub("\\&#x00056;","V",x) #  LATIN CAPITAL LETTER V
x<-gsub("\\&#x00058;","X",x) # LATIN CAPITAL LETTER X
x<-gsub("\\&#x000a7;","\u00a7",x) # SECTION SIGN
x<-gsub("\\&#x000b3;","^3",x) # SUPERSCRIPT THREE
x<-gsub("\\&#x000b8;","\u00b8",x) # CEDILLA
x<-gsub("\\&#x000c2;","\u00c2",x) # LATIN CAPITAL LETTER A WITH CIRCUMFLEX
x<-gsub("\\&#x000ce;","\u00ce",x) # LATIN CAPITAL LETTER I WITH CIRCUMFLEX
x<-gsub("\\&#x000dd;","\u00dd",x) # LATIN CAPITAL LETTER Y WITH ACUTE
x<-gsub("\\&#x00100;","\u0100",x) # LATIN CAPITAL LETTER A WITH MACRON
x<-gsub("\\&#x00113;","\u0113",x) # LATIN SMALL LETTER E WITH MACRON
x<-gsub("\\&#x0011b;","\u011b",x) # LATIN SMALL LETTER E WITH CARON
x<-gsub("\\&#x00124;","\u0124",x) # LATIN CAPITAL LETTER H WITH CIRCUMFLEX
x<-gsub("\\&#x00175;","\u0175",x) # LATIN SMALL LETTER W WITH CIRCUMFLEX
x<-gsub("\\&#x00190;","\u0190",x) # LATIN CAPITAL LETTER OPEN E
x<-gsub("\\&#x00198;","\u0198",x) # LATIN CAPITAL LETTER K WITH HOOK
x<-gsub("\\&#x0019f;","\u019f",x) # LATIN CAPITAL LETTER O WITH MIDDLE TILDE
x<-gsub("\\&#x001c0;","I",x) # LATIN LETTER DENTAL CLICK
x<-gsub("\\&#x001ce;","\u01ce",x) # LATIN SMALL LETTER A WITH CARON
x<-gsub("\\&#x00212;","\u0212",x) #  LATIN CAPITAL LETTER R WITH INVERTED BREVE
x<-gsub("\\&#x00245;","\u0245",x) # LATIN CAPITAL LETTER TURNED V
x<-gsub("\\&#x00255;","\u0255",x) #  LATIN SMALL LETTER C WITH CURL
x<-gsub("\\&#x00261;","\u0261",x) # LATIN SMALL LETTER SCRIPT G
x<-gsub("\\&#x00263;","\u0263",x) # LATIN SMALL LETTER GAMMA
x<-gsub("\\&#x00264;","\u0264",x) # LATIN SMALL LETTER RAMS HORN
x<-gsub("\\&#x0027e;","\u027e",x) # LATIN SMALL LETTER R WITH FISHHOOK
x<-gsub("\\&#x0027f;","\u027f",x) # LATIN SMALL LETTER REVERSED R WITH FISHHOOK
x<-gsub("\\&#x00285;","\u0285",x) # LATIN SMALL LETTER SQUAT REVERSED ESH
x<-gsub("\\&#x0029d;","\u029d",x) # LATIN SMALL LETTER J WITH CROSSED-TAIL
x<-gsub("\\&#x002a4;","\u02a4",x) #  LATIN SMALL LETTER DEZH DIGRAPH
x<-gsub("\\&#x002c3;",">",x) # MODIFIER LETTER RIGHT ARROWHEAD
x<-gsub("\\&#x002c4;","^",x) # MODIFIER LETTER UP ARROWHEAD
x<-gsub("\\&#x002c8;","'",x) # MODIFIER LETTER VERTICAL LINE
x<-gsub("\\&#x002d7;","-",x) # MODIFIER LETTER MINUS SIGN
x<-gsub("\\&#x002d8;","\u02d8",x) # BREVE
x<-gsub("\\&#x002da;","\u02da",x) # RING ABOVE
x<-gsub("\\&#x00301;","'",x) # COMBINING ACUTE ACCENT
x<-gsub("\\&#x00303;","\u0303",x) # COMBINING TILDE
x<-gsub("\\&#x0037e;",";",x) # GREEK QUESTION MARK look alike semicolun
x<-gsub("\\&#x0041a;","\u041a",x) # CYRILLIC CAPITAL LETTER KA
x<-gsub("\\&#x0041f;","\u041f",x) # CYRILLIC CAPITAL LETTER PE
x<-gsub("\\&#x00440;","\u0440",x) # CYRILLIC SMALL LETTER ER look alike p
x<-gsub("\\&#x00445;","\u0445",x) # CYRILLIC SMALL LETTER HA
x<-gsub("\\&#x01d02;","\u1d02",x) # LATIN SMALL LETTER TURNED AE
x<-gsub("\\&#x01d26;","\u1d26",x) # GREEK LETTER SMALL CAPITAL GAMMA
x<-gsub("\\&#x01d27;","\u1d27",x) # GREEK LETTER SMALL CAPITAL LAMDA
x<-gsub("\\&#x01d52;","\u1d52",x) # MODIFIER LETTER SMALL O
x<-gsub("\\&#x01d61;","\u1d61",x) # MODIFIER LETTER SMALL CHI
x<-gsub("\\&#x01e7d;","\u1e7d",x) # LATIN SMALL LETTER V WITH TILDE
x<-gsub("\\&#x01e8f;","\u1e8f",x) # LATIN SMALL LETTER Y WITH DOT ABOVE
x<-gsub("\\&#x01e9e;","\u1e9e",x) # LATIN CAPITAL LETTER SHARP S
x<-gsub("\\&#x02070;","\u2070",x) # SUPERSCRIPT ZERO
x<-gsub("\\&#x02080;","\u2080",x) # SUBSCRIPT ZERO
x<-gsub("\\&#x020a9;","\u20a9",x) # WON SIGN
x<-gsub("\\&#x020d7;","\u20d7",x) # COMBINING RIGHT ARROW ABOVE
x<-gsub("\\&#x02102;","\u2102",x) #  DOUBLE-STRUCK CAPITAL C
x<-gsub("\\&#x0210f;","\u210f",x) # PLANCK CONSTANT OVER TWO PI
x<-gsub("\\&#x02110;","\u2110",x) # SCRIPT CAPITAL I
x<-gsub("\\&#x02111;","\u2111",x) # BLACK-LETTER CAPITAL I
x<-gsub("\\&#x02112;","\u2112",x) # SCRIPT CAPITAL L
x<-gsub("\\&#x02118;","\u2118",x) # SCRIPT CAPITAL P
x<-gsub("\\&#x0211b;","\u211b",x) # SCRIPT CAPITAL R
x<-gsub("\\&#x02127;","\u2127",x) # INVERTED OHM SIGN
x<-gsub("\\&#x02133;","\u2133",x) # SCRIPT CAPITAL M
x<-gsub("\\&#x02153;","1/3",x) # VULGAR FRACTION ONE THIRD
x<-gsub("\\&#x02198;","\u2198",x) # SOUTH EAST ARROW
x<-gsub("\\&#x0220a;","\u220a",x) # SMALL ELEMENT OF
x<-gsub("\\&#x02213;","\u2213",x) # MINUS-OR-PLUS SIGN
x<-gsub("\\&#x022b2;","\u22b2",x) # NORMAL SUBGROUP OF
x<-gsub("\\&#x022c2;","\u22c2",x) # N-ARY INTERSECTION
x<-gsub("\\&#x02300;","\u2300",x) #  DIAMETER SIGN
x<-gsub("\\&#x02309;","\u2309",x) # RIGHT CEILING
x<-gsub("\\&#x0230a;","\u230a",x) # LEFT FLOOR
x<-gsub("\\&#x027e8;","\u27e8",x) # MATHEMATICAL LEFT ANGLE BRACKET
x<-gsub("\\&#x02a01;","\u2a01",x) # N-ARY CIRCLED PLUS OPERATOR
x<-gsub("\\&#x02a09;","\u2a09",x) # N-ARY TIMES OPERATOR
x<-gsub("\\&#x03002;","\u3002",x) # IDEOGRAPHIC FULL STOP
}



}

# some html and special characters  
x<-gsub("\\&le;|\u2264|\u2A7f|\u2a7d","<=",x) # less equal than
x<-gsub("\\&ge;|\u2A7e|\u2265",">=",x) # greater equal than
x<-gsub("\\&amp;","& ",x) #  &
x<-gsub("\\&lt;","<",x) # less than
x<-gsub("\\&le;","<=",x) # less equal than
x<-gsub("\\&gt;",">",x) # greater than
x<-gsub("\\&ge;",">=",x) # greater equal than
x<-gsub("\\&#61;|\\&equals;|\u003d","=",x) # equal sign
x<-gsub("\u2912","->",x) # rightwards arrow ->
x<-gsub("\u00A0","",x) # no break space

# invisible space
x<-gsub("\u200A"," ",x)
# minus/dash -
x<-gsub("\u2013|\u2014|\u2015|\u2212|\u2010|\u2011","-",x)

x<-gsub("\u00B2","^2",x) # superscript 2
x<-gsub("\u2018","'",x) # left single quote
x<-gsub("\u2019","'",x) # right single quote
x<-gsub("\u201D","'",x) # right double quote
x<-gsub("\u201C","'",x) # left double quote
x<-gsub("\u2032","'",x) # prime
x<-gsub("\u2033","'",x) # double prime
x<-gsub("\u2035","'",x) # reversed prime
x<-gsub("\u2036","'",x) # reversed double prime

x<-gsub("\u00D7","*",x) # times sign: x

if(greek2text==TRUE){
# some foreign alphabet letters
# alpha
x<-gsub("\u03b1|\u0251|\u221d","alpha",x)
# beta
x<-gsub("\u03b2|\u03d0|\u1e9e","b",x)
x<-gsub("\u0392","Beta",x)
# r
x<-gsub("\u027e","r",x)
x<-gsub("\u211b","R",x)

x<-gsub("\u03b3","gamma",x) 
# delta
x<-gsub("\u03b4|\u2206|\u25b5|\u25b3|\u22bf|\u0394","delta ",x)
# eta
x<-gsub("\u019e|\u03b7|\u014b|\u0273|\u0220","eta",x) 
x<-gsub("\u03ae|\u1f75","eta", x)
# epsilon
x<-gsub("\u025b|\u03f5|\u03b5","epsilon",x)  
#chi
x<-gsub("\u03a7|\u1d61","Chi",x) # capital greek Chi
x<-gsub("\u03c7|\u03f0","chi",x) # small greek Chi
# lok alike p
x<-gsub("\u0440","p",x) # CYRILLIC SMALL LETTER ER

# omega
x<-gsub("\u3c9","omega",x)
x<-gsub("\u03a9|\u2126","Omega",x) # greek captial leter Omega
# phi
x<-gsub("\u03c6","phi",x)
x<-gsub("\u03a6","Phi",x)
# rho
x<-gsub("\u03c1","rho",x)
# tau
x<-gsub("\u03c4","tau",x)
# lambda, etc
x<-gsub("\u03bb","lambda",x)
x<-gsub("\u03bd","Ny",x) # GREEK CAPITAL LETTER NEUTRUM
x<-gsub("\u03b9","iota",x) # greek letter iota
x<-gsub("\u0399","Iota",x) # greek capital letter Iota
x<-gsub("\u041A","ka",x) # kyrillik ka
x<-gsub("\u029F","L",x) # Latin L

x<-gsub("\u00B2","^2",x) # superscript 2

x<-gsub("\u0445","x",x) # CYRILLIC SMALL LETTER HA

}

# clean up white spaces
x<-gsub("^ *|(?<= ) | *$", "", x, perl = TRUE)

## convert cermine specific letter captures
## unsolvable and very crappy: minus gets sometimes converted to "2"
if(cermine==TRUE){
check<-x
  # clean up white spaces
  x<-gsub("^ *|(?<= ) | *$", "", x, perl = TRUE)
  # unify "-"
  x<-gsub("\u2012","-", x)
  
  # clean up cermines captures of degree of freedom as reference if input is cermine JATS
  # for "[ <xref.*"
  x<-gsub("([^a-zA-Z][FRrzZTt2])\\[ <xref.*?>([0-9,;\\. ]*)</xref> \\]","\\1(\\2)",x)
  # for "( <xref.*"
  x<-gsub("([^a-zA-Z][FRrzZTt2])\\( <xref.*?>([0-9,;\\. ]*)</xref> \\)","\\1(\\2)",x)
  x<-gsub("(chi2)\\( <xref.*?>([0-9,;\\. ]*)</xref> \\)","\\1(\\2)",x)
  x<-gsub("([^a-zA-Z][FRrzZTt2]) \\( <xref.*?>([0-9,;\\. ]*)</xref> \\)","\\1(\\2)",x)
  # " 5 [\\.0-9]" for "="
  x<-gsub(" 5 ([\\.0-9])","=\\1",x)
  # " 5 -[\\.0-9]" for "=-"
  x<-gsub(" 5 -([\\.0-9])","=-\\1",x)
  # " 5 - [\\.0-9]" for "=-"
  x<-gsub(" 5 -( [\\.0-9])","=-\\1",x)
  # " 5 )[\\.0-9]" for "=-"
  x<-gsub(" 5 \\)([\\.0-9])","=-\\1",x)
  # p as =
  x<-gsub("([0-9]\\)) p ([-\\.0-9])","\\1 = \\2",x)
  # "¼" for =
  x<-gsub("\u00BC","=",x)
# convert badly captured "= )" -> "= -"
  x<-gsub("([<>=]) \\)([.0-9])","\\1 -\\2",x)
  x<-gsub("([<>=])\\) ([.0-9])","\\1 -\\2",x)
  x<-gsub("([<>=]) \\) ([.0-9])","\\1 -\\2",x)
  x<-gsub("([<>=])\\)([.0-9])","\\1-\\2",x)
  
  # "p4" for "p>"
  x<-gsub(" p4\\.|^p4\\."," p>.",x)
  x<-gsub(" p40|^p40"," p>0",x)
  # "po" for "p<"
  x<-gsub(" po\\.|^po\\."," p<.",x)
  x<-gsub(" po0|^po0"," p<0",x)
  # unify partial eta2 and eta2
  x<-gsub("g2[qp]|np2|etap2|\u02732p|partial g2","eta2(p)",x)
  x<-gsub("etap 2|gp2|n2p","eta2(p)",x)
  x<-gsub("\u03C42|eta2|\u02732|\u03C4\u00B2|eta\u00B2|g2","eta2",x)
  # "b" as "<"
  x<-gsub("([^a-zA-Z][a-zA-Z]) b ([0-9\\.])","\\1 < \\2",x )
  x<-gsub("([^a-zA-Z][Ftr]s) b ([0-9\\.])","\\1 < \\2",x )
  x<-gsub("^([a-zA-Z]) b ([0-9\\.])","\\1 < \\2",x)
  x<-gsub("([^a-zA-Z][a-zA-Z]) b (-[0-9\\.])","\\1 < \\2",x)
  x<-gsub("([^a-zA-Z][Ftr]s) b (-[0-9\\.])","\\1 < \\2",x )
  x<-gsub("^([a-zA-Z]) b (-[0-9\\.])","\\1 < \\2",x )
  # "p\" as "p <"
  x<-gsub("([^a-z])p \\\\","\\1p <",x)
# convert "num 9 num" to num*num (for ANOVA)
  while(length(grep("[2-9] 9 [2-9]",x)>0)) x<-gsub("([2-9]) 9 ([2-9])","\\1*\\2",x)
# chi2
  x<-gsub("chi 2","chi2",x)
  x<-gsub("chi\\^2|chi\u00B2","chi2",x)
  x<-gsub("v\\^2\\(","chi2(",x)
  x<-gsub("v2([a-zA-Z])","chi2\\1",x)
  x<-gsub("v2[=]","chi2=",x)
  x<-gsub("v2\\(","chi2(",x)
  x<-gsub("all 2s\\(","all chi2s(",x)

  x<-gsub("v2 [=]","chi2=",x)
  x<-gsub("v2 \\(","chi2(",x)
  x<-gsub("v2s \\(","chi2s(",x)
  x<-gsub("v\\^2[=]","chi2=",x)
  x<-gsub("v\\^2\\(","chi2(",x)
  x<-gsub("v\\^2s\\(","chi2s(",x)
  x<-gsub("v\\^2 [=]","chi2=",x)
  x<-gsub("v\\^2 \\(","chi2(",x)
  x<-gsub("v\\^2s \\(","chi2s(",x)
  x<-gsub(" 2\\(([0-9Nnd][0-9f, =]*)"," chi2(\\1",x)
  x<-gsub(" 2 \\(([0-9Nnd][0-9f, =]*)"," chi2(\\1",x)
  x<-gsub(" v2 ="," chi2 =",x)
  x<-gsub(" v2s ="," chi2s =",x)
  x<-gsub("(v2) (\\([1-9])","chi2\\2",x)
  x<-gsub("v2\\(","chi2(",x)
  x<-gsub("v2s\\(","chi2s(",x)
  x<-gsub("([^a-zA-Z])[Cc]2\\(","\\1chi2(",x)
  x<-gsub("([^a-zA-Z])[Cc]2 \\(","\\1chi2(",x)
  x<-gsub("[Cc]2s\\(","chi2s(",x)
  x<-gsub("[Cc]\\^2\\(","chi2(",x)
  x<-gsub("[Cc]2\\(","chi2(",x)
  x<-gsub("[CcVv]2\\[","chi2[",x)
  x<-gsub("([^a-zA-Z])[Ccv]2\\(","\\1chi2(",x)
  x<-gsub("[Cc]\\^2s\\(","chi2s(",x)
  x<-gsub("w2\\(","chi2(",x)
  x<-gsub("w2s\\(","chi2s(",x)
  x<-gsub("w\\^2\\(","chi2(",x)
  x<-gsub("w\\^2s\\(","chi2s(",x)
  x<-gsub("X \\.2 \\(","chi2(",x)
  x<-gsub("([a-zA-Z])[Cc]2\\(","\\1chi2(",x)
  x<-gsub("([a-zA-Z])v2\\(","\\1chi2(",x)
  x<-gsub("([a-zA-Z])w2\\(","\\1chi2(",x)
  x<-gsub("([^a-zA-Z])[wcCv]2=","\\1chi2=",x)
  x<-gsub("([^a-zA-Z])[wcCv]2 =","\\1chi2 =",x)

  # eta2  
  x<-gsub(" Z2([<=>])"," eta2\\1",x)
# remove 2 after )
  x<-gsub("\\)2=",")=",x)
# correct "=xnum" -> "=-num"
  x<-gsub("([<>=])x([0-9\\.-])","\\1-\\2",x)
  x<-gsub("([<>=]) x([0-9\\.-])","\\1-\\2",x)
# remove space between - and num
  x<-gsub("([\\)<=>]) - ([\\.0-9])","\\1 -\\2",x)
# correct F without () for df1 and df2
  x<-gsub("F([0-9\\.]*,[0-9\\.]*)([=<> ])","F(\\1)\\2",x)

# correct ",[a-z]"  
  x<-  gsub(",([a-zA-Z])",", \\1",x)

# insert "<=>" for not captured presigns in standardStats
# F values with dfs
  x<-gsub("(F\\(.*?\\)) ([\\.0-9])","\\1<=>\\2",x)
  x<-gsub("(F[1-9 a-z]\\(.*?\\)) ([\\.0-9])","\\1<=>\\2",x)
  x<-gsub("(F[a-z]*?\\([1-9].*?\\)) ([\\.0-9])","\\1<=>\\2",x)
# F values without dfs
  x<-gsub("([\\( ]F) ([0-9]\\.[0-9])","\\1<=>\\2",gsub("([\\( ]F) (\\.[0-9])","\\1<=>\\2",x))
  x<-gsub("([\\( ]Fs) ([0-9]\\.[0-9])","\\1<=>\\2",gsub("([\\( ]Fs) (\\.[0-9])","\\1<=>\\2",x))
  x<-gsub("([\\( ]Fs)([0-9]\\.[0-9])","\\1<=>\\2",gsub("([\\( ]Fs)(\\.[0-9])","\\1<=>\\2",x))
  
# t values
# set "t_df" to "t(df)"
  x<-gsub("([^a-zA-Z]t)([0-9]*?) ([0-9\\.])","\\1(\\2) \\3",x) 
  x<-gsub("([\\( ]t) (\\(.*?\\)) ([\\0-9\\.])","\\1\\2<=>\\3",x)
  x<-gsub("([\\( ]t) (\\(.*?\\)) (-[\\0-9\\.])","\\1\\2<=>\\3",x)
  x<-gsub("([\\( ]t\\(.*?\\)) ([0-9\\.][0-9\\.])","\\1<=>\\2",x)
  x<-gsub("([\\( ]t\\(.*?\\)) (-[0-9\\.][0-9\\.])","\\1<=>\\2",x)
  x<-gsub("([\\( ]t[1-9 ]*?\\(.*?\\)) ([\\.0-9])","\\1<=>\\2",x)
  x<-gsub("([\\( ]t[1-9 ]*?\\(.*?\\)) (-[\\.0-9])","\\1<=>\\2",x)
  x<-gsub("([\\( ]t[a-z]\\(.*?\\)) ([\\.0-9])","\\1<=>\\2",x)
  x<-gsub("([\\( ]t[a-z]\\(.*?\\)) (-[\\.0-9])","\\1<=>\\2",x)
  x<-gsub("([\\( ]t) ([0-9\\.][0-9\\.])","\\1<=>\\2",x)
  x<-gsub("([\\( ]t) (-[0-9\\.][0-9\\.])","\\1<=>\\2",x)
  x<-gsub("([\\( ]t[1-9 ]*?\\(.*?\\)) (-[0-9]\\.)","\\1<=>\\2",x)
  x<-gsub("([\\( ]t[1-9 ]*?\\(.*?\\))(-[0-9]\\.)","\\1<=>\\2",x)
  x<-gsub("([\\( ]ts) ([0-9\\.])","\\1<=>\\2",x)
  x<-gsub("([\\( ]ts) (-[0-9\\.])","\\1<=>\\2",x)
  x<-gsub("^(t) (\\(.*?\\)) ([\\0-9\\.])","\\1\\2<=>\\3",x)
  x<-gsub("^(t) (\\(.*?\\)) (-[\\0-9\\.])","\\1\\2<=>\\3",x)
  x<-gsub("^(t) ([0-9\\.])","\\1<=>\\2",x)
  x<-gsub("^(t) (-[0-9\\.])","\\1<=>\\2",x)
  x<-gsub("^(t\\(.*?\\)) ([0-9\\.])","\\1<=>\\2",x)
  x<-gsub("^(t\\(.*?\\)) (-[0-9\\.])","\\1<=>\\2",x)
  
# d values
  x<-gsub("([\\( ]d) (0\\.[0-9])","\\1<=>\\2",gsub("([\\( ]d) (\\.[0-9])","\\1<=>\\2",x))
  x<-gsub("([\\( ]d) (-0\\.[0-9])","\\1<=>\\2",gsub("([\\( ]d) (-\\.[0-9])","\\1<=>\\2",x))
# r values
  x<-gsub("([\\( ]r) (0\\.[0-9])","\\1<=>\\2",gsub("([\\( ]r) (\\.[0-9])","\\1<=>\\2",x))
  x<-gsub("([\\( ]r) (-0\\.[0-9])","\\1<=>\\2",gsub("([\\( ]r) (-\\.[0-9])","\\1<=>\\2",x))
  x<-gsub("([\\( ]r\\([0-9]*?\\)) (-\\.[0-9])","\\1<=>\\2",x)
  x<-gsub("([\\( ]r\\([0-9]*?\\)) (\\.[0-9])","\\1<=>\\2",x)
  x<-gsub("([\\( ]r \\([0-9]*?\\)) (-\\.[0-9])","\\1<=>\\2",x)
  x<-gsub("([\\( ]r \\([0-9]*?\\)) (\\.[0-9])","\\1<=>\\2",x)
  x<-gsub("^(r\\([0-9]*?\\)) (-\\.[0-9])","\\1<=>\\2",x)
  x<-gsub("^(r\\([0-9]*?\\)) (\\.[0-9])","\\1<=>\\2",x)
# R2 values
  x<-gsub("([\\( ]r2) (0\\.[0-9])","\\1<=>\\2",gsub("([\\( ]r2) (\\.[0-9])","\\1<=>\\2",x))
  x<-gsub("([\\( ]R2) (0\\.[0-9])","\\1<=>\\2",gsub("([\\( ]R2) (\\.[0-9])","\\1<=>\\2",x))
  x<-gsub("[^a-zA-Z]r2","R2",x)
  
  # chi2 values
  x<-gsub("([\\( ]chi2) ([0-9]\\.[0-9])","\\1<=>\\2",gsub("([\\( ]chi2) (\\.[0-9])","\\1<=>\\2",x))
  x<-gsub("([\\( ]chi) ([0-9]\\.[0-9])","\\1<=>\\2",gsub("([\\( ]chi) (\\.[0-9])","\\1<=>\\2",x))
  x<-gsub("([\\( ]chi2\\([0-9 Nn,=df]*\\)) ([0-9\\.])","\\1=\\2",x)
  x<-gsub("([\\( ]chi\\([0-9 Nn,=df]*\\)) ([0-9\\.])","\\1=\\2",x)
  x<-gsub("([\\( ]chi2s\\([0-9 Nn,=df]*\\)) ([0-9\\.])","\\1=\\2",x)
  
# Q values
  x<-gsub("([\\( ]Q) (\\([0-9]*?\\)) ([0-9\\.])","\\1\\2<=>\\3",x)
  x<-gsub("([\\( ]Q\\([0-9]*?\\)) ([0-9\\.])","\\1<=>\\2",x)
  x<-gsub("([\\( ]Q) ([0-9\\.])","\\1<=>\\2",x)

# Z values
  x<-gsub("([\\( ][Zz]) (\\.[0-9])","\\1<=>\\2",x)
  x<-gsub("([\\( ][Zz]) (-\\.[0-9])","\\1<=>\\2",x)
  x<-gsub("([\\( ][Zz]) ([0-9]\\.[0-9])","\\1<=>\\2",x)
  x<-gsub("([\\( ][Zz]) (-[0-9]\\.[0-9])","\\1<=>\\2",x)
  #x<-gsub("([\\( ][Zz]\\(.*?\\)) ([0-9\\.])","\\1<=>\\2",x)
  #x<-gsub("([\\( ][Zz]\\(.*?\\)) (-[0-9\\.])","\\1<=>\\2",x)
# p value
  x<-gsub("([\\( ]p) (0\\.[0-9])","\\1<=>\\2",gsub("([\\( ]p) (\\.[0-9])","\\1<=>\\2",x))
  x<-gsub("([\\( ]ps) (0\\.[0-9])","\\1<=>\\2",gsub("([\\( ]ps) (\\.[0-9])","\\1<=>\\2",x))
# eta2
  x<-gsub("partial2","eta2<=>",x)
  x<-gsub("([\\( ]eta2) ([0-9\\.])","\\1<=>\\2",gsub("([\\( ]eta2) (\\.[0-9])","\\1<=>\\2",x))
  x<-gsub("([\\( ]eta) ([0-9\\.])","\\1<=>\\2",gsub("([\\( ]eta) (\\.[0-9])","\\1<=>\\2",x))
  x<-gsub("([\\( ]p2) (0\\.[0-9])"," eta2<=>\\2",gsub("([\\( ]p2) (\\.[0-9])"," eta2<=>\\2",x))

# others  
  x<-gsub("([\\( ]OR) ([0-9\\.])","\\1<=>\\2",x)
  x<-gsub("([\\( ]RR) ([0-9\\.])","\\1<=>\\2",x)
  x<-gsub("([\\( ][Oo]dds[ \\-][Rr]atio) ([0-9\\.])","\\1<=>\\2",x)
  x<-gsub("([\\( ][Oo]dds) ([0-9\\.])","\\1<=>\\2",x)
  x<-gsub("([\\( ][sS][dD]) ([0-9\\.])","\\1<=>\\2",x)
  x<-gsub("([\\( ]SE) ([0-9\\.])","\\1<=>\\2",x)
  x<-gsub("([\\( ]MSE) ([0-9\\.])","\\1<=>\\2",x)
  x<-gsub("([\\( ]RMSE) ([0-9\\.])","\\1<=>\\2",x)
  x<-gsub("([\\( ]RMSEA) ([0-9\\.])","\\1<=>\\2",x)
  x<-gsub("([\\( ][Rr]ange) ([0-9\\.])","\\1=\\2",x)
  x<-gsub("([\\( ]W) ([0-9\\.])","\\1<=>\\2",x)
  x<-gsub("([\\( ]D) ([0-9\\.])","\\1<=>\\2",x)
  
  x<-gsub("([\\( ][bB]) ([0-9\\.])","\\1<=>\\2",x)
  x<-gsub("([\\( ][bB]) (-[0-9\\.])","\\1<=>\\2",x)
  x<-gsub("([\\( ]slope) ([0-9\\.])","\\1=\\2",x)
  x<-gsub("([\\( ]slope) (-[0-9\\.])","\\1=\\2",x)
  x<-gsub("([\\( ][M]) ([0-9\\.])","\\1<=>\\2",x)
  x<-gsub("([\\( ][M]) (-[0-9\\.])","\\1<=>\\2",x)
  x<-gsub("([\\( ]beta) ([0-9\\.])","\\1=\\2",x)
  x<-gsub("([\\( ]beta) (-[0-9\\.])","\\1=\\2",x)
  
  x<-gsub("([\\( ]df) ([1-9])","\\1=\\2",x)
  x<-gsub("([\\( ][gkm]) ([\\.0-9])","\\1=\\2",x)

  x<-gsub("(power of 1) (\\.[0-9])","\\1-beta = \\2",x)
  x<-gsub("([ (\\[]power) (\\.[0-9])","\\1=\\2",x)
  
  # add () for df to stats without (): t12 = 12.3 
  x<-gsub("F([0-9]*?, [1-9]*?) ([<=>]*? [0-9\\.])","F(\\1) \\2",x)
  x<-gsub("F ([0-9]*?, [1-9]*?) ([<=>]*? [0-9\\.])","F(\\1) \\2",x)
  x<-gsub("t([0-9]*?) ([<=>]*? [0-9\\.])","t(\\1) \\2",x)
  x<-gsub("t ([0-9]*?) ([<=>]*? [0-9\\.])","t(\\1) \\2",x)
  x<-gsub("t([0-9]*?) (-[<=>]*? [0-9\\.])","t(\\1) \\2",x)
  x<-gsub("t ([0-9]*?) (-[<=>]*? [0-9\\.])","t(\\1) \\2",x)
  x<-gsub("r ([0-9]*?) ([<=>]*? [0-9\\.])","t(\\1) \\2",x)
  x<-gsub("r([0-9]*?) ([<=>]*? [0-9\\.])","r(\\1) \\2",x)
  x<-gsub("r ([0-9]*?) ([<=>]*? -[0-9\\.])","r(\\1) \\2",x)
  x<-gsub("r([0-9]*?) ([<=>]*? -[0-9\\.])","r(\\1) \\2",x)
  
  # add "=" between "N [1-9]"
  x<-gsub("([\\(\\[ ][Nn]) ([1-9])","\\1=\\2",x)

  ## correct badly captured "-" as 2 in r(df)<=>2.num
  x<-gsub("(r\\([0-9]*?\\)[<=>]*?)2\\.","\\1-.",x)
  x<-gsub("(r\\([0-9]*?\\) [<=>]*? )2\\.","\\1-.",x)

  ## correct badly captured "-" as 2 in r(df)<=>2num.num but not t(df)=2.num0
  x<-gsub("(t\\([0-9]*?\\)[<=>]*?)2([0-9]\\.)","\\1-\\2",x)
  x<-gsub("(t\\([0-9]*?\\) [<=>]*? )2([0-9]\\.)","\\1-\\2",x)

  ## correct badly captured "-" as 2 in z<=>2num.num but not z=2.num0
  x<-gsub("([^a-zA-Z][zZ][<=>]*?)2([0-9]\\.)","\\1-\\2",x)
  x<-gsub("([^a-zA-Z][zZ] [<=>]*? )2([0-9]\\.)","\\1-\\2",x)

  
if(warning==TRUE) if(sum(check==x)!=length(x)) warning("CERMINE specific letter conversion was used to correct for some conversion errors. '<=>' was inserted by letter.convert() to make statistics readable. The minus sign rarely gets converted to '2' what cannot always be handled correctly.")  
rm(check)  
  
  }

#cleanup
x<-gsub("\\t","",x)
# white spaces
x<-gsub("^ *|(?<= ) | *$", "", x, perl = TRUE)
# unify -
#x<-gsub("—|–","-",x)

return(x)
}
