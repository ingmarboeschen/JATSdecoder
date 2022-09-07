#' letter.convert
#'
#' Converts and unifies most hexadecimal and some HTML coded letters to Unicode characters. Performs CERMINE specific error correction (inserting operators, where these got lost while conversion). 
#' @param x text string to process.
#' @param cermine Logical. If TRUE CERMINE specific error handling and letter conversion will be applied.
#' @param greek2text Logical. If TRUE some greek letters and special characters will be unified to textual representation (important to extract stats).
#' @param warning Logical. If TRUE prints warning massage if CERMINE specific letter conversion was performed.
#' @return Character. Text with unified and corrected letter representation.
#' @export
#' @examples
#' x<-c("five &#x0003c; ten","five &lt; ten")
#' letter.convert(x)

# letter conversion
letter.convert<-function(x,cermine=FALSE,greek2text=FALSE,warning=TRUE){
# clean up white spaces
x<-gsub("^ *|(?<= ) | *$", "", x, perl = TRUE)
# right/left quotation mark
x<-gsub("\u2019","'",x)
x<-gsub("\u2018","'",x)
# unify "-"
x<-gsub("\u2012","-", x)

# if has hex letters convert to simplified letter
if(length(grep("&#",x))>0){
# prepare
# add 0 behind &#x if has only 4 characters between "&#x" and ";" and lowerize capture
x<-gsub("&#[Xx](....);","&#x0\\L\\1;",x,perl=T)
# convert further upper to lower case letter in hex code
x<-gsub("&#[Xx](.....);","&#x\\L\\1;",x,perl=T)

# start conversion
## letters to unify
if(length(grep("&#",x))>0){
x<-gsub("&x0227;","\u227",x) #  LATIN SMALL LETTER A WITH DOT ABOVE
x<-gsub("&x02a2;","\u2a2",x) # LATIN LETTER REVERSED GLOTTAL STOP WITH STROKE
x<-gsub("&x0215;","\u00fc",x) # LATIN SMALL LETTER U WITH DOUBLE GRAVE
x<-gsub("&#x0210;","\u210",x) # LATIN CAPITAL LETTER R WITH DOUBLE GRAVE
x<-gsub("&#x0220;","\u220",x) # LATIN CAPITAL LETTER N WITH LONG RIGHT LEGx<-gsub("&#x0feff;","",x) # empty seperator
x<-gsub("&#x0237a;","\u03b1",x) # APL FUNCTIONAL SYMBOL ALPHA -> alpha
x<-gsub("&#x0ff5e;","\u007e",x) #  FULLWIDTH TILDE
x<-gsub("&#x1d74c;","\u03c7",x) # bold italic small greek chi -> small greek chi
x<-gsub("&#x1d700;","\u03b5",x) # MATHEMATICAL ITALIC SMALL EPSILON

x<-gsub("&#x0003f;","?",x) # question mark
x<-gsub("&#x0ff1f;","? ",x) # FULLWIDTH QUESTION MARK
x<-gsub("&#x0ff05;","%",x) # FULLWIDTH PERCENT SIGN
x<-gsub("&#x0002b;","+",x) # high +
x<-gsub("&#x000b1;","+-",x) # plus minus
x<-gsub("&#x0ff0b;","+",x) #  FULLWIDTH PLUS SIGN
x<-gsub("&#x02212;","-",x) # minus
# quotes
x<-gsub("&#x002bc;","'",x) # '
x<-gsub("&#x00027;","'",x) # '
x<-gsub("&#x0201c;","'",x) # quotation start
x<-gsub("&#x0201d;","'",x) # quotation end
x<-gsub("&#x000b4;","'",x) # '
x<-gsub("&#x0201e;","'",x) # lower "
x<-gsub("&#x02019;","'",x) # right single quotation mark
x<-gsub("&#x02018;","'",x) # left single quotation mark
x<-gsub("&#x02033;","'",x) # double prime
x<-gsub("&#x02034;","'",x) # double prime end
x<-gsub("&#x02032;","'",x) # prime
x<-gsub("&#x002b9;","'",x) # MODIFIER LETTER PRIME
x<-gsub("&#x002bb;","'",x) # MODIFIER LETTER TURNED COMMA
x<-gsub("&#x01ffd;","'",x) # GREEK OXIA
x<-gsub("&#x005f3;","'",x) # HEBREW PUNCTUATION GERESH
x<-gsub("&#x00384;","'",x) # Greek tonos
x<-gsub("&#x002c8;","'",x) # MODIFIER LETTER VERTICAL LINE
x<-gsub("&#x00301;","'",x) # COMBINING ACUTE ACCENT
x<-gsub("&#x01fbf;","'",x) # GREEK PSILI
x<-gsub("&#x0201b;","'",x) #  SINGLE HIGH-REVERSED-9 QUOTATION MARK
x<-gsub("&#x02035;","'",x) # REVERSED PRIME
x<-gsub("&#x00300;","'",x) # COMBINING GRAVE ACCENT
x<-gsub("&#x0a78c;","'",x) # LATIN SMALL LETTER SALTILLO
x<-gsub("&#x01fbd;","'",x) # GREEK KORONIS
# spaces
x<-gsub("&#x02000;"," ",x) # space
x<-gsub("&#x02001;"," ",x) # space
x<-gsub("&#x02002;"," ",x) # space
x<-gsub("&#x02003;"," ",x) # em space
x<-gsub("&#x02004;"," ",x) # space
x<-gsub("&#x02005;"," ",x) # space
x<-gsub("&#x02006;"," ",x) # space
x<-gsub("&#x02007;"," ",x) # space
x<-gsub("&#x02008;"," ",x) # space
x<-gsub("&#x02009;"," ",x) # thin space
x<-gsub("&#x000a0;"," ",x) # non breaking space
x<-gsub("&#x02028;"," ",x) # line seperator
x<-gsub("&#x02029;"," ",x) # PARAGRAPH SEPARATOR
x<-gsub("&#x000A0;"," ",x) # no break space
x<-gsub("&#x03000;"," ",x) # IDEOGRAPHIC SPACE
x<-gsub("&#x0205f;"," ",x) # MEDIUM MATHEMATICAL SPACE
x<-gsub("&#x02062;"," ",x) # INVISIBLE TIMES 
x<-gsub("&#x0200a;"," ",x) # hair space
x<-gsub("&#x0115f;"," ",x) # HANGUL CHOSEONG FILLER

x<-gsub("&#x0200b;","",x) #  ZERO WIDTH SPACE
x<-gsub("&#x0200c;","",x) #  ZERO WIDTH NON-JOINER
x<-gsub("&#x0202c;","",x) #  POP DIRECTIONAL FORMATTING
x<-gsub("&#x0200d;","",x) # ZERO WIDTH JOINER
x<-gsub("&#x0200c;","",x) # ZERO WIDTH NON-JOINER
x<-gsub("&#x0200e;","",x) # LEFT-TO-RIGHT MARK
x<-gsub("&#x0202f;","",x) #  RIGHT-TO-LEFT MARK
x<-gsub("&#x02061;","",x) # FUNCTION APPLICATION 
}


if(length(grep("&#",x))>0){
# hyphens
x<-gsub("&#x02011;","-",x) # non breaking hyphen
x<-gsub("&#x02013;","-",x) # until/dash
x<-gsub("&#x02010;","-",x) # hyphen/dash
x<-gsub("&#x000ad;","-",x) # soft hyphen
x<-gsub("&#x02014;","-",x) # mdash -
x<-gsub("&#x02012;","-",x) # FIGURE DASH
x<-gsub("&#x02666;","-",x) # bullet point like hyper reference
x<-gsub("&#x02015;","-",x) #
x<-gsub("&#x002d9;","-",x) # &DiacriticalDot;
x<-gsub("&#x02500;","-",x) # BOX DRAWINGS LIGHT HORIZONTA
x<-gsub("&#x02022;"," - ",x) # bullet point
x<-gsub("&#x025e6;"," - ",x) # white bullet
x<-gsub("&#x025aa;"," - ",x) # bullet square
x<-gsub("&#x02219;"," - ",x) # BULLET OPERATOR (in categories)
x<-gsub("&#x025cf;"," - ",x) # BLACK CIRCLE (in categories)
x<-gsub("&#x025cb;"," - ",x) # circle
x<-gsub("&#x000b7;"," - ",x) #  mitdot
x<-gsub("&#x022c5;"," - ",x) #  sdot
x<-gsub("&#x002d7;","-",x) # MODIFIER LETTER MINUS SIGN
x<-gsub("&#x0ff0d;","-",x) # FULLWIDTH HYPHEN-MINUS
x<-gsub("&#x04e00;","-",x) # CJK UNIFIED IDEOGRAPH-4E00
# special signs
x<-gsub("&#x000d7;","*",x) # multiplied by
x<-gsub("&#x02217;","*",x) # times
x<-gsub("&#x02606;","*",x) # white star
x<-gsub("&#x02605;","*",x) # black star
x<-gsub("&#x0002a;","*",x) # asterix
x<-gsub("&#x022c6;","*",x) # STAR OPERATOR
x<-gsub("&#x00029;",")",x) # )
x<-gsub("&#x00028;","(",x) # (
x<-gsub("&#x03010;",")",x) # LEFT BLACK LENTICULAR BRACKET
x<-gsub("&#x03011;",")",x) # RIGHT BLACK LENTICULAR BRACKET
x<-gsub("&#x0ff09;",") ",x) # FULLWIDTH RIGHT PARENTHESIS
x<-gsub("&#x0ff08;"," (",x) # FULLWIDTH LEFT PARENTHESIS
x<-gsub("&#x00302;","^",x) #  COMBINING CIRCUMFLEX ACCENT
x<-gsub("&#x002c4;","^",x) # MODIFIER LETTER UP ARROWHEAD
x<-gsub("&#x002c6;","^",x) # circ
x<-gsub("&#x0fb01;","fi",x) # fi ligature
x<-gsub("&#x0fb04;","ffl",x) # ffl ligature
x<-gsub("&#x0223c;","~",x) # tilde
x<-gsub("&#x002dc;","~",x) # tilde
x<-gsub("&#x02261;","=",x) # identical to
x<-gsub("&#x02245;","=~",x) # tilde full equal
x<-gsub("&#x00026;","& ",x) #  &
x<-gsub("&#x00023;","# ",x) #  # , num
x<-gsub("&#x02026;","...",x) # ...
x<-gsub("&#x02192;","->",x)# rightwards arrow
x<-gsub("&#x025b8;","->",x)# rightwards arrow
x<-gsub("&#x02190;","<-",x)# leftwards arrow
x<-gsub("&#x0a789;",":",x) # MODIFIER LETTER COLON
x<-gsub("&#x0ff1a;",":",x) # FULLWIDTH COLON
x<-gsub("&#x02236;",":",x) # ratio
x<-gsub("&#x0003d;","=",x) # equal sign
x<-gsub("&#x0003c;","<",x) # less than
x<-gsub("&#x03008;","<",x) # less than
x<-gsub("&#x02329;","<",x) # less than
x<-gsub("&#x0226a;","<<",x) # much less than
x<-gsub("&#x02264;","<=",x) # less equal than
x<-gsub("&#x02a7d;","<=",x) # less equal than
x<-gsub("&#x0003e;",">",x) # greater than
x<-gsub("&#x0232a;",">",x) # greater than
x<-gsub("&#x03009;",">",x) # greater than
x<-gsub("&#x02a7e;",">=",x) # greater equal than
x<-gsub("&#x02267;",">=",x) # greater equal than
x<-gsub("&#x0226b;",">>",x) # much greater than
x<-gsub("&#x02260;","!=",x) # not greater 
x<-gsub("&#x02265;","<",x) # not greater equal than
x<-gsub("&#x0ff1c;","<",x) # FULLWIDTH LESS-THAN SIGN
x<-gsub("&#x0ff1d;","=",x) # FULLWIDTH EQUALS SIGN
x<-gsub("&#x0ff1e;",">",x) # FULLWIDTH GREATER-THAN SIGN
x<-gsub("&#x0203a;",">",x) #  SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
x<-gsub("&#x02039;","<",x) # SINGLE LEFT-POINTING ANGLE QUOTATION MARK
x<-gsub("&#x025ba;","->",x) # pointer right: ->
x<-gsub("&#x000ab;","<<",x) #
x<-gsub("&#x000bb;",">>",x) #
x<-gsub("&#x02912;","->",x) # rightwards arrow ->
x<-gsub("&#x002c2;","<",x) # MODIFIER LETTER LEFT ARROWHEAD
x<-gsub("&#x02215;","/",x) # divided by
x<-gsub("&#x000f7;","/",x) # DIVISION SIGN
x<-gsub("&#x02044;","/",x) # FRACTION SLASH
x<-gsub("&#x0002f;","/",x) # solidus /
x<-gsub("&#x000bd;"," 1/2",x) # one half
x<-gsub("&#x000bc;"," 1/4",x) # one fourth
x<-gsub("&#x000be;"," 3/4",x) # Three quarter
}

if(length(grep("&#",x))>0){
x<-gsub("&#x00068;","h",x) # LATIN SMALL LETTER H
x<-gsub("&#x00074;","t",x) #    LATIN SMALL LETTER T
x<-gsub("&#x02179;","x",x) # SMALL ROMAN NUMERAL TEN
x<-gsub("&x0030;","0",x) # DIGIT zero
x<-gsub("&x0031;","1",x) # DIGIT one 
x<-gsub("&x0032;","2",x) # DIGIT two
x<-gsub("&x0033;","3",x) # DIGIT three
x<-gsub("&x0034;","4",x) # DIGIT four
x<-gsub("&x0035;","5",x) # DIGIT five
x<-gsub("&x0036;","6",x) # DIGIT six 
x<-gsub("&x0037;","7",x) # DIGIT seven
x<-gsub("&x0038;","8",x) # DIGIT eight
x<-gsub("&x0039;","9",x) # DIGIT nine
x<-gsub("&x0044;","D",x) #     LATIN CAPITAL LETTER D
x<-gsub("&#x00421;","C",x) #  CYRILLIC CAPITAL LETTER ES
x<-gsub("&#x00406;","I",x) # CYRILLIC CAPITAL LETTER BYELORUSSIAN-UKRAINIAN I
x<-gsub("&#x00441;","c",x) # CYRILLIC SMALL LETTER ES
x<-gsub("&#x0ff0c;",", ",x) # FULLWIDTH COMMA
x<-gsub("&#x0060c;",",",x) # ARABIC COMMA
x<-gsub("&#x00410;","A",x) # CYRILLIC CAPITAL LETTER A
x<-gsub("&#x00051;","Q",x) # LATIN CAPITAL LETTER Q
x<-gsub("&#x00046;","F",x) # LATIN CAPITAL LETTER F
x<-gsub("&#x0043e;","o",x) # CYRILLIC SMALL LETTER O
x<-gsub("&#x00078;","x",x) # LATIN SMALL LETTER X
x<-gsub("&#x004c0;","I",x) #  CYRILLIC LETTER PALOCHKA
x<-gsub("&#x00044;","D",x) # LATIN CAPITAL LETTER D
x<-gsub("&#x00415;","E",x) # CYRILLIC CAPITAL LETTER IE
x<-gsub("&#x00397;","H",x) # GREEK CAPITAL LETTER ETA
x<-gsub("&#x00420;","P",x) # CYRILLIC CAPITAL LETTER ER
x<-gsub("&#x02131;","F",x) # SCRIPT CAPITAL F
x<-gsub("&#x02642;"," MALE ",x) # Male sign
x<-gsub("&#x02640;"," FEMALE ",x) # Female sign
x<-gsub("&#x00053;","'*S?",x) # superscript *S
x<-gsub("&#x00391;","A",x) #  A
x<-gsub("&#x02113;","l",x) # Latin letter &ell; -> l
x<-gsub("&#x00399;","I",x) # Capital I
x<-gsub("&#x0211c;","R",x) # BLACK-LETTER CAPITAL R
x<-gsub("&#x0266f;","#",x) #  MUSIC SHARP SIGN
}

if(length(grep("&#",x))>0){
x<-gsub("&#x00049;","I",x) # LATIN CAPITAL LETTER I
x<-gsub("&#x00056;","V",x) #  LATIN CAPITAL LETTER V
x<-gsub("&#x00058;","X",x) # LATIN CAPITAL LETTER X
x<-gsub("&#x001c0;","I",x) # LATIN LETTER DENTAL CLICK
x<-gsub("&#x002c3;",">",x) # MODIFIER LETTER RIGHT ARROWHEAD
x<-gsub("&#x0037e;",";",x) # GREEK QUESTION MARK look alike semicolun
x<-gsub("&#x00430;","a",x) # CYRILLIC SMALL LETTER A
x<-gsub("&#x00412;","B",x) # CYRILLIC CAPITAL LETTER VE
x<-gsub("&#x0041d;","H",x) # CYRILLIC CAPITAL LETTER EN
x<-gsub("&#x00456;","i",x) # CYRILLIC SMALL LETTER BYELORUSSIAN-UKRAINIAN I
x<-gsub("&#x00582;","L",x) # ARMENIAN SMALL LETTER YIWN
x<-gsub("&#x02153;","1/3",x) # VULGAR FRACTION ONE THIRD
x<-gsub("&#x02155;","1/5",x) # VULGAR FRACTION ONE FIFTH
x<-gsub("&#x0215b;","1/8",x) # VULGAR FRACTION ONE EIGHTH
x<-gsub("&#x0ff53;","s",x) # FULLWIDTH LATIN SMALL LETTER S
x<-gsub("&#x0010f;","d'",x) # LATIN SMALL LETTER D WITH CARON
x<-gsub("&#x00165;","t'",x) # LATIN SMALL LETTER T WITH CARON
x<-gsub("&#x00443;","y",x) #  CYRILLIC SMALL LETTER U
x<-gsub("&#x0039f;","O",x) # GREEK CAPITAL LETTER OMICRON
x<-gsub("&#x0041c;","M",x) # CYRILLIC CAPITAL LETTER EM
x<-gsub("&#x00422;","T",x) # CYRILLIC CAPITAL LETTER TE
x<-gsub("&#x00425;","X",x) # CYRILLIC CAPITAL LETTER HA
x<-gsub("&#x00435;","e",x) # CYRILLIC SMALL LETTER IE
x<-gsub("&#x0fb02;","fl",x) #  LATIN SMALL LIGATURE FL
x<-gsub("&#x0013d;","L'",x) # LATIN CAPITAL LETTER L WITH CARON
x<-gsub("&#x0013e;","l'",x) # LATIN SMALL LETTER L WITH CARON
x<-gsub("&#x001a1;","o",x) # LATIN SMALL LETTER O WITH HORN
x<-gsub("&#x000b9;","^1",x) # SUPERSCRIPT ONE
x<-gsub("&#x000b2;","^2",x) # SUPERSCRIPT TWO
x<-gsub("&#x000b3;","^3",x) # SUPERSCRIPT THREE
x<-gsub("&#x00395;","E",x) # GREEK CAPITAL LETTER EPSILON
x<-gsub("&#x0039d;","N",x) # GREEK CAPITAL LETTER NU
x<-gsub("&#x0039a;","K",x) # GREEK CAPITAL LETTER KAPPA
x<-gsub("&#x0039c;","M",x) # GREEK CAPITAL LETTER MU
x<-gsub("&#x003bf;","o",x) # GREEK SMALL LETTER OMICRON
x<-gsub("&#x0041e;","O",x) # CYRILLIC CAPITAL LETTER O
}

## unify hex
if(length(grep("&#",x))>0){
x<-gsub("&#x02550;","\u003d",x) # BOX DRAWINGS DOUBLE HORIZONTAL
x<-gsub("&#x02502;","\u007c",x) # BOX DRAWINGS LIGHT VERTICAL
x<-gsub("&#x00151;","\u00F6 ",x) # small ö &odblac;
}





## convert all other hexadecimals to unicode at once
if(length(grep("&#x[10]",x))>0){
i<-grep("&#x[10]",x)
x[i]<-unlist(lapply(x[i],udecode))
}
        

######################################################################
######################################################################
######################################################################


## OLD: manual conversion
if(length(grep("&#",x))>0){
x<-gsub("&#x00102;","\u0102",x) # Ă, LATIN CAPITAL LETTER A WITH BREVE 
x<-gsub("&#x000C1;","\u00C1",x) # Á 
x<-gsub("&#x000c1;","\u00c1",x) # Á 
x<-gsub("&#x000c4;","\u00c4",x) # Ä
x<-gsub("&#x000c6;","\u00c6",x) # AE &AElig;
x<-gsub("&#x00119;","\u0119",x) # e with little thingy: &eogon;
x<-gsub("&#x0015f;","\u015f",x) # s with little thingy: &scedil;
x<-gsub("&#x00105;","\u0105",x) # a with little thingy: &aogon;
x<-gsub("&#x000e4;","\u00e4",x) # ä
x<-gsub("&#x000dc;","\u00dc",x) # ü
x<-gsub("&#x000fc;","\u00fc",x) # ü
x<-gsub("&#x000f9;","\u00f9",x) # ù
x<-gsub("&#x000df;","\u00df",x) # ß
x<-gsub("&#x000e1;","\u00e1",x) # á
x<-gsub("&#x000e0;","\u00e0",x) # à
x<-gsub("&#x000e2;","\u00e2",x) # â a with hat
x<-gsub("&#x00103;","\u0103",x) # ă a with little u on top
x<-gsub("&#x00101;","\u0101",x) # a with bar
x<-gsub("&#x000e5;","\u00e5",x) # a with ° on top
x<-gsub("&#x000e6;","\u00E6",x) # a with e connected: &aelig;
x<-gsub("&#x0017C;","\u017C",x) # z with dot on top
x<-gsub("&#x0017c;","\u017C",x) # z with dot on top
x<-gsub("&#x0015b;","\u015B",x) # ś
x<-gsub("&#x00161;","\u0161",x) # s with turned ^ on top: &scaron;
x<-gsub("&#x0010d;","\u010d",x) # c with turned ^ on top: &ccaron;
x<-gsub("&#x000c7;","\u00C7",x) # C with little thingy: &Ccedil;
x<-gsub("&#x0010c;","\u010C",x) # Č, C with turned ^ on
x<-gsub("&#x00107;","\u0107",x) # ć
x<-gsub("&#x00108;","\u0108",x) # Ĉ, C with two dots
x<-gsub("&#x00109;","\u0109",x) # ĉ, c with hat
x<-gsub("&#x000e7;","\u00e7",x) # french c: &ccedil;
x<-gsub("&#x000c5;","\u00c5",x) # Angstrom letter: capital A with °
x<-gsub("&#x000e3;","\u00e3",x) # a with ~
x<-gsub("&#x000e8;","\u00e8",x) # è
x<-gsub("&#x000e9;","\u00e9",x) # é
x<-gsub("&#x0012b;","\u011b",x) # ě e with turned ^ on
x<-gsub("&#x0017b;","\u017b",x) # Ż, Z with dot on top
x<-gsub("&#x000d3;","\u00d3",x) # Ó O acute
x<-gsub("&#x00117;","\u0117",x) # ė e with point on top
x<-gsub("&#x000c9;","\u00C9",x) # É, e acute
x<-gsub("&#x000ea;","\u00ea",x) # ê
x<-gsub("&#x000ed;","\u00ed",x) # í i with '
x<-gsub("&#x000ec;","\u00ec",x) # ì i with `
x<-gsub("&#x000cd;","\u00cd",x) # Í 
x<-gsub("&#x00130;","\u0130",x) # I with a dot on top
x<-gsub("&#x00159;","\u0159",x) # r with turned ^on top ;  &rcaron;
x<-gsub("&#x00281;","\u02B6",x) # LATIN LETTER SMALL CAPITAL INVERTED R
x<-gsub("&#x0017e;","\u017E",x) # z with turned ^ on top: &zcaron;
x<-gsub("&#x0015a;","\u015A",x) # Ś with '
x<-gsub("&#x00160;","\u0160",x) # S with turned ^ on top: &Scaron;
x<-gsub("&#x0015e;","\u015E",x) # S with little thingy below: &Scedil;
x<-gsub("&#x01e6a;","\u1E6A",x) # Ṫ LATIN CAPITAL LETTER T WITH DOT ABOVE
x<-gsub("&#x0021b;","\u021B",x) # t with coma below ț
x<-gsub("&#x0016b;","\u016B",x) # u with bar
x<-gsub("&#x00219;","\u0219",x) # ș s with 'below
x<-gsub("&#x00141;","\u0141",x) # Ł: L with little / &Lstrok; Ł
x<-gsub("&#x00142;","\u0141",x) # ł: l with little / &lstrok;
x<-gsub("&#x000ef;","\u00EF",x) # iuml - ï - i with two dots on top
x<-gsub("&#x00131;","\u0131",x) # &imath;
x<-gsub("&#x0011f;","\u011F",x) # g with small u on top: &gbreve;
x<-gsub("&#x000d6;","\u00D6",x) # Ö
x<-gsub("&#x000f6;","\u00F6",x) # ö
x<-gsub("&#x000f8;","\u00F8",x) # ø: danish ö
x<-gsub("&#x00153;","\u0153",x) # oe: &oelig;
x<-gsub("&#x00152;","\u0152",x) # OE: &OElig;
x<-gsub("&#x000d8;","\u00D8",x) # Ø: danish Ö
x<-gsub("&#x000f3;","\u00F3",x) # ó
x<-gsub("&#x000f5;","\u00F5",x) # o tilde on top
x<-gsub("&#x000f2;","\u00F2",x) # ò
x<-gsub("&#x000f4;","\u00F4",x) # ô
x<-gsub("&#x000fd;","\u00FD",x) # ý
x<-gsub("&#x00176;","\u0176",x) # Y hat
x<-gsub("&#x003a5;","\u03a5",x) # Y 
x<-gsub("&#x0017a;","\u017A",x) # ź
x<-gsub("&#x000fa;","\u00FA",x) # ú
x<-gsub("&#x0016f;","\u016F",x) # u with ° on top
x<-gsub("&#x000f1;","\u00F1",x) # n-je ñ
x<-gsub("&#x05358;","\u00F1",x) # n-je ñ
x<-gsub("&#x00144;","\u0144",x) # ń
x<-gsub("&#x000eb;","\u00EB",x) # ë
# special signs
x<-gsub("&#x000ae;","\u00ae",x) #  REG-sign
x<-gsub("&#x000a9;","\u00a9",x) #  COPYRIGHT-sign
x<-gsub("&#x02122;","\u2122",x) #  TradeMark-sign
x<-gsub("&#x020ac;","\u20ac",x) #  Euro-sign
x<-gsub("&#x000a3;","\u00a3",x) #  Pound-sign
x<-gsub("&#x000a5;","\u00a5",x) #  Yen-sign
x<-gsub("&#x00024;","\u0024",x) #  Dollar-sign
x<-gsub("&#x02248;","\u2248",x) # approximate sign
x<-gsub("&#x0221e;","\u221e",x) # infinite sign
x<-gsub("&#x02460;","\u2460",x) # 1 in circle
x<-gsub("&#x02461;","\u2461",x) # 2 in circle
x<-gsub("&#x02462;","\u2462",x) # 3 in circle
x<-gsub("&#x02463;","\u2463",x) # 4 in circle
x<-gsub("&#x02464;","\u2464",x) # 5 in circle
x<-gsub("&#x00025;","\u0025",x) # percent
x<-gsub("&#x02030;","\u2030",x) # per mille
x<-gsub("&#x0224d;","\u003d",x) # EQUIVALENT TO
x<-gsub("&#x0007c;","\u007c",x) # vertical line
x<-gsub("&#x0043a;","\u043a",x) # kyrillik letter ka
x<-gsub("&#x003b9;","\u03b9",x) # greek letter iota
x<-gsub("&#x000c3;","\u00c3",x) # A with ^
x<-gsub("&#x000ee;","\u00ee",x) # i with ^
x<-gsub("&#x0014d;","\u014d",x) # o with bar 
x<-gsub("&#x000d4;","\u00d4",x) # O with ^
x<-gsub("&#x000bf;","\u00BF",x) # spanish start of question mark ¿
x<-gsub("&#x003b1;","\u03b1",x) # small greek alpha
x<-gsub("&#x00251;","\u0251",x) # small latin letter alpha
x<-gsub("&#x0221d;","\u221d",x) # PROPORTIONAL TO
x<-gsub("&#x003b2;","\u03b2",x) # small greek beta
x<-gsub("&#x00392;","\u0392",x) # capital greek beta
x<-gsub("&#x003c7;","\u03c7",x) # small greek chi
x<-gsub("&#x003a7;","\u03a7",x) # capital greek Chi
x<-gsub("&#x003c6;","\u03c6",x) # small greek phi
x<-gsub("&#x003a6;","\u03a6",x) # capital greek Phi
x<-gsub("&#x00424;","\u0424",x) # CYRILLIC CAPITAL LETTER EF -> looks like Phi
x<-gsub("&#x003d5;","\u03d5",x) # capital greek Phi
x<-gsub("&#x00278;","\u0278",x) # capital greek Phi
x<-gsub("&#x003a8;","\u03a8",x) # capital greek Psi
x<-gsub("&#x003c8;","\u03c8",x) # small greek Psi
x<-gsub("&#x00393;","\u0393",x) # capital greek Gamma
x<-gsub("&#x003c0;","\u03c0",x) # small greek pi
x<-gsub("&#x003af;","\u03af",x) # GREEK SMALL LETTER IOTA WITH TONOS
x<-gsub("&#x003bc;","\u03bc",x) # small greek mu
x<-gsub("&#x000b5;","\u00b5",x) # micro/small greek mu
x<-gsub("&#x003a3;","\u03a3",x) # Sum/capital greek sigma
x<-gsub("&#x02211;","\u2211",x) # Sum/capital greek sigma
x<-gsub("&#x0028a;","\u028a",x) # Latin upsilon
x<-gsub("&#x001b1;","\u01b1",x) # LATIN CAPITAL LETTER UPSILON
x<-gsub("&#x003c5;","\u03c5",x) # GREEK SMALL LETTER UPSILON
x<-gsub("&#x00254;","\u0254",x) # LATIN SMALL LETTER OPEN O
x<-gsub("&#x003c3;","\u03c3",x) # small greek sigma
x<-gsub("&#x003c2;","\u03c2",x) # GREEK SMALL LETTER FINAL SIGMA
x<-gsub("&#x003c9;","\u03c9",x) # small greek omega
x<-gsub("&#x003ce;","\u03ce",x) # GREEK SMALL LETTER OMEGA WITH TONOS
x<-gsub("&#x003bb;","\u03bb",x) # small greek lamda
x<-gsub("&#x003b4;","\u03b4",x) # small greek delta
x<-gsub("&#x02206;","\u2206",x) # capital greek delta (increment) triangle
x<-gsub("&#x025b5;","\u25b5",x) # utri (triangle)
x<-gsub("&#x025b3;","\u25b3",x) # WHITE UP-POINTING TRIANGLE
x<-gsub("&#x022bf;","\u22bf",x) # italic triangle
x<-gsub("&#x00394;","\u0394",x) #  delta-sign
x<-gsub("&#x003b5;","\u03b5",x) # small greek epsilon (epsi)
x<-gsub("&#x0025b;","\u025b",x) # LATIN SMALL LETTER OPEN E
x<-gsub("&#x003ad;","\u03ad",x) # GREEK SMALL LETTER EPSILON WITH TONOS 
x<-gsub("&#x003ba;","\u03ba",x) # small greek kappa
x<-gsub("&#x003f0;","\u03f0",x) # cursive kappa -> used as Chi
x<-gsub("&#x003b3;","\u03b3",x)# small greek gamma
x<-gsub("&#x003be;","\u03be",x) # small greek Xi 
x<-gsub("&#x0039e;","\u039e",x) # GREEK CAPITAL LETTER XI 
x<-gsub("&#x003A4;","\u03A4",x) # capital greek Tau
x<-gsub("&#x003c4;","\u03c4",x) # small greek tau 
x<-gsub("&#x0028c;","\u028c",x) # LATIN SMALL LETTER TURNED V
x<-gsub("&#x003c1;","\u03c1",x) # small greek rho
x<-gsub("&#x003b8;","\u03b8",x) # small greek theta
x<-gsub("&#x003d1;","\u03d1",x) # small greek theta
x<-gsub("&#x00398;","\u0398",x) # capital greek Theta
x<-gsub("&#x1d703;","\ud703",x) # capital greek Theta
x<-gsub("&#x003b6;","\u03b6",x) # capital greek Zeta
x<-gsub("&#x003a9;","\u03a9",x) # capital greek Omega (Ohm)
x<-gsub("&#x02126;","\u2126",x) # capital greek Omega (Ohm)
x<-gsub("&#x003bd;","\u03bd",x) # GREEK SMALL LETTER NU 
x<-gsub("&#x0039b;","\u039b",x) #  CAPITAL GREEK Lambda
x<-gsub("&#x003b7;","\u03b7",x) # eta
x<-gsub("&#x0014b;","\u014b",x) # engma for eta
x<-gsub("&#x00273;","\u0273",x) # eta
x<-gsub("&#x00220;","\u0220",x) # eta
x<-gsub("&#x0019e;","\u019e",x) # eta
x<-gsub("&#x000b0;","\u00B0",x) # degree sign
x<-gsub("&#x02103;","\u2103",x) # degree Celsius
x<-gsub("&#x000ba;","\u00ba",x) # MASCULINE ORDINAL INDICATOR 
x<-gsub("&#x02218;","\u2218",x) # RING OPERATOR
x<-gsub("&#x02020;","\u2020",x) # daggar (died)
x<-gsub("&#x02021;","\u2021",x) # double daggar (died)
x<-gsub("&#x020de;","\u20de",x) # square
x<-gsub("&#x025a1;","\u25a1",x) # white square
x<-gsub("&#x02225;","\u2225",x) # parallel to
x<-gsub("&#x02228;","\u2228",x) # &vee;
x<-gsub("&#x0221a;","\u221a",x) # square root
x<-gsub("&#x000af;","\u00af",x) # superscripted ¯
x<-gsub("&#x02202;","\u2202",x) # &PartialD;
x<-gsub("&#x022ef;","\u22EF",x) # MIDLINE HORIZONTAL ELLIPSIS 
x<-gsub("&#x02016;","\u2016",x) # Verbar
x<-gsub("&#x0210b;","\u210b",x) # HilbertSpace
x<-gsub("&#x02208;","\u2208",x) #  Element of
x<-gsub("&#x02229;","\u2229",x) #  INTERSECTION
x<-gsub("&#x0211d;","\u211d",x) #  DOUBLE-STRUCK CAPITAL R
x<-gsub("&#x02205;","\u2205",x) #  emptyset
x<-gsub("&#x0220f;","\u220f",x) #  Sum Product 
x<-gsub("&#x0222b;","\u222b",x) #  Integral
x<-gsub("&#x00283;","\u0283",x) #  LATIN SMALL LETTER ESH/Integral
x<-gsub("&#x00138;","\u0138",x) # &kgreen;
x<-gsub("&#x003f5;","\u03f5",x) # &varepsilon; (Element of)
x<-gsub("&#x02223;","\u2223",x) # &mid;
x<-gsub("&#x02209;","\u2209",x) # not element of
x<-gsub("&#x02227;","\u2227",x) # &and;
x<-gsub("&#x000ac;","\u00ac",x) # NOT-sign
x<-gsub("&#x02200;","\u2200",x) # ∀ for all-sign
x<-gsub("&#x02194;","\u2194",x) # Left right arrow
x<-gsub("&#x02193;","\u2193",x) # downwards arrow
x<-gsub("&#x02191;","\u2191",x) # upwards arrow
x<-gsub("&#x0025c;","\u025c",x) # LATIN SMALL LETTER REVERSED OPEN E
x<-gsub("&#x02713;","\u2713",x) # Hook
x<-gsub("&#x02670;","\u2670",x) # WEST SYRIAC CROSS 
x<-gsub("&#x000b6;","\u00b6",x) # English name pilcrow or paragraph mark
x<-gsub("&#x0005f;","\u005f",x) # low line
x<-gsub("&#x000a2;","\u00a2",x) # cent sign
x<-gsub("&#x000a8;","\u00a8",x) # DIAERESIS
x<-gsub("&#x00112;","\u0112",x) # LATIN CAPITAL LETTER E WITH MACRON
x<-gsub("&#x00192;","\u0192",x) #  LATIN SMALL LETTER F WITH HOOK
x<-gsub("&#x00194;","\u0194",x) # LATIN CAPITAL LETTER GAMMA
x<-gsub("&#x001a9;","\u01a9",x) # LATIN CAPITAL LETTER ESH
x<-gsub("&#x001bf;","\u01bf",x) # LATIN LETTER WYNN
x<-gsub("&#x00282;","\u0282",x) # LATIN SMALL LETTER S WITH HOOK
x<-gsub("&#x002c7;","\u02c7",x) # CARON
x<-gsub("&#x00305;","\u0305",x) # COMBINING OVERLINE
x<-gsub("&#x00307;","\u0307",x) # COMBINING DOT ABOVE
x<-gsub("&#x003a0;","\u03a0",x) # GREEK CAPITAL LETTER PI
x<-gsub("&#x003a4;","\u03a4",x) # GREEK CAPITAL LETTER TAU
x<-gsub("&#x003ae;","\u03ae",x) # GREEK SMALL LETTER ETA WITH TONOS
x<-gsub("&#x003d0;","\u03d0",x) # GREEK BETA SYMBOL
x<-gsub("&#x003d2;","\u03d2",x) # GREEK UPSILON WITH HOOK SYMBOL
x<-gsub("&#x003d6;","\u03d6",x) # GREEK PI SYMBOL
x<-gsub("&#x01d68;","\u1d68",x) # GREEK SUBSCRIPT SMALL LETTER RHO
x<-gsub("&#x01e57;","\u1e57",x) # LATIN SMALL LETTER P WITH DOT ABOVE
x<-gsub("&#x01f75;","\u1f75",x) # GREEK SMALL LETTER ETA WITH OXIA
x<-gsub("&#x02025;","\u2025",x) # TWO DOT LEADER
x<-gsub("&#x0203e;","\u203e",x) # OVERLINE
x<-gsub("&#x02115;","\u2115",x) # DOUBLE-STRUCK CAPITAL N
x<-gsub("&#x02119;","\u2119",x) # DOUBLE-STRUCK CAPITAL P
x<-gsub("&#x02124;","\u2124",x) # DOUBLE-STRUCK CAPITAL Z
x<-gsub("&#x021a6;","\u21a6",x) # RIGHTWARDS ARROW FROM BAR
x<-gsub("&#x021d2;","\u21d2",x) # RIGHTWARDS DOUBLE ARROW
x<-gsub("&#x021d4;","\u21d4",x) # LEFT RIGHT DOUBLE ARROW
x<-gsub("&#x02203;","\u2203",x) # THERE EXISTS
x<-gsub("&#x02207;","\u2207",x) # NABLA
x<-gsub("&#x0222a;","\u222a",x) # union set operator
x<-gsub("&#x02243;","\u2243",x) # ASYMPTOTICALLY EQUAL TO
x<-gsub("&#x02250;","\u2250",x) # APPROACHES THE LIMIT
x<-gsub("&#x02254;","\u2254",x) # COLON EQUALS
x<-gsub("&#x0225c;","\u225c",x) # DELTA EQUAL TO
x<-gsub("&#x02266;","\u2266",x) # LESS-THAN OVER EQUAL TO
x<-gsub("&#x0227a;","\u227a",x) # PRECEDES
x<-gsub("&#x0227b;","\u227b",x) # SUCCEEDS
x<-gsub("&#x02282;","\u2282",x) # SUBSET OF
x<-gsub("&#x02286;","\u2286",x) # SUBSET OF OR EQUAL TO
x<-gsub("&#x02295;","\u2295",x) # CIRCLED PLUS
x<-gsub("&#x02297;","\u2297",x) # CIRCLED TIMES
x<-gsub("&#x02299;","\u2299",x) # CIRCLED DOT OPERATOR
x<-gsub("&#x022a4;","\u22a4",x) # DOWN TACK
x<-gsub("&#x022a5;","\u22a5",x) # UP TACK
x<-gsub("&#x022b3;","\u22b3",x) # CONTAINS AS NORMAL SUBGROUP
x<-gsub("&#x022ba;","\u22ba",x) # INTERCALATE
x<-gsub("&#x022c0;","\u22c0",x) # N-ARY LOGICAL AND
x<-gsub("&#x022c3;","\u22c3",x) # N-ARY UNION
x<-gsub("&#x022ee;","\u22ee",x) # VERTICAL ELLIPSIS
x<-gsub("&#x022f1;","\u22f1",x) # DOWN RIGHT DIAGONAL ELLIPSIS
x<-gsub("&#x0230b;","\u230b",x) # RIGHT FLOOR
x<-gsub("&#x0231d;","\u231d",x) # TOP RIGHT CORNER
x<-gsub("&#x02322;","\u2322",x) # FROWN
x<-gsub("&#x025b7;","\u25b7",x) # WHITE RIGHT-POINTING TRIANGLE
x<-gsub("&#x025b9;","\u25b9",x) # WHITE RIGHT-POINTING SMALL TRIANGLE
x<-gsub("&#x02661;","\u2661",x) # WHITE HEART SUIT
x<-gsub("&#x027e9;","\u27e9",x) # MATHEMATICAL RIGHT ANGLE BRACKET
x<-gsub("&#x027f6;","\u27f6",x) # LONG RIGHTWARDS ARROW
x<-gsub("&#x0a64d;","\ua64d",x) # CYRILLIC SMALL LETTER BROAD OMEGA
x<-gsub("&#x0fe37;","\ufe37",x) # PRESENTATION FORM FOR VERTICAL LEFT CURLY BRACKET
x<-gsub("&#x0fe38;","\ufe38",x) # PRESENTATION FORM FOR VERTICAL RIGHT CURLY BRACKET
x<-gsub("&#x000a7;","\u00a7",x) # SECTION SIGN
x<-gsub("&#x000b8;","\u00b8",x) # CEDILLA
x<-gsub("&#x000c2;","\u00c2",x) # LATIN CAPITAL LETTER A WITH CIRCUMFLEX
x<-gsub("&#x000ce;","\u00ce",x) # LATIN CAPITAL LETTER I WITH CIRCUMFLEX
x<-gsub("&#x000dd;","\u00dd",x) # LATIN CAPITAL LETTER Y WITH ACUTE
x<-gsub("&#x00100;","\u0100",x) # LATIN CAPITAL LETTER A WITH MACRON
x<-gsub("&#x00113;","\u0113",x) # LATIN SMALL LETTER E WITH MACRON
x<-gsub("&#x0011b;","\u011b",x) # LATIN SMALL LETTER E WITH CARON
x<-gsub("&#x00124;","\u0124",x) # LATIN CAPITAL LETTER H WITH CIRCUMFLEX
x<-gsub("&#x00175;","\u0175",x) # LATIN SMALL LETTER W WITH CIRCUMFLEX
x<-gsub("&#x00190;","\u0190",x) # LATIN CAPITAL LETTER OPEN E
x<-gsub("&#x00198;","\u0198",x) # LATIN CAPITAL LETTER K WITH HOOK
x<-gsub("&#x0019f;","\u019f",x) # LATIN CAPITAL LETTER O WITH MIDDLE TILDE
x<-gsub("&#x001ce;","\u01ce",x) # LATIN SMALL LETTER A WITH CARON
x<-gsub("&#x00212;","\u0212",x) #  LATIN CAPITAL LETTER R WITH INVERTED BREVE
x<-gsub("&#x00245;","\u0245",x) # LATIN CAPITAL LETTER TURNED V
x<-gsub("&#x00255;","\u0255",x) #  LATIN SMALL LETTER C WITH CURL
x<-gsub("&#x00261;","\u0261",x) # LATIN SMALL LETTER SCRIPT G
x<-gsub("&#x00263;","\u0263",x) # LATIN SMALL LETTER GAMMA
x<-gsub("&#x00264;","\u0264",x) # LATIN SMALL LETTER RAMS HORN
x<-gsub("&#x0027e;","\u027e",x) # LATIN SMALL LETTER R WITH FISHHOOK
x<-gsub("&#x0027f;","\u027f",x) # LATIN SMALL LETTER REVERSED R WITH FISHHOOK
x<-gsub("&#x00285;","\u0285",x) # LATIN SMALL LETTER SQUAT REVERSED ESH
x<-gsub("&#x0029d;","\u029d",x) # LATIN SMALL LETTER J WITH CROSSED-TAIL
x<-gsub("&#x002a4;","\u02a4",x) #  LATIN SMALL LETTER DEZH DIGRAPH
x<-gsub("&#x002d8;","\u02d8",x) # BREVE
x<-gsub("&#x002da;","\u02da",x) # RING ABOVE
x<-gsub("&#x00303;","\u0303",x) # COMBINING TILDE
x<-gsub("&#x0041a;","\u041a",x) # CYRILLIC CAPITAL LETTER KA
x<-gsub("&#x0041f;","\u041f",x) # CYRILLIC CAPITAL LETTER PE
x<-gsub("&#x00440;","\u0440",x) # CYRILLIC SMALL LETTER ER look alike p
x<-gsub("&#x00445;","\u0445",x) # CYRILLIC SMALL LETTER HA
x<-gsub("&#x01d02;","\u1d02",x) # LATIN SMALL LETTER TURNED AE
x<-gsub("&#x01d26;","\u1d26",x) # GREEK LETTER SMALL CAPITAL GAMMA
x<-gsub("&#x01d27;","\u1d27",x) # GREEK LETTER SMALL CAPITAL LAMDA
x<-gsub("&#x01d52;","\u1d52",x) # MODIFIER LETTER SMALL O
x<-gsub("&#x01d61;","\u1d61",x) # MODIFIER LETTER SMALL CHI
x<-gsub("&#x01e7d;","\u1e7d",x) # LATIN SMALL LETTER V WITH TILDE
x<-gsub("&#x01e8f;","\u1e8f",x) # LATIN SMALL LETTER Y WITH DOT ABOVE
x<-gsub("&#x01e9e;","\u1e9e",x) # LATIN CAPITAL LETTER SHARP S
x<-gsub("&#x02070;","\u2070",x) # SUPERSCRIPT ZERO
x<-gsub("&#x02080;","\u2080",x) # SUBSCRIPT ZERO
x<-gsub("&#x020a9;","\u20a9",x) # WON SIGN
x<-gsub("&#x020d7;","\u20d7",x) # COMBINING RIGHT ARROW ABOVE
x<-gsub("&#x02102;","\u2102",x) #  DOUBLE-STRUCK CAPITAL C
x<-gsub("&#x0210f;","\u210f",x) # PLANCK CONSTANT OVER TWO PI
x<-gsub("&#x02110;","\u2110",x) # SCRIPT CAPITAL I
x<-gsub("&#x02111;","\u2111",x) # BLACK-LETTER CAPITAL I
x<-gsub("&#x02112;","\u2112",x) # SCRIPT CAPITAL L
x<-gsub("&#x02118;","\u2118",x) # SCRIPT CAPITAL P
x<-gsub("&#x0211b;","\u211b",x) # SCRIPT CAPITAL R
x<-gsub("&#x02127;","\u2127",x) # INVERTED OHM SIGN
x<-gsub("&#x02133;","\u2133",x) # SCRIPT CAPITAL M
x<-gsub("&#x02198;","\u2198",x) # SOUTH EAST ARROW
x<-gsub("&#x0220a;","\u220a",x) # SMALL ELEMENT OF
x<-gsub("&#x02213;","\u2213",x) # MINUS-OR-PLUS SIGN
x<-gsub("&#x022b2;","\u22b2",x) # NORMAL SUBGROUP OF
x<-gsub("&#x022c2;","\u22c2",x) # N-ARY INTERSECTION
x<-gsub("&#x02300;","\u2300",x) #  DIAMETER SIGN
x<-gsub("&#x02309;","\u2309",x) # RIGHT CEILING
x<-gsub("&#x0230a;","\u230a",x) # LEFT FLOOR
x<-gsub("&#x027e8;","\u27e8",x) # MATHEMATICAL LEFT ANGLE BRACKET
x<-gsub("&#x02a01;","\u2a01",x) # N-ARY CIRCLED PLUS OPERATOR
x<-gsub("&#x02a09;","\u2a09",x) # N-ARY TIMES OPERATOR
x<-gsub("&#x03002;","\u3002",x) # IDEOGRAPHIC FULL STOP
x<-gsub("&#x000a1;","\u00a1",x) # spanish INVERTED EXCLAMATION MARK
x<-gsub("&#x000ca;","\u00ca",x) # LATIN CAPITAL LETTER E WITH CIRCUMFLEX
x<-gsub("&#x00139;","\u0139",x) # LATIN CAPITAL LETTER L WITH ACUTE
x<-gsub("&#x00294;","\u0294",x) # LATIN LETTER GLOTTAL STOP
x<-gsub("&#x00396;","\u0396",x) # GREEK CAPITAL LETTER ZETA
x<-gsub("&#x00404;","\u0404",x) # CYRILLIC CAPITAL LETTER UKRAINIAN IE
x<-gsub("&#x01e87;","\u1e87",x) # LATIN SMALL LETTER W WITH DOT ABOVE
x<-gsub("&#x02081;","\u2081",x) # SUBSCRIPT ONE
x<-gsub("&#x02130;","\u2130",x) # SCRIPT CAPITAL E
x<-gsub("&#x02197;","\u2197",x) # NORTH EAST ARROW
x<-gsub("&#x021c0;","\u21c0",x) # RIGHTWARDS HARPOON WITH BARB UPWARDS
x<-gsub("&#x021d0;","\u21d0",x) #  LEFTWARDS DOUBLE ARROW
x<-gsub("&#x02204;","\u2204",x) # THERE DOES NOT EXIST
x<-gsub("&#x0220b;","\u220b",x) # CONTAINS AS MEMBER
x<-gsub("&#x0220d;","\u220d",x) # SMALL CONTAINS AS MEMBER
x<-gsub("&#x0220e;","\u220e",x) # end OF PROOF
x<-gsub("&#x02220;","\u2220",x) # ANGLE
x<-gsub("&#x0225d;","\u225d",x) # EQUAL TO BY DEFINITION
x<-gsub("&#x02272;","\u2272",x) # LESS-THAN OR EQUIVALENT TO
x<-gsub("&#x02273;","\u2273",x) # GREATER-THAN OR EQUIVALENT TO
x<-gsub("&#x02280;","\u2280",x) # DOES NOT PRECEDE
x<-gsub("&#x02283;","\u2283",x) # SUPERSET OF
x<-gsub("&#x02293;","\u2293",x) # SQUARE CAP
x<-gsub("&#x02294;","\u2294",x) # SQUARE CUP
x<-gsub("&#x02296;","\u2296",x) # CIRCLED MINUS
x<-gsub("&#x022a2;","\u22a2",x) # RIGHT TACK
x<-gsub("&#x022a8;","\u22a8",x) # TRUE
x<-gsub("&#x022c4;","\u22c4",x) # DIAMOND OPERATOR
x<-gsub("&#x02308;","\u2308",x) # LEFT CEILING
x<-gsub("&#x025bd;","\u25bd",x) # WHITE DOWN-POINTING TRIANGLE
x<-gsub("&#x025c7;","\u25c7",x) # WHITE DIAMOND
x<-gsub("&#x0266a;","\u266a",x) # EIGHTH NOTE
x<-gsub("&#x027f5;","\u27f5",x) # LONG LEFTWARDS ARROW
x<-gsub("&#x027fa;","\u27fa",x) # LONG LEFT RIGHT DOUBLE ARROW
x<-gsub("&#x02a6e;","\u2a6e",x) # EQUALS WITH ASTERISK
x<-gsub("&#x02a75;","\u2a75",x) # TWO CONSECUTIVE EQUALS SIGNS
x<-gsub("&#x02aa1;","\u2aa1",x) # DOUBLE NESTED LESS-THAN
x<-gsub("&#x02aeb;","\u2aeb",x) # DOUBLE UP TACK
x<-gsub("&#x03001;","\u3001",x) #  IDEOGRAPHIC COMMA
x<-gsub("&#x099dd;","\u99dd",x) # CJK UNIFIED IDEOGRAPH-99dd
x<-gsub("&#x0fffd;","\ufffd",x) # CIRCLED QUESTION MARK
x<-gsub("&#x0266;","\u266",x) # LATIN SMALL LETTER H WITH HOOK
x<-gsub("&#x025b6;","\u25b6",x) # Black right-pointing triangle
x<-gsub("&#x0204e;","\u204e",x) # LOW ASTERISK
x<-gsub("&#x0212b;","\u212b",x) #  ANGSTROM SIGN
x<-gsub("&#x000aa;","\u00aa",x) #  FEMININE ORDINAL INDICATOR
x<-gsub("&#x000da;","\u00da",x) # LATIN CAPITAL LETTER U WITH ACUTE
x<-gsub("&#x0201a;","\u201a",x) # SINGLE LOW-9 QUOTATION MARK
x<-gsub("&#x02160;","\u2160",x) # ROMAN NUMERAL ONE
x<-gsub("&#x02161;","\u2161",x) # ROMAN NUMERAL TWO
x<-gsub("&#x02162;","\u2162",x) # ROMAN NUMERAL Three
x<-gsub("&#x02163;","\u2163",x) # ROMAN NUMERAL FOUR
x<-gsub("&#x000f0;","\u00f0",x) # LATIN SMALL LETTER ETH
x<-gsub("&#x02591;","\u2591",x) #   LIGHT SHADE
x<-gsub("&#x0000d;","\u000d",x) #   CARRIAGE RETURN (CR)*
x<-gsub("&#x0200f;","\u200f",x) #   RIGHT-TO-LEFT MARK
x<-gsub("&#x0223d;","\u223d",x) # REVERSED TILDE
x<-gsub("&#x024c7;","\u24c7",x) # CIRCLED LATIN CAPITAL LETTER R
x<-gsub("&#x0029f;","\u029f",x) #    LATIN LETTER SMALL CAPITAL L
x<-gsub("&#x00304;","\u0304",x) # COMBINING MACRON
x<-gsub("&#x0017d;","\u017d",x) # LATIN CAPITAL LETTER Z WITH CARON
x<-gsub("&#x02082;","\u2082",x) # SUBSCRIPT TWO
x<-gsub("&#x000c0;","\u00c0",x) # LATIN CAPITAL LETTER A WITH GRAVE
x<-gsub("&#x0005e;","\u005e",x) # CIRCUMFLEX ACCENT
x<-gsub("&#x000fb;","\u00fb",x) # LATIN SMALL LETTER U WITH CIRCUMFLEX
x<-gsub("&#x002c9;","\u02c9",x) # MODIFIER LETTER MACRON
x<-gsub("&#x00387;","\u0387",x) # GREEK ANO TELEIA
x<-gsub("&#x001eb;","\u01eb",x) #  LATIN SMALL LETTER O WITH OGONEK
x<-gsub("&#x00218;","\u0218",x) # LATIN CAPITAL LETTER S WITH COMMA BELOW
x<-gsub("&#x02234;","\u2234",x) # THEREFORE
x<-gsub("&#x003ac;","\u03ac",x) # GREEK SMALL LETTER ALPHA WITH TONOS
x<-gsub("&#x02794;","\u2794",x) # HEAVY WIDE-HEADED RIGHTWARDS ARROW
x<-gsub("&#x00358;","\u0358",x) # COMBINING DOT ABOVE RIGHT
x<-gsub("&#x00444;","\u0444",x) # CYRILLIC SMALL LETTER EF
x<-gsub("&#x004d3;","\u04d3",x) # CYRILLIC SMALL LETTER A WITH DIAERESIS
x<-gsub("&#x02a52;","\u2a52",x) # LOGICAL OR WITH DOT ABOVE
x<-gsub("&#x00148;","\u0148",x) # LATIN SMALL LETTER N WITH CARON
x<-gsub("&#x001ac;","\u01ac",x) # LATIN CAPITAL LETTER T WITH HOOK
x<-gsub("&#x001fa;","\u01fa",x) # LATIN CAPITAL LETTER A WITH RING ABOVE AND ACUTE
x<-gsub("&#x01d05;","\u1d05",x) # LATIN LETTER SMALL CAPITAL D
x<-gsub("&#x02216;","\u2216",x) # SET MINUS
x<-gsub("&#x024b8;","\u24b8",x) # CIRCLED LATIN CAPITAL LETTER C
x<-gsub("&#x003a1;","\u03a1",x) # GREEK CAPITAL LETTER RHO
x<-gsub("&#x00413;","\u0413",x) # CYRILLIC CAPITAL LETTER GHE
x<-gsub("&#x00428;","\u0428",x) #  CYRILLIC CAPITAL LETTER SHA
x<-gsub("&#x00158;","\u0158",x) # LATIN CAPITAL LETTER R WITH CARON
x<-gsub("&#x00259;","\u0259",x) # LATIN SMALL LETTER SCHWA
x<-gsub("&#x002dd;","\u02dd",x) # DOUBLE ACUTE ACCENT
x<-gsub("&#x02534;","\u2534",x) # BOX DRAWINGS LIGHT UP AND HORIZONTAL
x<-gsub("&#x1d4ae;","\ud4ae",x) # HANGUL SYLLABLE D4AE
x<-gsub("&#x000cf;","\u00cf",x) # LATIN CAPITAL LETTER I WITH DIAERESIS
x<-gsub("&#x00110;","\u0110",x) # LATIN CAPITAL LETTER D WITH STROKE
x<-gsub("&#x00121;","\u0121",x) # LATIN SMALL LETTER G WITH DOT ABOVE
x<-gsub("&#x00163;","\u0163",x) # LATIN SMALL LETTER T WITH CEDILLA
x<-gsub("&#x00199;","\u0199",x) # LATIN SMALL LETTER K WITH HOOK
x<-gsub("&#x00279;","\u0279",x) # LATIN SMALL LETTER TURNED R
x<-gsub("&#x00457;","\u0457",x) # CYRILLIC SMALL LETTER YI
x<-gsub("&#x0049b;","\u049b",x) # CYRILLIC SMALL LETTER KA WITH DESCENDER
x<-gsub("&#x004a1;","\u04a1",x) # CYRILLIC SMALL LETTER BASHKIR KA
x<-gsub("&#x004b0;","\u04b0",x) # CYRILLIC CAPITAL LETTER STRAIGHT U WITH STROKE:
x<-gsub("&#x004e7;","\u04e7",x) # CYRILLIC SMALL LETTER O WITH DIAERESIS
x<-gsub("&#x021c4;","\u21c4",x) # RIGHTWARDS ARROW OVER LEFTWARDS ARROW
x<-gsub("&#x021cc;","\u21cc",x) # RIGHTWARDS HARPOON OVER LEFTWARDS HARPOON
x<-gsub("&#x025a0;","\u25a0",x) # BLACK SQUARE
x<-gsub("&#x000c8;","\u00c8",x) # LATIN CAPITAL LETTER E WITH GRAVE
x<-gsub("&#x00111;","\u0111",x) # LATIN SMALL LETTER D WITH STROKE
x<-gsub("&#x00125;","\u0125",x) # LATIN SMALL LETTER H WITH CIRCUMFLEX
x<-gsub("&#x00178;","\u0178",x) # LATIN CAPITAL LETTER Y WITH DIAERESIS
x<-gsub("&#x001ae;","\u01ae",x) # LATIN CAPITAL LETTER T WITH RETROFLEX HOOK
x<-gsub("&#x001b4;","\u01b4",x) # LATIN SMALL LETTER Y WITH HOOK
x<-gsub("&#x001d2;","\u01d2",x) # LATIN SMALL LETTER O WITH CARON
x<-gsub("&#x0021a;","\u021a",x) # LATIN CAPITAL LETTER T WITH COMMA BELOW
x<-gsub("&#x002cf;","\u02cf",x) # MODIFIER LETTER LOW ACUTE ACCENT
x<-gsub("&#x002d0;","\u02d0",x) # MODIFIER LETTER TRIANGULAR COLON
x<-gsub("&#x003f1;","\u03f1",x) # GREEK RHO SYMBOL
x<-gsub("&#x003f4;","\u03f4",x) # GREEK CAPITAL THETA SYMBOL
x<-gsub("&#x00640;","\u0640_",x) # ARABIC TATWEEL
x<-gsub("&#x01d67;","\u1d67",x) # GREEK SUBSCRIPT SMALL LETTER GAMMA
x<-gsub("&#x01e40;","\u1e40",x) # LATIN CAPITAL LETTER M WITH DOT ABOVE
x<-gsub("&#x0201f;","\u201f",x) # DOUBLE HIGH-REVERSED-9 QUOTATION MARK
x<-gsub("&#x0207a;","\u207a",x) # SUPERSCRIPT PLUS SIGN
x<-gsub("&#x02085;","\u2085",x) # SUBSCRIPT FIVE
x<-gsub("&#x020a6;","\u20a6",x) # NAIRA SIGN
x<-gsub("&#x020b5;","\u20b5",x) # CEDI SIGN
x<-gsub("&#x02116;","\u2116",x) # NUMERO SIGN
x<-gsub("&#x02120;","\u2120",x) # SERVICE MARK
x<-gsub("&#x021c6;","\u21c6",x) # LEFTWARDS ARROW OVER RIGHTWARDS ARROW
x<-gsub("&#x02259;","\u2259",x) # ESTIMATES
x<-gsub("&#x0229f;","\u229f",x) # SQUARED MINUS
x<-gsub("&#x02374;","\u2374",x) # APL FUNCTIONAL SYMBOL RHO
x<-gsub("&#x023b4;","\u23b4",x) # TOP SQUARE BRACKET
x<-gsub("&#x02756;","\u2756",x) # BLACK DIAMOND MINUS WHITE X
x<-gsub("&#x0d443;","\ud443",x) # HANGUL SYLLABLE D443
x<-gsub("&#x0fb00;","\ufb00",x) # LATIN SMALL LIGATURE FF
x<-gsub("&#x1d465;","\ud465",x) # HANGUL SYLLABLE D465
x<-gsub("&#x1d509;","\ud509",x) # HANGUL SYLLABLE D509
x<-gsub("&#x1d54a;","\ud54a",x) # HANGUL SYLLABLE D54A
x<-gsub("&#x0007e;","\u007e",x) # TILDE
x<-gsub("&#x000d0;","\u00d0",x) # LATIN CAPITAL LETTER ETH
x<-gsub("&#x000d2;","\u00d2",x) # LATIN CAPITAL LETTER O WITH GRAVE
x<-gsub("&#x000de;","\u00de",x) # LATIN CAPITAL LETTER THORN
x<-gsub("&#x00116;","\u0116",x) # LATIN CAPITAL LETTER E WITH DOT ABOVE
x<-gsub("&#x0011c;","\u011c",x) # LATIN CAPITAL LETTER G WITH CIRCUMFLEX
x<-gsub("&#x00129;","\u0129",x) # LATIN SMALL LETTER I WITH TILDE
x<-gsub("&#x00169;","\u0169",x) # LATIN SMALL LETTER U WITH TILDE
x<-gsub("&#x0018e;","\u018e",x) # LATIN CAPITAL LETTER REVERSED E
x<-gsub("&#x0043f;","\u043f",x) # CYRILLIC SMALL LETTER PE
x<-gsub("&#x00442;","\u0442",x) # CYRILLIC SMALL LETTER TE
x<-gsub("&#x0045b;","\u045b",x) # CYRILLIC SMALL LETTER TSHE
x<-gsub("&#x004d4;","\u04d4",x) # CYRILLIC CAPITAL LIGATURE A IE
x<-gsub("&#x0050e;","\u050e",x) # CYRILLIC CAPITAL LETTER KOMI TJE
x<-gsub("&#x00553;","\u0553",x) # ARMENIAN CAPITAL LETTER PIWR
x<-gsub("&#x005f4;","\u05f4",x) # HEBREW PUNCTUATION GERSHAYIM
x<-gsub("&#x00660;","\u0660",x) # ARABIC-INDIC DIGIT ZERO
x<-gsub("&#x008d9;","\u08d9",x) # ARABIC SMALL LOW NOON WITH KASRA
x<-gsub("&#x00903;","\u0903",x) # DEVANAGARI SIGN VISARG
x<-gsub("&#x015e1;","\u15e1",x) #  CANADIAN SYLLABICS CARRIER THA
x<-gsub("&#x01d00;","\u1d00",x) # LATIN LETTER SMALL CAPITAL A
x<-gsub("&#x01d2a;","\u1d2a",x) # GREEK LETTER SMALL CAPITAL PSI
x<-gsub("&#x01d5e;","\u1d5e",x) # MODIFIER LETTER SMALL GREEK GAMMA
x<-gsub("&#x01db2;","\u1db2",x) # MODIFIER LETTER SMALL PHI
x<-gsub("&#x01e16;","\u1e16",x) # LATIN CAPITAL LETTER E WITH MACRON AND ACUTE
x<-gsub("&#x01e21;","\u1e21",x) # LATIN SMALL LETTER G WITH MACRON
x<-gsub("&#x01e25;","\u1e25",x) # LATIN SMALL LETTER H WITH DOT BELOW
x<-gsub("&#x01e43;","\u1e43",x) # LATIN SMALL LETTER M WITH DOT BELOW
x<-gsub("&#x01e69;","\u1e69",x) # LATIN SMALL LETTER S WITH DOT BELOW AND DOT ABOVE
x<-gsub("&#x01e6b;","\u1e6b",x) #  LATIN SMALL LETTER T WITH DOT ABOVE
x<-gsub("&#x01e7c;","\u1e7c",x) # LATIN CAPITAL LETTER V WITH TILDE
x<-gsub("&#x01e8a;","\u1e8a",x) # LATIN CAPITAL LETTER X WITH DOT ABOVE
x<-gsub("&#x01e96;","\u1e96",x) # LATIN SMALL LETTER H WITH LINE BELOW
x<-gsub("&#x01eaf;","\u1eaf",x) # LATIN SMALL LETTER A WITH BREVE AND ACUTE
x<-gsub("&#x01ebf;","\u1ebf",x) # LATIN SMALL LETTER E WITH CIRCUMFLEX AND ACUTE
x<-gsub("&#x01ecb;","\u1ecb",x) # LATIN SMALL LETTER I WITH DOT BELOW
x<-gsub("&#x01ed9;","\u1ed9",x) # LATIN SMALL LETTER O WITH CIRCUMFLEX AND DOT BELOW
x<-gsub("&#x01f04;","\u1f04",x) # GREEK SMALL LETTER ALPHA WITH PSILI AND OXIA
x<-gsub("&#x01f20;","\u1f20",x) # GREEK SMALL LETTER ETA WITH PSILI
x<-gsub("&#x01fd6;","\u1fd6",x) # GREEK SMALL LETTER IOTA WITH PERISPOMENI:
x<-gsub("&#x01ffb;","\u1ffb",x) # GREEK CAPITAL LETTER OMEGA WITH OXIA
x<-gsub("&#x0202a;","\u202a",x) # LEFT-TO-RIGHT EMBEDDING
x<-gsub("&#x0202b;","\u202b",x) # RIGHT-TO-LEFT EMBEDDING
x<-gsub("&#x02043;","\u2043",x) # HYPHEN BULLET
x<-gsub("&#x02079;","\u2079",x) # SUPERSCRIPT NINE
x<-gsub("&#x0207b;","\u207b",x) # SUPERSCRIPT MINUS
x<-gsub("&#x02086;","\u2086",x) # SUBSCRIPT SIX
x<-gsub("&#x0211e;","\u211e",x) # PRESCRIPTION TAKE
x<-gsub("&#x0212c;","\u212c",x) # SCRIPT CAPITAL B
x<-gsub("&#x02135;","\u2135",x) # ALEF SYMBOL
x<-gsub("&#x02165;","\u2165",x) # ROMAN NUMERAL SIX
x<-gsub("&#x02173;","\u2173",x) # SMALL ROMAN NUMERAL FOUR
x<-gsub("&#x021a8;","\u21a8",x) # UP DOWN ARROW WITH BASE
x<-gsub("&#x021b1;","\u21b1",x) # UPWARDS ARROW WITH TIP RIGHTWARDS
x<-gsub("&#x021c7;","\u21c7",x) # LEFTWARDS PAIRED ARROWS
x<-gsub("&#x021cb;","\u21cb",x) # LEFTWARDS HARPOON OVER RIGHTWARDS HARPOON
x<-gsub("&#x0221b;","\u221b",x) # CUBE ROOT
x<-gsub("&#x0222e;","\u222e",x) # CONTOUR INTEGRAL
x<-gsub("&#x0224a;","\u224a",x) # ALMOST EQUAL OR EQUAL TO
x<-gsub("&#x02263;","\u2263",x) # STRICTLY EQUIVALENT TO
x<-gsub("&#x02279;","\u2279",x) # NEITHER GREATER-THAN NOR LESS-THAN
x<-gsub("&#x02288;","\u2288",x) # NEITHER A SUBSET OF NOR EQUAL TO
x<-gsub("&#x0228a;","\u228a",x) # SUBSET OF WITH NOT EQUAL TO
x<-gsub("&#x0228e;","\u228e",x) # MULTISET UNION
x<-gsub("&#x0229a;","\u229a",x) # CIRCLED RING OPERATOR
x<-gsub("&#x0229e;","\u229e",x) # SQUARED PLUS
x<-gsub("&#x022a0;","\u22a0",x) # SQUARED TIMES
x<-gsub("&#x022cd;","\u22cd",x) # REVERSED TILDE EQUALS
x<-gsub("&#x022d9;","\u22d9",x) # VERY MUCH GREATER-THAN
x<-gsub("&#x0231c;","\u231c",x) # TOP LEFT CORNER
x<-gsub("&#x02366;","\u2366",x) # APL FUNCTIONAL SYMBOL DOWN SHOE STILE
x<-gsub("&#x02394;","\u2394",x) # SOFTWARE-FUNCTION SYMBOL
x<-gsub("&#x02395;","\u2395",x) # APL FUNCTIONAL SYMBOL QUAD
x<-gsub("&#x024bb;","\u24bb",x) # CIRCLED LATIN CAPITAL LETTER F
x<-gsub("&#x02524;","\u2524",x) # BOX DRAWINGS LIGHT VERTICAL AND LEFT
x<-gsub("&#x02593;","\u2593",x) # DARK SHADE
x<-gsub("&#x025ac;","\u25ac",x) # BLACK RECTANGLE
x<-gsub("&#x025af;","\u25af",x) # WHITE VERTICAL RECTANGLE
x<-gsub("&#x025fd;","\u25fd",x) # WHITE MEDIUM SMALL SQUARE
x<-gsub("&#x0261b;","\u261b",x) # BLACK RIGHT POINTING INDEX
x<-gsub("&#x02781;","\u2781",x) # DINGBAT CIRCLED SANS-SERIF DIGIT TWO
x<-gsub("&#x0279d;","\u279d",x) # TRIANGLE-HEADED RIGHTWARDS ARROW
x<-gsub("&#x027c2;","\u27c2",x) # PERPENDICULAR
x<-gsub("&#x02a2f;","\u2a2f",x) # VECTOR OR CROSS PRODUCT
x<-gsub("&#x02c6a;","\u2c6a",x) # LATIN SMALL LETTER K WITH DESCENDER
x<-gsub("&#x0300e;","\u300e",x) # LEFT WHITE CORNER BRACKET
x<-gsub("&#x0305f;","\u305f",x) # HIRAGANA LETTER TA
x<-gsub("&#x0e5fb;","\ue5fb",x) # Private Use Area E5FB
x<-gsub("&#x0fb03;","\ufb03",x) # LATIN SMALL LIGATURE FFI
x<-gsub("&#x0ff65;","\uff65",x) # HALFWIDTH KATAKANA MIDDLE DOT
x<-gsub("&#x000a4;","\u00a4",x) # CURRENCY SIGN
x<-gsub("&#x000a6;","\u00a6",x) # BROKEN BAR
x<-gsub("&#x000cb;","\u00cb",x) # LATIN CAPITAL LETTER E WITH DIAERESIS
x<-gsub("&#x000cc;","\u00cc",x) # LATIN CAPITAL LETTER I WITH GRAVE
x<-gsub("&#x000d1;","\u00d1",x) #  LATIN CAPITAL LETTER N WITH TILDE
x<-gsub("&#x000d5;","\u00d5",x) #  LATIN CAPITAL LETTER O WITH TILDE
x<-gsub("&#x000d9;","\u00d9",x) # LATIN CAPITAL LETTER U WITH GRAVE
x<-gsub("&#x000fe;","\u00fe",x) # LATIN SMALL LETTER THORN
x<-gsub("&#x000ff;","\u00ff",x) # LATIN SMALL LETTER Y WITH DIAERESIS
x<-gsub("&#x00104;","\u0104",x) # LATIN CAPITAL LETTER A WITH OGONEK
x<-gsub("&#x00106;","\u0106",x) # LATIN CAPITAL LETTER C WITH ACUTE
x<-gsub("&#x0010b;","\u010b",x) # LATIN SMALL LETTER C WITH DOT ABOVE
x<-gsub("&#x00115;","\u0115",x) # LATIN SMALL LETTER E WITH BREVE
x<-gsub("&#x00118;","\u0118",x) # LATIN CAPITAL LETTER E WITH OGONEK
x<-gsub("&#x0011a;","\u011a",x) # LATIN CAPITAL LETTER E WITH CARON
x<-gsub("&#x0011d;","\u011d",x) # LATIN SMALL LETTER G WITH CIRCUMFLEX
x<-gsub("&#x0011e;","\u011e",x) # LATIN CAPITAL LETTER G WITH BREVE
x<-gsub("&#x00122;","\u0122",x) # LATIN CAPITAL LETTER G WITH CEDILLA
x<-gsub("&#x00123;","\u0123",x) # LATIN SMALL LETTER G WITH CEDILLA
x<-gsub("&#x00127;","\u0127",x) # LATIN SMALL LETTER H WITH STROKE
x<-gsub("&#x0012d;","\u012d",x) # LATIN SMALL LETTER I WITH BREVE
x<-gsub("&#x0012e;","\u012e",x) # LATIN CAPITAL LETTER I WITH OGONEK
x<-gsub("&#x00136;","\u0136",x) # LATIN CAPITAL LETTER K WITH CEDILLA
x<-gsub("&#x00137;","\u0137",x) # LATIN SMALL LETTER K WITH CEDILLA
x<-gsub("&#x0013a;","\u013a",x) # LATIN SMALL LETTER L WITH ACUT
x<-gsub("&#x0013b;","\u013b",x) # LATIN CAPITAL LETTER L WITH CEDILLA
x<-gsub("&#x0013c;","\u013c",x) # LATIN SMALL LETTER L WITH CEDILLA
x<-gsub("&#x00143;","\u0143",x) # LATIN CAPITAL LETTER N WITH ACUTE
x<-gsub("&#x00146;","\u0146",x) # LATIN SMALL LETTER N WITH CEDILLA
x<-gsub("&#x00147;","\u0147",x) # LATIN CAPITAL LETTER N WITH CARON
x<-gsub("&#x0014c;","\u014c",x) # LATIN CAPITAL LETTER O WITH MACRON
x<-gsub("&#x00150;","\u0150",x) # LATIN CAPITAL LETTER O WITH DOUBLE ACUTE
x<-gsub("&#x00154;","\u0154",x) # LATIN CAPITAL LETTER R WITH ACUTE
x<-gsub("&#x00155;","\u0155",x) # LATIN SMALL LETTER R WITH ACUT
x<-gsub("&#x0015d;","\u015d",x) # LATIN SMALL LETTER S WITH CIRCUMFLEX
x<-gsub("&#x00162;","\u0162",x) # LATIN CAPITAL LETTER T WITH CEDILLA
x<-gsub("&#x0016a;","\u016a",x) # LATIN CAPITAL LETTER U WITH MACRON
x<-gsub("&#x00170;","\u0170",x) # LATIN CAPITAL LETTER U WITH DOUBLE ACUTE
x<-gsub("&#x00171;","\u0171",x) # LATIN SMALL LETTER U WITH DOUBLE ACUTE
x<-gsub("&#x00179;","\u0179",x) # LATIN CAPITAL LETTER Z WITH ACUTE
x<-gsub("&#x00197;","\u0197",x) # LATIN CAPITAL LETTER I WITH STROKE
x<-gsub("&#x0019a;","\u019a",x) # LATIN SMALL LETTER L WITH BAR
x<-gsub("&#x001a0;","\u01a0",x) # LATIN CAPITAL LETTER O WITH HORN
x<-gsub("&#x001cd;","\u01cd",x) # LATIN CAPITAL LETTER A WITH CARON
x<-gsub("&#x001d0;","\u01d0",x) #  LATIN SMALL LETTER I WITH CARON
x<-gsub("&#x001d4;","\u01d4",x) # LATIN SMALL LETTER U WITH CARON
x<-gsub("&#x001d6;","\u01d6",x) # LATIN SMALL LETTER U WITH DIAERESIS AND MACRON
x<-gsub("&#x001da;","\u01da",x) # LATIN SMALL LETTER U WITH DIAERESIS AND CARON
x<-gsub("&#x001e7;","\u01e7",x) # LATIN SMALL LETTER G WITH CARON
x<-gsub("&#x001f9;","\u01f9",x) # LATIN SMALL LETTER N WITH GRAVE
x<-gsub("&#x001ff;","\u01ff",x) # LATIN SMALL LETTER O WITH STROKE AND ACUTE
x<-gsub("&#x00203;","\u0203",x) # LATIN SMALL LETTER A WITH INVERTED BREVE
x<-gsub("&#x00205;","\u0205",x) # LATIN SMALL LETTER E WITH DOUBLE GRAVE
x<-gsub("&#x00207;","\u0207",x) # LATIN SMALL LETTER E WITH INVERTED BREVE
x<-gsub("&#x0020b;","\u020b",x) #  LATIN SMALL LETTER I WITH INVERTED BREVE
x<-gsub("&#x0020d;","\u020d",x) # LATIN SMALL LETTER O WITH DOUBLE GRAVE
x<-gsub("&#x0020f;","\u020f",x) # LATIN SMALL LETTER O WITH INVERTED BREVE
x<-gsub("&#x00213;","\u0213",x) # LATIN SMALL LETTER R WITH INVERTED BREVE
x<-gsub("&#x00226;","\u0226",x) # LATIN CAPITAL LETTER A WITH DOT ABOVE
x<-gsub("&#x00227;","\u0227",x) # LATIN SMALL LETTER A WITH DOT ABOVE
x<-gsub("&#x00229;","\u0229",x) # LATIN SMALL LETTER E WITH CEDILLA
x<-gsub("&#x00236;","\u0236",x) # LATIN SMALL LETTER T WITH CURL
x<-gsub("&#x002db;","\u02db",x) # OGONEK
x<-gsub("&#x00306;","\u0306",x) # COMBINING BREVE
x<-gsub("&#x00308;","\u0308",x) # COMBINING DIAERESIS
x<-gsub("&#x00317;","\u0317",x) # COMBINING ACUTE ACCENT BELOW
x<-gsub("&#x00327;","\u0327",x) # COMBINING CEDILLA
x<-gsub("&#x00328;","\u0328",x) # COMBINING OGONEK
x<-gsub("&#x00363;","\u0363",x) # COMBINING LATIN SMALL LETTER A
x<-gsub("&#x003ca;","\u03ca",x) # GREEK SMALL LETTER IOTA WITH DIALYTIKA
x<-gsub("&#x003cb;","\u03cb",x) # GREEK SMALL LETTER UPSILON WITH DIALYTIKA
x<-gsub("&#x003cc;","\u03cc",x) # GREEK SMALL LETTER OMICRON WITH TONOS
x<-gsub("&#x00447;","\u0447",x) # CYRILLIC SMALL LETTER CHE
x<-gsub("&#x00450;","\u0450",x) # CYRILLIC SMALL LETTER IE WITH GRAVE
x<-gsub("&#x00451;","\u0451",x) # CYRILLIC SMALL LETTER IO
x<-gsub("&#x00481;","\u0481",x) # CYRILLIC SMALL LETTER KOPPA
x<-gsub("&#x004aa;","\u04aa",x) # CYRILLIC CAPITAL LETTER ES WITH DESCENDER
x<-gsub("&#x004ab;","\u04ab",x) # CYRILLIC SMALL LETTER ES WITH DESCENDER
x<-gsub("&#x004d1;","\u04d1",x) # CYRILLIC SMALL LETTER A WITH BREVE
x<-gsub("&#x004e6;","\u04e6",x) # CYRILLIC CAPITAL LETTER O WITH DIAERESIS
x<-gsub("&#x00822;","\u0822",x) # SAMARITAN VOWEL SIGN LONG A
x<-gsub("&#x01d0c;","\u1d0c",x) # LATIN LETTER SMALL CAPITAL L WITH STROKE
x<-gsub("&#x01d43;","\u1d43",x) # MODIFIER LETTER SMALL A
x<-gsub("&#x01d47;","\u1d47",x) # MODIFIER LETTER SMALL B
x<-gsub("&#x01d9c;","\u1d9c",x) # MODIFIER LETTER SMALL C
x<-gsub("&#x01e3f;","\u1e3f",x) # LATIN SMALL LETTER M WITH ACUTE
x<-gsub("&#x01e45;","\u1e45",x) # LATIN SMALL LETTER N WITH DOT ABOVE
x<-gsub("&#x01e4a;","\u1e4a",x) # LATIN CAPITAL LETTER N WITH CIRCUMFLEX BELOW
x<-gsub("&#x01e61;","\u1e61",x) # LATIN SMALL LETTER S WITH DOT ABOVE
x<-gsub("&#x01e63;","\u1e63",x) # LATIN SMALL LETTER S WITH DOT BELOW
x<-gsub("&#x01e97;","\u1e97",x) # LATIN SMALL LETTER T WITH DIAERESIS
x<-gsub("&#x01e99;","\u1e99",x) # LATIN SMALL LETTER Y WITH RING ABOVE
x<-gsub("&#x01ea1;","\u1ea1",x) # LATIN SMALL LETTER A WITH DOT BELOW
x<-gsub("&#x01ea3;","\u1ea3",x) # LATIN SMALL LETTER A WITH HOOK ABOVE
x<-gsub("&#x01ea5;","\u1ea5",x) # LATIN SMALL LETTER A WITH CIRCUMFLEX AND ACUTE
x<-gsub("&#x01ea7;","\u1ea7",x) # LATIN SMALL LETTER A WITH CIRCUMFLEX AND GRAVE
x<-gsub("&#x01ebb;","\u1ebb",x) # LATIN SMALL LETTER E WITH HOOK ABOVE
x<-gsub("&#x01ebd;","\u1ebd",x) # LATIN SMALL LETTER E WITH TILDE
x<-gsub("&#x01ec1;","\u1ec1",x) # LATIN SMALL LETTER E WITH CIRCUMFLEX AND GRAVE
x<-gsub("&#x01ec3;","\u1ec3",x) # LATIN SMALL LETTER E WITH CIRCUMFLEX AND HOOK ABOVE
x<-gsub("&#x01ec4;","\u1ec4",x) # LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND TILDE
x<-gsub("&#x01ec5;","\u1ec5",x) # LATIN SMALL LETTER E WITH CIRCUMFLEX AND TILDE
x<-gsub("&#x01ecd;","\u1ecd",x) # LATIN SMALL LETTER O WITH DOT BELOW
x<-gsub("&#x01ed1;","\u1ed1",x) # LATIN SMALL LETTER O WITH CIRCUMFLEX AND ACUTE
x<-gsub("&#x01ed3;","\u1ed3",x) # LATIN SMALL LETTER O WITH CIRCUMFLEX AND GRAVE
x<-gsub("&#x01edd;","\u1edd",x) # LATIN SMALL LETTER O WITH HORN AND GRAVE
x<-gsub("&#x01ee3;","\u1ee3",x) # LATIN SMALL LETTER O WITH HORN AND DOT BELOW
x<-gsub("&#x01eef;","\u1eef",x) #  LATIN SMALL LETTER U WITH HORN AND TILDE
x<-gsub("&#x01ef3;","\u1ef3",x) # LATIN SMALL LETTER Y WITH GRAVE
x<-gsub("&#x01ef7;","\u1ef7",x) # LATIN SMALL LETTER Y WITH HOOK ABOVE
x<-gsub("&#x01ef9;","\u1ef9",x) # LATIN SMALL LETTER Y WITH TILDE
x<-gsub("&#x01f77;","\u1f77",x) # GREEK SMALL LETTER IOTA WITH OXIA:
x<-gsub("&#x01f79;","\u1f79",x) # GREEK SMALL LETTER OMICRON WITH OXIA
x<-gsub("&#x01fc6;","\u1fc6",x) # GREEK SMALL LETTER ETA WITH PERISPOMENI
x<-gsub("&#x02251;","\u2251",x) # GEOMETRICALLY EQUAL TO
x<-gsub("&#x0227f;","\u227f",x) # SUCCEEDS OR EQUIVALENT TO
x<-gsub("&#x02298;","\u2298",x) #  CIRCLED DIVISION SLASH
x<-gsub("&#x0263a;","\u263a",x) # WHITE SMILING FACE
x<-gsub("&#x0010e;","\u010e",x) # LATIN CAPITAL LETTER D WITH CARON

}

}# End hexadecimal conversion

## unify some html letters  
x<-gsub("&amp;","& ",x) #  &
x<-gsub("&lt;","<",x) # less than
x<-gsub("&le;","<=",x) # less equal 
x<-gsub("&gt;",">",x) # greater than
x<-gsub("&ge;",">=",x) # greater equal 
x<-gsub("&equals;|&#61;","=",x) # equal sign
x<-gsub("&amp;","& ",x) #  &
x<-gsub("&acute;","'",x) # 
x<-gsub("&DiacriticalAcute;","'",x) # 
x<-gsub("&apos;","'",x) # 
x<-gsub("&semi;",";",x) # semi colon
x<-gsub("&comma;",",",x) # comma
x<-gsub("&quot;","'",x) # 
x<-gsub("&lpar;","(",x) # 
x<-gsub("&rpar;",")",x) # )
x<-gsub("&rsqb;|&rbrack;","]",x) # 
x<-gsub("&lsqb;|&lbrack;","[",x) # 
x<-gsub("&sup2;","^2",x) # superscript 2
x<-gsub("&sol;","/",x) # /
x<-gsub("&chi;","\u03a7",x) # Chi
x<-gsub("&beta;","\u03b2",x) # beta
x<-gsub("&alpha;","\u03b1",x) # alpha

## unify unicode special characters 
x<-gsub("\u2264|\u2A7f|\u2a7d","<=",x) # less equal 
x<-gsub("\u2A7e|\u2265",">=",x) # greater equal 
x<-gsub("\u003d","=",x) # equal sign
# special spaces
x<-gsub("\u00A0","",x) # no break space
x<-gsub("\u200b","",x) # invisible space
x<-gsub("\u200a|\u200c|\u200d|\u200e|\u200f"," ",x)
x<-gsub("\u2000|\u2001|\u2002|\u2003|\u2004"," ",x)
x<-gsub("\u2005|\u2006|\u2007|\u2008|\u2009"," ",x)
# minus/dash -
x<-gsub("\u2013|\u2014|\u2015|\u2212|\u2010|\u2011","-",x)
x<-gsub("\u00B2","^2",x) # superscript 2
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
# Zeta -> Z
x<-gsub("\u0396","Z",x)
x<-gsub("\u03b3","gamma",x) 
# delta
x<-gsub("\u03b4|\u2206|\u25b5|\u25b3|\u22bf|\u0394","delta ",x)
# eta
x<-gsub("\u019e|\u03b7|\u014b|\u0273|\u0220","eta",x) 
x<-gsub("\u03ae|\u1f75|\u220([^a-z0-9])","eta\\1", x)
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
x<-gsub("\u03c1|\u2374","rho",x)
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

# convert cermine specific letter captures
# unsolvable and very crappy: minus gets sometimes converted to "2"
if(cermine==TRUE){
check<-x
  # clean up white spaces
  x<-gsub("^ *|(?<= ) | *$", "", x, perl = TRUE)
 
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
  x<-gsub("(CI) \\)([.0-9])","\\1: -\\2",x)
  
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

  # correct badly captured "-" as 2 in r(df)<=>2.num
  x<-gsub("(r\\([0-9]*?\\)[<=>]*?)2\\.","\\1-.",x)
  x<-gsub("(r\\([0-9]*?\\) [<=>]*? )2\\.","\\1-.",x)

  # correct badly captured "-" as 2 in r(df)<=>2num.num but not t(df)=2.num0
  x<-gsub("(t\\([0-9]*?\\)[<=>]*?)2([0-9]\\.)","\\1-\\2",x)
  x<-gsub("(t\\([0-9]*?\\) [<=>]*? )2([0-9]\\.)","\\1-\\2",x)

  # correct badly captured "-" as 2 in z<=>2num.num but not z=2.num0
 # x<-gsub("([^a-zA-Z][zZ][<=>]*?)2([0-9]\\.)","\\1-\\2",x)
 # x<-gsub("([^a-zA-Z][zZ] [<=>]*? )2([0-9]\\.)","\\1-\\2",x)

  
if(warning==TRUE) if(sum(check==x)!=length(x)) warning("CERMINE specific letter conversion was used to correct for some conversion errors.\n '<=>' was inserted by letter.convert() to make statistics readable.\n Note: The minus sign is sometimes converted to '2' or not at all!")  
}

#cleanup
x<-gsub("\\t","",x)
# white spaces
x<-gsub("^ *|(?<= ) | *$", "", x, perl = TRUE)
# unify -
#x<-gsub("—|–","-",x)

return(x)
}

## Function to convert unicode to ASCII
udecode <- function(string){
# functions
uconv <- function(chars) intToUtf8(strtoi(chars, 16L))
  ufilter <- function(string){
  tryCatch({
  input<-string
    if(substr(string, 1, 1)=="|") return(uconv(substr(string,2,5))) else return(string)
    },error=function(e){return(input)})   
    #    if (length(grep("^[|][a-zA-z0-9]{4}",x))>0) uconv(substr(string, 2, 5)) else string
  }
  # convert | -> @@
  string<-gsub("[|]","@@",string)
# hex to unicode
  if(length(grep("&#x0",string))>0){
  i<-grep("&#x0",string)
  string[i]<-gsub("(\\u....);","\\1",gsub("&#x0","\\u",string[i],fixed=T))
  }
  if(length(grep("&#x1",string))>0){
  i<-grep("&#x1",string)
  string[i]<-gsub("(\\u....);","\\1",gsub("&#x1","\\u",string[i],fixed=T))
  }
  # unicode to raw
  string <- gsub("\\\\u([a-zA-z0-9]{4})", "|\\1", string, perl=TRUE)
  strings <- unlist(strsplit2(unlist(strsplit2(string, "[|][a-zA-z0-9]{4}","after")), "[|][a-zA-z0-9]{4}","before"))
  i<-which(is.na(sapply(strings, ufilter)))
  replacement<-sapply(strings, ufilter)
  replacement[i]<-names(replacement)[i]
  string <- paste(replacement, collapse='')
  # reconvert @@ -> |
  string<-gsub("@@","|",string)

  return(string)
}
