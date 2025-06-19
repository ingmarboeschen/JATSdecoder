#' table2matrix
#'
#' Extracts and converts HTML tables to a list of R character matrices
#' @param x HTML file or text or HTML tables as vector
#' @param letter.convert Logical. If TRUE hex codes will be unified and converted to utf-8 with JATSdecoder::letter.convert()
#' @param greek2text Logical. If TRUE and 'letter.convert=TRUE' converts and unifies various Greek letters to a text based form (e.g. 'alpha', 'beta') 
#' @param replicate Logical. If TRUE the content of cells with row/col span is replicated in all connected cells, if FALSE disconnected cells will be empty,
#' @param rm.duplicated Logical. If TRUE duplicated rows are removed from output
#' @param rm.empty.rows Logical. If TRUE empty rows are removed from output
#' @param rm.html Logical. If TRUE all HTML tags are removed, except <sub> and <sup> , </break> is converted to space
#' @param collapseHeader Logical. If TRUE header cells are collapsed for each column if header has 2 or more lines
#' @param header2colnames Logical. If TRUE and 'collapseHeader=TRUE' first table row is used for column names and removed from table
#' @examples 
#' x<-readLines("https://en.wikipedia.org/wiki/R_(programming_language)",warn=FALSE)
#' tabs<-table2matrix(x)
#' @return List with detected HTML tables as matrices.
#' @importFrom JATSdecoder get.tables
#' @importFrom JATSdecoder letter.convert
#' @importFrom JATSdecoder preCheck
#' @export

##############################################
table2matrix<-function(x,
                       letter.convert=TRUE,
                       greek2text=FALSE,
                       replicate=FALSE, 
                       rm.duplicated=TRUE,
                       rm.html=FALSE,
                       rm.empty.rows=TRUE,
                       collapseHeader=TRUE,
                       header2colnames=FALSE
){
  # escapes
  if(length(grep("^<table",x))==0 & file.exists(x) & 
     !is.element(toupper(gsub(".*\\.([A-z][A-z]*)$","\\1",x)),c("HTML","HML","XML","CERMXML")))
    stop("File input must be of either HTML, HML, XML or CERMXML format.")
  
  # run prechecks or readLines(x) if x is file
  x<-preCheck(x)
  if(length(grep("<table",substr(x[1],1,7)))==0) x<-get.tables(x)
  # remove newline sign
  x<-gsub("\\n"," ",x)
  # if has multiple tables within a table-wrap, split input at <table>-tag and repeat <table-wrap> for every table
  multiTable<-function(x){
    # function for single cell with table
    temp<-function(x){
      # if has <table-wrap>
      if(length(grep("<table-wrap",x))>0){
        # split at <table.
        y<-strsplit2(x,"<table[- >]",type="before")
        # if has <table-wrap> with  more than 1 <table>
        if(length(grep("<table-wrap",y[[1]][1]))==1 &
           length(grep("<table[ >]",y[[1]]))>1){
          wrapHead<-grep("<table-wrap[ >]",y[[1]],value=TRUE)
          wrapFoot<-grep("<table-wrap-foot",y[[1]],value=TRUE)
          tables<-gsub("</table-wrap>|<table-wrap/>","",grep("<table[ >]",y[[1]][-1],value=TRUE))
          x<-paste0(wrapHead,tables,wrapFoot)
        }}
      return(x)
    }  
    # convert every cell and paste results, input=output if no conversion was performed
    out<-NULL
    for(i in 1:length(x)) out<-c(out,temp(x[i]))
    return(out)
  }
  
  # split multiple tables inside of one <table-wrap>-tag
  x<-multiTable(x)
  
  # apply function singleTable2matrix
  out<-list()
  if(length(x)==1) out[[1]]<-singleTable2matrix(x,letter.convert=letter.convert,replicate=replicate,rm.html=rm.html,rm.duplicated=rm.duplicated,collapseHeader=collapseHeader,header2colnames=header2colnames)
  if(length(x)>1) out<-lapply(x,singleTable2matrix,letter.convert=letter.convert,replicate=replicate,rm.html=rm.html,rm.duplicated=rm.duplicated,collapseHeader=collapseHeader,header2colnames=header2colnames)
  if(length(out)==0) return(list())
  # remove empty lists
  if(length(out)>0){
    out<-out[which(unlist(lapply(out,length))!=0)]
    if(length(out)>0&!is.list(out)){
      temp<-out
      out<
        out[[1]]<-out
    }
  }
  
  # name list elements  
  if(is.list(out))
    if(length(out)>0)
      names(out)<-paste("table",1:length(out))
  
  return(out)
}


#############################
# function for single table
singleTable2matrix<-function(x,letter.convert=TRUE,# Logical. If TRUE hex codes will be unified and converted to utf-8
                             greek2text=FALSE,
                             replicate=TRUE, # Logical. If TRUE the content of cells with row/col span is replicated in all connected cells, if FALSE disconnected cells will be empty,
                             rm.duplicated=FALSE, # Logical. If TRUE duplicated rows are deleted
                             rm.empty.rows=TRUE,
                             rm.html=FALSE,
                             collapseHeader=TRUE, # Logical. If TRUE header cells are collapsed for each column if header has 2 or more lines
                             header2colnames=FALSE # Logical. If TRUE and collapse header==TRUE first table row is used for column names and removed from table
){
  # escape if x is empty
  if(length(x)==0) return(NULL)
  # table rows
  rows<-unlist(strsplit2(x,"</tr>",type="after"))
  # remove last row
  rows<-rows[-length(rows)]
  
  #############
  # rows to cells
  cells<-strsplit2(rows,"<t[dh][ >]|<t[dh]/>","before")
  # remove first row
  cells<-lapply(cells,"[",-1)
  
  # remove empty cells
  rows<-rows[unlist(lapply(cells,length))>0]
  cells<-cells[unlist(lapply(cells,length))>0]
  
  # escape if no cells are detected
  if(length(cells)==0) return(NULL)
  
  # remove col/rowspan==1
  cells<-lapply(cells,function(x) gsub(' *rowspan="1"| *colspan="1"',"",x))
  rows<-lapply(rows,function(x) gsub(' *rowspan="1"| *colspan="1"',"",x))
  
  ##############
  # function to insert cells to vector
  insert<-function(value,x,at){
    if(length(value)==0) return(x)
    if(length(at)>1) return(x)
    if(at<=1) out<-c(value,x[(at):length(x)])  
    if(at>1&at<=length(x)) out<-c(x[1:(at-1)],value,x[(at):length(x)])  
    if(at>length(x)) out<-c(x[1:length(x)],value)  
    return(out)
  }
  
  ##############
  # function to replicate cells with colspan>1
  insert.colspan<-function(cells,replicate=TRUE){
    line<-which(grepl('colspan=..[1-9]|colspan=.[1-9]',cells))
    if(length(line)>0){
      for(k in line){
        cellIndex<-which(grepl('colspan="[1-9]',cells[[k]]))
        times<-as.numeric(gsub('.*colspan="([1-9][0-9]*).*',"\\1",cells[[k]][cellIndex]))-1
        for(m in 1:length(cellIndex)){
          if(m==1){
            cell4rep<-ifelse(replicate==TRUE,cells[[k]][cellIndex[m]],"")
            cells[[k]]<-insert(rep(cell4rep,times[m]),cells[[k]],cellIndex[m]+1)
          }
          if(m>1){
            cell4rep<-ifelse(replicate==TRUE,cells[[k]][cellIndex[m]+sum(times[1:(m-1)])],"")
            cells[[k]]<-insert(rep(cell4rep,times[m]),cells[[k]],cellIndex[m]+1+sum(times[1:(m-1)]))
          }
        } # end entering
        cells[[k]]<-gsub(' *colspan="[0-9][0-9]*"',"",cells[[k]])
      } # per line
    }
    return(cells)
  }
  
  # apply function to insert cols by colspan
  cells<-insert.colspan(cells,replicate=replicate)
  
  ########  
  # function to replicate cells with rowspan>1
  insert.rowspan<-function(cells,replicate=TRUE){
    warn<-FALSE
    # lines with rowspan
    line<-which(grepl('rowspan=..[1-9]|rowspan=.[1-9]',cells))
    while(length(line)>0){
      # line with first position of rowspan in cells
      line<-line[order(unlist(lapply(cells[line],function(x) grep('rowspan="[1-9]',x)[1])))][1]
      # first cell index
      cellIndex<-which(grepl('rowspan="[1-9]',cells[[line]]))[1]
      times<-as.numeric(gsub('.*rowspan="([1-9][0-9]*).*',"\\1",cells[[line]][cellIndex]))-1
      # check if new cell is outside normal table area
      if((line+times)>length(cells)) warn<-TRUE
      # remove rowspan from processed cell
      cells[[line]][cellIndex]<-gsub(' *rowspan="[1-9][0-9]*"',"",cells[[line]][cellIndex])
      if(times>0&(line+times)<=length(cells)){
        cell4rep<-ifelse(replicate==TRUE, cells[[line]][cellIndex],"")
        for(m in 1:times){
          cells[[line+m]]<-insert(cell4rep,cells[[line+m]],cellIndex)
        }
      }
      # lines with rowspan left 
      line<-which(grepl('rowspan=..[1-9]',cells))
    }
    if(warn==TRUE) warning("Table compiling might have gone wrong due to complexety of cell connections.",call.=FALSE)
    
    return(cells)
  }
  
  # apply function to insert cells by rowspan
  cells<-insert.rowspan(cells)
  
  # escape if no cells are left
  if(length(cells)==0) return(NULL)
  
  # add missing cells to end of rows
  if(sum(!duplicated(unlist(lapply(cells,length))))>1){
    ind<-which(max(unlist(lapply(cells,length)))-unlist(lapply(cells,length))!=0)
    for(j in ind)   suppressWarnings(cells[[j]]<-c(cells[[j]],rep("",max(unlist(lapply(cells,length)))-unlist(lapply(cells,length))[j])))
  }
  
  # collapse first header lines if header has 2 or more lines
  ind<-ind1<-grepl("</*th>",cells)
  if(length((1:length(ind))[!ind])>0 & collapseHeader==TRUE&length(rows)>1 & sum(grepl("</*th>",cells))>1){
    ind[min((1:length(ind))[!ind]):length(ind)]<-FALSE
    # abort if table has multiple headers  
    if(sum(ind1)!=sum(ind)){
      warnings("The table contains multiple headers. Collapsing headers and conversion to data.frame was omitted.",call.=FALSE)
      collapseHeader<-FALSE
    }else{
      # set index for second block of header lines to FALSE 
      #ind[min((1:length(ind))[!ind]):length(ind)]<-FALSE
      if(sum(ind,na.rm=TRUE)>1){
        h<-rep("",length(cells[[1]]))
        for(j in 1:sum(ind)){
          w<- h!=unlist(cells[ind][j])
          h[w]<-gsub("^ ","",paste(h[w],unlist(cells[ind][j])[w]))
        }
        
        # insert collapsed header
        cells[[1]]<-h
        cells<-cells[-(2:sum(ind))]
      }
    }
  }  
  
  ###########
  # clean up

  # remove table and header html
  cells<-lapply(cells,function(x) gsub("<[/]*th>| *<th/>|<th [^>]*>","",x))
  cells<-lapply(cells,function(x) gsub("</t[dr]>|</t[dr]>|<t[dr]/*>|<t[dr] [^>]*>","",x))
  # remove html  
  if(rm.html==TRUE){
    # convert </break> to space, <sub> to _, <sup> to ^ and remove all other html-tags
    cells<-lapply(cells,function(x) gsub("</*break/*>"," ",x))
    cells<-lapply(cells,function(x) gsub(" *<sub> *","_",x))
    cells<-lapply(cells,function(x) gsub(" *<sup> *","^",x))
    cells<-lapply(cells,function(x) gsub("</*[a-z][^>]*/*>|</*[a-z]/*>","",x))
    cells<-lapply(cells,function(x) gsub("</*inline[^>]*/*>|</*inline[^>]*/*>","",x))
    }
  # remove empty rows
  if(rm.empty.rows==TRUE){
    ind<-unlist(lapply(cells,function(x) sum(nchar(x))>0))
    cells<-cells[ind]
  }
  if(length(cells)==0) return(NULL)
  # convert special characters
  if(letter.convert==TRUE) cells<-lapply(cells,JATSdecoder::letter.convert,greek2text=greek2text)
  # remove duplicated rows
  if(rm.duplicated==TRUE){
    rowText<-unlist(lapply(cells,function(x) paste(x,collapse="")))
    cells<-cells[!duplicated(rowText)]
  }
  # convert to matrix
  m<-suppressWarnings(matrix(unlist(cells),nrow=length(cells),byrow=T))
  # convert header text to column names
  if(header2colnames==TRUE&collapseHeader==TRUE) {
    colnames(m)<-m[1,]
    m<-m[-1,]
  }
  # output
  return(m)
}# end  singleTable2matrix


