# Roxygen documentation

#' Find HS codes for product names based on the search engines from EU customs and Eurostat.
#'
#' This function looks for HS codes using various search engines.
#'
#' @param products A vector of product names (in characters).
#' @param source Define which source(s) to check for HS codes (hs.descriptions, eurostat, eu.customs, zauba, e.to.china, google, hsbianma, eximguru, cybex). Default is 'all'.
#' @param aggregate Aggregate result to one row per product name - HS code combination. Default is TRUE.
#' @param check.archive Specify whether the search term should be checked against the existing HS code archive. Only new terms will be returned. Default is FALSE.
#' @param archive.location Specify the path to the archive location.
#' @param wait.time Time in seconds that you are willing to wait for a source to load.
#'
#'
#' @return A data frame including the product names, HS codes and sources plus a vector of search terms that resulted in error messages.
#' @references www.globaltradealert.org
#' @author Global Trade Alert


gta_hs_code_finder=function(products,
                            sources=c("hs.descriptions","eurostat", "eu.customs", "zauba", "e.to.china", "google","hsbianma", "eximguru", "cybex"),
                            aggregate=T,
                            check.archive=F,
                            archive.location=NULL,
                            wait.time=4){

  library(webdriver)
  library(splitstackshape)
  library(XML)
  library(stringr)
  library(gtalibrary)
  library(gtabastiat)

  find.hs=data.frame()

  pjs <- run_phantomjs()
  remDr<<-Session$new(port=pjs$port)

  check.errors=character()

  for(prd in products){

    print(paste("Finding matches for '",prd,"'.", sep=""))


    if(sum(as.numeric(c("eurostat", "eu.customs", "zauba", "e.to.china", "google","hsbianma","eximguru", "cybex") %in% tolower(sources)))>0){


      if("eurostat" %in% tolower(sources)){

        print("Checking Eurostat ...")

        remDr$go("https://eurostat.prod.3ceonline.com")
        html <- htmlParse(remDr$getSource()[[1]], asText=T)

        e=remDr$findElement(xpath="//textarea[@id='ccce-queryBox']")
        e$sendKeys(as.character(prd))
        e$sendKeys('\ue007')

        b_load_site(xpath="//h2[contains(text(),'assumed characteristics')]",
                    wait=wait.time)

        html <- htmlParse(remDr$getSource()[[1]], asText=T)
        if(length(xpathSApply(html, "//div[@id='hscode']",xmlValue))>0){

          find.hs=rbind(find.hs,
                        data.frame(product.name=prd,
                                   hs.code=as.character(paste(unlist(str_extract_all(xpathSApply(html, "//div[@id='hscode']",xmlValue), "\\d+")),collapse="")),
                                   hs.order=1,
                                   source="Eurostat",
                                   stringsAsFactors = F)
          )
        }

        rm(html)

      }


      if("eximguru" %in% tolower(sources)){

        print("Checking Eximguru ...")

        remDr$go("http://www.eximguru.com/hs-codes/default.aspx")
        html <- htmlParse(remDr$getSource()[[1]], asText=T)

        e=remDr$findElement(css="#uxContentPlaceHolder_ucSearchBox1_ContentPanel1_uxValueToSearchTextBox")
        e$sendKeys(as.character(prd))
        e$sendKeys('\ue007')

        guru.path="//div[@class='Search']/descendant::table/descendant::tr/td[1]/a"


        b_load_site(xpath=guru.path,
                    wait=wait.time)

        html <- htmlParse(remDr$getSource()[[1]], asText=T)

        if(length(xpathSApply(html, guru.path,xmlValue))>0){

          guru.hs=unique(substr(unlist(str_extract_all(xpathSApply(html, guru.path,xmlValue),"\\d+")),1,6))
          guru.hs=guru.hs[nchar(guru.hs)>=4]

          if(length(guru.hs)>0){

            find.hs=rbind(find.hs,
                          data.frame(product.name=prd,
                                     hs.code=guru.hs,
                                     hs.order=1:length(guru.hs),
                                     source="Eximguru",
                                     stringsAsFactors = F)
            )


          }

          rm(guru.hs, html)

        }


      }


      if("eu.customs" %in% tolower(sources)){

        print("Checking EU customs ...")

        remDr$go(paste("https://www.tariffnumber.com/2013/", gsub(" ","%20", prd),sep=""))

        html <- htmlParse(remDr$getSource()[[1]], asText=T)

        if(length(gsub("/2013/","",xpathSApply(html, "//a[@class='light text-nowrap']",xmlGetAttr, "href"))[nchar(gsub("/2013/","",xpathSApply(html, "//a[@class='light text-nowrap']",xmlGetAttr, "href")))>=6])>0){

          customs.found=unique(substr(gsub("/2013/","",xpathSApply(html, "//a[@class='light text-nowrap']",xmlGetAttr, "href"))[nchar(gsub("/2013/","",xpathSApply(html, "//a[@class='light text-nowrap']",xmlGetAttr, "href")))>=6],1,6))

          find.hs=rbind(find.hs,
                        data.frame(product.name=prd,
                                   hs.code=customs.found,
                                   hs.order=1:length(customs.found),
                                   source="EU Customs (2013 edition)",
                                   stringsAsFactors = F)
          )

          rm(customs.found, html)
        }
      }

      if("google" %in% tolower(sources)){

        print("Checking Google ...")

        remDr$go(paste("https://google.com/search?q=", paste("HS+Code",gsub(" ","+", prd), sep="+"),sep=""))
        html <- htmlParse(remDr$getSource()[[1]], asText=T)

        google.txt=tolower(paste(xpathSApply(html, "//*[descendant-or-self::text()]", xmlValue), collapse=" "))

        google.hs=unique(substr(unlist(str_extract_all(str_extract_all(google.txt, "(hs ?)?codes? \\d+"), "\\d+")), 1,6))
        google.hs=google.hs[nchar(google.hs)>=4]

        if(sum(as.numeric(google.hs!=0))>0){

          find.hs=rbind(find.hs,
                        data.frame(product.name=prd,
                                   hs.code=google.hs,
                                   hs.order=1:length(google.hs),
                                   source="Google",
                                   stringsAsFactors = F)
          )

        }

        rm(google.txt, google.hs, html)
      }

      if("zauba" %in% tolower(sources)){

        print("Checking Zauba ...")

        remDr$go(paste("https://www.zauba.com/USA-htscodes/", paste(gsub(" ","+", prd), sep="-"),sep=""))

        zauba.path="//table/descendant::td/descendant::a"

        b_load_site(xpath=zauba.path,
                    wait=wait.time)


        html <- htmlParse(remDr$getSource()[[1]], asText=T)

        if(length(xpathSApply(html, zauba.path, xmlValue))>0){


          zauba.hs=unique(substr(unlist(str_extract_all(xpathSApply(html, zauba.path, xmlValue), "\\d+")),1,6))
          zauba.hs=zauba.hs[nchar(zauba.hs)>=4]

          if(length(zauba.hs)>0){

            find.hs=rbind(find.hs,
                          data.frame(product.name=prd,
                                     hs.code=zauba.hs,
                                     hs.order=1:length(zauba.hs),
                                     source="Zauba",
                                     stringsAsFactors = F)
            )


          }
          rm(zauba.hs)

        }

        rm(html)
      }

      if("e.to.china" %in% tolower(sources)){

        print("Checking E-to-China ...")

        remDr$go(paste("http://hs.e-to-china.com/ks-", paste(gsub(" ","+", prd), sep="+"),"-d_3-t_1.html",sep=""))

        etc.path="//a[@class='hs_tree']"

        b_load_site(xpath=etc.path,
                    wait=wait.time)

        html <- htmlParse(remDr$getSource()[[1]], asText=T)

        if(length(xpathSApply(html, etc.path, xmlGetAttr, "name"))>0){

          etc.hs=unique(substr(xpathSApply(html, etc.path, xmlGetAttr, "name"), 1,6))
          etc.hs=etc.hs[nchar(etc.hs)>=4]

          if(length(etc.hs)>0){

            find.hs=rbind(find.hs,
                          data.frame(product.name=prd,
                                     hs.code=etc.hs,
                                     hs.order=1:length(etc.hs),
                                     source="E-to-China",
                                     stringsAsFactors = F)
            )


          }

          rm(etc.hs)

        }


        rm(html)

      }


      if("hsbianma" %in% tolower(sources)){

        print("Checking HSbianma ...")

        remDr$go(paste("https://hsbianma.com/Search?keywords=",gsub(" ","%20",prd),"&filterFailureCode=true", sep=""))

        bianma.path="//table[@class='result']//descendant::tr/td[1]/a"

        b_load_site(xpath=bianma.path,
                    wait=wait.time)

        html <- htmlParse(remDr$getSource()[[1]], asText=T)

        if(length(xpathSApply(html, bianma.path, xmlValue))>0){

          bianma.hs=unique(substr(unlist(str_extract_all(xpathSApply(html, bianma.path, xmlValue),"\\d+")),1,6))
          bianma.hs=bianma.hs[nchar(bianma.hs)>=4]

          if(length(bianma.hs)>0){

            find.hs=rbind(find.hs,
                          data.frame(product.name=prd,
                                     hs.code=bianma.hs,
                                     hs.order=1:length(bianma.hs),
                                     source="HSbianma",
                                     stringsAsFactors = F)
            )


          }

          rm(bianma.hs)

        }
        rm(html)


      }


      if("cybex" %in% tolower(sources)){

        print("Checking Cybex ...")

        remDr$go(paste("http://www.cybex.in/HS-Codes/of-",gsub(" ","-",prd),".aspx", sep=""))

        cybex.path="//span[contains(text(),'Hs Code')]/following-sibling::a[1]"

        b_load_site(xpath=cybex.path,
                    wait=wait.time)

        html <- htmlParse(remDr$getSource()[[1]], asText=T)

        if(length(xpathSApply(html, cybex.path, xmlValue))>0){

          cybex.hs=unique(substr(unlist(str_extract_all(xpathSApply(html, cybex.path, xmlValue),"\\d+")),1,6))
          cybex.hs=cybex.hs[nchar(cybex.hs)>=4]

          if(length(cybex.hs)>0){

            find.hs=rbind(find.hs,
                          data.frame(product.name=prd,
                                     hs.code=cybex.hs,
                                     hs.order=1:length(cybex.hs),
                                     source="Cybex",
                                     stringsAsFactors = F)
            )


          }

          rm(cybex.hs, html)

        }


      }

      if("sair" %in% tolower(sources)){

        # ADD this at some point:
        # https://www.seair.co.in/blue-cars-hs-code.aspx
      }



    } else {
      stop("No valid source specified.")
    }

    if("hs.descriptions" %in% tolower(sources)){
      print("Checking HS code descriptions ...")

      simple.hs=hs.names$HS12code[grepl(prd, hs.names$hs.name)]

      if(length(simple.hs)==0 &
         length(unique(unlist(str_split(prd, " "))))>1 &
         nrow(subset(find.hs, product.name==prd))==0){
        simple.hs=character()
        for(word in unique(unlist(str_split(prd, " ")))){
          simple.hs=c(simple.hs, hs.names$HS12code[grepl(word, hs.names$hs.name)])

        }

        simple.hs=unique(simple.hs)
      }

      if(length(simple.hs)>0)
        find.hs=rbind(find.hs,
                      data.frame(product.name=prd,
                                 hs.code=simple.hs,
                                 hs.order=1:length(simple.hs),
                                 source="HS code descriptions",
                                 stringsAsFactors = F)
        )
    }



    print(paste("Concluded match search for '",prd,"'.", sep=""))
  }


  remDr$delete()

  ## expanding HS codes
  if(nrow(subset(find.hs, nchar(hs.code)<=4))>0){

    short.hs=subset(find.hs, nchar(hs.code)<=4)

    for(i in 1:nrow(short.hs)){
      hs=gta_hs_code_check(as.integer(short.hs$hs.code[i]))

      if(is.null(hs)==F){
        short.hs$hs.code[i]=paste(gta_hs_code_check(as.integer(short.hs$hs.code[i])), collapse=";")
      }else{
        short.hs$hs.code[i]=999999
      }
    }
    short.hs=cSplit(short.hs, which(names(short.hs)=="hs.code"), sep=";", direction="long")
    short.hs=subset(short.hs, hs.code!=999999)

    find.hs=rbind(subset(find.hs, nchar(hs.code)>4), short.hs)

  }



  if(aggregate){
    if (nrow(find.hs)>0){
      nr.hits=aggregate(source ~ product.name + hs.code, find.hs, function(x) length(unique(x)))
      names(nr.hits)=c("product.name","hs.code","nr.sources")

      find.hs=merge(nr.hits, aggregate(source ~ product.name + hs.code, find.hs, function(x) paste(unique(x), collapse="; ")), by=c("product.name","hs.code"))
      names(find.hs)=c("product.name","hs.code","nr.sources", "source.names")
    }
  }

  already.in.archive=data.frame()
  if (check.archive) {
    if (nrow(find.hs)>0){
      if (is.null(archive.location)==T) {
        stop("Please provide archive location")
      } else {

        load(archive.location)

        if (unique(tolower(find.hs$product.name)) %in% unique(tolower(phrase.table$phrase))) {

          find.hs$in.archive=find.hs$hs.code %in% unique(subset(code.suggested, phrase.id %in% phrase.table$phrase.id[tolower(phrase.table$phrase) == tolower(products)])$hs.code.6)

        }
      }
    }
  }


  if (nrow(find.hs)>0){
    return(find.hs)
  } else {
    return("This query has not found any new codes.")
  }

  if(length(check.errors)==0){
    check.errors="This query went through without errors."
  }

  hs.code.check.errors<<-check.errors
}



