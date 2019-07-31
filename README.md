# crossover-trial

'Crossover-Trial-Simulation'
Introduce a function to simulate ABBA crossover study

'Crossover-Trial-Simulation-Power'
Showing the analysis of ABBA using lme4 and nlme

'Crossover-Trial-Further-Simulation-and-Plotting' 
Simulate more sophisticatedly where one can vary one parameter. Simulate one trial and go on to analyse, the focus though is on plotting the data.

'THE-TWO-PERIOD-CROSS-OVER-CLINICAL-TRIAL' Based on Stephen Senn talk on youtube https://www.youtube.com/watch?v=uTrVhIwy8e8 '70 Years Old and Still Here: The Randomized Clinical Trial and its Critics - Stephen Senn' Here I analyse the trial and plot the data whilst duplicating Senn's presentaion (permutation approach to the analysis). An improved plot for ABBA here also.

ABBA Shiny App.
This simulates a two period crossver trial, plots and performs analysis. The code is in the App in the repository and is deployed here:
https://eamonn.shinyapps.io/ABBA/

  
---
title: "SAS Proc Mixed repeated measures an exploration in R"
author: "Eamonn O'Brien"
date: '`r format(Sys.time(), "%d %B, %Y")`'
output:
  html_document: default
  pdf_document:
    fig_caption: yes
    fig_height: 6
    fig_width: 8
    number_sections: no
  word_document: default
header-includes:
- \usepackage{eso-pic,graphicx,transparent}
- \usepackage{graphicx}
- \usepackage{fancyhdr}
- \pagestyle{fancy}
- \setlength\headheight{22pt}
- \fancyfoot[RO]{Novartis IGE025E3401 AWARE}
- \usepackage{lastpage}
- \cfoot{Page \thepage\ of \pageref{LastPage}}
---


\AddToShipoutPictureFG{
  \AtPageCenter{% or \AtTextCenter
    \makebox[0pt]{\rotatebox[origin=c]{45}{%
      \scalebox{5}{\texttransparent{0.3}{ }}%
    }}
  }
}



```{r set-options, echo=FALSE, cache=FALSE, warning = FALSE}

#/***************************************************************************#
#Filename           :      xxxx.Rmd
#Author             :      obrieea1
#Date               :      xx-xx-2017
#R                  :      3.2.3 (2015-12-10) 
#Platform           :      x86_64-pc-linux-gnu (64-bit) 
#Project/Study      :      CIGE025E3401\pub_2  [example]
#Description        :      xxx 
#Assumptions:   
#Input              :         
#Output             :      
#Macros used        :      <macro>
#---------------------------------------------------------------------------
#MODIFICATION HISTORY: 
#    <DD-MON-YYYY>,
#    <Description> 
#    
#***************************************************************************/

 
        rm(list=ls())
 
        set.seed(1234)
        startTime<-proc.time()
        library(knitr)
        options(width=120)
        opts_chunk$set(comment = "", warning = FALSE, message = FALSE,
                       echo = TRUE, tidy = FALSE, size="tiny",  cache=FALSE,
                       progress=TRUE,
                       #out.width='500px', dpi=200,
                       fig.width=6, fig.height=4,
                       cache.path = 'program_Cache/',
                       fig.path='figure/')
        
        knitr::knit_hooks$set(inline = function(x) {
          knitr:::format_sci(x, 'md')
        })
        
                
        
        # create an R file of the code!
        # https://stackoverflow.com/questions/26048722/knitr-and-tangle-code-without-execution
        
         knit_hooks$set(purl = function(before, options) {
           if (before) return()
           input  = current_input()  # filename of input document
           output = paste(tools::file_path_sans_ext(input), 'R', sep = '.')
           if (knitr:::isFALSE(knitr:::.knitEnv$tangle.start)) {
           assign('tangle.start', TRUE, knitr:::.knitEnv)
           unlink(output)
         }
           cat(options$code, file = output, sep = '\n', append = TRUE)
         })

        
```        

### Introduction

```{r , echo=FALSE}

cat("\nSAS Proc mixed repeated measures is reproduced in R, also random effects analyses are used and treatment effect at month 3 estimated.\n")

```

### Contents
Maybe easier to use complete cases as NAs upset lsmeans package ...I use na.exclude and predict on the dataset to predict NAs

```{r , echo=FALSE}

 cat("
 * Import from GPS datasets saved into shared are
 * manage and simplify variable names
 * summary stats

 * Summary of change from baseline visit by treatment
 * Summary stats by visit and trt for response
 * Summary stats for response overall
 * Summary stats for change from baseline overall

 * histogram of response
 * look at pattern of visits
 * plot observed by trt and with SE
 * plot absolute chg from baseline by trt and with SE
 * plot percentage chg from baseline by trt and with SE
 * look at pattern of visits   
 * plot observed for each individual time profile by trt, country (limit y axes)
 * plot observed for each individual time profile by trt, country (unlimited y axes)
 * plot observed for each individual time profile by trt, country (unlimited y axes just 
adding dots)
 * plot observed for each individual time profile by trt
 * plot median time profile by trt
 * plot mean time profile by trt
 * plot observed for each individual time profile by trt
 * correlation 
 * look at pattern of visits (again!)
 * crude calculations

 * Modeling  
 * Gls model (a)
 * Gls model (tmp) on compete cases
 * check model fit residuals
 * predict plots
 * plot treatment effect
 * using model a plot predicted and observed
 * harrell summary for trt effect at m3 ***
 * harrell predict at m3 
 * harrell summary for trt effect at m3 (x2) ***

 * effects package (dont get trt effectct estimate?)
 * gls model (a1) using nlme to use the lsmeans and effects packages
 * effects package country plot and effects
 * effects package tr x time and effects
 * effects package baseline and effects

 * lsmeans package (can get trt efect estimate)
 * run gls model (a1) using complete cases (otherwise error)
 * lsmeans time
 * lsmeans trt on average
 * lsmeans trt at time points
 * lsmeans trt at time points
 * lsmeans trt effect***
 * lsmeans confusing plot of trt effect
 * lsmeans trt at time points
 * lsmeans confusing plot of trt with time
 * harrell summary for trt effect at m3 ***

 * Random effects

 * lmer 
 * effects package trt with time 
 * lsmeans package trt effect ***
 * lsmeans confusing plot of trt with time

 * lme
 * lme select best correlation structure
 * show covariance estimated matrix
 * (2 further lme model not used.)
 * best lme model plot fitted and observed 2 plots (dataset d1)
 * crude calcualttions of means for fitted and observed

 * contrast package for trt effect***(you can read off model as ref is month3)
 * contrast package for trt effect***(showing est not effected by country or basline value)
 * compare to harrell summary for trt effect at m3 ***

 * simulate and predict using lmer model with plots
 * analyse a simulation from lmer using lmer nlme and gls
 * fitted and observed from gls model a\n")

```

```{r dirs, eval=TRUE}

      # MODESIM directories
      wd <- Rhome <-'/home/obrieea1/Documents/CRFB002A_MAX_IL-01 UNRAVEL/'
      wd.code <- paste0(Rhome,'CODE')                    # note this new dir
      wd.data <- paste0(Rhome,'DATA')
 
      # GPS directories
      gps <- '/view/obrieea1_view/vob/CRFB002A/CRFB002A_MAX_IL_01/csr_1/'
      path.script <- paste0(gps, '/pgm/pkpd/')
      path.import <- paste0(gps, '/derived_data/')
      path.export <- paste0(gps, '/reports/pkpd/')
 
      # rounding functions
      p2 <- function(x) {formatC(x, format="f", digits=2)}
      p3 <- function(x) {formatC(x, format="f", digits=3)}
      p4 <- function(x) {formatC(x, format="f", digits=4)}
     
     # function to calculate the mode     
          getmode <- function(v) {
               uniqv <- unique(v)
               uniqv[which.max(tabulate(match(v, uniqv)))]
          }
 
       
        #perhaps help colour plot text based on loop count
        is.even <- function(x){ x %% 2 == 0 }
           
        options(width=90)


```
 
```{r prerequisites, echo=FALSE, eval=FALSE}

         # where<-"homeaa"
 
          where<-"homeaa"
 
          x  <- "data\\R Scripts\\PROJECTS\\UNRAVEL"

          path <- paste(x,"\\CODE",sep="")
          path2 <- paste(x,"\\DATA",sep="")
         
          
          work<-    paste("H:/", path, sep = "")
          nonwork<- paste("~/H/", path, sep = "")
          if (where=="home") {wd<- nonwork} else {wd<-work}
         
          work2<-    paste("H:/", path2, sep = "")
          nonwork2<- paste("~/H/", path2, sep = "")
          if (where=="home") {wd2<- nonwork2} else {wd2<-work2}
         
          
         # work3<-    paste("X:/FUNCTIONS/R", sep = "")
        #  nonwork3<- paste("~/X/FUNCTIONS/R", sep = "")
        #  if (where=="home") {wd3<- nonwork3} else {wd3<-work3}
 
          #wd <- "C:\\Users\\User\\Documents\\X\\PROJECTS\\UNRAVEL"
          setwd(wd2)
          
          opts_knit$set(root.dir = wd2)   # THIS 
       
          
```
 
```{r preliminaries perhaps , echo=FALSE, results='hide'}
 
          p3 <- function(x) {formatC(x, format="f", digits=3)}
          p4 <- function(x) {formatC(x, format="f", digits=4)}
          p2 <- function(x) {formatC(x, format="f", digits=2)}
          p1 <- function(x) {print(formatC(x, format="f", digits=1),quote=FALSE)}
          p1 <- function(x) {formatC(x, format="f", digits=1)}
          # Load packages
         # list.of.packages <- c("xlsx", "ggplot2", "arm", "sjPlot", "lme4", "rms", "plyr",     "doBy", "Matrix.utils", "reshape", "VCA", "stringr", "nlme", "car", "reshape2",  "loo",      "binom", "brms")
         
      #    new.packages <-
      #    list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
     #     if(length(new.packages)) install.packages(new.packages)
         
     #     sapply(X = list.of.packages, require, character.only = TRUE)
         
         # Function to construct 95% CI for variance
        
          var.interval = function(var, d.f, conf.level) {
             chilower = qchisq((1 - conf.level)/2, d.f)
             chiupper = qchisq((1 - conf.level)/2, d.f, lower.tail = FALSE)
             c(d.f * var/chiupper, d.f * var/chilower)
                  }
 
         
          options(mc.cores=parallel::detectCores())
         
```

### Import two data sets origianlly from GPS

```{r, echo=TRUE, eval=FALSE}

    foo <-  haven::read_sas("abclpk.sas7bdat") # GPS Blood collection and PK results
    foo1 <-  haven::read_sas("aident.sas7bdat")

    
```

### Import data from GPS

```{r, echo=TRUE}

    setwd(path.import)                         # point to GPS II project folder 
    foo <-  haven::read_sas("abclpk.sas7bdat") # GPS Blood collection and PK results
    foo1 <-  haven::read_sas("aident.sas7bdat")
    setwd(wd.code)  

```



### Manage foo

```{r, echo=TRUE}


     # LOOKING AT THE SAS CODE PROC MIXED USES THESE VARIABLES
    # SID1A TRTREG1C COU1A BASEVAL   # THESE ARE IN THE FOO DATASET
    # CAT VIS CHG1   # THESE REQUIRED DERIVING

    # CAT not needed?
    # VIS is a categorical version of visit ... not required?
    # CHG1 this is calcualted as follows in the sas code

    # PHKRSL1N1=PHKRSL1N;
    # if PHKRSL1N < 20 then PHKRSL1N1=10;
    # chg1 = PHKRSL1N1 - baseval; so only have to derive this it seems?

    attributes(foo$COU1A)    # see a label, there are usually too long so not shown
    Hmisc::contents(foo)     # see all labels

    head(foo$PHKRSL1N)   # response variable
    head(foo$BLFLG_1C)   # B gives the baseline results
    # View(foo[,c("PHKRSL1N","BLFLG_1C")])
   
```

### Manage foo1, get ids for FAS population

```{r, echo=TRUE}
 
    # get the ids for FAS population
    foo1a <- foo1[, c("SID1A", "FAS")]
    fas.id <- foo1a[foo1a$FAS %in% 1, "SID1A"]
    fas.id <- as.vector(unlist(fas.id))
    length(unique(fas.id))   #205, this matches the powerpoint presentation
   
```

### use fas.id from foo1 to subset the foo dataset

```{r, echo=TRUE}

   # steps 
   # keep relevant vars
   # subset vegf 
   # subset if vegf at baseline > lloq

   length(unique(foo$SID1A)) # seems like the membership of foo is already the FAS pop? N=205
   fooa <- foo[foo$SID1A %in%  fas.id ,]  
   length(unique(fooa$SID1A)) # N=205
  
   # let us drop superfluous vars
   # need to include BCLTYP1C as this allows identification of VEGF.   

   foob <- fooa[, c("SID1A","TRTREG1C","COU1A","BASEVAL","SMP1N","PHKRSL1N","BLFLG_1C", "BCLTYP1C", "CHG","TGPDSC1A")]  
  
   # first identify VEGF response data   
   vegf <- c(692,774)    # this information was from 2.4.2 ABCL: Blood Collection
   foob1 <- foob[foob$BCLTYP1C %in% vegf,]   # subset
 
   # second get ids with baseline above LLOQ of 20
   # https://stackoverflow.com/questions/14261619/subsetting-r-data-frame-results-in-mysterious-na-rows
   # wrap in which to avoid complete NA rows
   fooc <- foob1[which(foob1$BLFLG_1C %in% "B" & foob1$BASEVAL >20), ]  # N=59
   # now get the ids of these guys who qualify for analysis based on baseline values    
   id <- unique(fooc$SID1A     )
   # now using 'id' keep valid subjects based on baseline valuse
    
   # ignore or select patients depending above LOQ at baseline
   food <- foob1[foob1$SID1A %in%  id ,]  
   
   food <- foob1   # run this line to perform analysis on 205
   
   ###############################################
   #food <-  foob1   # code in for analysis of 205
   ###############################################

   
   length(unique(food$SID1A)) # check  #59 or 205

   # now select based on membership of fas.id, we have done already but check again
   fooe <- food[food$SID1A %in%  fas.id ,]  
   length(unique(fooe$SID1A)) # check  #59
   
    
    dd <- fooe
    dd$trt <- factor(dd$TGPDSC1A)
    dd$time <- dd$SMP1N-201
    dd$y <- ifelse(dd$PHKRSL1N < 20 ,10, dd$PHKRSL1N) # impute as per protocol
    dd$ID <- dd$SID1A
    
    # create the response 
    dd$chg1 = dd$y - dd$BASEVAL # absolute difference
    
    #dd$y <- dd$PHKRSL1N
    
   
    dd$TGPDSC1A <- dd$PHKRSL1N <- dd$SID1A <- dd$TRTREG1C <- dd$BCLTYP1C <- NULL
   
   
   dd<- dd[!dd$time %in% "0",] # this is a visit before baseline and is not required
   
   #"1" is baseline 
    
   # baseline chg is labelled as NA lets make it zero.
   dd$chg1 <- ifelse(dd$time %in% "1", 0, dd$chg1 )

   
  dd$country <- as.character(dd$COU1A)
  dd$COU1A <- NULL
 
  cat("more informative variables")
  dd$time <- ifelse(dd$time %in% "1" , "baseline",
               ifelse(dd$time %in% "2" , "baseline post",
                 ifelse(dd$time %in% "3" , "wk1",
                   ifelse(dd$time %in% "4" , "wk2",
                      ifelse(dd$time %in% "5" , "m1 pre",
                       ifelse(dd$time %in% "6" , "m1 post",
                         ifelse(dd$time %in% "7" , "m2 pre",
                            ifelse(dd$time %in% "8" , "m2 post","m3"))))))))


  dd$time <- factor(dd$time, 
  levels=c("baseline","baseline post","wk1","wk2" ,"m1 pre", 
           "m1 post", "m2 pre" , "m2 post", "m3") ,
  labels=c("baseline","baseline post","wk1","wk2" ,"m1 pre", 
           "m1 post", "m2 pre" , "m2 post", "m3"))

  dd$country <- factor(dd$country)
  dd$country <- relevel(dd$country, ref="TWN")
  dd$time <- relevel(dd$time, ref="m3")
   

  summary(dd$y)

```

### Summary statistics, compare to t42_1_02.rtf (CRFB002A\CRFB002A_MAX_IL_01\csr_1\reorts\pkpd\)

```{r}


#require(tidyverse)
require(plyr)
require(dplyr)
df <- dd


cat("Summary of change from baseline visit by treatment\n\n")
df_summarchg1x <- df %>% # the names of the new data frame and the data frame to be summarised
    group_by(time, trt) %>%                # the grouping variable
    summarize(mean_PL = mean(chg1, na.rm=TRUE),  # calculates the mean of each group
              sd_PL = sd(chg1, na.rm=TRUE),      # calculates the sd of each group
              n_PL = length(na.omit(chg1)),      # calculates the sample size per group
              SE_PL = sd(chg1, na.rm=TRUE)/sqrt(length(na.omit(chg1))), # SE of each group
              Ns=length(unique(ID)),   # unique subjects
              low = mean(chg1, na.rm=T) - qt(.975, length(na.omit(chg1))-1)*(sd(chg1, na.rm=T)/ length(na.omit(chg1))^.5),
              upp = mean(chg1, na.rm=T) + qt(.975, length(na.omit(chg1))-1)*(sd(chg1, na.rm=T)/ length(na.omit(chg1))^.5),
              median=median(chg1, na.rm=T) ,
              min = min(chg1, na.rm=T),  
              max = max(chg1, na.rm=T) ,
              #MCID = sd(chg1, na.rm=T)/2,
              miss = sum(is.na(chg1))
  ) 
require(knitr)
    food2 <- data.frame(df_summarchg1x)
    names(food2) <- c("Month","Variable","Mean", "SD","N","SE","subjects", "low", "high", "Med", "Min","Max", "NA")  
    print(kable(food2, format="pandoc", 
                row.names = FALSE, digits = c(2)))



cat("Summary stats by visit and trt for response\n\n")
df_summaryx <- df %>% # the names of the new data frame and the data frame to be summarised
    group_by(time, trt) %>%                # the grouping variable
    summarize(mean_PL = mean(y, na.rm=TRUE),  # calculates the mean of each group
              sd_PL = sd(y, na.rm=TRUE),      # calculates the sd of each group
              n_PL = length(na.omit(y)),      # calculates the sample size per group
              SE_PL = sd(y, na.rm=TRUE)/sqrt(length(na.omit(y))), # SE of each group
              Ns=length(unique(ID)),   # unique subjects
              low = mean(y, na.rm=T) - qt(.975, length(na.omit(y))-1)*(sd(y, na.rm=T)/ length(na.omit(y))^.5),
              upp = mean(y, na.rm=T) + qt(.975, length(na.omit(y))-1)*(sd(y, na.rm=T)/ length(na.omit(y))^.5),
              median=median(y, na.rm=T) ,
              min = min(y, na.rm=T),  
              max = max(y, na.rm=T) ,
              #MCID = sd(y, na.rm=T)/2,
              miss = sum(is.na(y))
  ) 
require(knitr)
    food2 <- data.frame(df_summaryx)
    names(food2) <- c("Month","Variable","Mean", "SD","N","SE","subjects", "low", "high", "Med", "Min","Max", "NA")  
    print(kable(food2, format="pandoc", 
                row.names = FALSE, digits = c(2)))



cat("Summary stats for response overall\n\n")
df_summaryx <- df %>% # the names of the new data frame and the data frame to be summarised
    group_by(time ) %>%                # the grouping variable
    summarise(mean_PL = mean(y, na.rm=TRUE),  # calculates the mean of each group
              sd_PL = sd(y, na.rm=TRUE),      # calculates the sd of each group
              n_PL = length(na.omit(y)),      # calculates the sample size per group
              SE_PL = sd(y, na.rm=TRUE)/sqrt(length(na.omit(y))), # SE of each group
              Ns=length(unique(ID)),   # unique subjects
              low = mean(y, na.rm=T) - qt(.975, length(na.omit(y))-1)*(sd(y, na.rm=T)/ length(na.omit(y))^.5),
              upp = mean(y, na.rm=T) + qt(.975, length(na.omit(y))-1)*(sd(y, na.rm=T)/ length(na.omit(y))^.5),
              median=median(y, na.rm=T) ,
              min = min(y, na.rm=T),  
              max = max(y, na.rm=T) ,
              #MCID = sd(y, na.rm=T)/2,
              miss = sum(is.na(y))
  ) 
require(knitr)
    food2 <- data.frame(df_summaryx)
    names(food2) <- c("Month","Mean", "SD","N","SE","subjects", "low", "high", "Med", "Min","Max", "NA")  
    print(kable(food2, format="pandoc", 
                row.names = FALSE, digits = c(2)))


cat("Summary stats for change from baseline overall\n\n")
df_summaryx <- df %>% # the names of the new data frame and the data frame to be summarised
    group_by(time ) %>%                # the grouping variable
    summarise(mean_PL = mean(chg1, na.rm=TRUE),  # calculates the mean of each group
              sd_PL = sd(chg1, na.rm=TRUE),      # calculates the sd of each group
              n_PL = length(na.omit(chg1)),      # calculates the sample size per group
              SE_PL = sd(chg1, na.rm=TRUE)/sqrt(length(na.omit(chg1))), # SE of each group
              Ns=length(unique(ID)),   # unique subjects
              low = mean(chg1, na.rm=T) - qt(.975, length(na.omit(chg1))-1)*(sd(chg1, na.rm=T)/ length(na.omit(chg1))^.5),
              upp = mean(chg1, na.rm=T) + qt(.975, length(na.omit(chg1))-1)*(sd(chg1, na.rm=T)/ length(na.omit(chg1))^.5),
              median=median(chg1, na.rm=T) ,
              min = min(chg1, na.rm=T),  
              max = max(chg1, na.rm=T) ,
              #MCID = sd(chg1, na.rm=T)/2,
              miss = sum(is.na(chg1))
  ) 
require(knitr)
    food2 <- data.frame(df_summaryx)
    names(food2) <- c("Month","Mean", "SD","N","SE","subjects", "low", "high", "Med", "Min","Max", "NA")  
    print(kable(food2, format="pandoc", 
                row.names = FALSE, digits = c(2)))



```

### Plot of trend over time by treatment group, LLoQ =20 pg/mL

```{r chunk1}
  
  ddx <- dd
require(ggplot2)
  ggplot(ddx,aes(x=y))+geom_histogram()+facet_grid(~trt) +
    ggtitle("Ditribution of VEGF ignoring time")

  # change levels of time so order is chronological
  ddx$time <- factor(ddx$time, 
  levels=c("baseline","baseline post","wk1","wk2" ,"m1 pre", 
           "m1 post", "m2 pre" , "m2 post", "m3") ,
  labels=c("baseline","baseline post","wk1","wk2" ,"m1 pre", 
           "m1 post", "m2 pre" , "m2 post", "m3"))

  ddx$time <- relevel(ddx$time, ref="baseline")
  
  
  # investigate pattern of timpoints but not if they have NA at time point
  with(ddx,table(tapply(time , ID , 
                      function(w) paste(sort(unique(w)), collapse= ','))))
 

  
  ggplot(ddx, aes(x=time, y=y, color = trt, group=trt)) +
    stat_summary(fun.y = mean, geom = "line") +
     stat_summary(fun.data = mean_se, geom = "pointrange") +
    ylab("VEGF pg/mL") + xlab("Visit") +
  geom_hline(aes(yintercept=20), colour="#990000", linetype="dashed") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  ggtitle(paste("observed response, subjects=",length(unique(ddx$ID))  ) )


ggplot(ddx, aes(time, chg1, color = trt, group=trt))  +
  stat_summary(fun.y = mean, geom = "line") +
   stat_summary(fun.data = mean_se, geom = "pointrange") +
  ylab("absolute change from baseline VEGF pg/mL") + xlab("Visit") +
geom_hline(aes(yintercept=0), colour="#990000", linetype="dashed") +
   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  ggtitle(paste("Absolute change from baseline, subjects=",length(unique(ddx$ID))  ) )


ddx$pchg <- ifelse(ddx$time!=0,  ((ddx$y - ddx$BASEVAL)/ ddx$BASEVAL)*100,0)#

# there are some % change greater than baseline , see them with this:
# View(dd[dd$time %in% c("wk1","wk2") & dd$pchg > 0,])

ggplot(ddx, aes(time, pchg, color = trt, group=trt)) +
  stat_summary(fun.y = mean, geom = "line") +
   stat_summary(fun.data = mean_se, geom = "pointrange") +
  ylab("percent change from baseline VEGF pg/mL") + xlab("Visit") +
  coord_cartesian(xlim=c(0, 9)) + 
geom_hline(aes(yintercept=0), colour="#990000", linetype="dashed") +
   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  ggtitle(paste("Percent change from baseline, subjects=",length(unique(ddx$ID))  ) )

##frank harrell inspired plot

with(ddx,table(tapply(time , ID , function(w) paste(sort(unique(w)), collapse= ','))))

# there are lots of y=10 can I plot the data clearly?
  dddx <- ddx[ddx$y >10,]
  dddx <- na.omit(dddx)
  xl <- xlab( 'Visit')
  yl <- ylab( 'response')
#

# no of observations
# https://stackoverflow.com/questions/13239843/annotate-ggplot2-facets-with-number-of-observations-per-facet

  z<-ddx[, c("ID","country","trt","y", "time")]
  z<-z[complete.cases(z),]
  z<-z[,1:3]

    z<-unique(z)
#require(plyr)
    NN <-  plyr::ddply(.data=z, plyr::.(trt, country), 
                 plyr::summarize, 
                 n=paste0("n=", length(ID)))

# double check, look at the actual data, pull out values not equal to 10
ddx[ddx$country=="TWN" & ddx$y !="10",c("ID","time","y","trt")]
ddx[ddx$country=="SGP" & ddx$y !="10",c("ID","time","y","trt")]

# print N but remove y=10 data to add clarity?
do  <- ddx[which(ddx$y !=10),]  # get rid of 10 for clarity?

ggplot(do , aes(x=time , y=y )) + 
  geom_line(aes(colour = ID, group = ID)) + 
  xl + yl + facet_grid (trt ~ country, scales="free_y")  +
  guides(color=FALSE) + ylim(0,150)  +  # override scales are free
   geom_text(data=NN, aes(x=7, y=150, label=n),   # add N
            colour="black", inherit.aes=FALSE, parse=FALSE)  +
     theme(text = element_text(size=7),
           axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  ggtitle("Observed profiles by country and trt, values of 10 removed to aid clarity, limit the y axis range") 
 
ggplot(do , aes(x=time , y=y )) + 
  guides(color=FALSE) + facet_grid (trt ~ country) +
     geom_line(aes(colour = ID, group = ID))  +
   geom_text(data=NN, aes(x=7, y=800, label=n),   # add N
              colour="black", inherit.aes=FALSE, parse=FALSE)  +
  theme(text = element_text(size=7),
           axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  ggtitle("Observed profiles by country and trt, values of 10 removed to aid clarity, unlimited y axis range") 


  ggplot(data = ddx, aes(x = time, y = y, group = ID, color = ID)) +
geom_line()  + facet_grid (trt ~country, scales="free_y") +
     geom_text(data=NN, aes(x=7, y=800, label=n),   # add N
              colour="black", inherit.aes=FALSE, parse=FALSE)  +
    geom_point(aes(colour=ID),               # colour depends on cond2
               size=1)  +
    guides(color=FALSE) +
  theme(text = element_text(size=7),
           axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  ggtitle("Observed profiles by country and trt, unlimited y axis range added dots") 
 

NN <-  plyr::ddply(.data=z, plyr::.(trt), 
                 plyr::summarize, 
                 n=paste0("n=", length(ID)))

 
ggplot (ddx , aes(x=time , y=(y) , color =factor (ID), group=ID)) + 
  geom_line () + xl + yl + facet_grid (trt ~., scales="free_y")  + 
  geom_text(data=NN, aes(x=1, y=800, label=n),   # add N
              colour="black", inherit.aes=FALSE, parse=FALSE)  +
  guides(color=FALSE) + #+ ylim(1,3)  # override scales are ree + 
  ggtitle("Observed profiles by trt, unlimited y axis") 



ggplot (ddx , aes(x=time , y=(y) , color =factor (trt), group=trt)) +
  xl + yl + ylim(0, 150) + 
     guides(color=FALSE) +
  stat_summary(fun.data = median_hilow, geom = "smooth",fun.args=list(conf.int=.9)) +
  facet_wrap(~trt, nrow=2) + # Fig. 7.2 
  ggtitle("Median with 90% CI") 

ggplot(dddx , aes(x=time , y=y , color =factor (trt), group=trt )) + 
  xl + yl + ylim(0, 35) + 
  stat_summary(fun.data="mean_cl_boot", geom= 'smooth')+ 
  facet_wrap(~trt, nrow=2) + # Fig. 750
   guides(color=FALSE) +
  theme(text = element_text(size=10),
           axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  ggtitle("Median with 90% CI, removed values equal to 10") 
 
 
 ggplot(data = ddx, aes(x = factor(time), y = y, color = ID)) +       
  geom_line(aes(group = ID)) + geom_point() +guides(color=FALSE) +
     facet_wrap(~trt, ncol=2) +
   theme(text = element_text(size=10),
           axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  ggtitle("Observed profiles by treatment") 
 


# make a wide datset
  xx <- ddx[, c("ID", "time","y")]
  w <- reshape2::dcast(xx,formula=ID~time,value.var="y")
  print(cov(w[,c(2:10)], use = "pairwise.complete.obs"), digits=0)
  print(cor(w[,c(2:10)], use = "pairwise.complete.obs"), digits=2)
  print(cov(w[,c(3:10)], use = "pairwise.complete.obs"), digits=0)
  print(cor(w[,c(3:10)], use = "pairwise.complete.obs"), digits=2)

 
```   

### Examine missing, two types of missing, missing a row for visit (ie not there) and having NA for a visit's response
   
```{r, missing}      
    
    cat("Examine missing, use the long dataset")

    # this will only examine the former 
    with(ddx,table(tapply(time , ID , function(w) paste(sort(unique(w)), collapse= ','))))
    
     # use the wide dataset
    w.na <- unique(w[which(is.na(w), arr.ind=TRUE)[,1],])
    plyr::arrange(w.na, ID)
    
    miss <- t(tapply(dd$y, list(dd$time,dd$ID), is.na))
    which(apply(miss, 1, any)) 
    
    a1 <- names( which(apply(miss, 1, any)) )   # NA
    b1 <- rownames(miss)[!complete.cases(miss)]  # don't have some rows for visits
    trouble <- c(a1, b1)
    
    #miss[rownames(miss) %in% trouble,]
    
    cat("Examine missing, use the long dataset")
    m <- dd[dd$ID %in% trouble,]
    plyr::arrange(m, ID, time)  # sort
    
    
    m <- w[w$ID %in% trouble,]
    plyr::arrange(m, ID )  # sort
    
    
```   

### Crude calculations
   
```{r}  

   d <-dd
   cat("crude change calculations from baseline with time")
   cat("trt mean calculations with time")
   tapply(d$y, list(d$time,d$trt), mean, na.rm=T)
   cat("crude change calculations from baseline with time")
   (crude.chg <-tapply(d$chg1, list(d$time,d$trt), mean, na.rm=T))
   cat("crude difference in change calculations from baseline with time")
   crude.chg[,1]-crude.chg[,2]

   #there is a missingreponse inthe first timepoint after basline
   #View(d[is.na(d$y),])
 

 
```   
   
#### More informative variables 
 
```{r data, echo=FALSE, warning=FALSE, message=FALSE}  
  
  head(d)

```

### Fit Gls model using rms package


```{r model a}
 
  require(rms)

  d1 <- d[!d$BLFLG_1C %in% "B",] # dont need this in the y as we have a sep baseline var
  
  ddz <- datadist(d1)
  options(datadist='ddz')
  
  #d1$chg1 <- ifelse(d1$y == 766, NA, d1$chg1)  #remove 'outlier' 
  # here is the  estimate trt=Ranibizumab                      
  # 16.3845 se 4.3975   t 3.73 p 0.0002 
  
  require(nlme)
  # generalised least squares matches SAS repeated measures 
  a <- Gls(chg1 ~ time * trt + BASEVAL *  time + country,
                  correlation=corSymm(form=~ as.numeric(time)|ID), 
                  weights=varIdent(form=~1|time),
                  d1, x=TRUE, 
           na.action=na.exclude )
  print(a)
  anova(a)

  
    #   path.script <- paste0(gps, '/pgm/pkpd/')
    #   path.import <- paste0(gps, '/derived_data/')
    #   path.export <- paste0(gps, '/reports/pkpd/')
    # 
    # 
    # wd <- Rhome <-'/home/obrieea1/Documents/DEVELOPMENT/'
    #   wd.code <- wd #paste0(Rhome,'CODE DEVELOPMENT')                  # note this new dir
    #   wd.data <- wd # paste0(Rhome,'DATA')
    #   setwd(wd)
    # #  write.table(x<- dput(d1), file ="temp")
    #                write.table(d1,  file ="temp1.txt", sep="\t")
    #   
    #  #             sink("test.txt")
    #   #            print(dput(d1))
       #           sink()
      
      
      
  #fileConn<- file("output.txt")
#writeLines(dput(d1), fileConn)
#close(fileConn)
  
  
        gps <- '/view/obrieea1_view/vob/CRFB002A/CRFB002A_MAX_IL_01/csr_1/'
      path.script <- paste0(gps, '/pgm/pkpd/')
      path.import <- paste0(gps, '/derived_data/')
      path.export <- paste0(gps, '/reports/pkpd/')
   setwd( path.export)
    #  write.table(x<- dput(d1), file ="temp")
                   write.table(d1,  file ="temp1.txt", sep="\t")
  
```

### Diagnostics

```{r}

  # run model again but with complete case function to avoid NA problems?
  d1 <- d[!d$BLFLG_1C %in% "B",] # dont need this in the y as we have a sep. baseline var
  d2 <- d1[complete.cases(d1),]  
  
  ddz <- datadist(d2)
  options(datadist='ddz')
  
  require(nlme)
  # generalised least squares matches SAS repeated measures 
  tmp <- Gls(chg1 ~ time * trt + BASEVAL *  time + country,
                  correlation=corSymm(form=~ as.numeric(time)|ID), 
                  weights=varIdent(form=~1|time),
                  d2, x=TRUE, 
           na.action=na.exclude )
  
  print(tmp)
  anova(tmp)

  d2$resid <- r <- resid(tmp)
  d2$fitted <- fitted(tmp)

# not great diagnostic results!
  d2$time <- relevel(d2$time, ref="baseline post") #
  
  yl <- ylab('Residuals ') 
  
  p1 <- ggplot(d2 , aes(x=fitted , y=resid)) + geom_point () + facet_grid(~ trt) + yl 
  p2 <- ggplot(d2 , aes(x=BASEVAL , y=resid)) + geom_point () + yl 
  p3 <- ggplot(d2 , aes(x=time , y=resid)) +  geom_point () + yl  +
    stat_summary(fun.data ="mean_sdl", geom='smooth') 
  p4 <- ggplot(d2 , aes(sample=resid)) + stat_qq() +
    geom_abline(intercept=mean(r), slope=sd(r)) + yl 
  
  gridExtra::grid.arrange(p1, p2, p3, p4, ncol=2) #

```


```{r}

plot(anova(a))  

#ylm <-ylim(25, 60) 
p1 <- ggplot(Predict(a,  trt , time, conf.int=FALSE), adj.subtitle=FALSE, 
             legend.position='top ') 

p2 <- ggplot(Predict(a, BASEVAL), adj.subtitle =FALSE)  

p3 <- ggplot(Predict(a, country , trt), adj.subtitle =FALSE, legend.position='top ')  

p4 <- ggplot(Predict(a, time ), adj.subtitle =FALSE, legend.position='top ')

gridExtra ::grid.arrange (p1, p2, p3, ncol=2) 


  time. <- c("baseline post", "wk1", "wk2", "m1 pre", "m1 post","m2 pre", "m2 post","m3")

  k1 <- contrast(a, list(time=time.,  trt = 'Aflibercept'),
                    list(time=time.,  trt = 'Ranibizumab'))

  options(width=80) 
  print (k1, digits =4)

  k1 <- as.data.frame(k1[c('time', 'Contrast', 'Lower', 'Upper')]) 
  
  p1 <- ggplot (k1, aes(x=time , y=Contrast, group=1)) + geom_point () + geom_line () +
    ylim(-50,10) +
    ylab( 'Aflibercept - Ranibizumab')+
    geom_errorbar(aes(ymin=Lower, ymax=Upper ), width =0) 
 
  print(p1)

```


```{r}

  
  #plot predicted
  ##https://gist.github.com/tmalsburg/df66e6c2ab494fad83ee
  
  d1$predicted <- predict(a, newdata=data.frame(d1))
  
  g <- plyr::arrange(d1, trt, ID, time)
  g <- droplevels(g)
  g$time <- relevel(g$time, ref="baseline post") # so axis order correct as m3 is ref level in model (only doing this to chec to sas output).

  gg <- unique(g[,c("trt","ID")])
  gr <- table(gg$trt)[[1]]
 
  x <- with(g, as.matrix(tapply(chg1,  list(ID, time), mean), ncol=9))
  y <- with(g, as.matrix(tapply(predicted, list(ID, time), mean), ncol=9))

  z <- c(levels(g$time)) # for labels on x axis

  zz <- length(unique(d1$ID))
    # y axis min -100 to see majority of data better (previously -500)?
  
    f <- function(xlab="Visit", ...) {
      plot(c(0,9), c(-400, 400), ylab="change from baseline", 
           xlab=xlab, xaxt="n",
           t="n", ...)
           axis(1, at=1:8, labels=z, las=2)
      grid()
    }
    par(mfrow=c(1,2), mar=c(4,4,1,1))
    
    #

    f(main="Obs. change from baseline", "")
    for (i in 1:zz)
      lines(x[i,]  ,     col=ifelse(i<=gr,"blue","red")) #30
    
    f(main="Predicted data, subjects", "")
    for (i in 1:zz)
      lines(y[i,]  ,     col=ifelse(i<=gr,"blue","red"))
     
    legend("topright", pch=c(1,1), col=c("blue","red"), 
           c("Aflibercept", "Ranibizumab"), bg="white")
 
 
  ##Predictions

  
  
  cat("See the treatment effect at m3 about halfway down")
  summary(a,   time=c("m3") ) #rms
  # d1 <-gendata(a, trt="Ranibizumab", time="m3")
  # predict(a,d1,type="lp",se=T)
  # 
  # d1 <-gendata(a, trt="Aflibercept", time="m3")
  # predict(a,d1,type="lp",se=T)

    cat("Harrell says predict rather than the lsmeans concept1")
   #http://r.789695.n4.nabble.com/how-to-get-quot-lsmeans-quot-td821113.html
   dz <-gendata(a, trt="Aflibercept", time="m3", BASEVAL =mean(d$BASEVAL),
               country=d$country,
               expand=TRUE)
  dz<-unique(dz)
  dz$Predicted <- predict(a,dz,type="lp") 
  dz
  
  dz<-gendata(a, trt="Ranibizumab", time="m3", BASEVAL =mean(d$BASEVAL),
               country=d$country,
               expand=TRUE)
  dz<-unique(dz)
  dz$Predicted <- predict(a,dz,type="lp") 
  dz
  
   cat("Trt effect at mth 3")
  rms::contrast(a, list(trt='Aflibercept', time='m3'),
                   list(trt='Ranibizumab', time='m3'), type='average')
  
  rms::contrast(a, list(trt='Aflibercept', time='m3'),
                   list(trt='Ranibizumab', time='m3'), type='joint')
  
 # (x<-contrast(a, list(time= 'baseline post', trt="Ranibizumab"),
 #                 list(time= 'm3', trt="Ranibizumab"), type="average") ) 
  
 #  (x<-contrast(a, list(time= 'baseline post', trt="Aflibercept"),
   #              list(time= 'm3', trt="Aflibercept"), type="average") )

   # summary(a, trt="Ranibizumab", time="m3")
  
    
  cat("Same model but using nlme so I can use effects and lsmeans packages")
  a1 <- nlme::gls(chg1~  (time) * trt + BASEVAL *  time + country,
                 correlation=corSymm(form=~ as.numeric(time)|ID), 
                 weights=varIdent(form=~1|time), na.action=na.exclude,
                 d1)
  a1
  
  #coef(a1$modelStruct$varStruct, uncons = FALSE, allCoef = TRUE)
  #coef(a1$modelStruct$corStruct, uncons = FALSE, allCoef = TRUE)
  
```

### Effects package

```{r, effects package}
  
  require(effects) # slow
  
  #ef <- allEffects(a1,"time*trt")
  #plot(allEffects(a1))  # this works but is slow
  ef <- allEffects(a1)
  #summary(ef)
  
  plot(ef$country)
  summary(ef$country)
  
  plot(ef$"time:trt")
  summary(ef$"time:trt")
  
  plot(ef$ "time:BASEVAL")
  summary(ef$"time:BASEVAL")
  
```  
  
### lsmeans package  , not working NAs?
  
  
```{r lsmeans}

  detach("package:rms", unload=TRUE)
  require(lsmeans)
 
 temp <- d1[complete.cases(d1),]
 a1 <- nlme::gls(chg1~  (time) * trt + BASEVAL *  time + country,
                 correlation=corSymm(form=~ as.numeric(time)|ID), 
                 weights=varIdent(form=~1|time), na.action=na.exclude,
                temp)
  a1

  #https://cran.r-project.org/web/packages/lsmeans/vignettes/using-lsmeans.pdf
  
  cat("not very useful as results are averaged over the levels of: trt, country")
  lsmeans(a1, "time")
  cat("not very useful as results are averaged over the levels of: time, country")
  lsmeans(a1, "trt")
  
  cat("The mth 3 treatment effects are here! as well as other time points")
  (t.e <- lsmeans(a1, ~ time * trt, at = "m3" )) # can compare with sas!
  


# 
#   cat("Exploring lsmeans functions, thats all")
#   days.lsm <- lsmeans(a1, "time")
#  ( days_contr.lsm <- contrast(days.lsm, "trt.vs.ctrl", ref = c(1,2)) )
#   
#    days.lsm <- lsmeans(a1, "trt")
#   ( days_contr.lsm <- contrast(days.lsm, "trt.vs.ctrl", ref = c(1)) )
#   ( days_contr.lsm <- contrast(days.lsm, "pairwise") )
  
   
   # days.lsm <- lsmeans(a1, "time")
   # (days_contr.lsm <- contrast(days.lsm, "trt.vs.ctrl") )
   # (days_contr.lsm <- contrast(days.lsm, "trt.vs.ctrlk") )
   # (days_contr.lsm <- contrast(days.lsm, "pairwise") )
  
   
   cat("these match sas point estimates, kr does not seem to work ?")
   ref.grid(a1)
   org.mlsm <- lsmeans(a1, ~ time | trt, mode = "kenward-roger") #kr not working?
   org.mlsm
   
   org.vardiff <- update(pairs(org.mlsm, by = "time"), by = NULL ) #dif
   coef(org.vardiff)
   cat("trt effect with time")
   org.vardiff
  # cld(org.vardiff, sort = FALSE)  # means
   plot(org.vardiff, comparisons = TRUE, alpha = .05)

   
  # cld(org.mlsm, sort = FALSE)  # means

   lsmeans(a1, ~ time | trt,   cov.reduce = T)
   
   cat("trt effect with time, red arrows meant to show sig diff")
   plot(org.mlsm, comparisons = TRUE, alpha = .05,  intervals = TRUE, int.adjust = "none")

```
 
#### Contrast, treatment effect at last time point 
 
```{r contrast, echo=FALSE, warning=FALSE, message=FALSE}  

 
cat("contrast of interest: trt effect at last time point")
(x<-rms::contrast(a, list(time= 'm3', trt="Ranibizumab"), 
                list(time= 'm3', trt="Aflibercept"), type="average") )

cat("R Gls function CIs are based on normal dist\n")
x$Contrast+c(-1,1)*qnorm(0.975)*x$SE

cat("If we calculate CI based on t dist, using df from SAS output  
the result matches sas when kenward roger df correction is not used\n")
x$Contrast+c(-1,1)*qt(0.975,50)*x$SE

cat("Therefore the same data is used and the correct model has been fit to the data\n")

p1 <- rms::Predict(a, trt, time="m3")


#newdat <- expand.grid(trt=levels(d$trt),time="m3", BASEVAL=mean(d$BASEVAL), country=NA)
#cbind(newdat, predict(a, newdat ,   na.action  =na.omit))

   # d1$f <- fitted(a, se.fit=T)
   # tapply(d1$f, list(d1$time,d1$trt), mean, na.rm=T)
 # d$p <- Predict(a)
 #  tapply(d$p, list(d$time,d$trt1), mean, na.rm=T)

```

# more crude calculations

```{r testing2 , eval=FALSE}
 
 
    # foo has baseline
    ddx<- foo
    ddx$trt1 <- ifelse(ddx$trt=="Aflibercept",1,0)
    ddx$time <-factor(ddx$time)

    ddx  <-ddx[,c("COU1A", "BASEVAL",  
                    "chg1",  "trt", "time", 
                    "y", "ID", "trt1")]
      
   d <-ddx
 
   cat("trt mean calculations with time")
   tapply(d$y, list(d$time,d$trt1), mean, na.rm=T)
   cat("crude change calculations from baseline with time ")
   (crude.chg <-tapply(d$chg1, list(d$time,d$trt1), mean, na.rm=T))
   crude.chg[,1]-crude.chg[,2]

   
```

#random effects...contrast package for nlme and lsmeans and effects for lmer

```{r}

# lme(y~time + trt , random=~time|ID, 
#     correlation=corSymm(), 
#     data=d1,
#     na.action=na.exclude)
# 
# ylme.1<- lme(y~time + trt,
#              correlation=corSymm(form = ~ 1 | ID) , 
#              random=~1|ID, data=d1, na.action=na.exclude)
# 
# ylme.1<- lme(chg1 ~ time * trt + BASEVAL *  time + country,
#                  random=~1|ID,
#                   correlation=corSymm(form=~ as.numeric(time)|ID), 
#                   weights=varIdent(form=~1|time),
#                   d, na.action=na.exclude)
#  

m1 <- lme4::lmer(chg1 ~ time * trt + BASEVAL * time + country + (as.numeric(time)|ID), data=d1, na.action=na.exclude)

m2 <- lme4::lmer(chg1 ~ time * trt + BASEVAL * time + country + (1|ID), data=d1, na.action=na.exclude)



anova(m1,m2)  # evidence random slopes is better fit N=205 p=0.061

cat("outlier present!")
resid<- residuals(m1)
qqnorm(resid, main="Normal Q-Q Plot of Residuals")
qqline(resid)
plot(residuals(m1))
plot(m1)

augData <- data.frame(d1,fitted=fitted(m1),resid=residuals(m1))
library(lattice)
xyplot(fitted~resid,data=augData)



#require(effects)
ef <- allEffects(m1)
summary(ef$"time:trt")
 
require(lsmeans)
org.mlsm <- lsmeans(m1, ~ time | trt, mode = "kenward-roger")
org.vardiff <- update(pairs(org.mlsm, by = "time"), by = NULL ) #dif 
   coef(org.vardiff)
   cat("trt effect with time")
   org.vardiff
   #cld(org.vardiff, sort = FALSE)  # means
   plot(org.vardiff, comparisons = TRUE, alpha = .05)
   
 
   
   
# f0 <- lme(chg1 ~ time * trt + BASEVAL * time + country ,
#           random = list(ID = pdDiag(form = ~ time - 1)),
#      correlation=corSymm(form = ~ 1 | ID) , data=d1,
#       na.action=na.exclude) 
# x <-lme(chg1 ~ time * trt + BASEVAL * time + country,
#         random = ~1|ID, 
#         correlation=corSymm(form = ~ time), d1,     weights=varIdent(form = ~1|time), na.action=na.exclude)
   
 
cat("correlation structure\n")
 
   
cp <- list(corAR1, corCAR1, corExp, corLin, corGaus,corSpher)
   
z <- vector('list', length(cp))

for(k in 1:length(cp)) {
  
  
    z[[k]] <- lme(chg1~1+ time * trt + BASEVAL * time + country ,                random=~1|ID,
              correlation=cp[[k]](form=~as.numeric(time)|ID ),
              weights=varIdent(form=~1|time),method="REML", 
              na.action=na.exclude, d1) 

   
   #  z[[k]] <- lme(chg1~1+ time * trt + BASEVAL * time + country ,                random=~1|ID,
    #         correlation=cp[[k]](form=~as.numeric(time)|ID ),
    #         weights=varIdent(form=~1|time),method="REML", 
    #         na.action=na.exclude, d1)  
   
   
 # print( plot(Variogram(z[[k]],form=~~as.numeric(time)|ID) , main=paste(k)))
  
}

cat("lowest AIC?\n")

anova(z[[1]],z[[2]], z[[3]], z[[4]], z[[5]],  z[[6]])
    
lme1 <- z[[6]]    
    
getVarCov(lme1, id = c("0901_00002"), type  = "random.effects")   
x <- getVarCov(lme1, ID = c("0901_00002"), type  = "conditional")
# correlation matrix
R <- cov2cor(x$`0901_00002`)
# vector of std. dev.
sds <- c(20, 23, 22, 55 ,9.1, 11, 18, 22) # lifted from model output 
R * sds * rep(sds, each = nrow(R))
    
cat("outlier present!")
resid<- residuals(lme1)
qqnorm(resid, main="Normal Q-Q Plot of Residuals")
qqline(resid)
plot(residuals(lme1))
qqnorm(lme1, ~ranef(., level=1))
plot(lme1)

# x<-lme1
# getVarCov(x, id = c("0901_00002"), type = "marginal")
# getVarCov(x, id = c("0901_00002"), type  = "conditional")
# getVarCov(x, id = c("0901_00002"), type  = "random.effects")   

# f0 <- lme(chg1 ~ time * trt + BASEVAL * time + country ,
#           random = list(ID = pdDiag(form = ~ time - 1)),
#          correlation=corSymm(form = ~ 1 | ID) , data=d1,
#           na.action=na.exclude)      
   
f <- nlme::lme(chg1 ~ time * trt + BASEVAL * time + country ,                     random=~1|ID, data=d1,
               weights=varIdent(form=~1|time),
               na.action=na.exclude)

fx <- nlme::lme(chg1 ~ time * trt   , random=~1|ID, data=d1,
               na.action=na.exclude)
 
# getVarCov(f)
# getVarCov(f, individual = "0901_00002", type = "marginal")
# getVarCov(f, type = "conditional")
# # only for nlme it seems
# print(
#   contrast(lme1,
#              list(time = "m3", trt="Ranibizumab", BASEVAL=mean(d1$chg1, na.rm=T), country="TWN"),
#              list(time = "m3", trt="Aflibercept", BASEVAL=mean(d1$chg1, na.rm=T), country="TWN")),
#    X = TRUE)



foo <- d1
foo$chg1 <- fitted(lme1)

foo %>%
   mutate(source = "fitted") %>%
   bind_rows(mutate(d1, source = "observed")) %>%
   ggplot(aes(time, chg1, color = source, group=trt)) +
   stat_summary(fun.y = mean, geom = "line") +
   stat_summary(fun.data = mean_se, geom = "pointrange")  +
   facet_grid(. ~  source)

foo %>%
   mutate(source = "fitted") %>%
   bind_rows(mutate(d1, source = "observed")) %>%
   ggplot(aes(time, chg1, color = trt, group=trt, linetype=source)) +
   stat_summary(fun.y = mean, geom = "line") +
   stat_summary(fun.data = mean_se, geom = "pointrange") 



# f <- nlme::lme(chg1 ~  trt +time , random=~1|ID, data=d1,
#                na.action=na.exclude)
# foo <- d1
# foo$chg1 <- fitted(f)
# 
# foo %>%
#   mutate(source = "fitted") %>%
#   bind_rows(mutate(d1, source = "observed")) %>%
#   ggplot(aes(time, chg1, color = trt,  linetype=source)) +
#   stat_summary(fun.y = mean, geom = "line") +
#   stat_summary(fun.data = mean_se, geom = "pointrange") 




###################  


tapply(foo$chg1, list(foo$trt, foo$time), mean, na.rm=TRUE)
tapply(d1$chg1, list(d1$trt, d1$time), mean, na.rm=TRUE)

require(contrast)

print(
  contrast::contrast(lme1,
             list(time = "m3", trt="Ranibizumab", BASEVAL =mean(d1$chg1, na.rm=T), country="TWN"),
             list(time = "m3", trt="Aflibercept", BASEVAL =mean(d1$chg1, na.rm=T), country="TWN")),
   X = TRUE)

print(
  contrast::contrast(lme1,
             list(time = "m3", trt="Ranibizumab", BASEVAL =median(d1$chg1, na.rm=T), country="MYS"),
             list(time = "m3", trt="Aflibercept", BASEVAL =median(d1$chg1, na.rm=T), country="MYS")),
   X = TRUE)

cat("compare to gls\n")
(x<-rms::contrast(a, list(time= 'm3', trt="Ranibizumab"), 
                list(time= 'm3', trt="Aflibercept"), type="average") )


# lme1 <- lme(chg1~1+ time * trt + BASEVAL * time + country ,      
#             random=~1|ID,
#               correlation=corSpher(form=~as.numeric(time)|ID ),
#               weights=varIdent(form=~1|time),method="REML", 
#               na.action=na.exclude, d1) 





``` 

#### Use lmer to simulate
 
```{r}
   
  cat("Cannot predict nlme when models have with 'corStruct' and/or 'varFunc' as objects not allowed'\n")
  d1$predicted <- d1$simulated <- NULL
  d1$predicted <- predict (m1)
  d1$simulated <- simulate(m1)$sim_1

  g <- plyr::arrange(d1, trt, ID, time)
  g <- droplevels(g)
  
  g$time <- factor(g$time, 
  levels=c( "baseline post","wk1","wk2" ,"m1 pre", 
           "m1 post", "m2 pre" , "m2 post", "m3") ,
  labels=c( "baseline post","wk1","wk2" ,"m1 pre", 
           "m1 post", "m2 pre" , "m2 post", "m3"))

  
  
#  g$time <- relevel(g$time, ref="baseline post") # so axis order correct as m3 is ref level in model (only doing this to chec to sas output).

  gg <- unique(g[,c("trt","ID")])
  gr <- table(gg$trt)[[1]]
  zlab <- c(levels(g$time)) # for labels on x axis

  zz <- length(unique(d1$ID))

x1 <- with(g, as.matrix(tapply(chg1,  list(ID, time), mean), ncol=9))
y1 <- with(g, as.matrix(tapply(predicted, list(ID, time), mean), ncol=9))
z1 <- with(g, as.matrix(tapply(simulated, list(ID, time), mean), ncol=9))

f <- function(xlab="visit", ...) {
    plot(c(1,8), c(-400, 800), ylab="change from baseline",
          xlab=xlab, xaxt="n",
       t="n", ...)
  axis(1, at=1:8,labels=zlab, col.axis="red", las=2)
  grid()
}
par(mfrow=c(1,3), mar=c(4,4,1,1))

 
f(main="Observed data", "")
   for (i in 1:zz)
      lines(x1[i,]  ,     col=ifelse(i<=gr,"blue","red")) #30

f(main="Predicted data, subjects", "")
   for (i in 1:zz)
      lines(y1[i,]  ,     col=ifelse(i<=gr,"blue","red")) #30
f(main="Simulated data, new subjects")
   for (i in 1:zz)
      lines(z1[i,]  ,     col=ifelse(i<=gr,"blue","red")) #30

legend("topright", pch=c(1,1), col=c("blue","red"), 
           c("Aflibercept", "Ranibizumab"), bg="white")
 

```

### analyse a simulation from lmer m1 model using models lmer lme Gls

```{r ,eval=FALSE}


 
  #d1$predicted <- predict (m1)
  set.seed(1234567)  # trial and error to get a simulation that will
  d1$simulated <- simulate(m1)$sim_1

  lmer.s <- lme4::lmer(simulated ~ time * trt + BASEVAL * time + country + (as.numeric(time)|ID), data=d1, na.action=na.exclude)

  lme.s <- lme(simulated~1+ time * trt + BASEVAL * time + country ,      
            random=~1|ID,
              correlation=corSpher(form=~as.numeric(time)|ID ),
              weights=varIdent(form=~1|time),method="REML", 
              na.action=na.exclude, d1) 

  gls.s <- rms::Gls(simulated ~ time * trt + BASEVAL *  time + country,
                  correlation=corSymm(form=~ as.numeric(time)|ID), 
                  weights=varIdent(form=~1|time),
                  d1, x=TRUE, 
           na.action=na.exclude )



 cat("observed analysed using harrell\n")

(x<-rms::contrast(a, list(time= 'm3', trt="Ranibizumab"), 
                list(time= 'm3', trt="Aflibercept"), type="average") )
 
cat("harrell package for analysis of simlulation\n")  
(x<-rms::contrast(gls.s, list(time= 'm3', trt="Ranibizumab"), 
                list(time= 'm3', trt="Aflibercept"), type="average") )  

cat("observed analysed using contrast package and lme model\n")  

 print(
  contrast::contrast(a1,
             list(time = "m3", trt="Ranibizumab", BASEVAL =mean(d1$chg1, na.rm=T), country="TWN"),
             list(time = "m3", trt="Aflibercept", BASEVAL =mean(d1$chg1, na.rm=T), country="TWN")),
   X = TRUE)
 
cat("contrast package for lme1 analysis of simulation\n")  

print(
  contrast::contrast(lme.s,
             list(time = "m3", trt="Ranibizumab", BASEVAL =mean(d1$chg1, na.rm=T), country="TWN"),
             list(time = "m3", trt="Aflibercept", BASEVAL =mean(d1$chg1, na.rm=T), country="TWN")),
   X = TRUE)

cat("lsmeans for contrasts using lmer observed data\n")  

require(lsmeans)
org.mlsm <- lsmeans(m1, ~ time | trt, mode = "kenward-roger")
org.vardiff <- update(pairs(org.mlsm, by = "time"), by = NULL ) #dif 
cat("trt effect with time")
org.vardiff
#plot(org.vardiff, comparisons = TRUE, alpha = .05)

org.mlsm <- lsmeans(lmer.s, ~ time | trt, mode = "kenward-roger")
org.vardiff <- update(pairs(org.mlsm, by = "time"), by = NULL ) #dif 
cat("trt effect with time")
org.vardiff
#plot(org.vardiff, comparisons = TRUE, alpha = .05)

```

# GLS predictions from model a

```{r}
 

  foo <- d1

  labs <- c( "baseline post","wk1","wk2" ,"m1 pre", 
           "m1 post", "m2 pre" , "m2 post", "m3")

  d1$time <- factor(d1$time, 
  levels= labs,
  labels=labs)
  
  foo$time <- factor(foo$time, 
  levels= labs,
  labels=labs)
  
  
  foo$chg1 <- predict(a, newdata=data.frame(d1))  # same var name as observed and stack them

foo %>%
   mutate(source = "fitted") %>%
   bind_rows(mutate(d1, source = "observed")) %>%
   ggplot(aes(time, chg1, color = source, group=trt)) +
   stat_summary(fun.y = mean, geom = "line") +
   stat_summary(fun.data = mean_se, geom = "pointrange")  +
   # scale_x_discrete(breaks=levels(foo$time), labels=levels(foo$time) )+
   facet_grid(. ~  source) +
   theme(text = element_text(size=10),
           axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
   ggtitle("Fitted and observed from GLS model")  + xlab("visit") + ylab("chg from baseline")

 
  d1$predicted   <- NULL
  d1$predicted <- predict(a, newdata=data.frame(d1)) 
 
  g <- plyr::arrange(d1, trt, ID, time)
  g <- droplevels(g)
  
  g$time <- factor(g$time, 
  levels=c( "baseline post","wk1","wk2" ,"m1 pre", 
           "m1 post", "m2 pre" , "m2 post", "m3") ,
  labels=c( "baseline post","wk1","wk2" ,"m1 pre", 
           "m1 post", "m2 pre" , "m2 post", "m3"))
 

  gg <- unique(g[,c("trt","ID")])
  gr <- table(gg$trt)[[1]]
  zlab <- c(levels(g$time)) # for labels on x axis

  zz <- length(unique(d1$ID))

  x1 <- with(g, as.matrix(tapply(chg1,  list(ID, time), mean), ncol=9))
  y1 <- with(g, as.matrix(tapply(predicted, list(ID, time), mean), ncol=9))
 

  f <- function(xlab="visit", ...) {
      plot(c(1,8), c(-400, 800), ylab="change from baseline",
            xlab=xlab, xaxt="n",
         t="n", ...)
    axis(1, at=1:8,labels=zlab, col.axis="red", las=2)
    grid()
  }
  par(mfrow=c(1,3), mar=c(4,4,1,1))

 
  f(main="Observed data", "")
     for (i in 1:zz)
        lines(x1[i,]  ,     col=ifelse(i<=gr,"blue","red")) #30
  
  f(main="Predicted data, subjects", "")
     for (i in 1:zz)
        lines(y1[i,]  ,     col=ifelse(i<=gr,"blue","red")) #30
   
  legend("topright", pch=c(1,1), col=c("blue","red"), 
             c("Aflibercept", "Ranibizumab"), bg="white")

```

# lmer predictions from model m1

```{r}
 
 foo <- d1
 foo$chg1 <- predict(m1,  allow.new.levels=TRUE) #match fitted
 foo$chg1 <- predict(m1,  allow.new.levels=TRUE, re.form=NA) #to condition on none of the random effects, simulating new values for all of the random effects. 
 foo$chg1 <- fitted(m1,  allow.new.levels=TRUE ) #to condition on none of the random effects, simulating new values for all of the random effects. 
  
  cat("understanding predict and fitted\n")
  head(predict(m1,  allow.new.levels=TRUE))
  head(fitted(m1,  allow.new.levels=TRUE )) #same as above
  head(predict(m1))
  
  # this differs
  head(predict(m1,  allow.new.levels=TRUE, re.form=NA))

  
foo %>%
   mutate(source = "fitted") %>%
   bind_rows(mutate(d1, source = "observed")) %>%
   ggplot(aes(time, chg1, color = source, group=trt)) +
   stat_summary(fun.y = mean, geom = "line") +
   stat_summary(fun.data = mean_se, geom = "pointrange")  +
   facet_grid(. ~  source) +
  theme(text = element_text(size=10),
           axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
   ggtitle("Fitted and observed from Lmer model")  + xlab("visit") + ylab("chg from baseline")


  d1$predicted <- d1$simulated <- NULL
  d1$predicted <- fitted(m1,  allow.new.levels=TRUE ) # 
  d1$simulated <- simulate(m1)$sim_1
  
  g <- plyr::arrange(d1, trt, ID, time)
  g <- droplevels(g)
  
  g$time <- factor(g$time, 
  levels=c( "baseline post","wk1","wk2" ,"m1 pre", 
           "m1 post", "m2 pre" , "m2 post", "m3") ,
  labels=c( "baseline post","wk1","wk2" ,"m1 pre", 
           "m1 post", "m2 pre" , "m2 post", "m3"))
 

  gg <- unique(g[,c("trt","ID")])
  gr <- table(gg$trt)[[1]]
  zlab <- c(levels(g$time)) # for labels on x axis

  zz <- length(unique(d1$ID))

  x1 <- with(g, as.matrix(tapply(chg1,  list(ID, time), mean), ncol=9))
  y1 <- with(g, as.matrix(tapply(predicted, list(ID, time), mean), ncol=9))
  z1 <- with(g, as.matrix(tapply(simulated, list(ID, time), mean), ncol=9))

  f <- function(xlab="visit", ...) {
      plot(c(1,8), c(-400, 800), ylab="change from baseline",
            xlab=xlab, xaxt="n",
         t="n", ...)
    axis(1, at=1:8,labels=zlab, col.axis="red", las=2)
    grid()
  }
  par(mfrow=c(1,3), mar=c(4,4,1,1))

 
  f(main="Observed data", "")
     for (i in 1:zz)
        lines(x1[i,]  ,     col=ifelse(i<=gr,"blue","red")) #30
  
  f(main="Predicted data, subjects", "")
     for (i in 1:zz)
        lines(y1[i,]  ,     col=ifelse(i<=gr,"blue","red")) #30
  f(main="Simulated data, new subjects")
     for (i in 1:zz)
        lines(z1[i,]  ,     col=ifelse(i<=gr,"blue","red")) #30
  
  legend("topright", pch=c(1,1), col=c("blue","red"), 
             c("Aflibercept", "Ranibizumab"), bg="white")

```

# nlme predictions from model lme1

```{r}  


 foo <- d1
 foo$chg1 <- predict(lme1,  allow.new.levels=TRUE) #match fitted

foo %>%
   mutate(source = "fitted") %>%
   bind_rows(mutate(d1, source = "observed")) %>%
   ggplot(aes(time, chg1, color = source, group=trt)) +
   stat_summary(fun.y = mean, geom = "line") +
   stat_summary(fun.data = mean_se, geom = "pointrange")  +
   facet_grid(. ~  source) +
  theme(text = element_text(size=10),
           axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
   ggtitle("Fitted and observed from nlme model")  + xlab("visit") + ylab("chg from baseline")


  d1$predicted <- predict(lme1,  allow.new.levels=TRUE) #
 
  g <- plyr::arrange(d1, trt, ID, time)
  g <- droplevels(g)
  
  g$time <- factor(g$time, 
  levels=c( "baseline post","wk1","wk2" ,"m1 pre", 
           "m1 post", "m2 pre" , "m2 post", "m3") ,
  labels=c( "baseline post","wk1","wk2" ,"m1 pre", 
           "m1 post", "m2 pre" , "m2 post", "m3"))
 

  gg <- unique(g[,c("trt","ID")])
  gr <- table(gg$trt)[[1]]
  zlab <- c(levels(g$time)) # for labels on x axis

  zz <- length(unique(d1$ID))

  x1 <- with(g, as.matrix(tapply(chg1,  list(ID, time), mean), ncol=9))
  y1 <- with(g, as.matrix(tapply(predicted, list(ID, time), mean), ncol=9))
 

  f <- function(xlab="visit", ...) {
      plot(c(1,8), c(-400, 800), ylab="change from baseline",
            xlab=xlab, xaxt="n",
         t="n", ...)
    axis(1, at=1:8,labels=zlab, col.axis="red", las=2)
    grid()
  }
  par(mfrow=c(1,3), mar=c(4,4,1,1))

 
  f(main="Observed data", "")
     for (i in 1:zz)
        lines(x1[i,]  ,     col=ifelse(i<=gr,"blue","red")) #30
  
  f(main="Predicted data, subjects", "")
     for (i in 1:zz)
        lines(y1[i,]  ,     col=ifelse(i<=gr,"blue","red")) #30
   
  legend("topright", pch=c(1,1), col=c("blue","red"), 
             c("Aflibercept", "Ranibizumab"), bg="white")

```

#### REFERENCES

######1 https://stackoverflow.com/questions/14261619/subsetting-r-data-frame-results-in-mysterious-na-rows  
######2 https://stackoverflow.com/questions/13239843/annotate-ggplot2-facets-with-number-of-observations-per-facet
######3 https://gist.github.com/tmalsburg/df66e6c2ab494fad83ee
######4 http://r.789695.n4.nabble.com/how-to-get-quot-lsmeans-quot-td821113.html
######5 https://cran.r-project.org/web/packages/lsmeans/vignettes/using-lsmeans.pdf
######6 https://stat.ethz.ch/pipermail/r-sig-mixed-models/2012q1/017637.html
######7 https://stackoverflow.com/questions/25792072/convert-mixed-model-with-repeated-measures-from-sas-to-r
######8 https://stats.stackexchange.com/questions/91445/r-gls-vs-sas-proc-mixed-with-interaction-why-does-r-complain-about-a-singulahttps://stat.ethz.ch/pipermail/r-help/2007-October/144357.html
######9 http://stla.github.io/stlapblog/posts/MixedRepeatModel.html
######10 https://stackoverflow.com/questions/25792072/convert-mixed-model-with-repeated-measures-from-sas-to-r
######11 http://02429.compute.dtu.dk/enote/afsnit/NUID192/
######12 http://stla.github.io/stlapblog/posts/AV1R_SASandR.html
######13 https://onlinecourses.science.psu.edu/stat510/?q=book/export/html/56
######14 https://www.uvm.edu/~dhowell/StatPages/More_Stuff/Mixed-Models-Repeated/Mixed-Models-for-Repeated-Measures1.html
######15 https://books.google.co.uk/books?id=oXqGDlPi_c8C&pg=PA195&lpg=PA195&dq=sas+repeated+measures+in+r+gls&source=bl&ots=X48dKWqTfE&sig=iRdzkcra52AudBFj-fNaB43cuI8&hl=en&sa=X&ved=0ahUKEwjPiovj2bnVAhUpJsAKHdu6A304FBDoAQhLMAU#v=onepage&q=sas%20repeated%20measures%20in%20r%20gls&f=false
######16 https://rpubs.com/kaz_yos/bio226hw2
######17 http://bstt513.class.uic.edu/bockAC.txt
######18 http://www.stats.uwo.ca/faculty/bellhouse/stat%209945b/BockData.pdf
######19 p108 of book
######20 file:///C:/Users/User/Downloads/Donald%20Hedeker,%20Robert%20D.%20Gibbons%20Applied%20Longitudinal%20Data%20Analysis%20.pdf
######21 https://stats.stackexchange.com/questions/24452/how-to-interpret-variance-and-correlation-of-random-effects-in-a-mixed-effects-m
######22 https://stackoverflow.com/questions/23665112/how-to-code-contrasts-in-lme4-or-nlme
\clearpage
 
# COMPUTING ENVIRONMENT
 
```{r, echo=FALSE}
 
options(width=70)
opts_knit$set(root.dir = wd.code)   # THIS SETS YOUR WORKING DIRECTORY
sessionInfo()
print(getwd())
 
```
 
```{r echo=FALSE}
 
stopTime<-proc.time()
 
```
 
This took `r (stopTime-startTime)[1][[1]]` seconds to execute on `r format(Sys.time(), "%a %b %d %X %Y")`.

```{r echo=FALSE}

# move stangle R file to a folder in GPS
# put this at bottom & give it the same name as RMD file , code will replace any blanks with underscore
# https://amywhiteheadresearch.wordpress.com/2014/11/12/copying-files-with-r/
xxx <- "SAS Proc mixed repeated measures using R an investigation.R"
rcode <-  gsub(' ','_', trimws(xxx))          # replace blank with underscore, this is needed
file.copy(rcode, wd.code,  overwrite=TRUE)    # make a copy of the rcode in a folder of choice

```

