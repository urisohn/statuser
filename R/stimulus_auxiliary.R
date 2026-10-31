# Internal helpers for stimulus.plot() and stimulus.beeswarm()
# (Vendored from the `stimulus` GitHub package, adapted for statuser.)

utils::globalVariables(c("m1", "m2", "m3", "m1.cluster"))


# ---- from utils.r ----

#0 Stimulus cache (package state)
  sp_stimulus_cache <- function() {
    if (is.null(.statuser_state$stimulus_cache)) {
      .statuser_state$stimulus_cache <- list()
    }
    .statuser_state$stimulus_cache
  }




#Functions

#1 sp_round2()     - Rounding (internal; do not collide with exported round2)
#2 exit()          - message to show when executoin ends early
#3 clean_string()  - Remove everything except letters, numbers, and underscores
#4 tidy.t()        - t.test outputs to data.frame
#5 formatted.p () - turn p-value into a formatted string (e.g. p=.023 or p<.001)
#6 get.ci()       - Get confidence interval from model
#7 add_named_if_exists() 
#8 namedList()    - automatically name objects in listAutomatically name elements in list with name of the objects in the list
#9 format_percent() - format number as a percent
#10 auto.decimals() - Number of decimals determined automatically
#11 get.md5()       - Compute the MD5 hash of a dataframe
#12 does.cache.d.exist() - Does cache exist
#13 eval2()         - Evaluate string as formula
#14 eval.arguments()
#15 message2()     - show message on screen with 'groundhog' color (not red, blue or black, which are not usually friendly texts)
#16 round_smart()  - automatically set number of digits
#17 Format format_msg() - turns a long character string into a fixed withd with possible header and pre text.
#18 get.counter.interval() - set how many interactions in counter before output is shown 
 


#----------------------------------

#1 Rounding
  sp_round2 <- function(x, digits = 2) {
    ncode <- paste0("%.", digits, "f")
    sub("^(-?)0.", "\\1.", sprintf(ncode, x))
  }


  
#2 gstop (uses statuser exit)
    gstop <- function(msg, format = FALSE) {
    if (format == TRUE) msg <- format_msg(msg)
    exit(paste0(msg, "\n----------------------------------------"))
    }
    
    
  
#3 Clean string
   clean_string <- function(input_string) {
      #Remove everything except letters, numbers, and underscores
      #cleaned_string <- gsub("[^A-Za-z0-9._]", "", input_string)
       cleaned_string <- gsub("\"", "", input_string)

  return(cleaned_string)
   }
   
   
   
#4 tidy.t
   
  tidy_t = function(t)
  {
  row=data.frame(
                 m1     = t$estimate[1],
                 m2     = t$estimate[2],
                 effect = t$estimate[1]-t$estimate[2],
                 t      = t$statistic,
                 df     = t$parameter,
                 p      = t$p.value,
                 ciL    = t$conf.int[1],
                 ciH    = t$conf.int[2], 
                 row.names = NULL)    
  return(row)    
    
  }
  

#5 formatted p-value
    formatted.p <- function(p) {
      format_single_p <- function(single_p) {
        p.clean <- round(single_p, 3)           # Round it
        if (p.clean==0) return ("p<.0001")
        p.clean <- substr(p.clean, 2, 6)        # Drop the 0
        p.clean <- paste0("=", p.clean)
        if (single_p < .0001) p.clean <- "<.0001"
        if (single_p > .9999) p.clean <- ">.9999"
        p.clean <- paste0("p", p.clean)
        return(p.clean)
      }
  
  # Apply the function to each element of the input vector
  result <- sapply(p, format_single_p)
  return(result)
}


  
#6 Confidence interval from model
     get.ci=function(m)
      {
      coe=summary(m)$coefficients 
      b=coe[2,1]                   #point estimate
      se=coe[2,2]                  #SE
      tc = qt(.975,df=coe[2,3])    #look-up t-distribution for 95% CI for those d.f.
      ci=c(b-tc*se , b+tc*se)
      return(ci)
     }
     
     
#7 Add to list if object exists
   add_named_if_exists <- function(obj_name, lst) {
  if (exists(obj_name, envir = parent.frame())) {
    obj_value <- get(obj_name, envir = parent.frame())
    lst[[obj_name]] <- obj_value
  }
  return(lst)
   }
   
   
#8 Automatically name elements in list with name of the objects in the list
    #https://stackoverflow.com/questions/16951080/can-lists-be-created-that-name-themselves-based-on-input-object-names
    namedList <- function(...) {
      L <- list(...)
      snm <- sapply(substitute(list(...)),deparse)[-1]
      if (is.null(nm <- names(L))) nm <- snm
      if (any(nonames <- nm=="")) nm[nonames] <- snm[nonames]
      setNames(L,nm)
    }
    
#9 Format as percent
format_percent <- function(x,decimals) {
  if (decimals=='auto') decimals <- auto.decimals(x * 100)
  formatted_number <- sapply(1:length(x), function(i) {
    paste0(formatC(x[i] * 100, format = "f", digits = decimals), "%")
  })
  return(formatted_number)
}
    
    
#10 Number of decimals
    auto.decimals <- function(x) {
      
      sapply(x, function(num) {
      num=abs(num)
      if (is.na(num)) {
        return(NA)
      } else if (num > 100) {
        return(0)
      } else if (num > 10) {
        return(1)
      } else if (num > 0.1) {
        return(2)
      } else if (num > 0.01) {
        return(3)
      } else {
        return(4)
      }
    })
  }

    
#11 # Function to compute the MD5 hash of a dataframe
  get.md5 <- function(df) {
    df_serialized <- serialize(df, NULL)
    md5_hash <- digest::digest(df_serialized, algo = "md5")
    return(md5_hash)
  }
  
#12 Does cache exist
  does.cache.d.exist = function(md5k)
  {
    cache <- sp_stimulus_cache()
    key_exists <- md5k %in% names(cache)
    return(key_exists)
  }
  
#13 eval2
 eval2 <- function(s)  eval(parse(text=s),  parent.frame())  #Added "parent.frame() because otherwise eval2() will not call the right objects within another function: Fix was found here; https://www.rdocumentation.org/packages/base/versions/3.5.1/topics/eval


#14 eval.arguments
 eval.arguments = function(model_string, dv, condition, stimulus, participant,dataname)
   {
     model_string <- gsub("dv", dv, model_string)
     model_string <- gsub("condition", condition, model_string)
     model_string <- gsub("stimulus", stimulus, model_string)
     model_string <- gsub(",data=df2", '', model_string)
     #model_string <- gsub("df2", dataname, model_string,fixed=TRUE)
     model_string <- gsub("participant", participant, model_string)
    return(model_string)
 }
 
#17 round_smart: dynamic number of digits
  round_smart <- function(x) {
    abs_x <- abs(x)  # Use the absolute value
  
    if (abs_x >= 0.01) {
      # Always show 2 decimals
      return(formatC(x, format = "f", digits = 2))
    } else if (abs_x >= 0.00001) {
      # Show the first non-zero decimal
      non_zero_digits <- sub("0\\.", "", sub(".*?([1-9]+.*)", "\\1", formatC(abs_x, format = "f", digits = 5)))
      return(formatC(x, format = "f", digits = nchar(non_zero_digits)))
    } else {
      # For numbers smaller than 0.00001
      return("<0.00001")
    }
  }
  
  
#18 Format msg
format_msg <- function(msg,width=70, header='IMPORTANT.', pre="| ")
    {
    #Line counter
    j<-0
    #Lines with formatted message starts empty
      msg.lines=c()
    #Turn message into vector of words
      msg.left <- strsplit(msg,' ')[[1]]

    #Loop over lines
      while (length(msg.left)>0)
      {
     j=j+1
     msg.lines[j]=''

    #loop over words
      while (nchar(msg.lines[j]) + nchar(msg.left[1]) <width)
      {
      new.word <- msg.left[1]
      msg.left <- msg.left[-1]
      if (regexpr('\n', new.word)>0) break   #skip line if \n is found
      msg.lines[j] <- paste0(msg.lines[j],new.word," ")   #add the next word
      
      if (length(msg.left)==0) break
    }
      msg.lines[j]<- paste0(pre,"    ", msg.lines[j] ) 
      if (length(msg.left)==0) break
    }
      
  #formatted 
    #Add |  
      msg.lines <- gsub("\n", "\n|", msg.lines)
      
      
    #Join al
      msg.formatted <- paste0(msg.lines,collapse="\n")
      
    #Add header
      msg.formatted <- paste0(pre,header,"\n",msg.formatted)
      
    #Add ------------- on top
      sep.line <- c(paste0(rep('-',width+5)) , "\n" )
      msg.formatted<-c(sep.line, msg.formatted)
    
    return(msg.formatted)
}



#18 Set how often feedback of interactions is shown 
  get.counter.interval = function(seconds)
  {
  # Predefined set of possible intervals
      intervals <- c(1,5, 10,20,25,50,seq(100,1000,100))
    
  # Calculate approximate interval to achieve roughly a 10-second print frequency
    target_time <- 3 #we want to show feedback every 3 seconds
    approx_interval <- target_time / seconds
    
  # Find the closest interval from the predefined set
    return(intervals[which.min(abs(intervals - approx_interval))])
    
  }
  
  
  

# ---- from check1.R ----

#Function used to validate inputs to functions in this package, checks type and length of variable


#1) Auxiliary - Integer?    
    is.integer2 = function(x) all(floor(x)==x)

    
    #Color
    valid_color <- function(col1) {
        # Try converting the color to RGB; if it fails, it's not a valid color
        tryCatch({
          col2rgb(col1)
          TRUE  # If no error, the color is valid
        }, error = function(e) {
          message("Error: '", col1, "' is not a valid color.")
          FALSE  # If error, the color is invalid
        })
      }
            

#2) check1(): function that evaluates type and length of a given argument in the function
      check1 = function(f,var, type.check,  nu.check = -1,args_passed)
      {
        
        #varname
          varname <- deparse(substitute(var))
          
        #If was not assigned a value 
          if (!varname %in% args_passed) return(TRUE)
        
        
      
        #Unique.values
          nu=length(unique(var))
          if (nu != nu.check & nu.check!= -1) {
              exit(paste0(f,"() says: the argument '",varname,"' must have '",nu.check,"' unique values, but it has '",nu,"'")) 
          }
            
      
        #Type integer
          if (type.check=='integer')
            {
            if (is.integer2(var)==FALSE) {
              exit(paste0(f,"() says: the argument '",varname,"' must be an integer, but '",var, "' isn't."))
            }
              
              }
        
        #Type character
          if (type.check=='character')
            {
            if (is.character(var)==FALSE) exit(paste0(f,"() says: the argument '",varname,"' must be a character variable but '",var, "' isn't."))
          }
        
        #Type numeric
          if (type.check=='numeric')
            {
            if (is.numeric(var)==FALSE) exit(paste0(f,"() says: the argument '",varname,"' must be a numeric, but '",var, "' isn't."))
          }
        #Type loical
          if (type.check=='logical')
          {
            
            if (is.logical(var)==FALSE)  exit(paste0(f,"() says: the argument '",varname,"' must be either TRUE or FALSE, but '",var, "' is neither."))
          }
          
        #Type color
          if (type.check=='color')
          {
            is.color <- valid_color(var) 
            if (!is.color) exit(paste0(f,"() says: the argument '",varname,"' must be a valid color but '",var, "' isn't recognized as a color."))
            
            }
            
          
      }
        
        
      
      

# ---- from validate.stimulus.plot.R ----

validate.stimulus.plot=function(plot.type, data,dv, condition, stimulus, save.as,  svg.width, svg.height,  sort.by,  flip.conditions,model, 
                                  overall.estimate, overall.ci,overall.p, overall.label,ylab1, ylab2, xlab1, xlab2,
                                  decimals, null.method,dv.is.percentage, legend.title,simtot, watermark, seed, ylim,args_passed)
    {
  
  #1 Function to identify source of error
      f = 'statuser::stimulus.plot'
      
  #2 Check all arguments are of the appropriate length and type; see file check1.R
      check1(f, plot.type, 'character', 1,args_passed)
      check1(f, save.as, 'character', 1,args_passed)
      check1(f, svg.width, 'numeric', 1,args_passed)
      check1(f, svg.height, 'numeric', 1,args_passed)
      check1(f, sort.by, 'charcter', 1,args_passed)
      check1(f, flip.conditions, 'logical', 1,args_passed)
      check1(f, overall.estimate, 'numeric', 1,args_passed)
      check1(f, overall.ci, 'numeric', -1,args_passed)       #Checked below, and it need not be equal to 2.    
      check1(f, overall.p, 'numeric', 1,args_passed)
      check1(f, overall.label, 'character', 1,args_passed)
      check1(f, ylab1, 'character', 1,args_passed)
      check1(f, ylab2, 'character', 1,args_passed)
      check1(f, xlab1, 'character', 1,args_passed)
      check1(f, xlab2, 'character', 1,args_passed)
      check1(f, decimals, 'numeric', 1,args_passed)
      check1(f, null.method, 'character', 1,args_passed)
      check1(f, dv.is.percentage, 'logical', 1,args_passed)
      check1(f, legend.title, 'character', 1,args_passed)
      check1(f, simtot, 'integer', 1,args_passed)
      check1(f, watermark, 'logical', 1,args_passed)
      check1(f, seed, 'numeric', 1,args_passed)
      check1(f, ylim, 'numeric', 2,args_passed)
      
      
  #3 Check with limited set of values
      if (!plot.type %in% c('means','effects'))     exit(paste0(f,"() says: the argument 'plot.type' must be either 'effects' or 'means', you entered '",plot.type,"'"))
      if (!null.method %in% c('shuffle', 'demean', 'demeans')) exit(paste0(f,"() says: the argument 'null.method' must be either 'shuffle' or 'demean', you entered '",null.method,"'"))
      if (any(!model %in% c('all','regression','intercepts','slopes'))) {
          exit(paste0(f,"() says: If the argument 'model' is set, it must include only a subset of the following four values:\n ",
             "'all','regression','intercepts','slopes' "))
      }
      

  #4 Custome checks
      check.save.as(save.as) #function 5 here

  #5 No stimulus plot for compared design
      t = table(data[,stimulus],data[,condition])
      matched = mean(t[,1]*t[,2]>0) > .5  #check that at least half the stimulus ids in both conditions
      
      if (matched==FALSE) {
              exit(format_msg(paste0(
              f,"() says: The stimulus variable ('", stimulus,"') does not have the same values ",
              "across conditions. If you have a compared-stimulus design, with different ",
              "and unmatched stimuli across condition, use stimulus.beeswarm(). If you do ",
              "have a treated- or matched-stimulus design, then check that you have a matching ",
              "stimulus identifier for the pairs of stimuli across conditions."),header='Cannot do Stimulus Plot for compared-stimulus designs'))
      
      }
      
  #6 overall
      n1=length(overall.estimate)
      n2=length(overall.ci)
      n3=length(overall.p)
      n4=length(overall.label)

      if (length(unique(c(n1,n3,n4)))>1 & n1>0) exit("The 'overall' arguments (estimate, p, and label) must have the same length")
      if (n2!=2*n1) exit(paste0(f,"() says: Make sure that there are twice as many values in 'overall.ci' as in 'overall.estimate'"))
     

      
      
  }

  
      
      
      

# ---- from validate.data.R ----



#Function - Validate data

  validate.data = function(f, data, dv, condition, stimulus, sort.by,participant,dataname)
  {
        if (missing(data))      exit(paste0(f,"() says: you must specify a dataframe"))
        if (missing(dv))        exit(paste0(f,"() says: you must specify the dependent variable ('dv')"))
        if (missing(condition)) exit(paste0(f,"() says: you must specify the condition variable ('condition')"))
        if (missing(stimulus))  exit(paste0(f,"() says: you must specify the stimulus variable ('stimulus')"))
    
    
      n1=names(data)
      if (!dv %in% n1)        exit(paste0(f,"() says the dv ('",dv,    "') is not in the dataset '",dataname,"'."))
      if (!condition %in% n1) exit(paste0(f,"() says the condition variable ('",condition,"') is not in the dataset '",dataname,"'."))
      if (!stimulus %in% n1)  exit(paste0(f,"() says the stimulus variable ('",stimulus,"') is not in the dataset '",dataname,"'."))
      if (!sort.by %in% c(n1,"")    ) exit(paste0(f,"() says the sort.by variable ('",sort.by,"') is not in the dataset '",dataname,"'."))
      if (!participant %in% c(n1,"")) exit(paste0(f,"() says the participant variable ('",participant,"') is not in the dataset '",dataname,"'."))
  }
  
  

# ---- from validate.dots.R ----

  validate.dots=function(f,...)
  {
    #Get the arguments  
      dot_args <- list(...)
      #dot_args <- paste0('"', dot_args, '"')

      
    # Get the list of valid arguments for plot
      valid_plot_args <- names(formals(graphics::plot.default))
  
    # Check if all names in ... are valid plot arguments
       invalid_args <- setdiff(names(dot_args), valid_plot_args)
  
  # If there are any invalid arguments, throw an error
  if (length(invalid_args) > 0) {
    
    invalid_args <- paste0('"', invalid_args, '"')
    
    exit(paste0("These arguments are neither part of ", f, "(), nor Base R plot():\n",
        paste(invalid_args, collapse = ", ")))
  }
  }

# ---- from validate.R ----


  
#FUnction 3 - validate dots

  
  
#Function 4 - Validate beeswarm
    validate.beeswarm=function(data,  dv, stimulus, condition, 
                        flip.conditions, 
                        dv.is.percentage,
                        simtot,
                        confidence,
                        ylim,
                        ylab1,
                        ylab2,
                        xlab1,
                        xlab2,
                        dot.spacing,
                        col1,
                        col2,
                        main,
                        watermark,
                        save.as,
                        svg.width,
                        svg.height,
                        args_passed)
    {
      f <- 'statuser::stimulus.beeswarm'
      check1(f, flip.conditions, 'logical', 1, args_passed)
      check1(f, dv.is.percentage, 'logical', 1, args_passed)
      check1(f, simtot, 'integer', 1, args_passed)
      check1(f, ylab1, 'character', 1, args_passed)
      check1(f, ylab2, 'character', 1, args_passed)
      check1(f, xlab1, 'character', 1, args_passed)
      check1(f, watermark, 'logical', 1, args_passed)
      check1(f, confidence, 'numeric', 1, args_passed)
      if (svg.height != '') check1(f, svg.height, 'numeric', 1, args_passed)
      if (svg.width != '') check1(f, svg.width, 'numeric', 1, args_passed)
      check.save.as(save.as)
      check.confidence(confidence)
    }
    
    
  check.save.as=function(save.as)
  {
    if (save.as!='')  
    {
           call_stack <- sys.calls()
  
        # Check the second last entry in the stack, which is the calling function
          if (length(call_stack) >= 2) {
          caller <- as.character(call_stack[[length(call_stack) - 2]][[1]])
          }
     extension= tools::file_ext(save.as)  
     
     if (!extension %in% c('svg','png')) exit(paste0(caller,"() says: 'save.as' must have extension .svg or .png"))
    }
  }
  
  check.confidence=function(confidence)
  {
     # Get the call stack
          call_stack <- sys.calls()
  
        # Check the second last entry in the stack, which is the calling function
          if (length(call_stack) >= 2) {
          caller <- as.character(call_stack[[length(call_stack) - 2]][[1]])
          }
    
    if (confidence<5.1 | confidence>=99.9) exit(paste0(caller,"() says: 'confidence' must be between 5.1 and 99.9; for 95% confidence use 95, not .95"))
  
    
  }
    
    

# ---- from get.means.by.condition.R ----


  

  get.means.condition <- function(data, dv, stimulus, condition,sort.by,flip.conditions) {
      

    #  Process stimulus and condition values
        #Stimulus
          stimulus.all=unique(data[,stimulus])
          
        #Condition
          ucond=sort(unique(data[,condition]))
          if (flip.conditions==FALSE) data[,condition]=factor(data[,condition], levels=ucond)
          if (flip.conditions==TRUE)  data[,condition]=factor(data[,condition], levels=rev(ucond))
          
    
         
    #2 Compute means and CI via t-test
        
      # Split data by `stimulus`
          split_data <- split(data, data[,stimulus])
          
          # Apply t-test for each split, then extract p-value and statistic
          t.all_list <- lapply(split_data, function(sub_data) {
            test <- t.test(sub_data[,dv] ~sub_data[,condition])
            tk=tidy_t(test)
            tk
          })
          
        # Combine the results into a dataframe
          t.all <- do.call(rbind, t.all_list)
          t.all <- data.frame(stimulus = rownames(t.all), t.all, row.names = NULL)
          
              
      # Rename the means columns
        names(t.all)[2:3] <- c(paste0(condition,"_",ucond[1]), paste0(condition,"_",ucond[2]))
        names(t.all)[1] <-stimulus 
      # Sort rows
          #Default: effect size
            if (sort.by=='') {
              t.all <- t.all[order(t.all$effect), ]
            }
        
          #Else, by sort.by
            if (sort.by!='') 
            {
              
              #Is the sort.by value unique to each stimulus (e.g., alphabetical order)
                t = table(data[,stimulus],data[,sort.by])
                
                #If there are just as many cells with frequencies >0 as there are stimuli, then it is unique
                  item.unique = FALSE
                  if (sum(t!=0) == length(unique(data[,stimulus]))) item.unique = TRUE
        
        
              #If unique
                if (item.unique==TRUE)
                {
                  #Dataframe with unique values of sort.by for each stimulus
                    sort.by.data <- unique(data[,c(stimulus,sort.by)])

                }
                
              #If not unique it's numeric, so we compute the mean
                if (item.unique==FALSE)
                  {
                  #Compute mean by item
                    sort.by.data <- aggregate(data[, sort.by],list(data[, stimulus]), mean)
                    names(sort.by.data)=c(stimulus,sort.by) 
                } #End if sort.by is not unique to each stimulus
                  #Merge with sort.by
                    t.all = merge(t.all, sort.by.data,by=stimulus)

             
                  
                
        #Sort it
            t.all <- t.all[order(t.all[,sort.by]), ]

        
        } #End if sort.by is not null
                  
              

              
       
                
      return(t.all)
  }
  
 

# ---- from get.null.shuffle.R ----



 get.null.shuffle = function(data,  dv, stimulus, condition, participant,simtot=100,flip.conditions,obs)
  {
    
    #Residualize stimulus effects
      m0 = lm(data[,dv]~factor(data[,stimulus]))
      data$r=residuals(m0)
      
      
    #Compute means on residuals
      message2("Will conduct ",simtot," resamples to estimate expected heterogeneity under null of homogeneity.")
      
      means.all=matrix(nrow=simtot,ncol=length(unique(data[,stimulus])))
      for (k in 1:simtot)
      {
        if (k==1) t1=Sys.time()
        
      #Shuffle item within condition
        data[,paste0(stimulus,"_shuffled")]  <- ave(data[,stimulus], data[,condition], FUN = function(x) sample(x))
      
      #Get means of residualized dv on shuffled stimuli
        tk   = get.means.condition(data=data,dv='r',stimulus=paste0(stimulus,"_shuffled"),condition=condition,sort.by='',flip.conditions = flip.conditions)
            
      #Extract estimates
        means.all[k,] = sort(tk$effect)
        
      #Counter
        if (k==1) {
          t2=Sys.time()
          seconds <- as.numeric(difftime(t2, t1, units = "secs"))
          counter.interval= get.counter.interval(seconds)
          }
        
        if (k%%counter.interval==0) cat('...',k)
    }
        cat("\n")
      
  #Compute the full set of sorted effect sizes
   dM=colMeans(means.all) 
   dL=apply(means.all,2,quantile,.025) 
   dH=apply(means.all,2,quantile,.975) 
  
  #Full resamples saved
   under.null.resamples=data.frame(means.all)
   names(under.null.resamples)   =paste0('stimulus_',1:ncol(under.null.resamples))
   rownames(under.null.resamples)=paste0('resample_',1:nrow(under.null.resamples))
   
  #Compute heterogeneity p-value
    d.obs  = obs$effect
    e2.obs = sum((dM-d.obs)^2)
    
    #Resampled
      e2.rows <- (sweep(under.null.resamples, 2, dM, FUN = "-"))^2
      e2.resamples <- rowSums(e2.rows)
    
    #p-value
      p.hetero = mean(e2.resamples >= e2.obs)
      p.hetero_text = formatted.p(p.hetero)
     if (p.hetero == 0) {
        rounded_value <- 1 / simtot
        
        # Check if rounded_value is less than 0.0001
        if (rounded_value < 0.0001) {
          p.hetero_text <- 'p<.0001'
        } else {
          p.hetero_text <- paste0('p<', format(rounded_value, digits = 1, scientific = FALSE))
        }
      }

      

  #Output
    list(under.null.summary   = data.frame(low=dL, mean=dM, high=dH),
         e2.obs=e2.obs, e2.resamples=e2.resamples,
         p.hetero=p.hetero,
         p.hetero_text=p.hetero_text,
         under.null.resamples = under.null.resamples )

  }
    
 

# ---- from get.null.demean.R ----

#Generates the expected distribution of effects by forcing the null without assuming homoskedsticity
#Only the mean is adjusted to force the null, allowing each stimulus to have its own variance\






 get.null.demean = function(data,  dv, stimulus, condition, participant,simtot=100,flip.conditions,obs)
  {
    #1 Unique conditions and participant ids
      uc=sort(unique(data[,condition]))
      ui=unique(data[,participant])
      nui=length(ui)
    
    #2 Make null true, all means the same
      #Observed means
        means.obs = get.means.condition(data,dv,stimulus,condition,sort.by='',FALSE)

      #Means for condition 1 & 2 for each stimulus
        m1.obs=means.obs[,1]
        m2.obs=means.obs[,2]
        
      #Overall means for conditions 1 & 2
        m1.all=mean(m1.obs)
        m2.all=mean(m2.obs)
      
      #Compute gap between condition mean and overall mean for each stimulus*condition
        gap1=m1.obs-m1.all
        gap2=m2.obs-m2.all
      
      #Turn gaps to dataframe that is merged with the data to have the gaps in the full data
        df.gaps = data.frame(means.obs[,stimulus],gap1,gap2)
        names(df.gaps)[1]=stimulus
        data=merge(data, df.gaps, by=stimulus)
        
      #Generate the null
        data[,'dv.null'] = ifelse(data[,condition]==uc[1], data[,dv]-data[,'gap1'], data[,dv]-data[,'gap2'])
        
      #Sampling error by randomly drawing participants
        means.all=matrix(nrow=simtot,ncol=length(unique(data[,stimulus])))
        for (k in 1:simtot)
       {
         
        #Generate data
            ids=sample(ui,replace=TRUE)
            length(unique(ids))
            
            data.null.list=list()
            j=1
            for (idk in ids)
            {
              data.null.list[[j]]=data[data[,participant]==idk,]
              j=j+1
            }
            data.boot <- do.call(rbind, data.null.list)
  
        #Get null.means
          mk   = get.means.condition(data=data.boot,dv='dv.null',stimulus=stimulus,condition=condition,sort.by='',flip.conditions = flip.conditions)
              
        #Extract estimates
          means.all[k,] = sort(mk$effect)
          
        #Counter
          if (k%%50==0) cat('...',k)
        }
      
      
      
  #Compute the full set of sorted effect sizes
   dM=colMeans(means.all) 
   dL=apply(means.all,2,quantile,.025) 
   dH=apply(means.all,2,quantile,.975) 
  
  #Full resamples saved
   under.null.resamples=data.frame(means.all)
   names(under.null.resamples)   =paste0('stimulus_',1:ncol(under.null.resamples))
   rownames(under.null.resamples)=paste0('resample_',1:nrow(under.null.resamples))
   
  #Compute heterogeneity p-value
    d.obs  = means.obs$effect
    e2.obs = sum((dM-d.obs)^2)
    
  #Resampled
      e2.rows <- (sweep(under.null.resamples, 2, dM, FUN = "-"))^2
      e2.resamples <- rowSums(e2.rows)
    
  #p-value
      p.hetero = mean(e2.resamples >= e2.obs)
      p.hetero_text = formatted.p(p.hetero)
      if (p.hetero==0) p.hetero_text = paste0('p<',1/simtot)
      

  #Output
    list(under.null.summary   = data.frame(low=dL, mean=dM, high=dH),
         e2.obs=e2.obs, e2.resamples=e2.resamples,
         p.hetero=p.hetero,
         p.hetero_text=p.hetero_text,
         under.null.resamples = under.null.resamples )

  }
    
 

# ---- from get.model.results.R ----

  #1 Make variable structure compatible with lmer which cannot do df[,dv]
  #2 Shared parameters
  #3 Regression
  #4 Stimulus intercepts
  #5 Stimulus slopes
  #6 Results



 get.model.results=function(df, dataname, dv, stimulus, condition, participant,model,flip.conditions)
{
#-------------------------------------------------------
  
           
  
          
  #1 Make variable structure compatible with lmer which cannot do df[,dv]
       df2=df
       df2$dv=df[,dv]
       df2$stimulus   = df[,stimulus]
       df2$condition  = df[,condition]
       if (participant !='') df2$participant  = df[,participant]

       ucond=sort(unique(df2$condition))
       if (flip.conditions==TRUE)    df2$condition=factor(df2$condition, levels=ucond)
       if (flip.conditions==FALSE)  df2$condition=factor(df2$condition, levels=rev(ucond))
      

#-------------------------------------------------------

  #2 Shared parameters
      crossed = FALSE
      if (participant!='')
          {  
            t=table(df2$participant,df2$condition)
            crossed <- sum(t[,1]*t[,2]>0)>0
      }
       
      #Stimulus
          stimulus.all=unique(df[,stimulus])
 
#-------------------------------------------------------

  #3 Regression
     if (any(c('all', 'regression') %in% model)) {
          message2("stimulus.plot() says:")
               
      #Fixed effects for participant only if they get different conditions
         #Set up teh regression
            if (crossed==FALSE)  m1.text = "m1=lm(dv~condition+factor(stimulus),data=df2)"
            if (crossed==TRUE)   m1.text = "m1=lm(dv~condition+factor(stimulus)+factor(participant),data=df2)"
            
            
          #Show feedback on screen
            m1.text.formatted= eval.arguments (m1.text, dv, condition, stimulus, participant, dataname)
            message(" ")
            message2("Estimating regression:\n    ",m1.text.formatted)
            
        #Evaluate the regression
            eval2(m1.text)

        #Get mean effect for condition and its  ci
            m1.mean = summary(m1)$coefficients[2,1]
            se =summary(m1)$coefficients[2,2]
            deg.free = m1$df.residual
            tc = qt(.975,df=deg.free)
            m1.p    = summary(m1)$coefficients[2,4]
            m1.ci = c(m1.mean - tc*se, m1.mean+tc*se)

       
        #Cluster by participant if needed
            if (participant!='') {
              m1.cluster.text = "lmtest::coeftest(m1,vcov=sandwich::vcovCL,type='HC3',cluster=~participant)"
          
             #Show feedback on screen
                m1.cluster.text.formatted = eval.arguments(m1.cluster.text, dv, condition, stimulus, participant, dataname)
                message(" ")
                message2("  Clustering the standard errors:\n    ",m1.cluster.text.formatted)
           
             #Estimate SE
                m1.cluster = try(eval2(m1.cluster.text))
                
                  if (class(m1.cluster) %in% 'try-error')
                    {
                    #Change to hc1
                      m1.cluster.text = "lmtest::coeftest(m1,vcov=sandwich::vcovCL,type='HC1',cluster=~participant)"

                    #Show feedback on scren
                      message2("Couldn't estimate HC3, will try HC1")
                      m1.cluster.text.formatted = eval.arguments(m1.cluster.text, dv, condition, stimulus, participant, dataname)
                      message(" ")
                      message2("  Clustering the standard errors:\n    ",m1.cluster.text.formatted)
                      
                    #Estimate new one
                      m1.cluster = eval2(m1.cluster.text)

                    }
                
                se = m1.cluster[2,2]
                m1.ci = c(m1.mean - tc*se, m1.mean+tc*se)
                m1.p = m1.cluster[2,4]
              
            } #End if clustering
            
        #Save 
            m.mean = m1.mean
            m.ci   = m1.ci  
            m.labels='Regression'
            m.p = m1.p
       } #End if regression 
    
   
#-------------------------------------------------------
     
  #4 Stimulus intercepts
     if (any(c('intercepts','all') %in% model)) 
     {


          #Run random model
           #Set up text
            if (participant!='') m2.text = "lmerTest::lmer(dv~condition+(1|stimulus)+(1|participant),data=df2)"
            if (participant=='') m2.text = "lmerTest::lmer(dv~condition+(1|stimulus),data=df2)"
            
          #Show feedback on screen
            m2.text.formatted= eval.arguments (m2.text, dv, condition, stimulus, participant, dataname)
            message(" ")
            message2("Estimating random intercepts model:\n    ",m2.text.formatted)
            m2=eval2(m2.text)

          #Get mean effect for condition and its  ci
            m2.mean = summary(m2)$coefficients[2,1]
            m2.ci  =  get.ci(m2)  #see utils.r function #6
            m2.p =  summary(m2)$coefficients[2,5]
            
          #Add
            lab='Random Intercepts'
            m.mean <- if (exists("m.mean")) c(m.mean, m2.mean)   else m2.mean
            m.ci   <- if (exists("m.ci"))   c(m.ci  , m2.ci)     else m2.ci
            m.labels <- if (exists("m.labels")) c(m.labels,lab)  else lab
            m.p       <- if (exists("m.p"))     c(m.p ,m2.p) else m2.p

              
    } #End if intercepts

#-------------------------------------------------------
      
#5 Stimulus slopes
     if (any(c('slopes','all') %in% model)) {

          #Run random model
            if (participant!='') m3.text ="lmerTest::lmer(dv~condition+(1+condition|stimulus)+(1|participant),data=df2)"
            if (participant=='') m3.text = "lmerTest::lmer(dv~condition+(1+condition|stimulus),data=df2)"
            message(" ")
            m3.text.formatted= eval.arguments (m3.text, dv, condition, stimulus, participant, dataname)
            message2("Estimating random slopes model:\n    ",m3.text.formatted)
            m3=eval2(m3.text)

          #Get mean effect for condition and its  ci
            m3.mean = summary(m3)$coefficients[2,1]
            m3.ci  =  get.ci(m3)  #see utils.r function #6
            m3.p =  summary(m3)$coefficients[2,5]

           #Add
            lab='Random Slopes'
            m.mean   <- if (exists("m.mean"))   c(m.mean, m3.mean) else m3.mean
            m.ci     <- if (exists("m.ci"))     c(m.ci  , m3.ci)   else m3.ci
            m.labels <- if (exists("m.labels")) c(m.labels,lab)    else lab
            m.p      <- if (exists("m.p"))     c(m.p , m3.p)      else m3.p

      } #End if slopes   
      
#-------------------------------------------------------
      
#6 Results

    #Summary results for plotting
      results=namedList(m.mean, m.ci, m.labels, m.p)
      
    #Full models
      if (exists('m1'))         results$regression = m1
      if (exists('m1.cluster')) results$regression.clustered_errors = m1.cluster
      if (exists('m2'))         results$random_intercepts = m2
      if (exists('m3'))         results$random_slopes = m3
  
       return(results)
}
      

# ---- from get.maxmin.confidence.R ----



#Does resampling under null of all stimuli having the same distribution, 
#see  stimulus.beeswarm.R 

    get.maxmin.confidence = function(data,  dv, stimulus, condition, simtot=500,confidence=95,ms1,ms2,dc1,dc2)
    {
        
        #how many stimuli
          n1=nrow(ms1)
          n2=nrow(ms2)
          
        #Empty matrices
          ms1.boot=matrix(nrow=simtot,ncol=n1)
          ms2.boot=matrix(nrow=simtot,ncol=n2)
        
          
        #Bootstrap itself under null of equal distributions
          datak=data
          message2("Will run ",simtot," resamples to compute confidence band")
          for (bk in 1:simtot)
          {
          #Shuffle the stimulus
            datak[,stimulus]=unsplit(lapply(split(datak[,stimulus], data[,condition]), sample), datak[,condition])

            ms1.boot[bk,]=sort(aggregate(datak[dc1,dv],list(datak[dc1,stimulus]),mean)$x)
            ms2.boot[bk,]=sort(aggregate(datak[dc2,dv],list(datak[dc2,stimulus]),mean)$x)
            
            if (bk %% 100==0) cat("...",bk)
          }
        
        #Set quantiles for confidence level required
            qL = ((100-confidence)/2)/100
            qH = 1-qL
            
        #Compute quantiles 
            b1L = quantile(ms1.boot[,1],qL)
            b1H = quantile(ms1.boot[,n1],qH)
            b2L = quantile(ms2.boot[,1],qL)
            b2H = quantile(ms2.boot[,n2],qH)
          
        #Output
            output=namedList(b1L,b1H,b2L,b2H)
            return(output)
      
    }

# ---- from stimulus.plot.means.R ----

 stimulus.plot.means = function(data, dv, condition, stimulus, 
                    participant, sort.by,
                    flip.conditions,
                    ylab1, ylab2,  xlab1, xlab2, 
                    decimals, 
                    dv.is.percentage,
                    legend.title, col1,col2,ylim,main,...)
    
      {

    #Grab the arguments passed on to ...  
     args = list(...)
      
     
    #Hard code the colors
      col1='black'
      col2='red4'
      
    
      
    #0 Is it a matched design?
          t = table(data[,stimulus],data[,condition])
          matched = FALSE
          if (mean(t[,1]*t[,2]>0) >.5 ) matched=TRUE
          
              #This computes the frequency of stimuli id by condition
              #if it is compared, a given ID appears only in one condition
              #if it is matched, it appears in both
              #this classifies a design as matched if 50%+
              #of stimuli appear in both conditions
             

    #1 Get the means by condition
          means.obs = get.means.condition(data=data,dv=dv,stimulus=stimulus,flip.conditions=flip.conditions, condition=condition,sort.by=sort.by)

   #2 local names
      n=nrow(means.obs)/2

      
      #Condition
       ucond=sort(unique(data[,condition]))
       cond1= paste0(condition,"_",ucond[1])
       cond2= paste0(condition,"_",ucond[2])
      
      
        y1 = means.obs[,cond1] #Condition 1
        y2 = means.obs[,cond2] #Condition 2
        label1  =  sub(paste0(condition,"_"), "", cond1)
        label2  =  sub(paste0(condition,"_"), "", cond2)
      
      
      #Which value is higher for each stimulus
        bh=pmax(y1 , y2)
        bl=pmin(y1 , y2)  
      


      #Overall means
        m1 = mean(y1)
        m2 = mean(y2)

      
    #3 ylim: range of y values in the plot
      if (length(ylim)<2)
      {
        ylim = range(c(y1,y2))
        dy = diff(ylim)
        ylim[2]=ylim[2]+.4*dy  #Give a 25% buffer on top (for the legend)
        ylim[1]=ylim[1]-.03*dy  #give a 3% buffer below, for the value labels
      }
    #4 Margins
      #Get current margins
        mar.before =  par("mar")
        mar.after  =  mar.before
         

      #Only change margin if not the default (so users can set own in)
        custom_mar <- getOption("graphics.par")$mar   #see if user has set different margins by default
        if (is.null(custom_mar))  mar.default = c(5.1, 4.1, 4.1, 2.1)
        if (!is.null(custom_mar)) mar.default = custom_mar
        
        
        max.x.label = max(nchar(unique(data[,stimulus])))
        xlabel.buffer = max(0,max.x.label)*.3
         
        if (all(mar.before==mar.default))
        {
        #4.1 Bottom
             mar.after[1] = mar.before[1] + xlabel.buffer
        
        #4.2 Top
          #Drop top margin if there is no main header
            mar.after[3] = ifelse (main=='',1,2)
        
        #4.3 Left
           width.y.label = nchar(max(pretty(y1)))
           mar.after[2] = max(width.y.label/3.5, 4)
           if (ylab2!='') mar.after[2]= mar.after[2] + 1
           if (dv.is.percentage==TRUE) {
             
             mar.after[2] = mar.after[2] + 1
             
           }
          
        #4.4 Assign it
           par(mar=mar.after)
           
        } 
           
  #5 black dots
       n=length(y1)
       if (dv.is.percentage==FALSE) plot(y1,pch=16,ylim=ylim,          xaxt='n',xlab='',las=1,ylab='',xlim=c(.5-.015*n,n+4 + n*.0125),cex=1.5, xaxs='i',...)
       if (dv.is.percentage==TRUE)  plot(y1,pch=16,ylim=ylim, yaxt='n',xaxt='n',xlab='',las=1,ylab='',xlim=c(.5-.015*n,n+4 + n*.0125),cex=1.5, xaxs='i',...)
       
  #6 Segments
    e=mean(means.obs$effect)    
    lty=ifelse( (y1 - y2)*(m1-m2)>0 ,1,2)
    col12=ifelse( (y1 - y2)*(m1-m2)>0 ,col1, col2)
    segments(x0=1:n, x1=1:n,y0=y1, y1=y2,lty=lty,col=col12)
    
  #7 White dots   
      points(y2,pch=21,col='black',bg='white',cex=1.5)
 
  #8 Redo black dots to cover any red lines
    points(y1,pch=16,cex=1.5)
    
  #9 Value labels
    
 
    #Color for text
      col.h = ifelse(bh==y1,adjustcolor(col1,.5),adjustcolor(col2,.91))
      col.l = ifelse(bl==y2,adjustcolor(col2,.5),adjustcolor(col1,.91))
      col.h=col.l='black'

     
    #Labels themselves
      if (dv.is.percentage==FALSE)
      {
      text(1:n,bh,sp_round2(bh,auto.decimals(bh)),col=col.h,cex=.75,pos=3)
      text(1:n,bl,sp_round2(bl,auto.decimals(bl)),col=col.l,cex=.75,pos=1)
      }
      
      if (dv.is.percentage==TRUE)
      {
      text(1:n,bh,format_percent(bh,decimals),col=col.h,cex=.75,pos=3)
      text(1:n,bl,format_percent(bl,decimals),col=col.l,cex=.75,pos=1)
      }
   
  #10 Overall means
    segments(x0=n+3, x1=n+3,y0=m1, y1=m2)
    
    points(n+3,m1,pch=16,cex=1.5*1.5) 
    points(n+3,m2,pch=21,cex=1.5*1.5,col='black',bg='white')
    
    axis(side=1,at=n+3,"MEAN",font=2)
    
    
    d.pos=c(3,1)
    if (m1<m2) d.pos=rev(d.pos)
    if (dv.is.percentage==FALSE)
      {
      text(n+3,m1 , sp_round2(m1,auto.decimals(m1)),cex=.8,col='black',pos=d.pos[1])
      text(n+3,m2 , sp_round2(m2,auto.decimals(m1)),cex=.8,col='black',pos=d.pos[2])
      }
    
    
      if (dv.is.percentage==TRUE)
      {
      text(n+3,m1 , format_percent(m1,decimals),cex=.8,col='black',pos=d.pos[1])
      text(n+3,m2 , format_percent(m2,decimals),cex=.8,col='black',pos=d.pos[2])
      }


  #11 Y axis
      if (ylab1=='') ylab1=dv
        
    
      if (!"yaxt" %in% names(args))
      {
      mtext(side=2,line=mar.after[2]-1.5,font=2,cex=1.2,ylab1)
      mtext(side=2,line=mar.after[2]-2.5,font=3,cex=1,ylab2)
      }

    
      if (dv.is.percentage==TRUE)
      {
          ys=pretty(c(y1,y2))
          if (dv.is.percentage==TRUE) axis(side=2,at=ys,paste0(ys*100,"%"),las=1)
      }
    
  #12 X-axis
    
    #12.1 Stimuli labels
        text(1:n,par('usr')[3] , paste0(means.obs[,stimulus],"  "),srt=80,xpd=TRUE,adj=1,col=col12)
       
    #12.2 Headers
        if (xlab2=="" & sort.by=='') xlab2='(sorted by effect size)'
        if (xlab2=="" & sort.by!='') xlab2=paste0('(sorted by ',sort.by,')')

            #For matched stimuli, the default is the above text
        
        mtext(side=1,line=2.7 + xlabel.buffer , font=2,cex=1.2,xlab1)
        mtext(side=1,line=3.7 + xlabel.buffer   ,font=3,cex=1,xlab2)

        
  #13 Legend
        labels=c(label1,label2)
        if (flip.conditions) labels=rev(labels)
        #leg1 = legend('topleft',pch=c(16,1), labels ,inset=.05,bty='n',cex=1.3, y.intersp = 1.5)
        
        #Legend title?
        if (legend.title=='') leg1 = legend('topleft',pch=c(16,1), labels ,inset=.02,bty='n',cex=1.3, y.intersp = 1)
        if (legend.title!='') leg1 = legend('topleft',pch=c(16,1), labels ,inset=.02,bty='n',cex=1.3, title.cex = 1.3,  x.intersp = 0.5, y.intersp = 1,title=legend.title,title.font=2, text.width = strwidth("W"))
        
  #14 Main header
    if (main!=''){
      mtext(side=3,line=.5, font=2,cex=1.5,main)
    }
      
        
        
  #14 Return margins to where they were
    par(mar=mar.before)
    
    return(means.obs)
  
  }
  

# ---- from stimulus.plot.effects.R ----

 stimulus.plot.effects=function(data,  dv, stimulus, condition, participant ,
                                dataname,
                                overall.estimate,
                                overall.ci,
                                overall.label, 
                                overall.p,
                                model,
                                sort.by, 
                                flip.conditions, 
                                null.method='shuffle',
                                decimals, 
                                dv.is.percentage,
                                ylab1,ylab2,xlab1,xlab2,
                                simtot,
                                ylim,
                                seed, 
                                main,
                                ...)
    {
    

         
         
  #------------------------------------
    #0 colors
      col.null1   = 'dodgerblue'
      col.null2   =  adjustcolor(col.null1,.1)
      col.ci      = 'gray50'
      col.overall = 'purple'
    
    #1 Grab the arguments passed on to ...  
      args = list(...)
      args_to_drop=c(decimals, dv.is.percentage,ylab1,ylab2,xlab1,xlab2)
      args <- args[!(names(args) %in% args_to_drop)]

    #2 Compute means by stimulus
       obs = get.means.condition(data=data,dv=dv,stimulus=stimulus,condition=condition,sort.by=sort.by,flip.conditions=flip.conditions)

      #Localize stimulus variables
        d = obs$effect
        ciL = obs$ciL
        ciH = obs$ciH
        n = length(d)
        label2  =  sub(paste0("^",condition,"_"), "", names(obs[3]))
        label1  =  sub(paste0("^",condition,"_"), "", names(obs[2]))
       
        if (flip.conditions)
        {
          l1 = label1
          l2 = label2
          label1 = l2
          label2 = l1
          }
        
    #3 Get the null distribution  (only if sort.by is not set)
          d0=rep(d,length(unique(data[,stimulus])))   #make it equal to d just to help with code below, e.g., ylim=range(...)
          dnull=data.frame(low=d0,high=d0,mean=d0)
        
        #Resampling if sort.by is not specified
            if (sort.by=='') 
                  {
              
                  #get new call's md5
                     #Arguments
                        mc <- match.call(expand.dots = FALSE)
                        args <- as.list(mc)[-1]
                        args <- lapply(args, eval, envir = parent.frame())
                        md5.args = get.md5(args)
                    
                     #dataframe
                      md5.data   = get.md5(data)  
                      
                    #Combine for single md5 
                      md5s=paste0(md5.args ,  md5.data)

                    
                  #if dnull for that md5s has been saved, load it
                    if (does.cache.d.exist(md5s)) 
                    {
                      list_resamples = sp_stimulus_cache()[[md5s]]
                      
                      #If it was not called by itself when re-runnig for showing on the screen
                      
                        if (sys.parent() < 4) {
                              #NOTE: we don't want to show this message when the user sets save.as='fig1.svg'
                              #and we run the function a second time, so here we check whether the call is nested 
                              #which occurs if is already stack 4, meaning the call has already gone through 4 opeartions
                            
                                message2("*Recycled results*:\n",
                               "You had run this same analysis before with all the same variables and options.\n",
                               "We are re-using stored results from the previous call.\n",
                               "To force new calculations clear your cache running: 'clear_stimulus_cache()'")
                                }
                      
                  #else run it                  
                    } else  {
                      if (null.method=='shuffle')
                        {
                        set.seed(seed)
                        list_resamples = get.null.shuffle (data=data, dv=dv, stimulus=stimulus, condition=condition, participant=participant,simtot=simtot,flip.conditions=flip.conditions,obs=obs)
                        }
                        
                      if (null.method=='demean')
                        {
                        set.seed(seed)
                        list_resamples = get.null.demean (data=data, dv=dv, stimulus=stimulus, condition=condition, participant=participant,simtot=simtot,flip.conditions=flip.conditions,obs=obs)
                        }
                   
                  #Save
                      cache <- sp_stimulus_cache()
                      cache[[md5s]] <- list_resamples
                      .statuser_state$stimulus_cache <- cache
                    
                }

                  
                      
            } #End if sort.by!=''
          
          
        #Extract the dnull object with summary
          dnull =  list_resamples$under.null.summary

        #Extract p-hetero
          p.hetero_text = list_resamples$p.hetero_text
          
          
    #4 ylim: range of y values in the plot
      
      if (length(ylim)<2)
      {
      ylim = range(c(ciL,ciH,dnull))
      dy = diff(ylim)
      
      ylim[2]=ylim[2]+.35*dy  #Give a 28% buffer on top (for the legend)
      ylim[1]=ylim[1]-.03*dy  #give a 3% buffer below, for the value labels
      }
          
    #5 get models if specified
       if (length(model)>0)
       {
        model.results = get.model.results(data, dataname, dv, stimulus, condition, participant,model,flip.conditions)

        overall.estimate  = model.results$m.mean
        overall.ci        = model.results$m.ci
        if (length(overall.label)==0) overall.label= model.results$m.labels
        overall.p         = model.results$m.p
  
       }

    #5 xlim 
      n1 = length(overall.estimate)
      xmax = ifelse(n1 > 0, length(d) + n1 +2, length(d)+1)
      xlim = c(0,xmax)
      
 
    #6 Margins
      #Get current margins
        mar.before =  par("mar")
        mar.after  =  mar.before
         
      #Only change margin if not the default (so users can set own)
        custom_mar <- getOption("graphics.par")$mar   #see if user has set different margins by default
        if (is.null(custom_mar))  mar.default = c(5.1, 4.1, 2, 2.1)
        if (!is.null(custom_mar)) mar.default = custom_mar
        
        
          #Get current margins
            mar.before =  par("mar")
            mar.after  =  mar.before
            

          #Label calculations for bottom margin 
            max.length = max(nchar(unique(data[,stimulus])))
            xlabel.buffer = max(0,max.length-3)*.3

          #Bottom
                max.x.label = max(nchar(unique(data[,stimulus])))
                xlabel.buffer = max(0,max.x.label)*.3
                mar.after[1] = mar.before[1] + xlabel.buffer
          
          #Top
            #Drop top margin if there is no main header
              mar.after[3] = ifelse (main=="",1,2)

          #Left
               width.y.label = nchar(max(d))
               mar.after[2] = max(width.y.label/3, 5.1)
               if (ylab2!='') mar.after[2]= mar.after[2] + 1
              
          #Implement
             par(mar=mar.after)
        
    
  #6 Black dots
     if (dv.is.percentage==FALSE)  {
        plot(d,         pch=16,ylim=ylim,xaxt='n',xlab='',las=1,ylab='', cex=1.5, xlim=xlim, xaxs='i', ...)
     }
        
      
     if (dv.is.percentage==TRUE )  {
       plot(d,yaxt='n', pch=16,ylim=ylim,xaxt='n',xlab='',las=1,ylab='', cex=1.5, xlim=xlim, xaxs='i', ...)
      }


      #horizontal line
        abline(h=0,lty=3,col='gray66')
    
   
  #7 Plot the null and its CI
    if (sort.by=='')
    {
    points(dnull$mean,type='l',col=col.null1,lty=2)
    polygon(x=c(1:n,n:1),y=c(dnull$low , rev(dnull$high)),col=col.null2,border=NA)
    }
  
  #8 Value labels

    #set position   
      y.text=d 
      
   #print them
    
      #How many decimals to show?
        if (decimals=='auto') d.decimals=auto.decimals(d)
        if (decimals!='auto') d.decimals=decimals
      
      if (dv.is.percentage==FALSE) text(1:n,d ,sp_round2(d , d.decimals),col='blue4',cex=.65,pos=4)
      if (dv.is.percentage==TRUE)  text(1:n,d ,format_percent(d,decimals), col='blue4',cex=.65,pos=4) #utils.R #9 <-- 'format_percent()'

     
  #7 CI
      arrows(x0=1:n, x1=1:n, y0=ciL,y1=ciH,col=adjustcolor('gray60',.8),code=3,angle=90,length=.02)

      
  #9 Y axis
      if (ylab1=='') ylab1=paste0("Effect on '",dv,"'")

      if (!"yaxt" %in% names(args))
      {
      mtext(side=2,line=mar.after[2]-1.5,font=2,cex=1.2,ylab1)
      mtext(side=2,line=mar.after[2]-2.5,font=3,cex=1,ylab2)
      
      #Y labels for percentages
        ys=pretty(c(ciL,ciH))
        if (dv.is.percentage==TRUE) axis(side=2,at=ys,paste0(ys*100,"%"),las=1)
      }

  
      
    #10 x-axis
      #Skip if xaxt='n' is set
      if (!'xaxt' %in% args)
      {
    
      #10.1 Stimuli labels
        text(1:n,par('usr')[3] , paste0(obs[,stimulus],"  "),srt=80,xpd=TRUE,adj=1)
        

    #14.2 Headers
        if (xlab2=="" & sort.by=='') xlab2='(sorted by effect size)'
        if (xlab2=="" & sort.by!='') xlab2=paste0('(sorted by ',sort.by,')')
        mtext(side=1,line=2.7 + xlabel.buffer , font=2,cex=1.2,xlab1, at=n/2)
        mtext(side=1,line=3.7 + xlabel.buffer   ,font=3,cex=1,xlab2, at=n/2)
      }
      
        
  #15 Legend
        if (sort.by=="")
        {
        
        #Band & p-value
           
        if (null.method=='shuffle') null_text =  'same distribution for all stimuli'
        if (null.method=='demean')  null_text  = 'same means for all stimuli'
        
        leg1 = legend('topleft',
                      bty='n',
                      pch=c(16,NA,NA,NA,NA), 
                      lty=c(NA,1,2,1,NA),
                      lwd=c(NA,1,1,14,NA),
                      col=c('black', col.ci, col.null1 , col.null2,NA),
                      c(paste0("Observed effect: '",label1,"' - '",label2,"'"),
                        "95% CI for observed effect (t-test for this stimulus)",
                        paste0("Expected under null of ",null_text),
                        "95% confidence band under null",
                        paste0("Heterogeneity test: ",p.hetero_text," (N=",simtot, " resamples)")),
                        inset=.03)
        } else {
            leg1 = legend('topleft',
                      bty='n',
                      pch=c(16,NA), 
                      lty=c(NA,1),
                      lwd=c(NA,1),
                      col=c('black',col.ci),
                      c(paste0('Observed effect: ',label1," - ",label2),
                        "95% CI for observed effect"),
                        inset=.03)
          
        }
          
          
  #16 Overall
      if (n1 > 0)
      {
        
        xs=(n+1):(n+n1)+1
        
      
      
      #Markers
        points( x=xs,
                y= overall.estimate,
                pch=16,
                cex=1.5*1.5,
                col=col.overall)
          
      #CI
        arrows(x0=xs,x1=xs, 
               y0=overall.ci[seq(1,n1*2,2)], y1=overall.ci[seq(2,n1*2,2)],
               col=col.overall, code=3, length=.03, angle=90)
        
      #Labels
         overall.label =  gsub("\\n", " \n", overall.label)
         text(xs,par('usr')[3] , paste0(overall.label," ") ,srt=80,xpd=TRUE,adj=1,col=col.overall)


      #p-value
         text(xs,max(overall.ci),pos=3,formatted.p(overall.p),col=col.overall,cex=.7,font=2)

     #"Overall" 
         y.overall=par('usr')[4]- 0.18* (par('usr')[4] - par('usr')[3])
         text(mean(xs),y.overall,pos=3,"Overall",cex=1.2,font=2)

     #Vertical separator
           abline(v= n+1 ,lwd=2) 

     #Overall value label
            if (dv.is.percentage==FALSE)  text(xs,overall.estimate , round(overall.estimate, auto.decimals(overall.estimate)),    cex=.65,col='purple',pos=4)
            if (dv.is.percentage==TRUE)   text(xs,overall.estimate , format_percent(overall.estimate, decimals),                  cex=.65,col='purple',pos=4)      
      }
      
      
      
  #Redo dots
    points(d, pch=16,cex=1.5)
      
  #Main header
    if (main!=''){
      mtext(side=3,line=.5, font=2,cex=1.5,main)
    }
      
    par(mar=mar.before)
    
    
    
    
    
  #Results
    results = list(observed=obs, p.hetero=p.hetero_text, under.null=dnull,resamples = list_resamples$under.null.resamples)
    if (exists('model.results')) results$model.results= model.results
    return(results)     
    
        
    }#End of function
  
 
