## The following code is based on the `debounce()` function.
# Note that 'r' has to be of type closure.
debounce_if <- function (r, test, millis_TRUE, millis_FALSE = 0, priority = 100, domain = getDefaultReactiveDomain()) 
{
  print("debounce_if_called")
  '%OR%' <- function (x, y) {
    if (is.null(x) || isTRUE(is.na(x))) 
      y
    else x
  }
  
  # Force the evalutation of the input parameters,
  # (Otherwise they are carried on as `r`, `millis_TRUE`, 'millis_FALSE` and `test` instead of their actual values.)
  force(r)
  force(millis_TRUE)
  force(millis_FALSE)
  force(test)
  
  # Get the name of `test` as from the caller's perspective as a string.
  # (The formal parameter `test` bound by `environment()` will be substituted by the actual parameter.)
  testVarName <- deparse(substitute(test, environment()))
  
  # Make sure that `millis_TRUE` is of type function.
  if (!is.function(millis_TRUE)) {
    origMillis1 <- millis_TRUE
    millis_TRUE <- function() origMillis1
  }
  
  # Make sure that `millis_FALSE` is of type function.
  if (!is.function(millis_FALSE)) {
    origMillis2 <- millis_FALSE
    millis_FALSE <- function() origMillis2
  }
  
  # `v$when` defines the point in time when the invalidation should occur (i.e. when `v$trigger` changes).
  v <- reactiveValues(trigger = NULL, when = NULL)
  
  firstRun <- TRUE
  print("debounce_if_preface")
  
  
  # Re-execute when the reactive `r` becomes invalid.
  observe({
    #print(eval(substitute(test, parent.env(environment()))))
    #print(force(test))
    # Make sure to get the latest value set for the actual parameter for `test`.
    # This can be consideres as an update of `test` on an invalidation of the reactive `r`.
    assign("test", get(testVarName), envir = parent.env(environment()))
    print(test)
    print(update_by_user)
    r()
    # Do nothing if the function is called the first time.
    if (firstRun) {
      firstRun <<- FALSE
      return()
    }
    # For each subseqeunt call update `v$when`, i.e. the time when to invalidate `r`.
    #################################################################################
    #### CRUCIAL EDIT RELATIVE TO ORIGINAL DEBOUNCE FUNCTION STARTS HERE ############
    if (test) {
      #print("TRUE")
      #print(force(millis_TRUE()))
      v$when <- Sys.time() + millis_TRUE()/1000
    }
    else {
      #print("FALSE")
      #print(force(millis_FALSE()))
      v$when <- Sys.time() + millis_FALSE()/1000
    }
    
    #### CRUCIAL EDIT RELATIVE TO ORIGINAL DEBOUNCE FUNCTION ENDS HERE ##############
    #################################################################################
  }, label = "debounce tracker", domain = domain, priority = priority)
  
  # Re-execute when `v$when` changes.
  observe({
    # Do nothing if the function is called the first time or `r` has been invalidated.
    if (is.null(v$when)) 
      return()
    
    now <- Sys.time()
    
    # If the time at which to invalidate `r` has passed update `v$trigger` in order to trigger the invalidation.
    if (now >= v$when) {
      # Use `isolate()` here to avoid re-execution upon changes of `v$trigger`.
      # (Otherwise the whole observe would also dependent on `v$trigger`.)
      v$trigger <- isolate(v$trigger %OR% 0)%%999999999 + 1
      v$when <- NULL
    }
    # Else, schedule the whole scope for later invalidation (and re-execution). 
    else {
      invalidateLater((v$when - now) * 1000)
    }
  }, label = "debounce timer", domain = domain, priority = priority)
  
  # Create the reactive `er` when `v$trigger` changes.
  er <- eventReactive(v$trigger, {
    # print(address(update_by_user))# Change the value of the global `test` variable. The `parent.env(environment())))` part is crucial.
    # print(address(test))
    # print(substitute(test, env = parent.env(environment())))
    # print(names(as.list(parent.env(environment()))))
    # print(names(as.list(parent.env(parent.env(environment())))))
    assign(testVarName, TRUE, envir = .GlobalEnv)
    # print(get(testVarName))
    assign("test", get(testVarName), envir = parent.env(environment()))
    # print(names(as.list(parent.env(environment()))))
    # print(names(as.list(parent.env(parent.env(environment())))))
    # print(update_by_user)
    # print(test)
    # print(ls())
    # print(parent.frame())
    # print(parent.frame(n=2))
    # library(pryr)
    # print(where("test"))
    # print(ls(envir = where("test")))
    # print(names(sapply(ls(), function(x) is.environment(get(x)))))
    # #print("Changed")
    r()
  }, label = "debounce result", ignoreNULL = FALSE, domain = domain)
  
  # This part is executed only once duringthe initial call. NECCESARY?
  primer <- observe({
    # Stop this observer from being executed ever again, once a change to `er` has been made.
    primer$destroy()
    er()
  }, label = "debounce primer", domain = domain, priority = priority)

  #assign(deparse(substitute(test)), TRUE, envir = .GlobalEnv)
  # Return value
  er
}