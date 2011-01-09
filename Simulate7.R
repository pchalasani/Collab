    # s = number of sessions for entire study
    # p = total number of people for entire study (can be set arbitarily high)
    # p1 = STARTING SESSION POPULATION 
    # m = maximum number of people per session
    # name = name of DATA SET
    # numses = number of sessions in series
    # mult = multiplier of numses to get how many sessions person can attend before being booted
    # numdrop = number of sessions you can miss before being booted
    # w = mean of waiting list additions per week (Poisson distributed)
    # classes: Note: These are cumulative
    # pdrop = proportion droppers
    # ptitrate = proportion titators
    # pcompleter = proportion completers
    # pother = proportion others - for us to play with
    # noise will be all others
    # likelihoods: drop = likelihood of droppers missing if made last session
    #              titrate = likelihood of titrators missing if made last session
    #              complete = likelihood of completers missing if made last session
    #              other = likelihood of others missing if made last session
    #              dropskip = likelihood of droppers missing if missed last session
    #              titrateskip = likelihood of titrators missing if missed last session
    #              completeskip = likelihood of completers missing if misse last session
    #              otherskip = likelihood of others missing if missed last session*/
    #              dropnew = likelihood of droppers missing if new
    #              titratenew = likelihood of titrators missing if new
    #              completenew = likelihood of completers missing  if misse last session
    #              othernew = likelihood of others missing if new*/
    
    #OUTPUT CODE   A = ATTENDED
    #              W = WAITING
    #              S = SKIPPED
    #              D = DONE 
    
    s <- 42 #SHOULD BE 42
    p <- 30 #SHOULD BE 400
    p1 <- 8 #Should be 8
    m <- 10 #shou;ld be higher
    w <- 2
    #CAPITAL LETTERS are counting versions of the lowercase
    numses <- 12
    mult <- 1
    maxses <- round(mult*numses,0)
    drop = .3; titrate = .2;  complete = .3; noise = .5; other = .1
    dropskip = .3; titrateskip = .2; completeskip = .3; noiseskip = .5; otherskip = .1;
    dropnew = .3; titratenew = .2; completenew = .9; noisenew = .5; othernew = .1;
    name = "Basic";
    pdrop = .1;  ptitrate = .2;  pcomplete =.7; pnoise = .9; pother = 1;
    
    #  THESE HAVE TO BE ASCENDING.  
    w = 10 
    ######################################################################################################################
    
    patients <- matrix(nrow = p, ncol = s, "NA") #all patients attendance
    invited  <- matrix(nrow = p, ncol = s, 0)  #number of sessions A or S
    missinrow <- matrix(nrow = p, ncol = s, 0) #number of sessions missed in a row
    insess <- vector("numeric", s) #number of people in a session
    set.seed(83877201)
    addlist <- rpois(s, w)
    
# Set up waitlist
waitlist <- vector("numeric", s)
waitlist[1] <- addlist[1]
for(i in 2:s)
{
 waitlist[i] <- waitlist[i-1] + addlist[i]
}
for (i in 1:p)
{
    for (j in 1:s)
    {  
    if (i < waitlist[j]) patients[i,j] <- "W"
    }
}
    
    
#Assign all patients to classes
classlist <- cut(runif(p), c(0, pdrop, ptitrate, pnoise, pcomplete, 1), labels = c("D", "T", "C", "N", "O"))
    
    
#First patient
#first session
{
   invited[1,1] <- 1
   insess[1] <- 1
   if (classlist[1] == "D")
   {
      if (runif(1) < dropnew) {patients[1, 1] <- 'A'} else
      {
        patients[1, 1] <- 'S'
        missinrow[1, 1] <- 1
      }
   }
   if (classlist[1] == "T")
   {
        if (runif(1) < titratenew) {patients[1, 1] <- 'A'} else 
        {
          patients[1, 1] <- 'S'
          missinrow[1, 1] <- 1
        }
    }
    if (classlist[1] == "C")
    {
        if (runif(1) < completenew) {patients[1, 1] <- 'A'} else 
        {
        patients[1, 1] <- 'S'
        missinrow[1, 1] <- 1
        }
    }
    if (classlist[1] == "N")
    {
        if (runif(1) < noisenew) {patients[1, 1] <- 'A'} else 
        {
        patients[1, 1] <- 'S'
        missinrow[1, 1] <- 1
        }
    }
    if (classlist[1] == "O")
    {
    if (runif(1) < othernew) {patients[1, 1] <- 'A'} else 
        {
        patients[1, 1] <- 'S'
        missinrow[1, 1] <- 1
        }
    }
}
#Later sessions
for (j in 2 : s)
{
    if (patients[1,(j-1)] == 'A'|patients[1,(j-1)] == 'S')
    {
       invited[1,j] <- invited[1,(j-1)] + 1
    } else
    {
       invited[1,j] <- invited[1,(j-1)]
    }
       if (invited[1,j] <= maxses & missinrow[1,j] < m)
    {
    # Skip or attend
    # If attended previous session    
        if (patients[1, (j-1)] == 'A')
        {
            if (classlist[1] == "D")
            {if (runif(1) < drop) {patients[1, j] <- 'A'} else
                {
                   patients[1, j] <- 'S'
                   missinrow[1, j] <- 1
                }
            } 
            if (classlist[1] == "T")
            {if (runif(1) < titrate) {patients[1, j] <- 'A'} else
                {
                   patients[1, j] <- 'S'
                   missinrow[1, j] <- 1
                }
            }
            if (classlist[1] == "C")
            {if (runif(1) < complete) {patients[1, j] <- 'A'} else
                {
                   patients[1, j] <- 'S'
                   missinrow[1, j] <- 1
                }
            }
            if (classlist[1] == "N")
            {
               if (runif(1) < noise) {patients[1, j] <- 'A'} else 
               {
                   patients[1, j] <- 'S'
                   missinrow[1, j] <- 1
               }
            }
            if (classlist[1] == "O")
            {
               if (runif(1) < other) {patients[1, j] <- 'A'} else 
               {
                    patients[1, j] <- 'S'
                    missinrow[1, j] <- 1
                }
            }            
        } else
        # If skipped previous session
        if (patients[1, (j-1)] == 'S')
        {
            if (classlist[1] == "D")
            {
               if (runif(1) < drop) {patients[1, j] <- 'A'} else
               {
                  patients[1, j] <- 'S'
                  missinrow[1, j] <- missinrow[1, (j-1)] + 1
               }
            } 
            if (classlist[1] == "T")
            {
               if (runif(1) < titrate) {patients[1, j] <- 'A'} else
               {
                 patients[1, j] <- 'S'
                 missinrow[1, j] <- missinrow[1, (j-1)] + 1
               }
            }
            if (classlist[1] == "C")
            {if (runif(1) < complete) {patients[1, j] <- 'A'} else
                {
                   patients[1, j] <- 'S'
                   missinrow[1, j] <- missinrow[1, (j-1)] + 1
                }
            }
            if (classlist[1] == "N")
            {if (runif(1) < noise) {patients[1, j] <- 'A'} else 
                {
                   patients[1, j] <- 'S'
                   missinrow[1, j] <- missinrow[1, (j-1)] + 1
                }
            }
            if (classlist[1] == "O")
            {
               if (runif(1) < other) {patients[1, j] <- 'A'} else 
               {
                    patients[1, j] <- 'S'
                    missinrow[1, j] <- missinrow[1, (j-1)] + 1
               }
            }            
        }
    } else {patients[1,j] <- 'D'}   
    # check for number of attended or missed sessions
    if (patients[1,(j-1)] == 'A' | patients[1,(j-1)] == 'S')
    {
        insess[j] <- 1
    }   else
    {
        insess[j] <- 0
    }
}


#Later patients
#Patient is waiting and there is space
  #First session
  
#FOR SOME REASON, THIS IS ONLY WORKING FOR PATIENT 2  when the first three statements are after the WHILE
# AND, WITH THEM AFTER FIRST FOR, IT NEVER HALTS AND RESETS PATIENTS [1,]
rest.original <- function() {
  for(i in 2:p)
    {
      while (insess[1] < m & waitlist[1] > 0)  #THIS MAY NEED TO BE A WHILE LOOP, FOR MULTIPLE PATIENTS
        {
          invited[i,1] <- 1
          waitlist[1] <- waitlist[1] - 1
          insess[1] <- insess[1] + 1
          ## the rest of the code below does not touch the loop-relevant variables i, insess, waitlist,
          ## so they can be commented out to make a minimal 'failing' example, see the rest1 fn below.
          if (classlist[i] == "D")
            {
              if (runif(1) < dropnew) {patients[i, 1] <- 'A'} else
              {
                patients[i, 1] <- 'S'
                missinrow[i, 1] <- 1
              }
            }
          if (classlist[i] == "T")
            {
              if (runif(1) < titratenew) {patients[i, 1] <- 'A'} else 
              {
                patients[i, 1] <- 'S'
                missinrow[i, 1] <- 1
              }
            }
          if (classlist[i] == "C")
            {
              if (runif(1) < completenew) {patients[i, 1] <- 'A'} else 
              {
                patients[i, 1] <- 'S'
                missinrow[i, 1] <- 1
              }
            }
          if (classlist[i] == "N")
            {
              if (runif(1) < noisenew) {patients[i, 1] <- 'A'} else 
              {
                patients[i, 1] <- 'S'
                missinrow[i, 1] <- 1
              }
            }
          if (classlist[i] == "O")
            {
              if (runif(1) < othernew) {patients[i, 1] <- 'A'} else 
              {
                patients[i, 1] <- 'S'
                missinrow[i, 1] <- 1
              }
            }
        }
    }
}



#Later sessions

#GET THIS FROM V6 AND THEN MODIFY, OR MODIFY ABOVE


# This is the minimal failing version of the above 'rest.orig' fn:
rest <- function() {
  for(i in 2:p)
    {
      # we are not changing insess or waitlist here, so once the while condition fails for i=2,
      # it will fail for all remaining i values
      while (insess[1] < m & waitlist[1] > 0)  #THIS MAY NEED TO BE A WHILE LOOP, FOR MULTIPLE PATIENTS
        {
          invited[i,1] <- 1
          waitlist[1] <- waitlist[1] - 1
          insess[1] <- insess[1] + 1
        }
    }
}



## NOTES ON HOW TO DEBUG THIS in general

## First install the 'debug' package, using this command:
if (FALSE) {

install.packages('debug')

## Now after running all the initial lines of the above code in R, paste the definition of the 'rest' function into R.
## Now set up the 'rest' function for debugging:
mtrace( rest )

## Now run the rest() function and you can step through it, and examine variables, etc.
## It should pop up a separate window where it shows which line you are at.
## You then set up a breakpoint at the 'while' statement using:
bp(3,TRUE)
## ( I am assuming the line number of the while is 3 in the debug-code-window )
## Then you can just ask it to run until it hits the breakpoint by saying
go()
## Once at the breakpiont you can examine the values of insess[1], waitlist[1], etc,
## and see what you need to change in your logic.

## You can also simply single-step through the code by hitting enter each time.

}


  
