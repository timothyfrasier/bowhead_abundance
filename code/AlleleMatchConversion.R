######################################
# CODE FOR CONVERTING AlleleMatch    #
# DATA TO CAPTURE HISTORY            #
######################################

histconversion <- function(captures, ninds, nlines, periods, nperiods) {
    

    caphist <- array(NA, dim=c(ninds, nperiods))    # Array to hold capture history data

    #-----------------------------#
    # Enter Data For First Line   #
    #-----------------------------#
    for (i in 1:nperiods) {
	
        # Enter a 1 for period where individual was
        # sighted, zero elsewhere. Use data from
        # both capture periods, without overwriting first with 0
        # (1s are okay)
        ifelse(captures[1, 3] == periods[i], caphist[1, i] <- 1, caphist[1, i] <- 0)
        if (captures[1, 6] == periods[i]) {
            caphist[1, i] <- 1
        }
    }

    #----------------------------#
    # For Subsequent Lines       #
    #----------------------------#

    #---Generate Counter for Rows---#
    rowcount <- 1

    #---Loop through lines---#
    for (i in 2:nlines) {
    
        #---Check if this is a new individual---#
        if (captures[i, 1] == captures[(i-1), 1]) {
		
            # If so, record new sighting in same row as above
		
            # Loop through periods
            for (j in 1:nperiods) {
			
                # Enter a 1 for periods were individual
                # was seen, ignore others so as to
                # not overwrite previous sightings with 0s
                # (1s are okay)
                if (captures[i, 3] == periods[j]) {
                    caphist[rowcount, j] <- 1
                }
                if (captures[i, 6] == periods[j]) {
                    caphist[rowcount, j] <- 1
                }
            }
                } else {
                
                    # If not, increase rowcount and record
                    # sighting in new row
                    rowcount <- rowcount + 1
                            
                    # Loop through periods
                    for (j in 1:nperiods) {
                                
                        # Enter a 1 for period where individual was
                        # sighted, zero elsewhere
                        ifelse(captures[i, 3] == periods[j], caphist[rowcount, j] <- 1, caphist[rowcount, j] <- 0)
                        if (captures[i, 6] == periods[j]) {
                            caphist[rowcount, j] <- 1
                        }
                    }
                }
    }
    return(caphist)
}


