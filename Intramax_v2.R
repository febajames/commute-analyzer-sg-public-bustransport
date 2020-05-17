################################################################################
### Masser and Brown Intramax regionalisation
################################################################################
#
# Version 0.9 - Martin Charlton, Maynooth University
# Licensed under GPL V3: https://gnu.org/licenses/gpl.html
#
# Refs:
#
#   Masser, I. and Brown, P. J. B. (1975),
#   "Hierarchical aggregation procedures for interaction data",
#   Environment and Planning A, Vol. 7, pp. 509–523.
#
#   Masser, I. and Brown, P. J. B. (1977),
#   "Spatial representation and spatial interaction", Papers of the
#   Regional Science Association, Vol. 38, pp. 71–92.
#
#   Masser, I. and Scheurwater, J. (1980),
#   "Functional regionalisation of spatial interaction data:
#    an evaluation of some suggested strategies",
#   Environment and Planning A, Vol. 12, pp. 1357–1382
#
################################################################################
###
### This version does not re-calculate the matrix of objective function values
### after each merge, but only the row and column representing the merged zones
###
### Takes about 40 minutes on a Dell T7400 with a 3.16GHz Intel Xeon X5460
###   under Windows XP for a problem with 3409 zones
###
################################################################################
###
### Arguments
###   Trips:       square matrix of interzonal trip counts
###   nZones:      number of regions required (default is 1)
###   zerofix:     fix for zones with zero marginals
###   zeroVal:     Objective function value for cell with zero marginal
###   diags:       print diagnostics
###
### Returns
###   Alloc:       vector of allocated region codes
###   Tmat:        between region trip counts
###   ObjMax:      values of max objective function for each iteration
###
################################################################################
Intramax <- function(Trips,nZones=1,zeroFix=T, zeroVal=0, diags=FALSE) {

###
### Initial setup
###
      nJoins <- nrow(Trips)-nZones
      T2 <- Trips
      Alloc <- rownames(Trips)
      names(Alloc) <- rownames(Trips)
      
      # add code: list assignment
      alloc <- list()
      trips <- list()
      pair <- list()
      
#
# Compute marginal totals - with fix for zero totals
#
      Orig <- rowSums(T2)
      Dest <- colSums(T2)
      if (zeroFix) {
         Orig[which(Orig == 0)] <- 1
         Dest[which(Dest == 0)] <- 1
      }
      N <- length(Orig)
#
# Compute starting Objective Function matrix
#
# If you've used the zero marginal fix, then there's be a non-zero value in
#  every cell. If, not, and any of Oi, Dj, Oj, or Di are zero, the cell
#  gets set to zero. If they were set to some suitably large value, then
#  these zones would be fused early in the process. There's room for experiment.
#
# Diagonal Tii are set to zero
#
      ObjFun <- matrix(NA,N,N)
      for (i in 1:N) {
         for (j in 1:N) {
             if ((Orig[i] > 0 & Dest[j] > 0) & (Orig[j] > 0 & Dest[i] > 0)) {
                ObjFun[i,j] <- T2[i,j]/(Orig[i]*Dest[j]) + T2[j,i]/(Orig[j]*Dest[i])
             } else {
                ObjFun[i,j] <- zeroVal
             }
         }
      }
      rownames(ObjFun) <- names(Orig)
      colnames(ObjFun) <- names(Dest)
      diag(ObjFun) <- 0
      if(diags) print(ObjFun)

###
### Main processing loop
###
   for (iter in 1:nJoins) {
      cat("*** ITERATION ",iter,"\n")
         if(diags) {
         cat("\n\n************************************************************\n")
         cat("*** ITERATION ",iter,"\n")
         cat(    "************************************************************\n")
      }
#
# Find maximum value, the fow/col of the cell and the names of the zones, and add
#  this value to the vector of iteration maxima
#
      maxObj <- max(ObjFun)
      maxPair <- unique(sort(which(ObjFun == maxObj, arr.ind=T)))
      maxName <- names(Orig)[maxPair]
      if (iter == 1) {
         maxVec <- maxObj
      } else {
         maxVec <- c(maxVec,maxObj)
      }
      if(diags) print(maxPair)
      if(diags) print(maxName)
      
      # add code:
      pair[[iter]] <- as.data.frame(maxName)[1:2,]

#
# Fuse the two rows and two columns
#  ... remembering to update the marginals
#
         T2[maxPair[1],] <- T2[maxPair[1],]+T2[maxPair[2],]
         T2[maxPair[2],] <- 0
         T2[,maxPair[1]] <- T2[,maxPair[1]]+T2[,maxPair[2]]
         T2[,maxPair[2]] <- 0
         if(diags) {
            print(T2)
            print(sum(T2))
         }
         Orig[maxPair[1]] <- Orig[maxPair[1]] + Orig[maxPair[2]]
         Dest[maxPair[1]] <- Dest[maxPair[1]] + Dest[maxPair[2]]
      
      # add code: append sum of matrix diagonals as a % of total trips
      trips[[iter]] <- as.data.frame(round(sum(diag(T2))*100/sum(T2)))

#
# Update row i with new objective function values
#
         i <- maxPair[1]
         for (j in 1:N) {
         if(diags)print(c(iter,i,j))
             if ((Orig[i] > 0 & Dest[j] > 0) & (Orig[j] > 0 & Dest[i] > 0)) {
                ObjFun[i,j] <- T2[i,j]/(Orig[i]*Dest[j]) + T2[j,i]/(Orig[j]*Dest[i])
             } else {
                ObjFun[i,j] <- zeroVal
             }
         }
#
# Update column j likewise
#
         j <- maxPair[1]
         for (i in 1:N) {
         if(diags) print(c(iter,i,j))
             if ((Orig[i] > 0 & Dest[j] > 0) & (Orig[j] > 0 & Dest[i] > 0)) {
                ObjFun[i,j] <- T2[i,j]/(Orig[i]*Dest[j]) + T2[j,i]/(Orig[j]*Dest[i])
             } else {
                ObjFun[i,j] <- 0
             }
         }
#
# ... and reset diag to zero
#
         diag(ObjFun) <- 0


#
# Then... remove row and column representing second zone from:
#   1. T2
#   2. ObjFun
#   3. Oi,Dj
#  ... rembering special case with T2 is reduced to only one row
#
         T2 <- T2[-maxPair[2],]
         ObjFun <- ObjFun[-maxPair[2],]
         Orig <- Orig[-maxPair[2]]
         Dest <- Dest[-maxPair[2]]
         if(is.null(nrow(T2))) {
            T2 <- T2[-maxPair[2]]
            ObjFun <- ObjFun[-maxPair[2]]
         } else {
            T2 <- T2[,-maxPair[2]]
            ObjFun <- ObjFun[,-maxPair[2]]
         }
#
# Finally, update the Allocation vector
#
         Alloc[which(Alloc == maxName[2])] <- Alloc[maxName[1]]
         if(diags) print(Alloc)

         N <- N - 1
   
      # add code: append allocation
      alloc[[iter]] <- as.data.frame(Alloc)
      
   }
      
   # add code: combine all df
   pair_df <- bind_cols(pair)
   trips_df <- bind_rows(trips)
   alloc_df <- bind_cols(alloc)
   
   # add code: rename df
   rownames(alloc_df) <- rownames(Trips)
   colnames(pair_df) <- colnames(alloc_df)
   colnames(trips_df) <- "Cumulative Intrazonal Interaction"
   rownames(trips_df) <- colnames(alloc_df)

   # add code: write to csv
   write.csv(pair_df, file = "pair.csv")
   write.csv(trips_df, file = "trips.csv")
   write.csv(alloc_df, file = "alloc.csv")   
   
      
   # add code: mute return code
   # return(list(Alloc=Alloc,Tmat=T2,ObjMax=maxVec))
   

}
################################################################################
################################################################################
