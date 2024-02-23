# R package development libraries -----------------------------------------
# library(roxygen2) # In-Line Documentation for R
# library(devtools) # Tools to Make Developing R Packages Easier
# library(testthat) # Unit Testing for R
# library(usethis)  # Automate Package and Project Setup


# Check R library name ----------------------------------------------------
# library(available)
# available::available("ClusterDescriptions", browse = FALSE)

# -------------------------------------------------------------------------
# Multivariate Clustering v04
# BHC

# Started: 2023-12-29
# Updated: 2024-02-23
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
## FUNCTIONS ##
## FUNCTION - Analyze Clusters
# Inputs:
#    - Data with uniqueID and cluster assignments
# Outputs:
#    - Cluster metric values
#    - Cluster metric plots
#    - Cluster descriptions if desired
#    - Excel file output if desired


## FUNCTION - Describe Clusters
# Inputs:
#    - Data with uniqueID and a list of chosen cluster assignments
# Outputs:
#    - Cluster description by cluster:
#         - Number of obs in cluster
#         - Percent of total obs in cluster
#         - By variable:
#              - Variable name
#              - Mean
#              - Mean difference from out-cluster
#              - Standard mean difference from out-cluster
#    - By variable:
#         - Plot colored histograms of cluster values for each variable with marked means; this will show the distribution


## FUNCTION - Describe Observation
# Inputs:
#    - Data with uniqueID and cluster assignments
# Outputs:
#    - Plot colored histograms of each variable with means and observation values marked
# -------------------------------------------------------------------------


# TODO --------------------------------------------------------------------

# Add check for total sample size >2

# Add header page to excel output with metrics if descriptions fcn was called from metrics fcn
# Add header page to excel output with proportions
# Try with clusters of single values for div 0 errors
# Add distr plots for each variable by cluster, one plot per var

# Add single variable description for R
# Add confusion matrices for cross-cluster comparison

# Add radar plots for clusters
# Add cluster size table for each solution before cluster breakouts
# Add an all clusters plot under cluster size table
# Add sig interval around radar plots
# Add legend for plots, incl. that sig interval is for original given sig threshold

# Force solutions to end up as 1:n if any clusters are missing in between
# Check for FIX comments!!
# Check for DELETE comments!!

# Change pooled variance to
#   - Hedges for <=20
# <https://www.statisticshowto.com/hedges-g/>
#   - Cohens for >30
# <https://www.statisticshowto.com/probability-and-statistics/statistics-definitions/cohens-d/>


# -------------------------------------------------------------------------


library(tidyverse)
options(scipen=9999)

setwd("/Users/benclaassen/Documents/_Workshop/_Code Utilities/Statistics/MultivariateDescriptionsPackage/Test Data")
# save(clustDat, file = "clusterDataAndSolns.Rda") # [clustDat] test data
load(file = "clusterDataAndSolns.Rda") # [clustDat] test data
dim(clustDat)
head(clustDat)
length(unique(clustDat$GEOID20)) # 9193

# Function - Cluster Metrics ----------------------------------------------
clusterData = clustDat

uniqueID = 1
clusterSolutions = c(2:6)
dataColumns = c(26:28)

# clusterSolutions = sample(c(2:20))
# clusterData <- cbind(clusterData[,uniqueID], clusterData[, clusterSolutions], clusterData[, c(21:25)], clusterData[, dataColumns])
# names(clusterData)[1] <- names(clustDat)[1]
# head(clusterData)

# clusterSolutions = c(2:20)
# sapply(clusterData %>% select(all_of(clusterSolutions)), max)
head(clusterData)

plotMetrics = TRUE
includeDescriptions = T
exportOutput = TRUE
ifPlot = TRUE
exportSignificantDigits = 3
calledFromMetrics = F

# Input vars with names instead of col numbers ->
uniqueID = names(clusterData)[1]
clusterSolutions = names(clusterData)[2:6]
# clusterSolutions = sample(names(clusterData)[2:20])
dataColumns = names(clusterData)[26:28]

clusterDistances <- dist(clusterData %>% select(lclzd_JobsAndPopulation_std, lclzd_NumberOfBorderingRoads_std, lclzd_MedianValue_std)
  , method = "euclidean")

clusterMetrics = ""

getClusterMetrics <- function(clusterData, uniqueID, clusterSolutions, dataColumns, clusterDistances, clusterMetrics = "", plotMetrics = TRUE, includeDescriptions = TRUE, exportOutput = TRUE, exportSignificantDigits = 3)
{
  ## FUNCTION - Cluster Metrics
  # Inputs:
  #    - Data with uniqueID and cluster assignments
  # Outputs:
  #    - Cluster metric values
  #    - Cluster metric plots
  #    - Cluster descriptions if desired
  #    - Excel file output if desired

  # [clusterData] requires a data.frame with the following data:
  #   - [uniqueID]: a column number for the unique IDs
  #   - [clusterSolutions]: a list of which columns contain the clustering solutions (one column per solution)
  #   - [dataColumns]: a list of which columns contain the clustering data

  # Libraries and options
  require(tidyverse)
  require(fpc)
  options(scipen = 9999)

  # Check inputs ----------------------------------------------------------

  # Check [exportSignificantDigits], must be >=1
  if(class(exportSignificantDigits)[1] != "numeric")
  {
    stop("[exportSignificantDigits] must be numeric")
  }

  # Confirm [clusterData] is a data.frame, or convert if it is a matrix
  if(class(clusterData)[1] == "matrix")
  {
    clusterData = as.data.frame(clusterData)
  }
  if(!class(clusterData) == "data.frame")
  {
    stop("[ClusterData] must be a data.frame")
  }


  # Confirm [uniqueID] are column number or a column name in [clusterData]
  if(class(uniqueID)[1] == "numeric" & length(uniqueID) == 1) # Check for [uniqueID] type
  {
    if(uniqueID < 0 | uniqueID > dim(clusterData)[2]) # Check if [clusterData] has given column number
    {
      stop("[uniqueID] column number not in [clusterData] dimensions")
    }

    # Store both forms of [uniqueID] for checking overlap
    tmp_uniqueID_char = names(clusterData)[uniqueID]
    tmp_uniqueID_num = uniqueID

  } else if(class(uniqueID)[1] == "character" & length(uniqueID) == 1) # Check for [uniqueID] type
  {

    if(!uniqueID %in% names(clusterData)) # Check if [clusterData] has given name
    {
      stop("[uniqueID] name not in [clusterData]")
    }

    # Store both forms of [uniqueID] for checking overlap
    tmp_uniqueID_char = uniqueID
    tmp_uniqueID_num = which(names(clusterData) == uniqueID)

  } else # Else not a numeric or character entry
  {
    stop("[uniqueID] must be a single number or string")
  }

  # Confirm [clusterSolutions] are column numbers or column names in [clusterData]
  if(class(clusterSolutions)[1] == "integer" | class(clusterSolutions)[1] == "numeric" ) # Check for [clusterSolutions] type
  {

    # Check if [clusterSolutions] are outside the dimensions of [clusterData]
    if(any(clusterSolutions > dim(clusterData)[2]) | any(clusterSolutions < 1))
    {
      stop("[clusterSolutions] column numbers must be within the dimensions of [clusterData]")
    }

    # Store both forms of [uniqueID] for checking overlap
    tmp_clusterSolutions_char = names(clusterData)[clusterSolutions]
    tmp_clusterSolutions_num = clusterSolutions

  } else if(class(clusterSolutions)[1] == "character") # Check for [clusterSolutions] type
  {

    # Check if [clusterSolutions] are outside the dimensions of [clusterData]
    if( !all(clusterSolutions %in% names(clusterData)) )
    {
      stop("[clusterSolutions] names must be in [clusterData] names")
    }

    # Store both forms of [uniqueID] for checking overlap
    tmp_clusterSolutions_char = clusterSolutions
    tmp_clusterSolutions_num = sapply(clusterSolutions, FUN = function(x) { which( names(clusterData) == x ) })

  } else # Else not a numeric or character entry
  {
    stop("[clusterSolutions] must be a list of numbers or strings")
  }

  # Confirm [dataColumns] are column numbers or column names in [clusterData]
  if(class(dataColumns)[1] == "integer") # Check for [dataColumns] type
  {

    # Check if [dataColumns] are outside the dimensions of [clusterData]
    if(any(dataColumns > dim(clusterData)[2]) | any(dataColumns < 1))
    {
      stop("[dataColumns] column numbers must be within the dimensions of [clusterData]")
    }

    # Store both forms of [uniqueID] for checking overlap
    tmp_dataColumns_char = names(clusterData)[dataColumns]
    tmp_dataColumns_num = dataColumns

  } else if(class(dataColumns)[1] == "character") # Check for [dataColumns] type
  {

    # Check if [dataColumns] are in names of [clusterData]
    if( !all(dataColumns %in% names(clusterData)) )
    {
      stop("[dataColumns] names must be in [clusterData] names")
    }

    # Store both forms of [uniqueID] for checking overlap
    tmp_dataColumns_char = dataColumns
    tmp_dataColumns_num = sapply(dataColumns, FUN = function(x) { which( names(clusterData) == x ) })

  } else # Else not a numeric or character entry
  {
    stop("[dataColumns] must be a list of numbers or strings")
  }

  # Confirm there is no overlap in [uniqueID], [clusterSolutions], or [dataColumns]
  if(any(duplicated(unlist(
      c(
        tmp_uniqueID_num
        , tmp_clusterSolutions_num
        , tmp_dataColumns_num
      )
  ))))
  {
    stop("[uniqueID], [clusterSolutions], and [dataColumns] must all be mutually exclusive")
  }

  # Once duplicates are confirmed not to exist, store all column names in label form
  # These all are now the character versions of the original user input
  uniqueID <- tmp_uniqueID_char
  clusterSolutions <- tmp_clusterSolutions_char
  dataColumns <- tmp_dataColumns_char

  # Reorder [clusterSolutions] by number of clusters (i.e. max cluster ID by column)
  clusterSolutions <- clusterSolutions[
      order(
        sapply(clusterData %>% select(all_of(clusterSolutions)), max)
        )
    ]

  # Subset [clusterData] to [uniqueID], [clusterSolutions], and [dataColumns] only
  clusterData <- clusterData %>% select(
    all_of(uniqueID)
    , all_of(clusterSolutions)
    , all_of(dataColumns)
  )


  # Set total number of cluster solutions
  tmp_numClustSolutions <- length(clusterSolutions)

  if(clusterMetrics[1] == "")
  {
    clusterMetrics = c(
      "within.cluster.ss" # Within-cluster sum of squares
    , "avg.silwidth" # Average silhouette width
    , "ch" # Calinski-Harabasz index
    , "wb.ratio" # Within/between SSE ratio
    )
  } else # Check that each given cluster metric is valid
  {
    if(
      !all(
        clusterMetrics %in%
        c("n", "cluster.number", "cluster.size", "min.cluster.size", "noisen", "diameter", "average.distance", "median.distance", "separation", "average.toother", "separation.matrix", "ave.between.matrix", "average.between", "average.within", "n.between", "n.within", "max.diameter", "min.separation", "within.cluster.ss", "clus.avg.silwidths", "avg.silwidth", "g2", "g3", "pearsongamma", "dunn", "dunn2", "entropy", "wb.ratio", "ch", "cwidegap", "widestgap", "sindex", "corrected.rand", "vi")
      )
    )
    {
      stop(paste0("Cluster fit metrics must be from {fpc} package fcn [cluster.stats] \n -> Valid entries: n, cluster.number, cluster.size, min.cluster.size, noisen, diameter, average.distance, median.distance, separation, average.toother, separation.matrix, ave.between.matrix, average.between, average.within, n.between, n.within, max.diameter, min.separation, within.cluster.ss, clus.avg.silwidths, avg.silwidth, g2, g3, pearsongamma, dunn, dunn2, entropy, wb.ratio, ch, cwidegap, widestgap, sindex, corrected.rand, vi"))
    }
  }

  # Create cluster fit metrics storage matrix
  tmp_clustFitMetricStorage <- as.data.frame(matrix(, tmp_numClustSolutions, (length(clusterMetrics) + 2)) ) # Storage matrix for cluster fit metrics; no metrics for 1 cluster so n-1
  names(tmp_clustFitMetricStorage) <- c("ClusterSolutionName", "NumClusters"
      , clusterMetrics
    )

  # Add cluster solution names to storge
  tmp_clustFitMetricStorage[,1] <- clusterSolutions
  # Fill in 'Cluster Number' column with max of cluster IDs for each solution set, i.e. number of clusters in given solution
  tmp_clustFitMetricStorage[,2] <- sapply(clusterData %>% select(all_of(clusterSolutions)), max)

  # Calculate cluster metrics for each [clusterSolutions]
  for(tmp_clustSolution in clusterSolutions)
  {
    print(paste0("Fitting metrics for [k=", tmp_clustFitMetricStorage %>% filter(ClusterSolutionName == tmp_clustSolution) %>% select(NumClusters), "]"))
    tmp_clustIDs <- unlist(clusterData %>% select(all_of(tmp_clustSolution))) # Store cluster IDs
    tmp_clustStats <- cluster.stats(clusterDistances, tmp_clustIDs) # Get cluster statistics

    tmp_numberStartingCols <- 2 # The (2) starting columns are 'ClusterSolutionName' and 'NumClusters'. This is the offset for downstream column selection

    tmp_currentSolutionRow <- which(clusterSolutions == tmp_clustSolution) # Set row of [tmp_clustFitMetricStorage] for current cluster solution [tmp_clustSolution]

    # Confirm 'cluster.number' == tmp_clustFitMetricStorage[,2]
    if(!tmp_clustFitMetricStorage[tmp_currentSolutionRow,2] == tmp_clustStats$cluster.number)
    {
      stop("Number of clusters in solution do not match 'cluster.stats' output")
    }

    # Store desired cluster metrics
    tmp_clustFitMetricStorage[tmp_currentSolutionRow, (1:length(clusterMetrics) + tmp_numberStartingCols)] <- tmp_clustStats[ clusterMetrics ]

    rm(tmp_clustStats)
  }

  # Print metrics
  print(tmp_clustFitMetricStorage)

  # Plot cluster metrics
  if(plotMetrics)
  {
    # Iterate over fit metrics
    for(x in (tmp_numberStartingCols+1):dim(tmp_clustFitMetricStorage)[2]) {
      plot(tmp_clustFitMetricStorage[,2], tmp_clustFitMetricStorage[,x], type = 'l', main = names(tmp_clustFitMetricStorage)[x], xlab = "Cluster Number", xaxt = "n", ylab = "Metric Value")
      axis(side = 1, at = tmp_clustFitMetricStorage[,2], labels = tmp_clustFitMetricStorage[,2])
      grid()
    }
  }

  # If [includeDescriptions] is TRUE, call descriptions function
  if(includeDescriptions)
  {
    tmp_clusterDescriptionsStorage <- describeClusters(clusterData, tmp_uniqueID_char, tmp_clusterSolutions_char, tmp_dataColumns_char, exportSignificantDigits, calledFromMetrics = TRUE)
  }

  # If [exportOutput] is TRUE, export the output created above
  if(exportOutput)
  {
    # Export to current working directory
    return(tmp_clusterDescriptionsStorage)
  }


}



describeClusters <- function(clusterData, uniqueID, clusterSolutions, dataColumns, exportOutput = TRUE, ifPlot = TRUE, exportSignificantDigits = 3, calledFromMetrics = FALSE)
{
  ## FUNCTION - Cluster Descriptions
  # Inputs:
  #    - Data with uniqueID and a list of chosen cluster assignments
  # Outputs:
  #    - Cluster description by cluster:
  #         - Number of obs in cluster
  #         - Percent of total obs in cluster
  #         - By variable:
  #              - Variable name
  #              - Variable mean
  #              - Variable interquartile range
  #              - Variable mean difference from out-cluster
  #              - Variable standard mean difference from out-cluster
  #    - By variable:
  #         - Plot colored histograms of cluster values for each variable with marked means; this will show the distribution

  # [clusterData] requires a data.frame with the following data:
  #   - [uniqueID]: a column number for the unique IDs
  #   - [clusterSolutions]: a list of which columns contain the clustering solutions (one column per solution)
  #   - [dataColumns]: a list of which columns contain the clustering data

  require(tidyverse)
  options(scipen = 9999)

  # Check inputs ----------------------------------------------------------

  # Confirm [clusterData] is a data.frame, or convert if it is a matrix
  if(class(clusterData)[1] == "matrix")
  {
    clusterData = as.data.frame(clusterData)
  }
  if(!class(clusterData) == "data.frame")
  {
    stop("[ClusterData] must be a data.frame")
  }


  # Confirm [uniqueID] are column number or a column name in [clusterData]
  if(class(uniqueID)[1] == "numeric" & length(uniqueID) == 1) # Check for [uniqueID] type
  {
    if(uniqueID < 0 | uniqueID > dim(clusterData)[2]) # Check if [clusterData] has given column number
    {
      stop("[uniqueID] column number not in [clusterData] dimensions")
    }

    # Store both forms of [uniqueID] for checking overlap
    tmp_uniqueID_char = names(clusterData)[uniqueID]
    tmp_uniqueID_num = uniqueID

  } else if(class(uniqueID)[1] =="character" & length(uniqueID) == 1) # Check for [uniqueID] type
  {
    if(!uniqueID %in% names(clusterData)) # Check if [clusterData] has given name
    {
      stop("[uniqueID] name not in [clusterData]")
    }

    # Store both forms of [uniqueID] for checking overlap
    tmp_uniqueID_char = uniqueID
    tmp_uniqueID_num = which(names(clusterData) == uniqueID)

  } else # Else not a numeric or character entry
  {
    stop("[uniqueID] must be a single number or string")
  }

  # Confirm [clusterSolutions] are column numbers or column names in [clusterData]
  if(class(clusterSolutions)[1] == "integer" | class(clusterSolutions)[1] == "numeric") # Check for [clusterSolutions] type
  {

    # Check if [clusterSolutions] are outside the dimensions of [clusterData]
    if(any(clusterSolutions > dim(clusterData)[2]) | any(clusterSolutions < 1))
    {
      stop("[clusterSolutions] column numbers must be within the dimensions of [clusterData]")
    }

    # Store both forms of [uniqueID] for checking overlap
    tmp_clusterSolutions_char = names(clusterData)[clusterSolutions]
    tmp_clusterSolutions_num = clusterSolutions

  } else if(class(clusterSolutions)[1] == "character") # Check for [clusterSolutions] type
  {
    if( !all(clusterSolutions %in% names(clusterData)) )
    {
      stop("[clusterSolutions] names must be in [clusterData] names")
    }

    # Store both forms of [uniqueID] for checking overlap
    tmp_clusterSolutions_char = clusterSolutions
    tmp_clusterSolutions_num = sapply(clusterSolutions, FUN = function(x) { which( names(clusterData) == x ) })

  } else # Else not a numeric or character entry
  {
    stop("[clusterSolutions] must be a list of numbers or strings")
  }

  # Confirm [dataColumns] are column numbers or column names in [clusterData]
  if(class(dataColumns)[1] == "integer") # Check for [dataColumns] type
  {
    # Check if [dataColumns] are outside the dimensions of [clusterData]
    if(any(dataColumns > dim(clusterData)[2]) | any(dataColumns < 1))
    {
      stop("[dataColumns] column numbers must be within the dimensions of [clusterData]")
    }

    # Store both forms of [uniqueID] for checking overlap
    tmp_dataColumns_char = names(clusterData)[dataColumns]
    tmp_dataColumns_num = dataColumns

  } else if(class(dataColumns)[1] == "character") # Check for [dataColumns] type
  {
    if( !all(dataColumns %in% names(clusterData)) )
    {
      stop("[dataColumns] names must be in [clusterData] names")
    }

    # Store both forms of [uniqueID] for checking overlap
    tmp_dataColumns_char = dataColumns
    tmp_dataColumns_num = sapply(dataColumns, FUN = function(x) { which( names(clusterData) == x ) })

  } else # Else not a numeric or character entry
  {
    stop("[dataColumns] must be a list of numbers or strings")
  }

  # Confirm there is no overlap in [uniqueID], [clusterSolutions], or [dataColumns]
  if(any(duplicated(unlist(
      c(
        tmp_uniqueID_num
        , tmp_clusterSolutions_num
        , tmp_dataColumns_num
      )
  ))))
  {
    stop("[uniqueID], [clusterSolutions], and [dataColumns] must all be mutually exclusive")
  }

  # Once duplicates are confirmed not to exist, store all column names in label form
  uniqueID <- tmp_uniqueID_char
  clusterSolutions <- tmp_clusterSolutions_char
  dataColumns <- tmp_dataColumns_char

  # Reorder [clusterSolutions] by number of clusters (i.e. max cluster ID by column)
  clusterSolutions <- clusterSolutions[
      order(
        sapply(clusterData %>% select(all_of(clusterSolutions)), max)
        )
    ]

  # Subset [clusterData] to [uniqueID], [clusterSolutions], and [dataColumns] only
  clusterData <- clusterData %>% select(
    all_of(uniqueID)
    , all_of(clusterSolutions)
    , all_of(dataColumns)
  )


  # Begin descriptions ----------------------------------------------------
  # Set number variables in [clusterData]
  tmp_numVariables <- length(dataColumns)

  # Create a description for each solution
  for(tmp_clustSolution in clusterSolutions)
  {
    print(paste0("Generating cluster descriptions for solution [", tmp_clustSolution,"]"))

    # Iterate over each group in the current solution
    for(tmp_currentCluster in 1:length(table(clusterData[, tmp_clustSolution])) )
    {
      # Initialize storage for current cluster ID within current cluster solution
      tmp_clusterProportions <- as.data.frame(matrix(, 1, 4)) # Cluster size and proportion
      names(tmp_clusterProportions) <- c("Cluster Number", "Total Number of Clusters", "Number of Observations", "Proportion")
      tmp_clusterVarDescriptions <- as.data.frame(matrix(, tmp_numVariables, 6)) # Cluster variables description (rows: num variables, cols: 5 metrics)
      names(tmp_clusterVarDescriptions) <- c("Variable", "Mean", "Mean Diff", "Std Mean Diff", "Pooled Std Dev", "Out-Cluster Mean")

      ## Fill in description
      # Current Cluster Number
      tmp_clusterProportions[1,1] <- tmp_currentCluster
      # Total Number of Clusters
      tmp_clusterProportions[1,2] <- length(table(clusterData[, tmp_clustSolution]))
      # Number of Observations
      tmp_clusterProportions[1,3] <- table(clusterData[, tmp_clustSolution])[tmp_currentCluster]
      # Proportion
      tmp_clusterProportions[1,4] <- table(clusterData[, tmp_clustSolution])[tmp_currentCluster] / sum( table(clusterData[, tmp_clustSolution]) )


      # Pull out variables in [dataColumns] for current cluster ID within the current cluster solution
      tmp_inClusterStdDiff <- clusterData[which(clusterData[,tmp_clustSolution] == tmp_currentCluster), ] %>% select(all_of(dataColumns))
      tmp_outClusterStdDiff <- clusterData[which(clusterData[,tmp_clustSolution] != tmp_currentCluster), ] %>% select(all_of(dataColumns))

      # Variable
      tmp_clusterVarDescriptions[,1] <- dataColumns # Variable names passed by user

      # In-Cluster Means
      tmp_clusterVarDescriptions[,2] <- sapply( # Vector of means for each variable in in-cluster data
        tmp_inClusterStdDiff, FUN = function(x) {mean(x, na.rm = T)}
      )

      # In-/Out-Cluster Mean Diffs
      tmp_clusterVarDescriptions[,3] <- tmp_clusterVarDescriptions[,2] -
        sapply( # Vector of means for each variable in out-cluster data
          tmp_outClusterStdDiff, FUN = function(x) {mean(x, na.rm = T)}
        )

      # Calculate In-/Out-Cluster Pooled Variance
      # NOTE: Only ever two 'samples' because we are looking at in-/out-cluster groups
      # NOTE: Pooled variance is ( (n_1-1)*var_1 + (n_2-1)*var_2 ) / (n_1 + n_2 - 2) <https://en.wikipedia.org/wiki/Pooled_variance>

      if(dim(tmp_inClusterStdDiff)[1] > 1 & dim(tmp_outClusterStdDiff)[1] > 1)
      {
        tmp_currentPooledVariance <- (
          ( (dim(tmp_inClusterStdDiff)[1]  - 1) * sapply(tmp_inClusterStdDiff,  FUN = function(x) {var(x, na.rm = T)}) ) +
          ( (dim(tmp_outClusterStdDiff)[1] - 1) * sapply(tmp_outClusterStdDiff, FUN = function(x) {var(x, na.rm = T)}) )
        ) / (
          dim(tmp_inClusterStdDiff)[1] + dim(tmp_outClusterStdDiff)[1] - 2
        )
      } else # Else, at least one of the in-/out-cluster datasets has 1 observation
      {
        if(dim(tmp_inClusterStdDiff)[1] == 1)
        {
          tmp_currentPooledVariance <- (
            rep(0, dim(tmp_inClusterStdDiff)[2]) +
            ( (dim(tmp_outClusterStdDiff)[1] - 1) * sapply(tmp_outClusterStdDiff, FUN = function(x) {var(x, na.rm = T)}) )
          ) / (
            dim(tmp_inClusterStdDiff)[1] + dim(tmp_outClusterStdDiff)[1] - 2
          )
        }

        if(dim(tmp_outClusterStdDiff)[1] == 1)
        {
          tmp_currentPooledVariance <- (
            ( (dim(tmp_inClusterStdDiff)[1]  - 1) * sapply(tmp_inClusterStdDiff,  FUN = function(x) {var(x, na.rm = T)}) ) +
            ( rep(0, dim(tmp_outClusterStdDiff)[2]) )
          ) / (
            dim(tmp_inClusterStdDiff)[1] + dim(tmp_outClusterStdDiff)[1] - 2
          )
        }
      }

      # In-/Out-Cluster Std Mean Diffs
      tmp_clusterVarDescriptions[,4] <- tmp_clusterVarDescriptions[,3] / sqrt(tmp_currentPooledVariance)

      # Pooled Variance
      tmp_clusterVarDescriptions[,5] <- sqrt(tmp_currentPooledVariance)

      # In-/Out-Cluster Mean Diffs
      tmp_clusterVarDescriptions[,6] <- sapply( # Vector of means for each variable in out-cluster data
          tmp_outClusterStdDiff, FUN = function(x) {mean(x, na.rm = T)}
        )

      # Sort by strength of standard mean differences
      tmp_clusterVarDescriptions <- tmp_clusterVarDescriptions[order(tmp_clusterVarDescriptions[,"Std Mean Diff"], decreasing =  T), ]


      if(tmp_currentCluster == 1)
      {
        # Create storage list
        tmp_singleClusterDescriptionStorage = list(
          list(
            tmp_clusterProportions,
            tmp_clusterVarDescriptions
          )
        )
      } else
      {
        tmp_singleClusterDescriptionStorage[tmp_currentCluster] = list(
          list(
            tmp_clusterProportions,
            tmp_clusterVarDescriptions
          )
        )
      }
    }

    # All descriptions for the current clustering solution are now complete
    # If working on the first clustering solution, initialize the overall storage list
    if(tmp_clustSolution == clusterSolutions[1])
    {
      tmp_solutionDescriptionIndex <- 1

      tmp_clusterDescriptionsStorage <- list(tmp_singleClusterDescriptionStorage)

    } else # Else if creating the second or latter solution, add to the storage list
    {
      tmp_solutionDescriptionIndex <- tmp_solutionDescriptionIndex + 1

      tmp_clusterDescriptionsStorage[tmp_solutionDescriptionIndex] <- list(tmp_singleClusterDescriptionStorage)
    }

  }


  # Export output to excel
  if(exportOutput)
  {
    require(openxlsx)

    # Check [exportSignificantDigits], must be numeric and >=1
    if(class(exportSignificantDigits)[1] != "numeric")
    {
      stop("[exportSignificantDigits] must be numeric")
    }
    if(exportSignificantDigits < 1)
    {
      exportSignificantDigits = 1
    }

    # Set which table rows to write to
    tmp_titleRow = 2
    tmp_proportionRow = 4
    tmp_descriptionRow = 7

    # Initialize excel file
    tmp_wb <- createWorkbook()

    # Formats
    tmp_style_posSig <- createStyle(bgFill = "#33F5B7")
    tmp_style_negSig <- createStyle(bgFill = "#FCB099")
    tmp_style_clusterHeader <- createStyle(fontSize = 14, textDecoration = "Bold")
    tmp_style_count <- createStyle(numFmt="#,##0")
    tmp_style_pct <- createStyle(numFmt="0.0%")
    tmp_style_float <- createStyle(numFmt= paste0( "#,##0.0", paste0(rep("0", (exportSignificantDigits-1)), collapse = "") ) )
    tmp_style_bold <- createStyle(textDecoration = "Bold")

    # Create parameters workbook
    addWorksheet(tmp_wb, "Parameters")
    writeData(tmp_wb, "Parameters", "Standard difference threshold:", startCol = 2, startRow = 2)
    writeData(tmp_wb, "Parameters", 0.2, startCol = 3, startRow = 2)


    # Create cluster sizes workbook
    # Initialize storage for all cluster counts and all cluster proportions
    tmp_clusterCountsTable <- as.data.frame(matrix(, length(clusterSolutions), (max(clusterData %>% select(all_of(clusterSolutions))) - min(clusterData %>% select(all_of(clusterSolutions))) + 1) )) # Rows: number of cluster solutions; Cols: [max cluster number across all solutions] - [min cluster number across all solutions] + 1

    # Assign names
    names(tmp_clusterCountsTable) <- paste0( "Cluster ", sort(unique(unlist(clusterData %>% select(all_of(clusterSolutions))))) ) # Sorted list of all unique cluster numbers across all solutions
    row.names(tmp_clusterCountsTable) <- clusterSolutions

    # Copy proportions storage from counts storage
    tmp_clusterProportionsTable <- tmp_clusterCountsTable


    # Pull cluster sizes from each solution
    # Iterate over each cluster solution
    for(s in 1:length(clusterSolutions))
    {
      # Iterate over each cluster within the solution
      for(c in 1:length(tmp_clusterDescriptionsStorage[[s]]))
      {
        # Store cluster counts and proportions
        tmp_clusterCountsTable[s,c] <- tmp_clusterDescriptionsStorage[[s]][[c]][[1]][1,3]
        tmp_clusterProportionsTable[s,c] <- tmp_clusterDescriptionsStorage[[s]][[c]][[1]][1,4]
      }
    }

    tmp_countTable_startRow <- 3
    tmp_countTable_startCol <- 3
    tmp_propTable_startRow <- 3
    tmp_propTable_startCol <- dim(tmp_clusterCountsTable)[2] + 3 + 2

    tmp_clusterNames <- as.data.frame(matrix(row.names(tmp_clusterCountsTable), dim(tmp_clusterCountsTable)[1], 1))
    names(tmp_clusterNames) <- ""

    addWorksheet(tmp_wb, "Cluster Sizes")
    # Add cluster counts table
    writeData(tmp_wb, "Cluster Sizes", "Cluster Counts", startRow = (tmp_countTable_startRow-1), startCol = (tmp_countTable_startCol-1))
    writeData(tmp_wb, "Cluster Sizes", tmp_clusterNames, startRow = tmp_countTable_startRow, startCol = (tmp_countTable_startCol-1))
    writeData(tmp_wb, "Cluster Sizes", tmp_clusterCountsTable, startRow = tmp_countTable_startRow, startCol = tmp_countTable_startCol)
    # Add cluster proportions table
    writeData(tmp_wb, "Cluster Sizes", "Cluster Proportions", startRow = (tmp_propTable_startRow-1), startCol = (tmp_propTable_startCol-1))
    writeData(tmp_wb, "Cluster Sizes", tmp_clusterNames, startRow = tmp_propTable_startRow, startCol = (tmp_propTable_startCol-1))
    writeData(tmp_wb, "Cluster Sizes", tmp_clusterProportionsTable, startRow = tmp_propTable_startRow, startCol = tmp_propTable_startCol)

    # Format headers
    addStyle(tmp_wb, "Cluster Sizes", style=tmp_style_bold, rows = (tmp_countTable_startRow-1), cols = c(tmp_countTable_startCol-1, tmp_propTable_startCol-1), stack = F)
    # Format count table
    addStyle(tmp_wb, "Cluster Sizes", style=tmp_style_count, rows = ((tmp_countTable_startRow+1):(tmp_countTable_startRow+dim(tmp_clusterCountsTable)[1])) , cols = ((tmp_countTable_startCol):(tmp_countTable_startCol + dim(tmp_clusterCountsTable)[2]-1)), stack = F, gridExpand = T)
    # Format proportions table
    addStyle(tmp_wb, "Cluster Sizes", style=tmp_style_pct, rows = ((tmp_propTable_startRow+1):(tmp_propTable_startRow+dim(tmp_clusterCountsTable)[1])), cols = c((tmp_propTable_startCol):(tmp_propTable_startCol + dim(tmp_clusterCountsTable)[2]-1)), stack = F, gridExpand = T)

    # If descriptions is called from metrics
    if(calledFromMetrics)
    {
      # Create cluster metrics and plots workbook
      addWorksheet(tmp_wb, "Cluster Metrics")
      writeData(tmp_wb, "Cluster Metrics", tmp_clustFitMetricStorage, startCol = 2, startRow = 2)

      # Set rows for spacing out metrics plots
      tmp_plotStartRow = dim(tmp_clustFitMetricStorage) + 4 # 3 rows below the end of the [tmp_clustFitMetricStorage] table
      tmp_plotRowIncrease = 22 # Number of rows to match 4 inches of plot height plus a margin
      tmp_numPlots = 0 # Initialize plot count to 0

      for(x in (tmp_numberStartingCols+1):dim(tmp_clustFitMetricStorage)[2]) {
        plot(tmp_clustFitMetricStorage[,2], tmp_clustFitMetricStorage[,x], type = 'l', main = names(tmp_clustFitMetricStorage)[x], xlab = "Cluster Number", xaxt = "n")
        axis(side = 1, at = tmp_clustFitMetricStorage[,2], labels = tmp_clustFitMetricStorage[,2])
        grid()

        # Add plot
        insertPlot(tmp_wb, "Cluster Metrics", width = 6, height = 4,
        startRow = tmp_plotStartRow + tmp_plotRowIncrease * tmp_numPlots, startCol = 2, fileType = "png", units = "in", dpi = 300)

        # Iterate plot count
        tmp_numPlots = tmp_numPlots + 1
      }

    } # End [calledFromMetrics] if statement


    # Iterate over each cluster solution
    for(s in 1:length(tmp_clusterDescriptionsStorage))
    {
      # Iterate over each cluster in each solutions
      tmp_numClustersInSolution <- tmp_clusterDescriptionsStorage[[s]][[1]][[1]][1,2] # Returns second entry in first proportion table for current solution [s]

      # Add worksheet
      tmp_worksheetName <- paste0(tmp_numClustersInSolution, " Clusters")
      addWorksheet(tmp_wb, tmp_worksheetName)

      # Set first table column
      tmp_currentCol <- 2


      for(c in 1:tmp_numClustersInSolution)
      {
        # Write cluster number 'title'
        writeData(tmp_wb, tmp_worksheetName, paste0("Cluster ", tmp_clusterDescriptionsStorage[[s]][[c]][[1]][1,1]), startCol = tmp_currentCol, startRow = tmp_titleRow)

        # Add proportions table to worksheet
        writeData(tmp_wb, tmp_worksheetName, tmp_clusterDescriptionsStorage[[s]][[c]][[1]][1,3:4], startCol = tmp_currentCol, startRow = tmp_proportionRow)

        # Add descriptions table to worksheet
        writeData(tmp_wb, tmp_worksheetName, tmp_clusterDescriptionsStorage[[s]][[c]][[2]][,1:4], startCol = tmp_currentCol, startRow = tmp_descriptionRow)

        # Format table
        addStyle(tmp_wb, tmp_worksheetName, style=tmp_style_clusterHeader, cols = tmp_currentCol, rows = tmp_titleRow)
        addStyle(tmp_wb, tmp_worksheetName, style=tmp_style_count, cols = tmp_currentCol, rows = tmp_proportionRow + 1, stack = F)
        addStyle(tmp_wb, tmp_worksheetName, style=tmp_style_pct, cols = tmp_currentCol + 1, rows = tmp_proportionRow + 1, stack = F)
        addStyle(tmp_wb, tmp_worksheetName, style=tmp_style_bold, cols = c(tmp_currentCol:(tmp_currentCol+1)), rows = tmp_proportionRow, stack = F)
        addStyle(tmp_wb, tmp_worksheetName, style=tmp_style_bold, cols = c(tmp_currentCol:(tmp_currentCol+3)), rows = tmp_descriptionRow, stack = F)

        # Format numbers using [exportSignificantDigits]
        addStyle(tmp_wb, tmp_worksheetName, style=tmp_style_float, cols = (tmp_currentCol+1):(tmp_currentCol+3), rows = ((tmp_descriptionRow+1):(tmp_descriptionRow+tmp_numVariables)), stack = F, gridExpand = T)

        # Format standard diffs in descriptions table
        conditionalFormatting(tmp_wb, tmp_worksheetName, cols=tmp_currentCol + 3, rows=8:100, rule=" > Parameters!$C$2", type = "expression", style = tmp_style_posSig)
        conditionalFormatting(tmp_wb, tmp_worksheetName, cols=tmp_currentCol + 3, rows=8:100, rule=" < (-1)*Parameters!$C$2", type = "expression", style = tmp_style_negSig)


        if(ifPlot)
        {
          require(fmsb)
          # Get variable means by cluster for current solution
          # tmp_currentSolutionMeansStorage <- as.data.frame(matrix(, (2+tmp_numClustersInSolution), (1+length(dataColumns))))
          # names(tmp_currentSolutionMeansStorage) <- c("Variable", dataColumns )
          # tmp_currentSolutionMeansStorage[,1] <- c("Max", "Min", paste0("k=", c(1:tmp_numClustersInSolution)))
          #
          # for(i in 1:tmp_numClustersInSolution)
          # {
          #   tmp_currentSolutionMeansStorage[i+2,-1] <- tmp_clusterDescriptionsStorage[[s]][[i]][[2]][,2]
          # }
          #
          # tmp_currentSolutionMeansStorage[1,-1] <- apply(tmp_currentSolutionMeansStorage[-c(1:2), -1], MARGIN = 2, FUN = function(x) {max(x, na.rm = T)})
          # tmp_currentSolutionMeansStorage[2,-1] <- apply(tmp_currentSolutionMeansStorage[-c(1:2), -1], MARGIN = 2, FUN = function(x) {min(x, na.rm = T)})

          # Get variable means by cluster for current solution
          tmp_currentSolutionMeansStorage <- as.data.frame(matrix(, length(dataColumns), (1+tmp_numClustersInSolution)))
          names(tmp_currentSolutionMeansStorage) <- c("Variable", paste0("k=", c(1:tmp_numClustersInSolution)) )
          tmp_currentSolutionMeansStorage[,1] <- dataColumns

          for(i in 1:tmp_numClustersInSolution)
          {
            tmp_currentSolutionMeansStorage[,i+1] <- tmp_clusterDescriptionsStorage[[s]][[i]][[2]][,2]
          }


          tmp_radarPlotStartRow = tmp_descriptionRow + length(dataColumns) + 4 # 3 rows below the end of the [] table
          tmp_radarPlotColIncrease = 6 # Number of cols to match 6 inches of plot width plus a margin

          for(p in 1:length(dataColumns))
          {
            tmp_plotDat <- tmp_currentSolutionMeansStorage[p,-1]
            tmp_maxDat <- as.data.frame( matrix(max(tmp_plotDat), 1, (dim(tmp_plotDat)[2])) )
            tmp_minDat <- as.data.frame( matrix(min(tmp_plotDat), 1, (dim(tmp_plotDat)[2])) )

            names(tmp_maxDat) <- names(tmp_plotDat)
            names(tmp_minDat) <- names(tmp_plotDat)

            tmp_plotDat <- rbind(
                tmp_maxDat
                , tmp_minDat
                , tmp_plotDat
              )

            # print(tmp_plotDat)

            radarchartcirc(
              tmp_plotDat
              , cglty = 3
              , caxislabels = seq(from = floor(tmp_plotDat[2,1]), to = ceiling(tmp_plotDat[1,1]), length.out = 5)
              , title = dataColumns[p]
              , axistype = 1
              , axislabcol = "#222222"
              # , paxislabels
            )


            # Add plot
            insertPlot(tmp_wb, tmp_worksheetName, width = 6, height = 4
              , startRow = tmp_radarPlotStartRow
              , startCol = (p-1) * tmp_radarPlotColIncrease + 1
              , fileType = "png", units = "in", dpi = 300)
          }


          #         # for(x in (tmp_numberStartingCols+1):dim(tmp_clustFitMetricStorage)[2]) {
          #     #   plot(tmp_clustFitMetricStorage[,2], tmp_clustFitMetricStorage[,x], type = 'l', main = names(tmp_clustFitMetricStorage)[x], xlab = "Cluster Number", xaxt = "n")
          #     #   axis(side = 1, at = tmp_clustFitMetricStorage[,2], labels = tmp_clustFitMetricStorage[,2])
          #     #   grid()
          #     #
          #     #   # Add plot
          #     #   insertPlot(tmp_wb, "Cluster Metrics", width = 6, height = 4,
          #     #   startRow = tmp_plotStartRow + tmp_plotRowIncrease * tmp_numPlots, startCol = 2, fileType = "png", units = "in", dpi = 300)
          #     #
          #     #   # Iterate plot count
          #     #   tmp_numPlots = tmp_numPlots + 1
          #     # }
        #   # DELETE
        #   # # Create and fill storage for radar plots
        #   # tmp_inClusterStdDiff <- as.data.frame(matrix(, (tmp_numClustersInSolution), (tmp_numVariables+1))) # Holds: [cluster solutions] x [num variable + 1 (row names)]
        #   # tmp_inClusterStdDiff[,1] <- c(paste0("Cluster ", c(1:tmp_numClustersInSolution)))
        #   # tmp_outClusterStdDiff <- tmp_inClusterStdDiff
        #   #
        #   # names(tmp_inClusterStdDiff) <- c("Cluster", c(paste0("in_", dataColumns)))
        #   # names(tmp_outClusterStdDiff) <- c("Cluster", c(paste0("out_", dataColumns)))
        #
        #
        #   # # Pull out variables in [dataColumns] for current cluster ID within the current cluster solution
        #   # for(tmp_c in 1:tmp_numClustersInSolution)
        #   # {
        #   #   tmp_inClusterStdDiff[tmp_c,-1] <- sapply(clusterData[which(clusterData[,clusterSolutions[s]] == tmp_c), ] %>% select(all_of(dataColumns)), FUN = function(x) {mean(x, na.rm = T)})
        #   #   tmp_outClusterStdDiff[tmp_c,-1] <- sapply(clusterData[which(clusterData[,clusterSolutions[s]] != tmp_c), ] %>% select(all_of(dataColumns)), FUN = function(x) {mean(x, na.rm = T)})
        #   # }
        #
        #   # tmp_inClusterStdDiff[1,-1] <- apply(tmp_inClusterStdDiff[-c(1:2), -1], MARGIN = 2, FUN = function(x) {max(x, na.rm = T)})
        #   # tmp_inClusterStdDiff[2,-1] <- apply(tmp_inClusterStdDiff[-c(1:2), -1], MARGIN = 2, FUN = function(x) {min(x, na.rm = T)})
        #   #
        #   # tmp_outClusterStdDiff[1,-1] <- apply(tmp_outClusterStdDiff[-c(1:2), -1], MARGIN = 2, FUN = function(x) {max(x, na.rm = T)})
        #   # tmp_outClusterStdDiff[2,-1] <- apply(tmp_outClusterStdDiff[-c(1:2), -1], MARGIN = 2, FUN = function(x) {min(x, na.rm = T)})
        #   #
        }

        # # Create radar plot for current cluster
        # if(ifPlot)
        # {
        #   require(fmsb)
        #
        #   # tmp_clusterDescriptionsStorage[[s]][[c]][[2]]
        #
        #   # tmp_currentClusterPlotData <- as.data.frame(matrix(,4, dim(tmp_inClusterStdDiff)[2]))
        #   # names(tmp_currentClusterPlotData) <- c("Var", c(dataColumns))
        #   # tmp_currentClusterPlotData[,1] <- c("max", "min", "cluster_in", "cluster_out")
        #   # tmp_currentClusterPlotData[3,-1] <- tmp_inClusterStdDiff[c,-1]
        #   # tmp_currentClusterPlotData[4,-1] <- tmp_outClusterStdDiff[c,-1]
        #   # tmp_currentClusterPlotData[1,-1] <- sapply(tmp_currentClusterPlotData[3:4,-1], FUN = function(x) {max(x, na.rm = T)})
        #   # tmp_currentClusterPlotData[2,-1] <- sapply(tmp_currentClusterPlotData[3:4,-1], FUN = function(x) {min(x, na.rm = T)})
        #
        #
        #   # Create baseline plot
        #   # radarchart(tmp_currentClusterPlotData[,-1], pfcol = c(
        #   #     adjustcolor( "gray", alpha.f = 0.2)
        #   #     , adjustcolor( "red", alpha.f = 0.2)
        #   #   ), plty = 'solid'
        #   # )
        #   #
        #   # # Add inner sig interval line
        #   # par(new = TRUE)
        #   # radarchart(tmp_currentClusterPlotData[,-1] - 0.2, pfcol = c(
        #   #     adjustcolor( "gray", alpha.f = 0.2)
        #   #     , adjustcolor( "red", alpha.f = 0.2)
        #   #   ), plty = 'dotted'
        #   # )
        #   #
        #   # # Add outer sig interval line
        #   # par(new = TRUE)
        #   # radarchart((tmp_currentClusterPlotData[,-1] + 0.2), pfcol = c(
        #   #     adjustcolor( "gray", alpha.f = 0.2)
        #   #     , adjustcolor( "red", alpha.f = 0.2)
        #   #   ), plty = 'dotted'
        #   # )
        #
        #
        #
        #         # for(x in (tmp_numberStartingCols+1):dim(tmp_clustFitMetricStorage)[2]) {
        #     #   plot(tmp_clustFitMetricStorage[,2], tmp_clustFitMetricStorage[,x], type = 'l', main = names(tmp_clustFitMetricStorage)[x], xlab = "Cluster Number", xaxt = "n")
        #     #   axis(side = 1, at = tmp_clustFitMetricStorage[,2], labels = tmp_clustFitMetricStorage[,2])
        #     #   grid()
        #     #
        #     #   # Add plot
        #     #   insertPlot(tmp_wb, "Cluster Metrics", width = 6, height = 4,
        #     #   startRow = tmp_plotStartRow + tmp_plotRowIncrease * tmp_numPlots, startCol = 2, fileType = "png", units = "in", dpi = 300)
        #     #
        #     #   # Iterate plot count
        #     #   tmp_numPlots = tmp_numPlots + 1
        #     # }
        #
        # }

        # IF not last cluster in solution, iterate table columns
        tmp_currentCol <- tmp_currentCol + 6

      }
    }

    # setwd("/Users/benclaassen/Documents/_Workshop/_Code Utilities/Statistics/MultivariateClusteringPackage/TestOutput")
    saveWorkbook(tmp_wb, paste0("clusterExample_fromDescriptionsFunction ", Sys.time(), ".xlsx"), TRUE)

# -------------------------------------------------------------------------



  }

  return(tmp_clusterDescriptionsStorage)
}

#
# foo = describeClusters(clusterData, uniqueID, clusterSolutions, dataColumns)
# foo
# foo[[7]]
#
# bar = getClusterMetrics(clusterData, uniqueID, clusterSolutions, dataColumns, clusterDistances, clusterMetrics = "", plotMetrics = TRUE, includeDescriptions = F, exportOutput = TRUE, exportSignificantDigits = 3)
