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
# Updated: 2024-03-03
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
## PACKAGE FUNCTION ##
## FUNCTION - describeClusters ##
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
#         - Plot colored distributions of cluster values for each variable with marked means and medians; this will show the distribution
#    - Return results from [FUNCTION - Analyze Clusters]


## FUNCTION - Analyze Clusters
# Inputs:
#    - Data with uniqueID and cluster assignments
# Outputs:
#    - Cluster metric values
#    - Cluster metric plots
#    - Cluster descriptions if desired
#    - Excel file output if desired
# -------------------------------------------------------------------------



# TODO --------------------------------------------------------------------

# Add check for total sample size >2
# Make sure distr plots works with single observation (mean only)

# Add functionality without variance so a set of singletons can be compared

# Any way to look for missing combinations? business opportunity for gap in the market

# Add variable precision and units secondary table
# Make distr plots in GGPlot
# Re-add legend below plot

# Add one-pager describing how to use product

# Add vector to change colors of pos and neg sig
# Add vector to change distr plot colors

# Create structure overview


# Add header page to excel output with metrics if descriptions fcn was called from metrics fcn
# Add header page to excel output with proportions
# Try with clusters of single values for div 0 errors
# Add distr plots for each variable by cluster, one plot per var
# NOTE: If more than 20 clusters, colors are annoying, use [tmp_ifTooManyClustersForColors] from cluster assignment

# Add single variable description for R
# Add confusion matrices for cross-cluster comparison

# Add cluster size table for each solution before cluster breakouts

# Force solutions to end up as 1:n if any clusters are missing in between

# Check for FIX comments!!
# Check for TODO comments!!
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

# Input vars with names instead of col numbers ->
uniqueID = names(clusterData)[1]
clusterSolutions = names(clusterData)[2:6]
# clusterSolutions = sample(names(clusterData)[2:20])
dataColumns = names(clusterData)[26:28]
clusterNamesColumns = ""
exportOutput = TRUE
exportSignificantDigits = 3
calledFromMetrics = FALSE
includeRadarPlots = FALSE
includeDistributionPlots = TRUE


# foo <- describeClusters(clusterData, uniqueID, clusterSolutions, dataColumns = dataColumns, exportOutput = TRUE, exportSignificantDigits = 3, calledFromMetrics = FALSE, includeRadarPlots = TRUE, includeDistributionPlots = TRUE)
# foo <- describeClusters(
#   clusterData = clustDat
#   , uniqueID = "GEOID20"
#   , clusterSolutions = c(2:20)
#   , dataColumns = c("lclzd_JobsAndPopulation_std", "lclzd_NumberOfBorderingRoads_std", "lclzd_MedianValue_std")
#   , includeRadarPlots = TRUE
#   )

clusterDistances <- dist(clusterData %>% select(all_of(dataColumns))
  , method = "euclidean")

clusterFitMetrics = ""

describeClusters <- function(clusterData, uniqueID, clusterSolutions, dataColumns, clusterNamesColumns = "", clusterDistances = "", clusterFitMetrics = "", clusterSolutionToFitMetricsOn = "", exportOutput = TRUE, exportSignificantDigits = 3, includeRadarPlots = FALSE, includeDistributionPlots = TRUE, includeClusterDescriptions = TRUE, includeClusterFitMetrics = TRUE)
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
  #   - [clusterNamesColumns]: a list of names for clustering solutions (one column per solution, use [""] or [0] if no names)
  #   - [dataColumns]: a list of which columns contain the clustering data

  require(tidyverse)
  options(scipen = 9999)

  # Store current R plot margins from par()$mar and reset data after plotting
  # tmp_previousRPlotMarginSettings <- par()$mar

  # Check inputs ----------------------------------------------------------

  # Confirm [clusterData] is a data.frame
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

    # Store both forms of [clusterSolutions] for checking overlap
    tmp_clusterSolutions_char = names(clusterData)[clusterSolutions]
    tmp_clusterSolutions_num = clusterSolutions

  } else if(class(clusterSolutions)[1] == "character") # Check for [clusterSolutions] type
  {
    if( !all(clusterSolutions %in% names(clusterData)) )
    {
      stop("[clusterSolutions] names must be in [clusterData] names")
    }

    # Store both forms of [clusterSolutions] for checking overlap
    tmp_clusterSolutions_char = clusterSolutions
    tmp_clusterSolutions_num = sapply(clusterSolutions, FUN = function(x) { which( names(clusterData) == x ) })

  } else # Else not a numeric or character entry
  {
    stop("[clusterSolutions] must be a list of numbers or strings")
  }


  # Confirm [clusterNamesColumns] are column numbers or column names in [clusterNamesColumns]
  if(class(clusterNamesColumns)[1] == "integer" | class(clusterNamesColumns)[1] == "numeric") # Check for [clusterNamesColumns] type
  {

    # Check if [clusterNamesColumns] are outside the dimensions of [clusterData] or are '0'
    if(any(clusterNamesColumns > dim(clusterData)[2]) | any(clusterNamesColumns < -1))
    {
      stop("[clusterNamesColumns] column numbers must be within the dimensions of [clusterData]")
    }

    # Store both forms of [clusterNamesColumns] for checking overlap
    tmp_clusterNamesColumns_char = NULL
    tmp_clusterNamesColumns_num = NULL

    for(n in clusterNamesColumns)
    {
      if(n == 0)
      {
        tmp_clusterNamesColumns_char = c(tmp_clusterNamesColumns_char, "")
        tmp_clusterNamesColumns_num = c(tmp_clusterNamesColumns_num, 0)
      } else
      {
        tmp_clusterNamesColumns_char = c(tmp_clusterNamesColumns_char, names(clusterData)[n])
        tmp_clusterNamesColumns_num = c(tmp_clusterNamesColumns_num, n)
      }
    }


  } else if(class(clusterNamesColumns)[1] == "character") # Check for [clusterNamesColumns] type
  {
    if( !all(clusterNamesColumns[which(clusterNamesColumns != "")] %in% names(clusterData)) )
    {
      stop("[clusterNamesColumns] names must be in [clusterData] names")
    }

    # Store both forms of [clusterNamesColumns] for checking overlap
    tmp_clusterNamesColumns_char = NULL
    tmp_clusterNamesColumns_num = NULL

    for(n in clusterNamesColumns)
    {
      if(n == "")
      {
        tmp_clusterNamesColumns_char = c(tmp_clusterNamesColumns_char, "")
        tmp_clusterNamesColumns_num = c(tmp_clusterNamesColumns_num, 0)
      } else
      {
        tmp_clusterNamesColumns_char = c(tmp_clusterNamesColumns_char, n)
        tmp_clusterNamesColumns_num = c( tmp_clusterNamesColumns_num, which(names(clusterData) == n) )
      }
    }

  } else # Else not a numeric or character entry
  {
    stop("[clusterNamesColumns] must be a list of numbers or strings")
  }


  # Confirm [dataColumns] are column numbers or column names in [clusterData]
  if(class(dataColumns)[1] == "integer") # Check for [dataColumns] type
  {
    # Check if [dataColumns] are outside the dimensions of [clusterData]
    if(any(dataColumns > dim(clusterData)[2]) | any(dataColumns < 1))
    {
      stop("[dataColumns] column numbers must be within the dimensions of [clusterData]")
    }

    # Store both forms of [dataColumns] for checking overlap
    tmp_dataColumns_char = names(clusterData)[dataColumns]
    tmp_dataColumns_num = dataColumns

  } else if(class(dataColumns)[1] == "character") # Check for [dataColumns] type
  {
    if( !all(dataColumns %in% names(clusterData)) )
    {
      stop("[dataColumns] names must be in [clusterData] names")
    }

    # Store both forms of [dataColumns] for checking overlap
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
        , tmp_clusterNamesColumns_num[which(tmp_clusterNamesColumns_num != 0)] # Ignore any blanks in the cluster names
        , tmp_dataColumns_num
      )
  ))))
  {
    stop("[uniqueID], [clusterSolutions], [clusterNamesColumns] and [dataColumns] must all be mutually exclusive")
  }

  # Once duplicates are confirmed not to exist, store all column names in label form
  uniqueID <- tmp_uniqueID_char
  clusterSolutions <- tmp_clusterSolutions_char
  clusterNamesColumns <- tmp_clusterNamesColumns_char
  dataColumns <- tmp_dataColumns_char

  # Subset [clusterData] to [uniqueID], [clusterSolutions], [clusterNamesColumns], and [dataColumns] only
  if(any(clusterNamesColumns != ""))
  {
    clusterData <- clusterData %>% select(
      all_of(uniqueID)
      , all_of(clusterSolutions)
      , all_of(clusterNamesColumns[which(clusterNamesColumns != "")])
      , all_of(dataColumns)
    )
  } else
  {
      clusterData <- clusterData %>% select(
      all_of(uniqueID)
      , all_of(clusterSolutions)
      , all_of(dataColumns)
    )
  }

  # Reorder [clusterSolutions] and [clusterNamesColumns] by number of clusters (i.e. max cluster ID by column)
  if(any(clusterNamesColumns != ""))
  {
    clusterNamesColumns <- clusterNamesColumns[
        order(
          sapply(clusterData %>% select(all_of(clusterSolutions)), max)
          )
      ]

    clusterSolutions <- clusterSolutions[
      order(
        sapply(clusterData %>% select(all_of(clusterSolutions)), max)
        )
    ]

    clusterNames <- as.data.frame(t(clusterNamesColumns))
    names(clusterNames) <- clusterSolutions

  } else
  {
    clusterSolutions <- clusterSolutions[
      order(
        sapply(clusterData %>% select(all_of(clusterSolutions)), max)
        )
    ]

    clusterNames <- as.data.frame(matrix("", 1,length(clusterSolutions)))
    names(clusterNames) <- clusterSolutions
  }




  # Begin descriptions ----------------------------------------------------
  if(includeClusterDescriptions)
  {
    # Set number variables in [clusterData]
    tmp_numVariables <- length(dataColumns)

    # Create a description for each solution
    for(tmp_clustSolution in clusterSolutions)
    {
      print(paste0("Generating cluster descriptions for solution [", tmp_clustSolution,"]"))

      # Set cluster names if available
      if(clusterNames[1,tmp_clustSolution] == "")
      {
        tmp_currentSolutionNames <- ""
      } else
      {
        # Set current cluster solution name col based on ID col
        tmp_namesCol <- clusterNames[1, tmp_clustSolution]

        # Get unique pairs of cluster IDs/names
        tmp_currentSolutionNames <- unique(clusterData[,c(tmp_namesCol, tmp_clustSolution)])

        # Confirm cluster names are unique to cluster IDs for current solution
        if(length(unique(tmp_currentSolutionNames[,1])) !=  dim(tmp_currentSolutionNames)[1] | length(unique(tmp_currentSolutionNames[,2])) !=  dim(tmp_currentSolutionNames)[1] )
        {
          print(tmp_currentSolutionNames)
          stop("[clusterSolutions] IDs must uniquely match given cluster names")
        }
      }


      # Iterate over each group in the current solution
      for(tmp_currentCluster in 1:length(table(clusterData[, tmp_clustSolution])) )
      {
        # Initialize storage for current cluster ID within current cluster solution
        tmp_clusterProportions <- as.data.frame(matrix(, 1, 5)) # Cluster size and proportion
        names(tmp_clusterProportions) <- c("Cluster Number", "Total Number of Clusters", "Number of Observations", "Proportion", "Cluster Name")
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
        # Cluster name
        if(length(tmp_currentSolutionNames) == 2) # This check if there are names for clusters, even if some are blanks ("")
        {
          tmp_clusterProportions[1,5] <- tmp_currentSolutionNames[which(tmp_currentSolutionNames[,2] == tmp_currentCluster),1]
        } else
        {
          tmp_clusterProportions[1,5] <- ""
        }

        # Pull out variables in [dataColumns] for current cluster ID within the current cluster solution
        tmp_inClusterStdDiffData <- clusterData[which(clusterData[,tmp_clustSolution] == tmp_currentCluster), ] %>% select(all_of(dataColumns))
        tmp_outClusterStdDiffData <- clusterData[which(clusterData[,tmp_clustSolution] != tmp_currentCluster), ] %>% select(all_of(dataColumns))

        # Variable
        tmp_clusterVarDescriptions[,1] <- dataColumns # Variable names passed by user

        # In-Cluster Means
        tmp_clusterVarDescriptions[,2] <- sapply( # Vector of means for each variable in in-cluster data
          tmp_inClusterStdDiffData, FUN = function(x) {mean(x, na.rm = T)}
        )

        # In-/Out-Cluster Mean Diffs
        tmp_clusterVarDescriptions[,3] <- tmp_clusterVarDescriptions[,2] -
          sapply( # Vector of means for each variable in out-cluster data
            tmp_outClusterStdDiffData, FUN = function(x) {mean(x, na.rm = T)}
          )

        # Calculate In-/Out-Cluster Pooled Variance
        # NOTE: Only ever two 'samples' because we are looking at in-/out-cluster groups
        # NOTE: Pooled variance is ( (n_1-1)*var_1 + (n_2-1)*var_2 ) / (n_1 + n_2 - 2) <https://en.wikipedia.org/wiki/Pooled_variance>

        if(dim(tmp_inClusterStdDiffData)[1] > 1 & dim(tmp_outClusterStdDiffData)[1] > 1)
        {
          tmp_currentPooledVariance <- (
            ( (dim(tmp_inClusterStdDiffData)[1]  - 1) * sapply(tmp_inClusterStdDiffData,  FUN = function(x) {var(x, na.rm = T)}) ) +
            ( (dim(tmp_outClusterStdDiffData)[1] - 1) * sapply(tmp_outClusterStdDiffData, FUN = function(x) {var(x, na.rm = T)}) )
          ) / (
            dim(tmp_inClusterStdDiffData)[1] + dim(tmp_outClusterStdDiffData)[1] - 2
          )
        } else # Else, at least one of the in-/out-cluster datasets has 1 observation
        {
          if(dim(tmp_inClusterStdDiffData)[1] == 1)
          {
            tmp_currentPooledVariance <- (
              rep(0, dim(tmp_inClusterStdDiffData)[2]) +
              ( (dim(tmp_outClusterStdDiffData)[1] - 1) * sapply(tmp_outClusterStdDiffData, FUN = function(x) {var(x, na.rm = T)}) )
            ) / (
              dim(tmp_inClusterStdDiffData)[1] + dim(tmp_outClusterStdDiffData)[1] - 2
            )
          }

          if(dim(tmp_outClusterStdDiffData)[1] == 1)
          {
            tmp_currentPooledVariance <- (
              ( (dim(tmp_inClusterStdDiffData)[1]  - 1) * sapply(tmp_inClusterStdDiffData,  FUN = function(x) {var(x, na.rm = T)}) ) +
              ( rep(0, dim(tmp_outClusterStdDiffData)[2]) )
            ) / (
              dim(tmp_inClusterStdDiffData)[1] + dim(tmp_outClusterStdDiffData)[1] - 2
            )
          }
        }

        # In-/Out-Cluster Std Mean Diffs
        tmp_clusterVarDescriptions[,4] <- tmp_clusterVarDescriptions[,3] / sqrt(tmp_currentPooledVariance)

        # Pooled Variance
        tmp_clusterVarDescriptions[,5] <- sqrt(tmp_currentPooledVariance)

        # In-/Out-Cluster Mean Diffs
        tmp_clusterVarDescriptions[,6] <- sapply( # Vector of means for each variable in out-cluster data
            tmp_outClusterStdDiffData, FUN = function(x) {mean(x, na.rm = T)}
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

    } # End loop over each solution to create descriptions
  } # End loop for [includeClusterDescriptions]



  # Begin metrics ---------------------------------------------------------
  if(includeClusterFitMetrics)
  {
    require(fpc)

    if(clusterSolutionToFitMetricsOn[1] == "")
    {
      metricClusterSolutions <- clusterSolutions
      tmp_numClustSolutions <- length(metricClusterSolutions)
    } else
    {
      metricClusterSolutions <- clusterSolutionToFitMetricsOn
      tmp_numClustSolutions <- length(metricClusterSolutions)
    }


    if(clusterFitMetrics[1] == "")
    {
      clusterFitMetrics = c(
        "within.cluster.ss" # Within-cluster sum of squares
      , "avg.silwidth" # Average silhouette width
      , "ch" # Calinski-Harabasz index
      , "wb.ratio" # Within/between SSE ratio
      )
    } else # Check that each given cluster metric is valid
    {
      if(
        !all(
          clusterFitMetrics %in%
          c("n", "cluster.number", "cluster.size", "min.cluster.size", "noisen", "diameter", "average.distance", "median.distance", "separation", "average.toother", "separation.matrix", "ave.between.matrix", "average.between", "average.within", "n.between", "n.within", "max.diameter", "min.separation", "within.cluster.ss", "clus.avg.silwidths", "avg.silwidth", "g2", "g3", "pearsongamma", "dunn", "dunn2", "entropy", "wb.ratio", "ch", "cwidegap", "widestgap", "sindex", "corrected.rand", "vi")
        )
      )
      {
        stop(paste0("Cluster fit metrics must be from {fpc} package fcn [cluster.stats] \n -> Valid entries: n, cluster.number, cluster.size, min.cluster.size, noisen, diameter, average.distance, median.distance, separation, average.toother, separation.matrix, ave.between.matrix, average.between, average.within, n.between, n.within, max.diameter, min.separation, within.cluster.ss, clus.avg.silwidths, avg.silwidth, g2, g3, pearsongamma, dunn, dunn2, entropy, wb.ratio, ch, cwidegap, widestgap, sindex, corrected.rand, vi"))
      }
    }

    # Create cluster fit metrics storage matrix
    tmp_clustFitMetricStorage <- as.data.frame(matrix(, tmp_numClustSolutions, (length(clusterFitMetrics) + 2)) ) # Storage matrix for cluster fit metrics; no metrics for 1 cluster so n-1
    names(tmp_clustFitMetricStorage) <- c("ClusterSolutionName", "NumClusters"
        , clusterFitMetrics
      )

    # Add cluster solution names to storage
    tmp_clustFitMetricStorage[,1] <- metricClusterSolutions
    # Fill in 'Cluster Number' column with max of cluster IDs for each solution set, i.e. number of clusters in given solution
    tmp_clustFitMetricStorage[,2] <- sapply(clusterData %>% select(all_of(metricClusterSolutions)), max)

    # Calculate cluster metrics for each [metricClusterSolutions]
    for(tmp_clustSolution in metricClusterSolutions)
    {
      print(paste0("Fitting metrics for [k=", tmp_clustFitMetricStorage %>% filter(ClusterSolutionName == tmp_clustSolution) %>% select(NumClusters), "]"))

      tmp_clustIDs <- unlist(clusterData %>% select(all_of(tmp_clustSolution))) # Store cluster IDs
      tmp_clustStats <- cluster.stats(clusterDistances, tmp_clustIDs) # Get cluster statistics

      tmp_numberStartingCols <- 2 # The (2) starting columns are 'ClusterSolutionName' and 'NumClusters'. This is the offset for downstream column selection

      tmp_currentSolutionRow <- which(metricClusterSolutions == tmp_clustSolution) # Set row of [tmp_clustFitMetricStorage] for current cluster solution [tmp_clustSolution]

      # Confirm 'cluster.number' == tmp_clustFitMetricStorage[,2]
      if(!tmp_clustFitMetricStorage[tmp_currentSolutionRow,2] == tmp_clustStats$cluster.number)
      {
        stop("Number of clusters in solution do not match 'cluster.stats' output")
      }

      # Store desired cluster metrics
      tmp_clustFitMetricStorage[tmp_currentSolutionRow, (1:length(clusterFitMetrics) + tmp_numberStartingCols)] <- tmp_clustStats[ clusterFitMetrics ]

      rm(tmp_clustStats)
    }
  }

  # Export output to excel ------------------------------------------------
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
      exportSignificantDigits <- 1
    }

    # Set which table rows to write to
    tmp_titleRow <- 2
    tmp_proportionRow <- 4
    tmp_descriptionRow <- 7

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
    writeData(tmp_wb, "Parameters", 0.5, startCol = 3, startRow = 2)


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

    # Include cluster metrics if requested
    if(includeClusterFitMetrics)
    {
      # tmp_clustFitMetricStorage
      # Create cluster metrics and plots workbook
      addWorksheet(tmp_wb, "Cluster Metrics")
      writeData(tmp_wb, "Cluster Metrics", tmp_clustFitMetricStorage, startCol = 2, startRow = 2)

      # Set rows for spacing out metrics plots
      tmp_plotStartRow = dim(tmp_clustFitMetricStorage)[1] + 4 # 3 rows below the end of the [tmp_clustFitMetricStorage] table
      tmp_plotRowIncrease = 22 # Number of rows to match 4 inches of plot height plus a margin
      tmp_numPlots = 0 # Initialize plot count to 0

      # Iterate over fit metrics
      for(x in (tmp_numberStartingCols+1):dim(tmp_clustFitMetricStorage)[2])
      {
        plot(tmp_clustFitMetricStorage[,2], tmp_clustFitMetricStorage[,x], type = 'l', main = names(tmp_clustFitMetricStorage)[x], xlab = "Cluster Number", xaxt = "n", ylab = "Metric Value")
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
      # Set number of clusters in current solution
      tmp_numClustersInSolution <- tmp_clusterDescriptionsStorage[[s]][[1]][[1]][1,2] # Returns second entry in first proportion table for current solution [s]

      # Add worksheet
      tmp_worksheetName <- paste0(tmp_numClustersInSolution, " Clusters (", clusterSolutions[s],")")
      addWorksheet(tmp_wb, tmp_worksheetName)

      # Set first table column
      tmp_currentCol <- 2

      # Initialize cluster names storage
      tmp_clusterSolution_clusterNames <- as.data.frame(matrix(, tmp_numClustersInSolution, 2))
      names(tmp_clusterSolution_clusterNames) <- c('clusterName', 'clusterID')

      # Set current solution name
      tmp_plotClusterSolution <- clusterSolutions[s]

      # Initialize cluster colors
      tmp_numCluster <- length(unique(clusterData[,tmp_plotClusterSolution]))
      # First eight colors from <https://developer.r-project.org/Blog/public/2019/11/21/a-new-palette-for-r/>
      tmp_firstEightColors <- c("#000000", "#df536b", "#61d04f", "#2297e6", "#28e2e5", "#cd0bbc", "#f5c710", "#626262")
      # Next twelve from a combination of [RColorBrewer] library
      tmp_firstTwentyColors <- c(tmp_firstEightColors
        ,"#276419", "#2D004B", "#D53E4F", "#542788", "#F46D43", "#66C2A5", "#7F3B08", "#7FBC41", "#5E4FA2", "#9E0142", "#DE77AE", "#FDB863")

      if(tmp_numCluster <= 8)
      {
        tmp_clusterColors <- tmp_firstEightColors[1:tmp_numCluster]
        tmp_ifTooManyClustersForColors = FALSE
      } else if(tmp_numCluster <= 20)
      {
        tmp_clusterColors <- tmp_firstTwentyColors[1:tmp_numCluster]
        tmp_ifTooManyClustersForColors = FALSE
      } else
      {
        tmp_clusterColors <- tmp_firstTwentyColors
        tmp_ifTooManyClustersForColors = TRUE
      }


      ## OLD COLOR CODE: up to 35 clusters ##
      # if(tmp_numCluster <= 2)
      # {
      #   tmp_clusterColors <- c("#000000", "#ff0000")
      #   tmp_ifTooManyClustersForColors = FALSE
      # } else if(tmp_numCluster <= 13)
      # {
      #   tmp_clusterColors <- c("#000000", "#ff0000", brewer.pal((tmp_numCluster-2), "Spectral"))
      #   tmp_ifTooManyClustersForColors = FALSE
      # } else if(tmp_numCluster <= 24)
      # {
      #   tmp_clusterColors <- c("#000000", "#ff0000", brewer.pal((11), "Spectral"), brewer.pal((tmp_numCluster-13), "PuOr"))
      #   tmp_ifTooManyClustersForColors = FALSE
      # } else if(tmp_numCluster <= 35)
      # {
      #   tmp_clusterColors <- c("#000000", "#ff0000", brewer.pal((11), "Spectral"), brewer.pal((11), "PuOr"), brewer.pal((tmp_numCluster-24), "PiYG"))
      #   tmp_ifTooManyClustersForColors = FALSE
      # } else
      # {
      #   tmp_clusterColors <- c("#000000", "#ff0000", brewer.pal((11), "Spectral"), brewer.pal((11), "PuOr"), brewer.pal(11, "PiYG"))
      #   tmp_ifTooManyClustersForColors = TRUE
      # }


      # tmp_clusterColors <- c(palette(rainbow(tmp_numCluster)))
      # tmp_clusterColors <- c("black", brewer.pal((tmp_numCluster-1), "BrBG"))

      for(c in 1:tmp_numClustersInSolution)
      {
        # Write cluster number
        writeData(tmp_wb, tmp_worksheetName, paste0("Cluster ", tmp_clusterDescriptionsStorage[[s]][[c]][[1]][1,1]), startCol = tmp_currentCol, startRow = tmp_titleRow)

        # Write cluster name and color
        if(tmp_clusterDescriptionsStorage[[s]][[c]][[1]][1,5] != "")
        {
          # Cluster name
          writeData(tmp_wb, tmp_worksheetName, tmp_clusterDescriptionsStorage[[s]][[c]][[1]][1,5], startCol = tmp_currentCol + 1, startRow = tmp_titleRow)
          # Cluster color
          # tmp_style_clusterColor <- createStyle(fgFill = tmp_clusterColors[tmp_clusterDescriptionsStorage[[s]][[c]][[1]][1,1]])
          # addStyle(tmp_wb, tmp_worksheetName, style=tmp_style_clusterColor, cols = tmp_currentCol + 2, rows = tmp_titleRow, stack = F)
        } else
        {
          # Cluster color
          # tmp_style_clusterColor <- createStyle(fgFill = tmp_clusterColors[tmp_clusterDescriptionsStorage[[s]][[c]][[1]][1,1]])
          # addStyle(tmp_wb, tmp_worksheetName, style=tmp_style_clusterColor, cols = tmp_currentCol + 1, rows = tmp_titleRow, stack = F)
        }
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
        conditionalFormatting(tmp_wb, tmp_worksheetName, cols = tmp_currentCol + 3, rows = c(8:c(8+length(dataColumns))), rule = " > Parameters!$C$2", type = "expression", style = tmp_style_posSig)
        conditionalFormatting(tmp_wb, tmp_worksheetName, cols = tmp_currentCol + 3, rows = c(8:c(8+length(dataColumns))), rule = " < (-1)*Parameters!$C$2", type = "expression", style = tmp_style_negSig)

        # Store cluster names
        tmp_clusterSolution_clusterNames[c,1] <- tmp_clusterDescriptionsStorage[[s]][[c]][[1]][1,5]
        tmp_clusterSolution_clusterNames[c,2] <- tmp_clusterDescriptionsStorage[[s]][[c]][[1]][1,1]


        # IF not last cluster in solution, iterate table columns
        tmp_currentCol <- tmp_currentCol + 6

      } # End loop for proportions and description information for each cluster within current solution for [exportOutput]

      # Keep cluster names if any are present
      if( all(tmp_clusterSolution_clusterNames[,1] == "") )
      {
        tmp_clusterSolution_clusterNames = ""
      }

      # Plot if requested
      if(includeRadarPlots | includeDistributionPlots)
      {
        # Set start rows for plots depending on if both sets of plots are present or not
        if(includeRadarPlots)
        {
          tmp_radarPlotStartRow = tmp_descriptionRow + length(dataColumns) + 4 # 3 rows below the end of the descriptions table
          tmp_radarPlotColIncrease = 7 # Number of cols to match 6 inches of plot width plus a margin

          # If there are also distribution plots requested, set those values based on the radar plot start row value
          if(includeDistributionPlots & tmp_numClustersInSolution >= 3) # If there are 3 or more clusters, than radar plots are viable for this solution [s]
          {
            tmp_distrPlotStartRow = tmp_radarPlotStartRow + 24 # [tmp_radarPlotStartRow] plus 24 rows to skip radar plots (size of plot at 4 inches high)
            tmp_distrPlotColIncrease = 7 # Number of cols to match 6 inches of plot width plus a margin
          } else # Else there are too few data points for radar plots, so only a notice will be inserted in lieu of these plots
          {
            tmp_distrPlotStartRow = tmp_radarPlotStartRow + 4 # [tmp_radarPlotStartRow] plus 3 rows to skip the notice that there are no radar plots for solutions with fewer than 3 clusters
            tmp_distrPlotColIncrease = 7 # Number of cols to match 6 inches of plot width plus a margin
          }

        } else
        {
          tmp_distrPlotStartRow = tmp_descriptionRow + length(dataColumns) + 4 # 3 rows below the end of the descriptions table
          tmp_distrPlotColIncrease = 7 # Number of cols to match 6 inches of plot width plus a margin
        }

        # If any plotting, fill means table. Use this table to establish variable plot order as well
        # Initialize variable means by cluster for current solution
        tmp_currentSolutionMeansStorage <- as.data.frame(matrix(, length(dataColumns), (1+tmp_numClustersInSolution)))

        # Set proper names if given
        if( !all(tmp_clusterSolution_clusterNames == "") )
        {
          names(tmp_currentSolutionMeansStorage) <- c("Variable",  tmp_clusterSolution_clusterNames[order(tmp_clusterSolution_clusterNames[,2]),1])
        } else
        {
          names(tmp_currentSolutionMeansStorage) <- c("Variable", paste0("k=", c(1:tmp_numClustersInSolution)) )
        }

        # Set first col to variable names
        tmp_currentSolutionMeansStorage[,1] <- sort(dataColumns)

        # Pull means data for use in plotting
        for(i in 1:tmp_numClustersInSolution)
        {
          # Get means from storage
          tmp_meansData <- tmp_clusterDescriptionsStorage[[s]][[i]][[2]][,c(1:2)]
          tmp_meansData[order(tmp_meansData[,1]),]
          # Write sorted data to i+1 to account for labels column
          tmp_currentSolutionMeansStorage[,i+1] <- tmp_meansData[order(tmp_meansData[,1]),2]

          # TESTING
          # t(as.data.frame(clusterData %>% group_by(borough) %>% summarize(
          #   meanDem = mean(bg_percVotesDem, na.rm = T)
          #   , meanOther = mean(bg_percVotesOther, na.rm = T)
          #   , meanRep = mean(bg_percVotesRep, na.rm = T)
          #   , meanAge = mean(MedianAge, na.rm = T)
          #   , meanIncome = mean(MedianHHIncome, na.rm = T)
          #   , meanBache = mean(percBachelorsPlus, na.rm = T)
          #   , meanDrove = mean(percDroveToWork, na.rm = T)
          #   , meanFamilyPov = mean(percFamilyHHinPoverty, na.rm = T)
          #   , meanHHPov = mean(percHHinPoverty, na.rm = T)
          #   , meanMarried = mean(percMarriedHHs, na.rm = T)
          #   )))
        }

        # If [includeRadarPlots], make radar plots
        if(includeRadarPlots)
        {
          # Radar charts must have 3 or more points. If only two clusters are present, skip charting for this solution
          if(tmp_numClustersInSolution < 3)
          {
            writeData(tmp_wb, tmp_worksheetName, "NOTE: No radar charts are made for fewer than 3 clusters", startRow = tmp_radarPlotStartRow, startCol = 2)
          } else
          {
            # Library for radar plots
            require(fmsb)

            for(p in 1:dim(tmp_currentSolutionMeansStorage)[1])
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


              radarchartcirc(
                tmp_plotDat
                , cglty = 3
                , caxislabels = round(seq(from = tmp_plotDat[2,1], to = tmp_plotDat[1,1], length.out = 5),3)
                , title = tmp_currentSolutionMeansStorage[p,1]
                , axistype = 1
                , axislabcol = "#222222"
                , pcol = 'black'
                # , paxislabels
              )


              # Add plot
              insertPlot(tmp_wb, tmp_worksheetName, width = 6, height = 4
                , startRow = tmp_radarPlotStartRow
                , startCol = (p-1) * tmp_radarPlotColIncrease + 1
                , fileType = "png", units = "in", dpi = 300)

            }

          } # End multivar radar plots
        } # End [includeRadarPlots]

        # If [includeDistributionPlots], plot distribution plots
        if(includeDistributionPlots)
        {
          # If there are more clusters than colors assigned (max of 20), then print warning and change plot row to account for it
          if(tmp_ifTooManyClustersForColors)
          {
            writeData(tmp_wb, tmp_worksheetName, "NOTE: The number of clusters exceeds the plotting capability (20 cluster maximum). Clusters above this number are excluded from plots.", startRow = tmp_distrPlotStartRow, startCol = 2)
            tmp_distrPlotStartRow = tmp_distrPlotStartRow + 2
          }

          # # Create legend graphic
          # # plot(5,5, xlim = c(-1,1), ylim = c(-1,1), xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
          # plot(NULL, xaxt = 'n', yaxt = 'n', xlab = '', ylab = '', bty = 'n', xlim = c(0,1), ylim = c(0,1))
          # legend("center", legend = c(tmp_clusterSolution_clusterNames[,1]), col = c(tmp_clusterColors), lty = 1, bty = 'n')
          # mtext("Clusters", at = 0.2, cex = 1.25)
          #
          # # Add plot
          # insertPlot(tmp_wb, tmp_worksheetName, width = 6, height = 4
          # , startRow = tmp_distrPlotStartRow
          # , startCol = (p-1) * tmp_distrPlotColIncrease + 1
          # , fileType = "png", units = "in", dpi = 300)
          #
          # # IF not last cluster in solution, iterate table columns
          # tmp_currentPlotCol <- tmp_currentPlotCol + 6

          # NOTE: This function only has enough colors to plot 20 plots
          tmp_numPlottableClustersInSolution <- min(tmp_numClustersInSolution, 20) # Set the clusters to plot to be 20 if actual solution has more

          # Iterate over each variable and create distribution plots
          for(tmp_currentDistrPlotVariableNumber in 1:dim(tmp_currentSolutionMeansStorage)[1])
          {

            # Iterate over clusters to find max density
            tmp_maxDensity_y <- 0
            tmp_maxDensity_x <- 0
            # Get data for current var [tmp_currentDistrPlotVariableNumber] across all clusters in current solution [tmp_plotClusterSolution]
            tmp_densityParameterizationData <- clusterData %>% select(all_of(tmp_plotClusterSolution), all_of(tmp_currentSolutionMeansStorage[tmp_currentDistrPlotVariableNumber,1]))

            for(tmp_clustInCurrentSolution in 1:tmp_numPlottableClustersInSolution)
            {
              # Check density if cluster has 2 or more observations
              if( dim(tmp_densityParameterizationData[which(tmp_densityParameterizationData[,tmp_plotClusterSolution] == tmp_clustInCurrentSolution), ])[1] > 1 )
              {
                tmp_filteredDensityParameterizationData <- tmp_densityParameterizationData[which(tmp_densityParameterizationData[,tmp_plotClusterSolution] == tmp_clustInCurrentSolution), tmp_currentSolutionMeansStorage[tmp_currentDistrPlotVariableNumber,1]]
                # Get density and save maximums if they are higher than previous maxs
                tmp_clusterDen <- density(
                  tmp_filteredDensityParameterizationData
                  , na.rm = TRUE
                  , from = min(tmp_filteredDensityParameterizationData, na.rm = T)
                  , to = max(tmp_filteredDensityParameterizationData, na.rm = T)
                )

                if(max(tmp_clusterDen$x) > tmp_maxDensity_x) {tmp_maxDensity_x = max(tmp_clusterDen$x)}
                if(max(tmp_clusterDen$y) > tmp_maxDensity_y) {tmp_maxDensity_y = max(tmp_clusterDen$y)}
              }
            }

            # If all clusters have a single value, set [tmp_maxDensity] to 1
            if(tmp_maxDensity_y == 0)
            {
              tmp_maxDensity_y = 1
            }

            if(tmp_maxDensity_x == 0)
            {
              tmp_maxDensity_x = 1
            }

            # Write table for legend - THIS COMES AFTER THE PLOT IN THE FINAL OUTPUT
            # NOTE: Getting a good legend for the cluster names and means was a
            #    real pain. Dynamic location was tricky and putting the legend
            #    below the plot gave a double-spaced legend when exported to excel.
            #    In addition putting the legend below the plot turned up a bunch of
            #    errors from changing the margin to include a longer legend. I
            #    dropped this approach in favor of writing the colors, names, and
            #    means to a table below each plot.


            # Write variable name for legend table
            writeData(tmp_wb, tmp_worksheetName
              , tmp_currentSolutionMeansStorage[tmp_currentDistrPlotVariableNumber,1]
              , startCol = (tmp_currentDistrPlotVariableNumber-1) * tmp_distrPlotColIncrease + 1
              , startRow = tmp_distrPlotStartRow + 24)

            addStyle(tmp_wb, tmp_worksheetName, style=tmp_style_clusterHeader
              , rows = tmp_distrPlotStartRow + 24
              , cols = (tmp_currentDistrPlotVariableNumber-1) * tmp_distrPlotColIncrease + 1)

            # Write header for legend table
            tmp_distrPlotTableHeader <- as.data.frame(matrix("", 1, 3))
            names(tmp_distrPlotTableHeader) <- c("Cluster Color", "Cluster Name", "Mean")
            writeData(tmp_wb, tmp_worksheetName
              , tmp_distrPlotTableHeader
              , startCol = (tmp_currentDistrPlotVariableNumber-1) * tmp_distrPlotColIncrease + 1
              , startRow = tmp_distrPlotStartRow + 25)

            addStyle(tmp_wb, tmp_worksheetName, style=tmp_style_bold
              , rows = tmp_distrPlotStartRow + 25
              , cols = ((tmp_currentDistrPlotVariableNumber-1) * tmp_distrPlotColIncrease + 1):((tmp_currentDistrPlotVariableNumber-1) * tmp_distrPlotColIncrease + 3))


            # Iterate over each cluster for the current variable and plot the densities
            for(tmp_clustInCurrentSolution in 1:tmp_numPlottableClustersInSolution)
            {

              # If not first, set plot to add new data
              if(tmp_clustInCurrentSolution > 1)
              {
                par(new = TRUE)
              } else # Else, if first, add extra space to plot for legend
              {
                # par(mar=c((5.1 + tmp_numPlottableClustersInSolution), 4.1, 4.1, 2.1), xpd=TRUE) # Note: the bottom margin needs: (1 line for each cluster) - (5.1 to space below the x-axis and for bottom space)
              }

              # If cluster has 2 or more observations then plot density, else only plot mean
              if( dim(tmp_densityParameterizationData[which(tmp_densityParameterizationData[,tmp_plotClusterSolution] == tmp_clustInCurrentSolution), ])[1] > 1 )
              {
                tmp_densityData = tmp_densityParameterizationData[which(tmp_densityParameterizationData[,tmp_plotClusterSolution] == tmp_clustInCurrentSolution), tmp_currentSolutionMeansStorage[tmp_currentDistrPlotVariableNumber,1]]

                # Set temporary density
                tmp_clusterDen <- density(tmp_densityData
                  , na.rm = T
                  , from = min(tmp_densityData, na.rm = TRUE)
                  , to = max(tmp_densityData, na.rm = TRUE)
                )

                # Plot density
                plot(tmp_clusterDen$x, tmp_clusterDen$y, type = 'l', ylim = c(0, tmp_maxDensity_y), xlim = c(0,tmp_maxDensity_x), col = tmp_clusterColors[tmp_clustInCurrentSolution], xlab = tmp_currentSolutionMeansStorage[tmp_currentDistrPlotVariableNumber,1], ylab = "Relative Density", main = paste0("Distribution plot of [", tmp_currentSolutionMeansStorage[tmp_currentDistrPlotVariableNumber,1], "]"))
              }

              # Add mean
              tmp_varMean <- tmp_currentSolutionMeansStorage[tmp_currentDistrPlotVariableNumber, tmp_clustInCurrentSolution+1]
              abline(v = tmp_varMean, lty = 2, col = tmp_clusterColors[tmp_clustInCurrentSolution], xpd = FALSE)

              # # Add text for mean, include cluster name if present
              # if( all(tmp_clusterSolution_clusterNames == "") )
              # {
              #   # text(tmp_varMean, tmp_maxDensity*0.9, labels = paste0("k=", tmp_clustInCurrentSolution, "; mean: ", round(tmp_varMean,3)), col = tmp_clusterColors[tmp_clustInCurrentSolution], pos = 4)
              #   mtext(text = paste0("k=", tmp_clustInCurrentSolution, "; mean: ", round(tmp_varMean,3)), col = tmp_clusterColors[tmp_clustInCurrentSolution], side = 3, at = tmp_varMean, las = 2)
              # } else
              # {
              #   # mtext(tmp_varMean, tmp_maxDensity*1.2, labels = paste0(, "; mean: ", round(tmp_varMean,3)))
              #   mtext(text = paste0(tmp_clusterSolution_clusterNames[tmp_clustInCurrentSolution,1], "; mean: ", round(tmp_varMean,3)), col = tmp_clusterColors[tmp_clustInCurrentSolution], side = 3, at = tmp_varMean, las = 2)
              # }

              # Add color tile to legend table
              tmp_style_clusterColor <- createStyle(fgFill = tmp_clusterColors[tmp_clustInCurrentSolution])
              addStyle(tmp_wb, tmp_worksheetName, style=tmp_style_clusterColor
                , cols = (tmp_currentDistrPlotVariableNumber-1) * tmp_distrPlotColIncrease + 1
                , rows = tmp_distrPlotStartRow + 26 + (tmp_clustInCurrentSolution-1))


              # Set cluster name
              if( all(tmp_clusterSolution_clusterNames == "") )
              {
                tmp_plotClusterName = paste0("k=", tmp_clustInCurrentSolution)
              } else
              {
                tmp_plotClusterName = tmp_clusterSolution_clusterNames[tmp_clustInCurrentSolution,1]
              }


              # Write cluster name
              writeData(tmp_wb, tmp_worksheetName
                , tmp_plotClusterName
                , startCol = (tmp_currentDistrPlotVariableNumber-1) * tmp_distrPlotColIncrease + 2
                , startRow = tmp_distrPlotStartRow + 26 + (tmp_clustInCurrentSolution-1))

              # Write mean
              writeData(tmp_wb, tmp_worksheetName
                , round(tmp_varMean, 3)
                , startCol = (tmp_currentDistrPlotVariableNumber-1) * tmp_distrPlotColIncrease + 3
                , startRow = tmp_distrPlotStartRow + 26 + (tmp_clustInCurrentSolution-1))
            }

            # Add legend
            # legend("bottomleft"
            #   , inset=c(0, -.4)
            #   , legend = c( paste0(names(tmp_currentSolutionMeansStorage)[-1], "; mean: ",  round(tmp_currentSolutionMeansStorage[tmp_currentDistrPlotVariableNumber, -1],3)), "Means" )
            #   , col = c( tmp_clusterColors, "#000000")
            #   , lty = c(rep(1,length(tmp_clusterColors)), 2)
            #   , cex = 0.75
            # )

            # Add variable plot to workbook
            insertPlot(tmp_wb, tmp_worksheetName, width = 6, height = 4
              , startRow = tmp_distrPlotStartRow + 1
              , startCol = (tmp_currentDistrPlotVariableNumber-1) * tmp_distrPlotColIncrease + 1
              , fileType = "png", units = "in", dpi = 300)



          } # End loop iterating over all variables

        } # End [includeDistributionPlots]

      } # End plots for export

    } # End loop over each cluster solution for [exportOutput]

    # Set timestamp for file name
    tmp_timestamp <- gsub(":", "-", Sys.time())
    tmp_timestamp <- gsub(" ", "_", tmp_timestamp)
    tmp_timestamp <- gsub("\\.[0-9]+", "", tmp_timestamp)
    # Export excel file
    saveWorkbook(tmp_wb, paste0("Cluster Descriptions ", tmp_timestamp, ".xlsx"), TRUE)

  } # End [exportOutput]

  return(tmp_clusterDescriptionsStorage)
} # END FUNCTION - [describeClusters] #
