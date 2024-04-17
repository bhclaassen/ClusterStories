# R package development libraries -----------------------------------------
# library(roxygen2) # In-Line Documentation for R
# library(devtools) # Tools to Make Developing R Packages Easier
# library(testthat) # Unit Testing for R
# library(usethis)  # Automate Package and Project Setup


# Check R library name ----------------------------------------------------
# library(available)
# available::available("ClusterStories", browse = FALSE)

# -------------------------------------------------------------------------
# Multivariate Clustering v05
# BHC

# Started: 2024-03-08
# Updated: 2024-04-17
# -------------------------------------------------------------------------


# TODO --------------------------------------------------------------------
# Add one-pager describing how to use product
# Create structure overview

# Add single-obs description

# Change pooled variance to
#   - Hedges for <=20
# <https://www.statisticshowto.com/hedges-g/>
#   - Cohens for >30
# <https://www.statisticshowto.com/probability-and-statistics/statistics-definitions/cohens-d/>

# -------------------------------------------------------------------------

# Function to check if user-given colors can be used
ifValidColorsCheck <- function(trialColorList)
{
  # If no colors were give, then NOT a valid list
  if(all(trialColorList == ""))
  {
    return(FALSE)
  }

  for(tmp_color in trialColorList)
  {
    # Confirm each entry in passed color vector is a single entry
    if(length(tmp_color) != 1)
    {
      print("NOTE: Given colors must be a vector of single entries")
      return(FALSE)
    }

    # Confirm each entry in passed color vector has 7 characters (#RRGGBB format)
    if(nchar(tmp_color) != 7)
    {
      print("NOTE: Each color given must be in the format '#RRGGBB'")
      return(FALSE)
    }

    # Split colors into individual characters
    tmp_colorCheck <- unlist(strsplit(tmp_color, ""))

    # Confirm that each character is valid for specifying a color
    for(tmp_colorCharacterIndex in 1:length(tmp_colorCheck))
    {
      if(tmp_colorCharacterIndex == 1)
      {
        # First character must be '#'
        if(tmp_colorCheck[tmp_colorCharacterIndex] != "#")
        {
          return(FALSE)
        }
      } else if( # Every character after the first must be 0-9 or a-f/A-F to conform to hexidecimal coding
        !(
          grepl("[0-9]", tmp_colorCheck[tmp_colorCharacterIndex]) |
          grepl("[a-f]", tmp_colorCheck[tmp_colorCharacterIndex], ignore.case = T)
        )
      )
      {
        return(FALSE)
      }
    } # End loop over each character in [tmp_colorCheck]
  } # End loop over each color

  # If no issues are encountered, return TRUE
  return(TRUE)
}


# -------------------------------------------------------------------------

# Function to find cluster fit metrics ------------------------------------
fitClusterMetrics <- function(fit_clusterData, fit_clusterSolutionsToFitMetricsOn, fit_clusterDistances, fit_clusterFitMetrics = c("within.cluster.ss", "avg.silwidth", "ch", "wb.ratio"))
{
  # Takes in the following parameters:
    # - fit_clusterData <- clusterData
    # - fit_clusterSolutionsToFitMetricsOn <- clusterSolutionsToFitMetricsOn
    # - fit_clusterDistances <- clusterDistances
    # - fit_clusterFitMetrics <- clusterFitMetrics

  require(tidyverse)
  require(fpc) # For 'cluster.stats' command

  tmp_numClustSolutions <- length(fit_clusterSolutionsToFitMetricsOn)

  # Set fit metrics
  # Defaults:
  # "within.cluster.ss" # Within-cluster sum of squares
  # "avg.silwidth" # Average silhouette width
  # "ch" # Calinski-Harabasz index
  # "wb.ratio" # Within/between SSE ratio

  # Check that each given cluster metric is valid if not using defaults
  if(
    !all(
      fit_clusterFitMetrics %in% c("n", "cluster.number", "min.cluster.size", "noisen", "average.between", "average.within", "n.between", "n.within", "max.diameter", "min.separation", "within.cluster.ss", "avg.silwidth", "g2", "g3", "pearsongamma", "dunn", "dunn2", "entropy", "wb.ratio", "ch", "widestgap", "sindex", "corrected.rand", "vi")
      # c("n", "cluster.number", "cluster.size", "min.cluster.size", "noisen", "diameter", "average.distance", "median.distance", "separation", "average.toother", "separation.matrix", "ave.between.matrix", "average.between", "average.within", "n.between", "n.within", "max.diameter", "min.separation", "within.cluster.ss", "clus.avg.silwidths", "avg.silwidth", "g2", "g3", "pearsongamma", "dunn", "dunn2", "entropy", "wb.ratio", "ch", "cwidegap", "widestgap", "sindex", "corrected.rand", "vi")
    )
  )
  {
    stop(paste0("Cluster fit metrics must be scalar values from {fpc} package fcn [cluster.stats] \n -> Valid entries: n, cluster.number, min.cluster.size, noisen, average.between, average.within, n.between, n.within, max.diameter, min.separation, within.cluster.ss, avg.silwidth, g2, g3, pearsongamma, dunn, dunn2, entropy, wb.ratio, ch, widestgap, sindex, corrected.rand, vi"))
  }

  # Create cluster fit metrics storage matrix
  tmp_clustFitMetricStorage <- as.data.frame(matrix(, tmp_numClustSolutions, (length(fit_clusterFitMetrics) + 2)) ) # Storage matrix for cluster fit metrics; no metrics for 1 cluster so n-1
  names(tmp_clustFitMetricStorage) <- c("Cluster Solution", "Total Clusters"
      , fit_clusterFitMetrics
    )

  # Add cluster solution names to storage
  tmp_clustFitMetricStorage[,1] <- fit_clusterSolutionsToFitMetricsOn
  # Fill in 'Cluster Number' column with max of cluster IDs for each solution set, i.e. number of clusters in given solution excluding NAs
  tmp_clustFitMetricStorage[,2] <- sapply(fit_clusterData %>% select(all_of(fit_clusterSolutionsToFitMetricsOn)), function(x) {length(table(x))})


  # Calculate cluster metrics for each [fit_clusterSolutionsToFitMetricsOn]
  for(tmp_clustSolution in fit_clusterSolutionsToFitMetricsOn)
  {

    print(paste0("Fitting metrics for [", tmp_clustSolution, "]"))

    tmp_clustIDs <- unlist(fit_clusterData %>% select(all_of(tmp_clustSolution))) # Store cluster IDs

    # If cluster IDs are not numeric, or are not ordered from 1:(number of clusters) then the 'cluster.stats()' command will not work
    # Therefore, create a new vector of the correct format that overwrites [tmp_clustIDs]
    if(class(tmp_clustIDs) == "character"
      | !all( sort(names(table(tmp_clustIDs))) == c(1:length(table(tmp_clustIDs))) )
      # !all( sort(unique(tmp_clustIDs)) == c(1:length(unique(tmp_clustIDs))) )
      )
    {
      # Create list of unique IDs and sort them
      tmp_clustIDsList <- as.data.frame(matrix(, length(unique(tmp_clustIDs)), 3))
      names(tmp_clustIDsList) <- c("ClusterNumber", "ClusterName", "TmpClusterName")

      tmp_clustIDsList[,1] <- 1:dim(tmp_clustIDsList)[1]
      tmp_clustIDsList[,2] <- sort(unique(tmp_clustIDs))

      # Append a signifier to current IDs
      tmp_clustIDs <- paste0("tmp", tmp_clustIDs)
      tmp_clustIDsList[,3] <- paste0("tmp", tmp_clustIDsList[,2])


      # For each of the IDs, replace with a number from 1:length(unique(tmp_clustIDs))
      for(tmp_clustIDReplacement in 1:dim(tmp_clustIDsList)[1])
      {
        tmp_clustIDs[which(tmp_clustIDs == tmp_clustIDsList[tmp_clustIDReplacement, 3])] = tmp_clustIDReplacement
      }

      tmp_clustIDs <- as.numeric(tmp_clustIDs)

      # print(table(tmp_clustIDs, fit_clusterData[,tmp_clustSolution]))
    }

    tmp_clustStats <- cluster.stats(fit_clusterDistances, tmp_clustIDs) # Get cluster statistics

    tmp_numberStartingCols <- 2 # The (2) starting columns are 'ClusterSolutionName' and 'NumClusters'. This is the offset for downstream column selection

    tmp_currentSolutionRow <- which(fit_clusterSolutionsToFitMetricsOn == tmp_clustSolution) # Set row of [tmp_clustFitMetricStorage] for current cluster solution [tmp_clustSolution]

    # Confirm 'cluster.number' == tmp_clustFitMetricStorage[,2]
    if(!tmp_clustFitMetricStorage[tmp_currentSolutionRow,2] == tmp_clustStats$cluster.number)
    {
      stop("Number of clusters in solution do not match 'cluster.stats' output")
    }

    # Check if any of the calculated metrics return NULL
    for(tmp_metric in fit_clusterFitMetrics)
    {
      # print(tmp_clustStats[ tmp_metric ])
      # print(length(tmp_clustStats[ tmp_metric ]))
      # print(is.null(tmp_clustStats[ tmp_metric ]))
      # if(tmp_clustStats[ tmp_metric ])
      if(is.null(unlist(tmp_clustStats[ tmp_metric ])))
      {
        tmp_clustStats[ tmp_metric ] = "#N/A" # Must be "#N/A" to export properly to Excel
      }
    }

    # Store desired cluster metrics
    tmp_clustFitMetricStorage[tmp_currentSolutionRow, (1:length(fit_clusterFitMetrics) + tmp_numberStartingCols)] <- tmp_clustStats[ fit_clusterFitMetrics ]

    rm(tmp_clustStats)
  }

  return(tmp_clustFitMetricStorage)

} ## END FUNCTION [fitClusterMetrics] ##


# -------------------------------------------------------------------------

createClusterDescriptions <- function(descr_clusterData, descr_clusterSolutions, descr_dataColumns)
{
  # Takes in the following parameters:
  #   - descr_clusterData <- clusterData
  #   - descr_clusterSolutions <- clusterSolutions
  #   - descr_dataColumns <- dataColumns

  # Set number variables in [descr_clusterData]
  tmp_numVariables <- length(descr_dataColumns)

  # Create a description for each solution
  for(tmp_clustSolution in descr_clusterSolutions)
  {
    print(paste0("Generating cluster descriptions for solution [", tmp_clustSolution,"]"))

    # Iterate over each group in the current solution
    for(tmp_currentCluster in 1:length(table(descr_clusterData[, tmp_clustSolution])) )
    {
      # Initialize storage for current cluster ID within current cluster solution
      tmp_clusterProportions <- as.data.frame(matrix(, 1, 5)) # Cluster size and proportion
      names(tmp_clusterProportions) <- c("Cluster Number", "Total Number of Clusters", "Number of Observations", "Proportion", "Original ID")
      tmp_clusterVarDescriptions <- as.data.frame(matrix(, tmp_numVariables, 6)) # Cluster variables description (rows: num variables, cols: 5 metrics)
      names(tmp_clusterVarDescriptions) <- c("Variable", "Mean", "Mean Diff", "Std Mean Diff", "Pooled Std Dev", "Out-Cluster Mean")

      ## Fill in description
      tmp_currentClusterTable <- table(descr_clusterData[, tmp_clustSolution])
      # Current Cluster Number
      tmp_clusterProportions[1,1] <- tmp_currentCluster
      # Total Number of Clusters
      tmp_clusterProportions[1,2] <- length(tmp_currentClusterTable)
      # Number of Observations
      tmp_clusterProportions[1,3] <- tmp_currentClusterTable[tmp_currentCluster]
      # Proportion
      tmp_clusterProportions[1,4] <- tmp_currentClusterTable[tmp_currentCluster] / sum(tmp_currentClusterTable)
      # Original cluster number/name
      tmp_clusterProportions[1,5] <- names(tmp_currentClusterTable)[tmp_currentCluster]

      # Pull out variables in [descr_dataColumns] for current cluster ID within the current cluster solution
      tmp_inClusterStdDiffData <- descr_clusterData[which(descr_clusterData[,tmp_clustSolution] == tmp_clusterProportions[1,5]), ] %>% select(all_of(descr_dataColumns))
      tmp_outClusterStdDiffData <- descr_clusterData[which(descr_clusterData[,tmp_clustSolution] != tmp_clusterProportions[1,5]), ] %>% select(all_of(descr_dataColumns))

      # Variable
      tmp_clusterVarDescriptions[,1] <- descr_dataColumns # Variable names passed by user

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


      # If first cluster in solution, create list, else append
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
    } # End loop over clusters within current solution #

    # All descriptions for the current clustering solution are now complete
    # If working on the first clustering solution, initialize the overall storage list
    if(tmp_clustSolution == descr_clusterSolutions[1])
    {
      tmp_solutionDescriptionIndex <- 1

      tmp_clusterDescriptionsStorage <- list(tmp_singleClusterDescriptionStorage)

    } else # Else if creating the second or latter solution, add to the storage list
    {
      tmp_solutionDescriptionIndex <- tmp_solutionDescriptionIndex + 1

      tmp_clusterDescriptionsStorage[tmp_solutionDescriptionIndex] <- list(tmp_singleClusterDescriptionStorage)
    }

  } # End loop over each solution to create descriptions

  return(tmp_clusterDescriptionsStorage)

} ## END CLUSTER DESCRIPTION CREATION FUNCTION ##


# -------------------------------------------------------------------------

describeClusters <- function(clusterData, uniqueID, clusterSolutions, dataColumns, exportOutput = TRUE, exportDecimalPlaces = 3, exportPositiveNegativeSignificanceColors = c("#33F5B7", "#FCB099"), includeClusterFitMetrics = FALSE, clusterDistances = "", clusterFitMetrics = c("within.cluster.ss", "avg.silwidth", "ch", "wb.ratio"), clusterSolutionsToFitMetricsOn = "", includeClusterDescriptions = TRUE, includeDistributionPlots = TRUE, includeRadarPlots = FALSE, plotQuietly = FALSE)
{
  ## LIBRARY REQUIREMENTS ##
  # - {tidyverse} for general use
  # - {fpc} for clustering fit metrics
  # - {openxlsx} for exporting to Excel
  # - {ggplot2} for distribution plots
  # - {fmsb} for radar plots

  ## PARAMETER REQUIREMENTS ##
  # - [clusterData] must be a data.frame
  # - [uniqueID] must be a single column number or name that is in [clusterData]
  # - [clusterSolutions] must be a list of column numbers only, or of column names only, that are in [clusterData]
  # - [dataColumns] must be a list of column numbers only, or of column names only, that are in [clusterData]
  # - [exportOutput] bool; Set to TRUE if the information should be exported to Excel (Default: TRUE)
  # - [exportDecimalPlaces] Must be a single number or a data.frame Allows for control of formatting in Excel output (Default: 3)
  #       If [exportDecimalPlaces] is a data.frame, the following structure must be followed:
  #           -
  #           -
  # - [exportPositiveNegativeSignificanceColors] must be a list of valid R colors. Sets the colors for significanctly positive/negative in the Excel output (Default: c("#33F5B7", "#FCB099"))
  # - [includeClusterFitMetrics] bool; Set to TRUE if cluster fit metrics should be calculated.
  #       REQUIRES distances for [clusterDistances] (Default: FALSE)
  # - [clusterDistances] must be a list of distances from the 'dist()' function. Only included if cluster metrics are requested (Default: "")
  # - [clusterFitMetrics] must be a list of possible outputs from the 'cluster.stats()' function in the {fpc} library.
  #       Valid entries are: n, cluster.number, min.cluster.size, noisen, average.between, average.within, n.between,
  #       n.within, max.diameter, min.separation, within.cluster.ss, avg.silwidth, g2, g3, pearsongamma, dunn, dunn2,
  #       entropy, wb.ratio, ch, widestgap, sindex, corrected.rand, vi. (Default: c("within.cluster.ss", "avg.silwidth", "ch", "wb.ratio"))
  # - [clusterSolutionsToFitMetricsOn] must be left blank ("") or must be a set/subset of the list of [clusterSolutions].
  #       This allows clustering statistics to be run only on a subset of all given solutions (Default: "")
  # - [includeClusterDescriptions] bool; Set to TRUE if cluster descriptions should be generated (Default: TRUE)
  # - [includeDistributionPlots] bool; Set to TRUE if plots of variable distribution by cluster should be plotted (Default: TRUE)
  # - [includeRadarPlots] bool; Set to TRUE if radar plots of variable means by cluster should be plotted (Default: FALSE)
  # - [plotQuietly] bool; Set to TRUE if the plots should be not be plotted (Default: TRUE)

  require(tidyverse)

  # Check inputs ----------------------------------------------------------
  # Confirm [clusterData] is a data.frame
  if(!class(clusterData) == "data.frame")
  {
    stop("[ClusterData] must be a data.frame")
  }


  # Confirm [uniqueID] is a single value and is either a column number or a column name in [clusterData]
  if(length(uniqueID) == 1 & class(uniqueID)[1] == "numeric") # Check for [uniqueID] length and type
  {
    if(uniqueID < 0 | uniqueID > dim(clusterData)[2]) # Check if [clusterData] has given column number
    {
      stop("[uniqueID] column number not in [clusterData] dimensions")
    }

    # Store both forms of [uniqueID] for checking overlap
    tmp_uniqueID_char = names(clusterData)[uniqueID]
    tmp_uniqueID_num = uniqueID

  } else if(length(uniqueID) == 1 & class(uniqueID)[1] =="character") # Check for [uniqueID] length and type
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
    stop("[uniqueID] must be a single column number or name")
  }


  # Confirm [clusterSolutions] are column numbers or column names in [clusterData]
  if( all(sapply(clusterSolutions, class) == "numeric") | all(sapply(clusterSolutions, class) == "integer") ) # Check if all [clusterSolutions] are integers and therefore are assumed to be column numbers
  {
    # Check if [clusterSolutions] are outside the dimensions of [clusterData]
    if(any(clusterSolutions > dim(clusterData)[2]) | any(clusterSolutions < 1))
    {
      stop("[clusterSolutions] column numbers must be within the dimensions of [clusterData]")
    }

    # Store both forms of [clusterSolutions] for checking overlap
    tmp_clusterSolutions_char = names(clusterData)[clusterSolutions]
    tmp_clusterSolutions_num = clusterSolutions

  } else if( all(sapply(clusterSolutions, class) == "character") ) # Check if all [clusterSolutions] are strings and therefore are assumed to be column names
  {
    if( !all(clusterSolutions %in% names(clusterData)) )
    {
      stop("[clusterSolutions] names must be in [clusterData] names")
    }

    # Store both forms of [clusterSolutions] for checking overlap
    tmp_clusterSolutions_char = clusterSolutions
    tmp_clusterSolutions_num = sapply(clusterSolutions, FUN = function(x) { which( names(clusterData) == x ) })

  } else # Else not a list of integers nor a list of characters, so is invalid
  {
    stop("[clusterSolutions] must be a list of numbers or strings")
  }

  # There cannot be any NAs in cluster solutions
  if( any(is.na(clusterData %>% select(all_of(clusterSolutions)))) )
  {
    stop("[clusterSolutions] cannot have any NA values")
  }

  # There cannot be any cluster solutions with only one cluster ID (breaks fitting metrics and calculating significance via pooled variance)
  if(
    # any( sapply (clusterData[clusterSolutions], FUN = function(x) {var(x, na.rm = T)} ) == 0 ) # If any cluster IDs
    any( sapply (clusterData[clusterSolutions], FUN = function(x) {length(unique(x))} ) == 1 ) # If any cluster IDs
  )
  {
    stop("All [clusterSolutions] must have more than one cluster ID each")
  }



  # Confirm [dataColumns] are column numbers or column names in [clusterData]
  if( all(sapply(dataColumns, class) == "numeric") | all(sapply(dataColumns, class) == "integer") ) # Check if all [dataColumns] are integers and therefore are assumed to be column numbers
  {
    # Check if [dataColumns] are outside the dimensions of [clusterData]
    if(any(dataColumns > dim(clusterData)[2]) | any(dataColumns < 1))
    {
      stop("[dataColumns] column numbers must be within the dimensions of [clusterData]")
    }

    # Store both forms of [dataColumns] for checking overlap
    tmp_dataColumns_char = names(clusterData)[dataColumns]
    tmp_dataColumns_num = dataColumns

  } else if( all(sapply(dataColumns, class) == "character") ) # Check if all [dataColumns] are integers and therefore are assumed to be column names
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
        , tmp_dataColumns_num
      )
  ))))
  {
    stop("[uniqueID], [clusterSolutions], [clusterNamesColumns] and [dataColumns] must all be mutually exclusive")
  }

  # Once duplicates are confirmed not to exist, store all column names in label form
  uniqueID <- tmp_uniqueID_char
  clusterSolutions <- tmp_clusterSolutions_char
  dataColumns <- tmp_dataColumns_char

  # Subset [clusterData] to [uniqueID], [clusterSolutions], [clusterNamesColumns], and [dataColumns] only
  clusterData <- clusterData %>% select(
    all_of(uniqueID)
    , all_of(clusterSolutions)
    , all_of(dataColumns)
  )

  # Reorder [clusterSolutions] by number of clusters (i.e. number of unique values excluding NAs)
  clusterSolutions <- clusterSolutions[
    order(
      sapply(clusterData %>% select(all_of(clusterSolutions)), FUN = function(x) {length(table(x))}) # Using 'table' here not 'unique' because the latter includes NAs
      )
  ]

  # There cannot be any clusters with all NAs for a given variables
  for( tmp_solnVarNaCheck in clusterSolutions  )
  {
    for( tmp_variableNaCheck in dataColumns )
    {
      tmp_naCheckData <- clusterData %>% select(all_of(tmp_solnVarNaCheck), all_of(tmp_variableNaCheck))
      names(tmp_naCheckData) <- c("tmpSoln", "tmpVar")
      tmp_naCheckMeans <- tmp_naCheckData %>% group_by(tmpSoln) %>% summarize(tmp_naMean = mean(tmpVar, na.rm = T))

      # print(tmp_naCheckMeans)

      if( any(is.na(tmp_naCheckMeans[,2])) )
      {
        stop("Cluster [", tmp_naCheckMeans[which(is.na(tmp_naCheckMeans[,2])), 1], "] in solution [", tmp_solnVarNaCheck, "] has all NAs for variable [", tmp_variableNaCheck, "]")
      }

    }
  }


# -------------------------------------------------------------------------


  # If [exportOutput] is TRUE, then begin export process with cleaned data
  if(exportOutput)
  {
    require(openxlsx)

    # [exportDecimalPlaces] must be a single number, or a data.frame
    if( ( length(exportDecimalPlaces) == 1 & ( class(exportDecimalPlaces) == "numeric" | class(exportDecimalPlaces) == "integer" ) ) )
    {
      # [exportDecimalPlaces] must be a whole number
      exportDecimalPlaces <- round(exportDecimalPlaces)

      # If [exportDecimalPlaces] is a single number, is must be greater than 0
      if(exportDecimalPlaces < 0)
      {
        print("NOTE: [exportDecimalPlaces] must be non-negative. It has been converted to the default value of 3")
        exportDecimalPlaces <- 3 # If not, reset to default of 3
      }

      if(any(exportDecimalPlaces$DataDecimals > 20))
      {
        print("NOTE: [exportDecimalPlaces] must be less than 20. It has been converted to the default value of 3")
        exportDecimalPlaces <- 3 # If not, reset to default of 3
      }

    } else if( class(exportDecimalPlaces) == "data.frame" )
    {
      # If [exportDecimalPlaces] is a data.frame, it must have three columns and the same number of rows as [dataColumns] plus 2, 1 each for metrics and descriptions table
      if(dim(exportDecimalPlaces)[1] != (length(dataColumns) + 2) | dim(exportDecimalPlaces)[2] != 3)
      {
        stop("If [exportDecimalPlaces] is a data.frame then it must have 3 columns and the same number of rows as [dataColumns] plus 2 (1 for fit metric precision and 1 for descriptions table precision)")
      }

      # If [exportDecimalPlaces] is a data.frame, it must start with a row for 'Metrics' and a row for 'Descriptions'
      if(exportDecimalPlaces[1,1] != 'Metrics' | exportDecimalPlaces[2,1] != 'Descriptions')
      {
        stop("[exportDecimalPlaces] data.frame must have the first row be named 'Metrics' and the second row be named 'Descriptions'")
      }


      if(any(exportDecimalPlaces$DataDecimals < 0))
      {
        print("NOTE: [exportDecimalPlaces] 'DataDecimal' values must all be non-negative. Negative values have been converted to the default value of 3")
        exportDecimalPlaces[which(exportDecimalPlaces$DataDecimals < 2), "DataDecimals"] <- 3 # If not, reset to default of 3
      }

      if(any(exportDecimalPlaces$DataDecimals > 20))
      {
        print("NOTE: [exportDecimalPlaces] 'DataDecimal' values must all be less than 20. Values over this have been converted to the default value of 3")
        exportDecimalPlaces[which(exportDecimalPlaces$DataDecimals > 20), "DataDecimals"] <- 3 # If not, reset to default of 3
      }

      # Round [exportDecimalPlaces] decimals to be whole numbers if not given as such
      exportDecimalPlaces[,3] = round(exportDecimalPlaces[,3], 0)

      if(!all(exportDecimalPlaces$DataType %in% c("", "%", "$")))
      {
        stop("All values for [exportDecimalPlaces] 'DataType' must be one of the following: '%', '$', or blank ('')")
      }

    } else
    {
      stop("[exportDecimalPlaces] must be a number or a data.frame")
    }


    # Set which table rows to write to
    tmp_titleRow <- 2
    tmp_proportionRow <- 4
    tmp_descriptionRow <- 7

    # Initialize excel file
    tmp_wb <- createWorkbook()

    ## Set Formats ##

    # Check if there are colors given for [exportPositiveNegativeSignificanceColors], and if they are valid
    ifGivenValidColors <- ifValidColorsCheck(exportPositiveNegativeSignificanceColors)

    # If given colors for [exportPositiveNegativeSignificanceColors] are valid, then use them
    if(ifGivenValidColors)
    {
      tmp_style_posSig <- createStyle(bgFill = exportPositiveNegativeSignificanceColors[1])
      tmp_style_negSig <- createStyle(bgFill = exportPositiveNegativeSignificanceColors[2])
    } else
    {
      # Defaults:
      tmp_style_posSig <- createStyle(bgFill = "#33F5B7")
      tmp_style_negSig <- createStyle(bgFill = "#FCB099")
    }

    # Create formatting styles
    tmp_style_header <- createStyle(fontSize = 14, textDecoration = "Bold")
    tmp_style_count <- createStyle(numFmt="#,##0")
    tmp_style_pct <- createStyle(numFmt="0.0%")
    tmp_style_pctZeroOne <- createStyle(numFmt="0%")
    tmp_style_bold <- createStyle(textDecoration = "Bold")
    tmp_style_boldAndCenter <- createStyle(textDecoration = "Bold", halign = "Center")

    # Create green shade styles for crosswalk tables
    # tmp_style_greenShade_1 <- createStyle(fgFill = "#ffffff") # Green shading for [0%, 20%)
    # tmp_style_greenShade_2 <- createStyle(fgFill = "#edf8e9") # Green shading for [20%, 40%)
    # tmp_style_greenShade_3 <- createStyle(fgFill = "#bae4b3") # Green shading for [40%, 60%)
    # tmp_style_greenShade_4 <- createStyle(fgFill = "#74c476") # Green shading for [60%, 80%)
    # tmp_style_greenShade_5 <- createStyle(fgFill = "#238b45") # Green shading for [80%, 100%]

    tmp_style_greenShade_1 <- createStyle(fgFill = "#FFFFFF") # Green shading for [0%, 20%)
    tmp_style_greenShade_2 <- createStyle(fgFill = "#DBECE0") # Green shading for [20%, 40%)
    tmp_style_greenShade_3 <- createStyle(fgFill = "#B9DCBF") # Green shading for [40%, 60%)
    tmp_style_greenShade_4 <- createStyle(fgFill = "#99CCA1") # Green shading for [60%, 80%)
    tmp_style_greenShade_5 <- createStyle(fgFill = "#7ABC81") # Green shading for [80%, 100%]

    if(class(exportDecimalPlaces) == 'data.frame')
    {
      tmp_style_floatMetricsTable <- createStyle(numFmt= paste0( "#,##0.", paste0(rep("0", (exportDecimalPlaces[1,3])), collapse = "") ) )
      tmp_style_floatDescriptionTable <- createStyle(numFmt= paste0( "#,##0.", paste0(rep("0", (exportDecimalPlaces[2,3])), collapse = "") ) )
    } else
    {
      tmp_style_floatMetricsTable <- createStyle(numFmt= paste0( "#,##0.", paste0(rep("0", (exportDecimalPlaces)), collapse = "") ) )
      tmp_style_floatDescriptionTable <- tmp_style_floatMetricsTable
    }

    # Create parameters workbook
    addWorksheet(tmp_wb, "Parameters")
    writeData(tmp_wb, "Parameters", "Standard difference threshold:", startCol = 2, startRow = 2)
    writeData(tmp_wb, "Parameters", 0.5, startCol = 3, startRow = 2)

  } # END EXPORT - INITIALIZATION #


  # Begin fit metrics -----------------------------------------------------
  if(includeClusterFitMetrics)
  {
    # If [includeClusterFitMetrics] is TRUE, then check for necessary parameters to fit cluster metrics

    # If no distances given, do not fit metrics
    if(!length(clusterDistances) > 1)
    {
      print("NOTE: Distances using the 'dist()' command must be provided in [clusterDistances] for fit metrics to be calculated")
      includeClusterFitMetrics <- FALSE
    }

    # If no fit metrics given, do not fit metrics
    if(clusterFitMetrics[1] == "")
    {
      print("NOTE: [clusterFitMetrics] is blank, no metrics will be fit")
      includeClusterFitMetrics <- FALSE
    }

    # If no subset of clusters solutions is given specifically for metrics to be fit on, use all cluster solutions
    if(clusterSolutionsToFitMetricsOn[1] == "")
    {
      clusterSolutionsToFitMetricsOn <- clusterSolutions
    }

    # If any of [clusterSolutionsToFitMetricsOn] has only one observation per cluster, then fitting metrics for that solution is not possible
    for(tmp_clusterMetricSolution in clusterSolutionsToFitMetricsOn)
    {
      if( all(table(clusterData[,tmp_clusterMetricSolution]) == 1) )
      {
        print(paste0("NOTE: [", tmp_clusterMetricSolution, "] has only one observation in each cluster, no metrics will be fit for this solution."))
        clusterSolutionsToFitMetricsOn <- clusterSolutionsToFitMetricsOn[ -which(clusterSolutionsToFitMetricsOn == tmp_clusterMetricSolution) ]
      }

      # If all solutions are removed in this manner, then do not fit cluster metrics
      if(length(clusterSolutionsToFitMetricsOn) == 0)
      {
        print("NOTE: All solutions have only one observation per cluster, no metrics will be fit")
        includeClusterFitMetrics <- FALSE
      }
    }

    # If the conditions above are fulfilled, fit metrics and allow for export
    if(includeClusterFitMetrics)
    {
      # Call function [fitClusterMetrics] and store results
      tmp_clusterFitMetrics <- fitClusterMetrics(clusterData, clusterSolutionsToFitMetricsOn, clusterDistances, clusterFitMetrics)

      # Remove distances for fit metrics (often a very large object)
      rm(clusterDistances)

      # If exporting, add fit metrics data to Excel file from within the [includeClusterFitMetrics] check
      if(exportOutput)
      {
        # Set [tmp_numMetrics] to be the number of metrics fit
        tmp_numMetrics <- length(clusterFitMetrics)

        # Create cluster metrics and plots workbook
        addWorksheet(tmp_wb, "Cluster Metrics")

        # Write table and format header
        writeData(tmp_wb, "Cluster Metrics", "Cluster Fit Metrics", startRow = 2, startCol = 2)
        addStyle(tmp_wb, "Cluster Metrics", style = tmp_style_header, rows = 2, cols = 2)



        # Write and format metrics table
        writeData(tmp_wb, "Cluster Metrics", tmp_clusterFitMetrics, startRow = 3, startCol = 2)
        addStyle(tmp_wb, "Cluster Metrics", style = tmp_style_boldAndCenter, rows = 3, cols = c(2:(4+tmp_numMetrics-1))) # Format table headers
        addStyle(tmp_wb, "Cluster Metrics", style = tmp_style_floatMetricsTable, rows = c(4:(4+length(clusterSolutionsToFitMetricsOn)-1)), cols = c(4:(4+tmp_numMetrics-1)), gridExpand = TRUE) # Format decimal precision
        addStyle(tmp_wb, "Cluster Metrics", style = tmp_style_bold, rows = c(4:(3+dim(tmp_clusterFitMetrics)[1])), cols = 2)

        # If any cluster solutions have been excluded from the metrics calculations, then note their exclusion in the 'Cluster Metrics' tab
        if( !all(clusterSolutions %in% clusterSolutionsToFitMetricsOn) )
        {
          # Set start column for listing missing solutions from metrics:
          #   => [2 for space for metrics table]
          #      + [2 for metrics table headers (solution name and number of clusters)]
          #      + [the number metrics included (length(clusterFitMetrics))]
          #      + [3 for spacing from table and to be outside of graphs if list of exclusions is long]
          tmp_startExcludedMetricsSolutionNotesColumn <- 2 + 2 + length(clusterFitMetrics) + 2

          # Pull missing solutions
          tmp_metricsExclusionsList <- clusterSolutions[which(!(clusterSolutions %in% clusterSolutionsToFitMetricsOn))]

          # Write note of missing solutions
          writeData(tmp_wb, "Cluster Metrics", "Clusters excluded from fit metrics calculations:", startRow = 3, startCol = tmp_startExcludedMetricsSolutionNotesColumn)
          addStyle(tmp_wb, "Cluster Metrics", style=tmp_style_bold, rows = 3, cols = tmp_startExcludedMetricsSolutionNotesColumn)

          for(tmp_excludedSolutionNumber in 1:length(tmp_metricsExclusionsList))
          {
            writeData(tmp_wb, "Cluster Metrics", tmp_metricsExclusionsList[tmp_excludedSolutionNumber], startCol = tmp_startExcludedMetricsSolutionNotesColumn, startRow = 3+tmp_excludedSolutionNumber)
          }
        }

        # Set rows for spacing out metrics plots
        tmp_plotStartRow = dim(tmp_clusterFitMetrics)[1] + 6 # 5 rows below the end of the [tmp_clusterFitMetrics] table
        tmp_plotRowIncrease = 22 # Number of rows to match 4 inches of plot height plus a margin
        tmp_numPlots = 0 # Initialize plot count to 0

        # Create metric plot labels
        tmp_allMetricPlotLabels <- data.frame(
          stringsAsFactors = FALSE,
          metricName = c("n","cluster.number","min.cluster.size","noisen"
            ,"average.between","average.within","n.between","n.within"
            ,"max.diameter","min.separation","within.cluster.ss","avg.silwidth"
            ,"g2","g3","pearsongamma","dunn","dunn2","entropy","wb.ratio","ch"
            ,"widestgap","sindex","corrected.rand","vi"),
          plottingName = c("Number of Cases (n)","Number of Clusters (cluster.number)"
            ,"Smallest Cluster Size (min.cluster.size)","Number of Noise Points (noisen)"
            ,"Average Distance Between Clusters (average.between)"
            ,"Average Distance Within Clusters (average.within)"
            ,"Number of Distances Between Clusters (n.between)"
            ,"Number of Distances Within Clusters (n.within)"
            ,"Max Cluster Diameter (max.diameter)"
            ,"Minimum Cluster Separation (min.separation)"
            ,"Within Cluster Sum of Squares (within.cluster.ss)"
            ,"Average Silhouette Width (avg.silwidth)"
            ,"Goodman and Kruskal's Gamma Coefficient (g2)","G3 Coefficient (g3)"
            ,"Pearson's Gamma (pearsongamma)","Dunn Index (dunn)","Dunn Index - 2 (dunn2)"
            ,"Entropy (entropy)","Within/Between Ratio (wb.ratio)"
            ,"Calinski-Harabasz Index (ch)","Widest Within-Cluster Gap (widestgap)"
            ,"Separation Index (sindex)","Corrected Rand Index (corrected.rand)"
            ,"Variation of Information Index (vi)")
          )

        # Iterate over fit metrics and plot
        for(tmp_metricToPlot in 3:dim(tmp_clusterFitMetrics)[2])
        {

          # If any of the fit metrics returned NA, do not plot that metric
          if(any(tmp_clusterFitMetrics[,tmp_metricToPlot] == "#N/A"))
          {
            writeData(tmp_wb, "Cluster Metrics", paste0("ERROR: NAs in fit metric [", names(tmp_clusterFitMetrics)[tmp_metricToPlot] ,"]" ), startRow = tmp_plotStartRow + tmp_plotRowIncrease * tmp_numPlots, startCol = 2)
          } else
          {

            tmp_metricPlotLabel <- tmp_allMetricPlotLabels[which(tmp_allMetricPlotLabels[,1] == names(tmp_clusterFitMetrics)[tmp_metricToPlot]), 2]

            if(dim(tmp_clusterFitMetrics)[1] > 1)
            {
              plot(c(1:dim(tmp_clusterFitMetrics)[1]), tmp_clusterFitMetrics[,tmp_metricToPlot], type = 'l', main = tmp_metricPlotLabel, xlab = "Cluster", xaxt = "n", ylab = "Metric Value")
              axis(side = 1, at = c(1:dim(tmp_clusterFitMetrics)[1]), labels = tmp_clusterFitMetrics[,1])
              grid()

              # Add plot
              insertPlot(tmp_wb, "Cluster Metrics", width = 6, height = 4,
              startRow = tmp_plotStartRow + tmp_plotRowIncrease * tmp_numPlots, startCol = 2, fileType = "png", units = "in", dpi = 300)
            }
            else
            {
              plot(c(1:dim(tmp_clusterFitMetrics)[1]), tmp_clusterFitMetrics[,tmp_metricToPlot], type = 'p', pch = 19, main = names(tmp_clusterFitMetrics)[tmp_metricToPlot], xlab = "Cluster", xaxt = "n", ylab = "Metric Value")
              axis(side = 1, at = c(1:dim(tmp_clusterFitMetrics)[1]), labels = tmp_clusterFitMetrics[,1])
              grid()

              # Add plot
              insertPlot(tmp_wb, "Cluster Metrics", width = 6, height = 4,
              startRow = tmp_plotStartRow + tmp_plotRowIncrease * tmp_numPlots, startCol = 2, fileType = "png", units = "in", dpi = 300)
            }

            if(plotQuietly)
            {
              dev.off()
            }
          }

          # Iterate plot count for row calculation
          tmp_numPlots = tmp_numPlots + 1
        }

      } # END EXPORT - FIT METRICS #

    } # END OF SECOND [includeClusterFitMetrics] CHECK

  } # END FIT METRICS #



  # Begin descriptions ----------------------------------------------------
  if(includeClusterDescriptions)
  {

    # Call function to create cluster descriptions
    tmp_clusterDescriptionsList <- createClusterDescriptions(clusterData, clusterSolutions, dataColumns)

    # If exporting output, add cluster descriptions to excel file from within the [includeClusterDescriptions] call
    if(exportOutput)
    {

      ## ADD CLUSTER SIZES TAB ##

      # Create cluster sizes workbook
      # Initialize storage for all cluster counts and all cluster proportions
      tmp_clusterCountsTable <- as.data.frame(matrix(
        , length(clusterSolutions) # Rows: number of cluster solutions
        , max(sapply(clusterData %>% select(all_of(clusterSolutions)), FUN = function(x) {length(unique(x))}), na.rm = T) # Cols: [max number of clusters number across all solutions]
        # , (max(clusterData %>% select(all_of(clusterSolutions))) - min(clusterData %>% select(all_of(clusterSolutions))) + 1) )  # Rows: number of cluster solutions; Cols: [max number of clusters number across all solutions] - [min number of clusters across all solutions] + 1
        ))

      # Assign names
      names(tmp_clusterCountsTable) <- paste0( "Cluster ", 1:dim(tmp_clusterCountsTable)[2])
      row.names(tmp_clusterCountsTable) <- clusterSolutions

      # Copy proportions storage layout from counts storage
      tmp_clusterProportionsTable <- tmp_clusterCountsTable


      # Pull cluster sizes from each solution
      # Iterate over each cluster solution
      for(s in 1:length(clusterSolutions))
      {
        # Iterate over each cluster within the solution
        for(c in 1:length(tmp_clusterDescriptionsList[[s]]))
        {
          # Store cluster counts and proportions
          tmp_clusterCountsTable[s,c] <- tmp_clusterDescriptionsList[[s]][[c]][[1]][1,3]
          tmp_clusterProportionsTable[s,c] <- tmp_clusterDescriptionsList[[s]][[c]][[1]][1,4]
        }
      }

      tmp_countTable_startRow <- 3
      tmp_countTable_startCol <- 3
      tmp_propTable_startRow <- 3
      tmp_propTable_startCol <- dim(tmp_clusterCountsTable)[2] + 3 + 2

      tmp_clusterNames <- as.data.frame(matrix(row.names(tmp_clusterCountsTable), dim(tmp_clusterCountsTable)[1], 1))
      names(tmp_clusterNames) <- ""


      # Initialize 'Cluster Sizes' tab
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
      addStyle(tmp_wb, "Cluster Sizes", style=tmp_style_header, rows = (tmp_countTable_startRow-1), cols = c(tmp_countTable_startCol-1, tmp_propTable_startCol-1), stack = F)

      # Format table column names
      addStyle(tmp_wb, "Cluster Sizes", style=tmp_style_boldAndCenter, rows = tmp_countTable_startRow, cols = c(tmp_countTable_startCol:(tmp_countTable_startCol + dim(tmp_clusterCountsTable)[2] - 1)))
      addStyle(tmp_wb, "Cluster Sizes", style=tmp_style_boldAndCenter, rows = tmp_countTable_startRow, cols = c(tmp_propTable_startCol:(tmp_propTable_startCol + dim(tmp_clusterCountsTable)[2] - 1)))
      # Format table row names
      addStyle(tmp_wb, "Cluster Sizes", style=tmp_style_bold, rows = c((tmp_countTable_startRow+1):(tmp_countTable_startRow+dim(tmp_clusterNames)[1])), cols = (tmp_countTable_startCol-1))
      addStyle(tmp_wb, "Cluster Sizes", style=tmp_style_bold, rows = c((tmp_propTable_startRow+1):(tmp_propTable_startRow+dim(tmp_clusterNames)[1])), cols = (tmp_propTable_startCol-1))

      # Format count table
      addStyle(tmp_wb, "Cluster Sizes", style=tmp_style_count, rows = ((tmp_countTable_startRow+1):(tmp_countTable_startRow+dim(tmp_clusterCountsTable)[1])) , cols = ((tmp_countTable_startCol):(tmp_countTable_startCol + dim(tmp_clusterCountsTable)[2]-1)), stack = F, gridExpand = T)
      # Format proportions table
      addStyle(tmp_wb, "Cluster Sizes", style=tmp_style_pct, rows = ((tmp_propTable_startRow+1):(tmp_propTable_startRow+dim(tmp_clusterCountsTable)[1])), cols = c((tmp_propTable_startCol):(tmp_propTable_startCol + dim(tmp_clusterCountsTable)[2]-1)), stack = F, gridExpand = T)


      # If there is more than one solution included, create crosswalk tables (these use a confusion matrix format)
      if(dim(tmp_clusterCountsTable)[1] > 1)
      {
        # Add cluster cross-walk tables
        tmp_crosswalkStartRow <- tmp_countTable_startRow + dim(tmp_clusterCountsTable)[1] + 5
        tmp_crosswalkStartCol <- 2

        # Write cross-walk tables header
        writeData(tmp_wb, "Cluster Sizes", "Cluster ID Cross-walks", startRow = tmp_crosswalkStartRow, startCol = tmp_crosswalkStartCol)
        addStyle(tmp_wb, "Cluster Sizes", style=tmp_style_header, rows = tmp_crosswalkStartRow, cols = tmp_crosswalkStartCol)


        # Increase [tmp_crosswalkStartRow] to start of first table
        tmp_crosswalkStartRow <- tmp_crosswalkStartRow + 3

        # Iterate across all combinations of solutions
        for(tmp_crosswalkSolution_1 in 1:(length(clusterSolutions)-1))
        {
          # Set number of clusters in solution #1
          tmp_lengthCrosswalkSolution_1 <- length(unique(clusterData[, clusterSolutions[tmp_crosswalkSolution_1]]))

          for(tmp_crosswalkSolution_2 in (tmp_crosswalkSolution_1+1):length(clusterSolutions))
          {
            # Set number of clusters in solution #2
            tmp_lengthCrosswalkSolution_2 <- length(unique(clusterData[, clusterSolutions[tmp_crosswalkSolution_2]]))

            # Write cluster solution names and directional-read arrows for current table
            writeData(tmp_wb, "Cluster Sizes", clusterSolutions[tmp_crosswalkSolution_1], startRow = (tmp_crosswalkStartRow+1), startCol = (tmp_crosswalkStartCol-1)) # Cluster solution name for [tmp_crosswalkSolution_1]
            # writeFormula(tmp_wb, "Cluster Sizes", x="UNICHAR(8595)", startRow = (tmp_crosswalkStartRow+2), startCol = (tmp_crosswalkStartCol-2)) # Cluster solution read-arrow for [tmp_crosswalkSolution_1] (In excel: UNICHAR(8595))
            writeData(tmp_wb, "Cluster Sizes", clusterSolutions[tmp_crosswalkSolution_2], startRow = (tmp_crosswalkStartRow-1), startCol = (tmp_crosswalkStartCol+1)) # Cluster solution name for [tmp_crosswalkSolution_2]
            # writeFormula(tmp_wb, "Cluster Sizes", x="UNICHAR(8594)", startRow = (tmp_crosswalkStartRow-1), startCol = (tmp_crosswalkStartCol+1)) # Cluster solution read-arrow for [tmp_crosswalkSolution_2] (In excel: UNICHAR(8594))


            # Format cluster solution names
            addStyle(tmp_wb, "Cluster Sizes", style=tmp_style_bold, rows = (tmp_crosswalkStartRow+1), cols = (tmp_crosswalkStartCol-1))
            addStyle(tmp_wb, "Cluster Sizes", style=tmp_style_bold, rows = (tmp_crosswalkStartRow-1), cols = (tmp_crosswalkStartCol+1))

            # print(paste0(clusterSolutions[tmp_crosswalkSolution_1], ",", clusterSolutions[tmp_crosswalkSolution_2]))

            # Create proportions table of cluster crosswalk table
            tmp_crosswalkTable <- proportions(
                table(
                  clusterData[,clusterSolutions[tmp_crosswalkSolution_1]]
                  , clusterData[,clusterSolutions[tmp_crosswalkSolution_2]]
                )
                , margin = 1
              )
            # print(tmp_crosswalkTable)

            # Write crosswalk table
            writeData(tmp_wb, "Cluster Sizes", tmp_crosswalkTable, startRow = tmp_crosswalkStartRow, startCol = tmp_crosswalkStartCol)

            # Format cluster names
            addStyle(tmp_wb, "Cluster Sizes", style=tmp_style_boldAndCenter, rows = c((tmp_crosswalkStartRow+1):(tmp_crosswalkStartRow+1+tmp_lengthCrosswalkSolution_1-1)), cols = tmp_crosswalkStartCol) # Row names
            addStyle(tmp_wb, "Cluster Sizes", style=tmp_style_boldAndCenter, rows = tmp_crosswalkStartRow, cols = c((tmp_crosswalkStartCol+1):(tmp_crosswalkStartCol+1+tmp_lengthCrosswalkSolution_2-1))) # Column names

            # Format proportions table
            # For percent formatting, if 0, format as % with no decimals, else, format as % with 1 decimal
            # For background, color in 5 shades from white to green
            for(tmp_rowI in 1:tmp_lengthCrosswalkSolution_1)
            {
              # Give adjustment within spreadsheet to start at table top-left corner
              tmp_rowI_propTable <- tmp_rowI + tmp_crosswalkStartRow

              for(tmp_colJ in 1:tmp_lengthCrosswalkSolution_2)
              {
                # Give adjustment within spreadsheet to start at table top-left corner
                tmp_colJ_propTable <- tmp_colJ + tmp_crosswalkStartCol

                # print(paste0(tmp_rowI_propTable, ",", tmp_colJ_propTable))

                # addStyle(tmp_wb, "Cluster Sizes", style=tmp_style_bold, rows = (tmp_crosswalkStartRow-1), cols = (tmp_crosswalkStartCol))
                # print(tmp_crosswalkTable[tmp_rowI,tmp_colJ])
                # print(tmp_crosswalkTable[tmp_rowI,tmp_colJ] == 0 | tmp_crosswalkTable[tmp_rowI,tmp_colJ] == 1)

                if(tmp_crosswalkTable[tmp_rowI,tmp_colJ] == 0 | tmp_crosswalkTable[tmp_rowI,tmp_colJ] == 1)
                {
                  addStyle(tmp_wb, "Cluster Sizes", style=tmp_style_pctZeroOne, rows = tmp_rowI_propTable, cols = tmp_colJ_propTable)
                } else
                {
                  addStyle(tmp_wb, "Cluster Sizes", style=tmp_style_pct, rows = tmp_rowI_propTable, cols = tmp_colJ_propTable)
                }

                # Color tables with background spectrum white:green for [0,1] proportion
                if(tmp_crosswalkTable[tmp_rowI,tmp_colJ] < 0.2)
                {
                  addStyle(tmp_wb, "Cluster Sizes", style = tmp_style_greenShade_1, rows = tmp_rowI_propTable, cols = tmp_colJ_propTable, stack = TRUE)
                } else if(tmp_crosswalkTable[tmp_rowI,tmp_colJ] < 0.4)
                {
                  addStyle(tmp_wb, "Cluster Sizes", style = tmp_style_greenShade_2, rows = tmp_rowI_propTable, cols = tmp_colJ_propTable, stack = TRUE)
                } else if(tmp_crosswalkTable[tmp_rowI,tmp_colJ] < 0.6)
                {
                  addStyle(tmp_wb, "Cluster Sizes", style = tmp_style_greenShade_3, rows = tmp_rowI_propTable, cols = tmp_colJ_propTable, stack = TRUE)
                } else if(tmp_crosswalkTable[tmp_rowI,tmp_colJ] < 0.8)
                {
                  addStyle(tmp_wb, "Cluster Sizes", style = tmp_style_greenShade_4, rows = tmp_rowI_propTable, cols = tmp_colJ_propTable, stack = TRUE)
                } else if(tmp_crosswalkTable[tmp_rowI,tmp_colJ] <= 1)
                {
                  addStyle(tmp_wb, "Cluster Sizes", style = tmp_style_greenShade_5, rows = tmp_rowI_propTable, cols = tmp_colJ_propTable, stack = TRUE)
                } else
                {
                  print(paste0("NOTE: Non-numeric value in 'Cluster Sizes' tab at row/col [", tmp_rowI_propTable, ",", tmp_colJ_propTable, "]"))
                }
              }
            }

            # Iterate row anchor for current solutions specified by [tmp_crosswalkSolution_1]
            tmp_crosswalkStartCol <- tmp_crosswalkStartCol + length(unique(clusterData[, clusterSolutions[tmp_crosswalkSolution_2]])) + 3

          } # End loop across current [tmp_crosswalkSolution_2]

          # Reset [tmp_crosswalkStartCol] to 2 (Column 'B')
          tmp_crosswalkStartCol <- 2
          # Iterate row anchor for current solutions specified by [tmp_crosswalkSolution_1]
          tmp_crosswalkStartRow <- tmp_crosswalkStartRow + length(unique(clusterData[, clusterSolutions[tmp_crosswalkSolution_1]])) + 4

        } # End loop across current [tmp_crosswalkSolution_1]
      } # ALL CROSSWALK TABLES ADDED #

      # Add cluster stories -----------------------------------------------

      # Iterate over each cluster solution and create story for each solution in Excel
      for(tmp_clusterStorySolution in 1:length(tmp_clusterDescriptionsList))
      {
        # Set number of clusters in current solution
        tmp_numClustersInSolution <- tmp_clusterDescriptionsList[[tmp_clusterStorySolution]][[1]][[1]][1,2] # Returns second entry in first proportion table for current solution [tmp_clusterStorySolution]

        # Add worksheet
        tmp_worksheetName <- paste0(tmp_numClustersInSolution, " Clusters (", clusterSolutions[tmp_clusterStorySolution],")")
        addWorksheet(tmp_wb, tmp_worksheetName)

        # Set first table column
        tmp_currentCol <- 2


        # Export the stories for each cluster within the current solution
        for(tmp_clusterInSolution in 1:tmp_numClustersInSolution)
        {
          # Write cluster number
          writeData(tmp_wb, tmp_worksheetName, paste0("Cluster ", tmp_clusterDescriptionsList[[tmp_clusterStorySolution]][[tmp_clusterInSolution]][[1]][1,1]), startCol = tmp_currentCol, startRow = tmp_titleRow)

          # Write cluster name to title if different from 1:n cluster number
          if(tmp_clusterDescriptionsList[[tmp_clusterStorySolution]][[tmp_clusterInSolution]][[1]][1,5] != as.character(tmp_clusterDescriptionsList[[tmp_clusterStorySolution]][[tmp_clusterInSolution]][[1]][1,1]))
          {
            # Cluster name
            writeData(tmp_wb, tmp_worksheetName, paste0("(", tmp_clusterDescriptionsList[[tmp_clusterStorySolution]][[tmp_clusterInSolution]][[1]][1,5], ")"), startCol = tmp_currentCol + 1, startRow = tmp_titleRow)
          }

          # Add proportions table to worksheet
          writeData(tmp_wb, tmp_worksheetName, tmp_clusterDescriptionsList[[tmp_clusterStorySolution]][[tmp_clusterInSolution]][[1]][1,3:4], startCol = tmp_currentCol, startRow = tmp_proportionRow)

          # Add descriptions table to worksheet
          writeData(tmp_wb, tmp_worksheetName, tmp_clusterDescriptionsList[[tmp_clusterStorySolution]][[tmp_clusterInSolution]][[2]][,1:4], startCol = tmp_currentCol, startRow = tmp_descriptionRow)


          # Format table
          addStyle(tmp_wb, tmp_worksheetName, style=tmp_style_header, cols = tmp_currentCol, rows = tmp_titleRow)
          addStyle(tmp_wb, tmp_worksheetName, style=tmp_style_count, cols = tmp_currentCol, rows = tmp_proportionRow + 1)
          addStyle(tmp_wb, tmp_worksheetName, style=tmp_style_pct, cols = tmp_currentCol + 1, rows = tmp_proportionRow + 1)
          addStyle(tmp_wb, tmp_worksheetName, style=tmp_style_boldAndCenter, cols = c(tmp_currentCol:(tmp_currentCol+1)), rows = tmp_proportionRow)
          addStyle(tmp_wb, tmp_worksheetName, style=tmp_style_boldAndCenter, cols = c(tmp_currentCol:(tmp_currentCol+3)), rows = tmp_descriptionRow)

          # Format numbers using [exportDecimalPlaces]
          if(class(exportDecimalPlaces) == "numeric" | class(exportDecimalPlaces) == "integer")
          {
            tmp_style_userSpecifiedFloat <- createStyle(numFmt= paste0( "#,##0.", paste0(rep("0", (exportDecimalPlaces)), collapse = "") ) )
            addStyle(tmp_wb, tmp_worksheetName, style=tmp_style_userSpecifiedFloat, cols = c((tmp_currentCol+1):(tmp_currentCol+3)), rows = c((tmp_descriptionRow+1):(tmp_descriptionRow+1+dim(tmp_clusterDescriptionsList[[tmp_clusterStorySolution]][[tmp_clusterInSolution]][[2]])[1]-1)), stack = F, gridExpand = T)
          } else # [exportDecimalPlaces] cleaned above so must be a number or a data.frame
          {
            # Set current description table for multiple calls
            tmp_descriptionTable <- tmp_clusterDescriptionsList[[tmp_clusterStorySolution]][[tmp_clusterInSolution]][[2]][,1:4]
            for(tmpVarNum in 1:dim(tmp_descriptionTable)[1])
            {
              # Pull current var row from [exportDecimalPlaces]
              tmp_currentVarFormatting <- exportDecimalPlaces[which(exportDecimalPlaces[,1] == tmp_descriptionTable[tmpVarNum,1]),]

              if(tmp_currentVarFormatting$DataType == "%")
              {
                # Set custom style for percents
                tmp_exportDecimalPlaces_floatDescriptionTable <- createStyle(numFmt= paste0( "0.", paste0(rep("0", (tmp_currentVarFormatting$DataDecimals)), collapse = ""),  "%") )

              } else if(tmp_currentVarFormatting$DataType == "$")
              {
                # Set custom style for dollars
                tmp_exportDecimalPlaces_floatDescriptionTable <- createStyle(numFmt= paste0( "$#,##0.", paste0(rep("0", (tmp_currentVarFormatting$DataDecimals)), collapse = "") ) )

              } else if(tmp_currentVarFormatting$DataType == "")
              {
                # Set custom style for counts
                tmp_exportDecimalPlaces_floatDescriptionTable <- createStyle(numFmt= paste0( "#,##0.", paste0(rep("0", (tmp_currentVarFormatting$DataDecimals)), collapse = "") ) )

              } else
              {
                stop(paste0("Unrecognized character in [exportDecimalPlaces] 'DataType' column for variable '", tmp_currentVarFormatting$Variables, "'"))
              }

              # Apply custom formatting to first two columns. These are the only two expressed in the original units
              addStyle(tmp_wb, tmp_worksheetName, style=tmp_exportDecimalPlaces_floatDescriptionTable, cols = c((tmp_currentCol+1):(tmp_currentCol+2)), rows = (tmp_descriptionRow+tmpVarNum), gridExpand = T)
            }

            # Format the standard differences from 'Descriptions' in [exportDecimalPlaces]
            addStyle(tmp_wb, tmp_worksheetName, style=tmp_style_floatDescriptionTable, cols = (tmp_currentCol+3), rows = c((tmp_descriptionRow+1):(tmp_descriptionRow+1+dim(tmp_clusterDescriptionsList[[tmp_clusterStorySolution]][[tmp_clusterInSolution]][[2]])[1]-1)), stack = F, gridExpand = T)

          }

          # Format standard diffs in descriptions table
          conditionalFormatting(tmp_wb, tmp_worksheetName, cols = tmp_currentCol + 3, rows = c(8:(8+length(dataColumns)-1)), rule = " > Parameters!$C$2", type = "expression", style = tmp_style_posSig)
          conditionalFormatting(tmp_wb, tmp_worksheetName, cols = tmp_currentCol + 3, rows = c(8:(8+length(dataColumns)-1)), rule = " < (-1)*Parameters!$C$2", type = "expression", style = tmp_style_negSig)

          # IF not last cluster in solution, iterate table columns
          tmp_currentCol <- tmp_currentCol + 6

        } # End loop for proportions and description information for each cluster within current solution for [exportOutput]


        # Add plots ---------------------------------------------------------------

        # Set plotting rows and column increases depending on which plots are requested
        if(includeDistributionPlots | includeRadarPlots)
        {
          # # Set start rows for plots depending on if both sets of plots are present or not
          # if(includeRadarPlots)
          # {
          #   tmp_radarPlotStartRow = tmp_descriptionRow + length(dataColumns) + 4 # 3 rows below the end of the descriptions table
          #   tmp_radarPlotColIncrease = 7 # Number of cols to match 6 inches of plot width plus a margin
          #
          #   # If there are also distribution plots requested, set those values based on the radar plot start row value
          #   if(includeDistributionPlots & tmp_numClustersInSolution >= 3) # If there are 3 or more clusters, than radar plots are viable for this solution [s]
          #   {
          #     tmp_distrPlotStartRow = tmp_radarPlotStartRow + 25 # [tmp_radarPlotStartRow] plus 24 rows to skip radar plots (size of plot at 4 inches high)
          #     tmp_distrPlotColIncrease = 7 # Number of cols to match 6 inches of plot width plus a margin
          #   } else # Else there are too few data points for radar plots, so only a notice will be inserted in lieu of these plots
          #   {
          #     tmp_distrPlotStartRow = tmp_radarPlotStartRow + 4 # [tmp_radarPlotStartRow] plus 3 rows to skip the notice that there are no radar plots for solutions with fewer than 3 clusters
          #     tmp_distrPlotColIncrease = 7 # Number of cols to match 6 inches of plot width plus a margin
          #   }
          #
          # } else
          # {
          #   tmp_distrPlotStartRow = tmp_descriptionRow + length(dataColumns) + 4 # 3 rows below the end of the descriptions table
          #   tmp_distrPlotColIncrease = 7 # Number of cols to match 6 inches of plot width plus a margin
          # }

          # Set start rows for plots depending on if both sets of plots are present or not
          if(includeDistributionPlots)
          {
            tmp_distrPlotStartRow <- tmp_descriptionRow + length(dataColumns) + 4 # 3 rows below the end of the descriptions table
            tmp_distrPlotColIncrease <- 7 # Number of cols to match 6 inches of plot width plus a margin

            if(includeRadarPlots)
            {
              tmp_radarPlotStartRow <- tmp_distrPlotStartRow + 25 # [tmp_radarPlotStartRow] plus 24 rows to skip radar plots (size of plot at 4 inches high)
              tmp_radarPlotColIncrease <- 7 # Number of cols to match 6 inches of plot width plus a margin
            }
          } else if(includeRadarPlots)
          {
            tmp_radarPlotStartRow <- tmp_descriptionRow + length(dataColumns) + 4 # 3 rows below the end of the descriptions table
            tmp_radarPlotColIncrease <- 7 # Number of cols to match 6 inches of plot width plus a margin
          }


          # Create means table for plots. Use this table to establish variable plot order as well
          # Initialize variable means by cluster for current solution
          tmp_currentSolutionMeansStorage <- as.data.frame(matrix(, length(dataColumns), 1))
          names(tmp_currentSolutionMeansStorage)[1] <- "Variable"

          # Set first col to variable names
          tmp_currentSolutionMeansStorage[,1] <- sort(dataColumns)

          # Get cluster names and means for each cluster in current solution
          for(tmp_meansClusterInSolution in 1:tmp_numClustersInSolution)
          {
            # Get cluster mean from storage
            tmp_meansData <- tmp_clusterDescriptionsList[[tmp_clusterStorySolution]][[tmp_meansClusterInSolution]][[2]][,c(1:2)]

            # Write sorted data to i+1 to account for labels column
            tmp_currentSolutionMeansStorage <- left_join(tmp_currentSolutionMeansStorage, tmp_meansData, by = "Variable")


            # Set current original cluster name for checking and modifying
            tmp_originalClusterName <- tmp_clusterDescriptionsList[[tmp_clusterStorySolution]][[tmp_meansClusterInSolution]][[1]][1,5]

            # Set names of clusters
            if( grepl("^[0-9]+$", tmp_originalClusterName) ) # If Original ID is a number, set name to 'k=#'
            {
              # Set cluster name
              names(tmp_currentSolutionMeansStorage)[tmp_meansClusterInSolution + 1] <- paste0("k=", tmp_originalClusterName)
            } else
            {
              names(tmp_currentSolutionMeansStorage)[tmp_meansClusterInSolution + 1] <- tmp_originalClusterName
            }

          } # END LOOP OVER CLUSTERS FOR MEANS/NAMES



          # If [includeDistributionPlots], plot distribution plots
          if(includeDistributionPlots)
          {
            require(ggplot2)

            # Iterate over each variable and create distribution plots
            for(tmp_currentDistrPlotVariableNumber in 1:dim(tmp_currentSolutionMeansStorage)[1])
            {

              # Get data for current var [tmp_currentDistrPlotVariableNumber] across all clusters in current solution [tmp_plotClusterSolution]
              tmp_varDensityData <- clusterData %>% select(all_of(clusterSolutions[tmp_clusterStorySolution]), all_of(tmp_currentSolutionMeansStorage[tmp_currentDistrPlotVariableNumber,1]))
              names(tmp_varDensityData) <- c("tmp_plotClusterID", "tmp_plotVariable")

              # Drop any NAs
              tmp_varDensityData <- tmp_varDensityData %>% filter(!is.na(tmp_plotVariable))

              # Convert cluster IDs into a factor for ggplot
              tmp_varDensityData[,"tmp_plotClusterID"] <- as.factor(tmp_varDensityData[,"tmp_plotClusterID"])

              # Set means by cluster ID
              # tmp_clusterVarMeans <- tmp_varDensityData %>% group_by(tmp_plotClusterID) %>% summarize(clusterMean = mean(tmp_plotVariable, na.rm = T))
              tmp_clusterVarMeans <- as.data.frame(matrix(
                , length(table(tmp_varDensityData[,1]))
                , 2
              ))
              names(tmp_clusterVarMeans) <- c("tmp_plotClusterID", "clusterMean")

              tmp_clusterVarMeans[,1] <- names(table(tmp_varDensityData[,1]))
              tmp_clusterVarMeans[,2] <- unlist(c(tmp_currentSolutionMeansStorage[tmp_currentDistrPlotVariableNumber,-1]))


# HERE --------------------------------------------------------------------


              if( ( length(exportDecimalPlaces) == 1 & ( class(exportDecimalPlaces) == "numeric" | class(exportDecimalPlaces) == "integer" ) ) )
              {
                tmp_clusterVarMeans$clusterMean <- round(tmp_clusterVarMeans$clusterMean, exportDecimalPlaces)
              } else
              {
                # Set custom formatting for current variable [tmp_currentDistrPlotVariableNumber]
                tmp_varName <- tmp_currentSolutionMeansStorage[tmp_currentDistrPlotVariableNumber,1] #[tmp_currentDistrPlotVariableNumber]
                tmp_varFormatting <- exportDecimalPlaces %>% filter(Variables == tmp_varName)

                # Format percentages as [100*value]
                if(tmp_varFormatting$DataType == "%")
                {
                  tmp_clusterVarMeans$clusterMeanLabelPrep <- tmp_clusterVarMeans$clusterMean*100
                } else
                {
                  tmp_clusterVarMeans$clusterMeanLabelPrep <- tmp_clusterVarMeans$clusterMean
                }

                # Round to given digits
                tmp_clusterVarMeans$clusterMeanLabelPrep <- round(tmp_clusterVarMeans$clusterMeanLabelPrep, tmp_varFormatting$DataDecimals)
                # Add commas if necessary
                tmp_clusterVarMeans$clusterMeanLabelPrep <- formatC(tmp_clusterVarMeans$clusterMeanLabelPrep, format="f", big.mark=",", digits=tmp_varFormatting$DataDecimals)

                # Add '%' or '$' formatting to mean label
                if(tmp_varFormatting$DataType == "%")
                {
                  # Add '%'
                  tmp_clusterVarMeans$clusterMean_formatted <- paste0(tmp_clusterVarMeans$clusterMeanLabelPrep, "%")

                } else if(tmp_varFormatting$DataType == "$")
                {
                  # Add commas
                  # tmp_clusterVarMeans$clusterMean_formatted <- prettyNum(tmp_clusterVarMeans$clusterMeanLabelPrep, big.mark = ",", scientific = FALSE)
                  # formatC(tmp_clusterVarMeans$clusterMeanLabelPrep, format="f", big.mark=",", digits=tmp_varFormatting$DataDecimals)
                  # Add '$'
                  tmp_clusterVarMeans$clusterMean_formatted <- paste0("$", tmp_clusterVarMeans$clusterMeanLabelPrep)

                } else
                {
                  tmp_clusterVarMeans$clusterMean_formatted <- tmp_clusterVarMeans$clusterMeanLabelPrep
                }

              }


              # Create labels including means for plotting
              tmp_clusterVarMeans$clusterName <- names(tmp_currentSolutionMeansStorage)[-1]
              tmp_clusterVarMeans$meanPlotLabels <- paste0(tmp_clusterVarMeans$clusterName, " - mean: ", tmp_clusterVarMeans$clusterMean_formatted)

              # Attach 'meanPlotLabels' to [tmp_varDensityData]
              tmp_varDensityData <- left_join(tmp_varDensityData, tmp_clusterVarMeans[, c("tmp_plotClusterID", "meanPlotLabels")], by = "tmp_plotClusterID")
              tmp_varDensityData$meanPlotLabels <- as.factor(tmp_varDensityData$meanPlotLabels)


              # Set variables with more than one observation for distribution plotting. Others will be added with mean line only
              # tmp_allIDs <- table(tmp_varDensityData$tmp_plotClusterID) # NOTE: NAs have been dropped
              # tmp_multiObsIDs <- names(tmp_allIDs)[which(tmp_allIDs > 1)]
              # tmp_singleObsIDs <- names(tmp_allIDs)[which(tmp_allIDs == 1)]
              #
              # # Confirm all IDs are included in [tmp_multiObsIDs] and [tmp_singleObsIDs]
              # if( !all( sort(names(tmp_allIDs)) == sort(c(tmp_multiObsIDs, tmp_singleObsIDs)) ) )
              # {
              #   stop("Some cluster IDs have a negative(?) count")
              # }
              #
              # # Split data
              # tmp_varDensityData_multiObsOnly <- tmp_varDensityData %>% filter(tmp_plotClusterID %in% tmp_multiObsIDs)

              # Create density plot lines by cluster ID
              tmp_distrPlot <- ggplot(tmp_varDensityData, aes(x=tmp_varDensityData[,2], fill = tmp_varDensityData[,3], color=tmp_varDensityData[,3])) +
              geom_density(alpha = 0.25) +
              geom_vline(data=tmp_clusterVarMeans, aes(xintercept=clusterMean, color=meanPlotLabels), linetype="dashed") +
              xlab(tmp_currentSolutionMeansStorage[tmp_currentDistrPlotVariableNumber,1]) +
              ggtitle( paste0("Distribution of ["
                , tmp_currentSolutionMeansStorage[tmp_currentDistrPlotVariableNumber,1]
                ,"] Values by Cluster") ) +
              theme(legend.title = element_blank(), legend.position="bottom") +
              guides(fill = guide_legend(nrow = 5))

              if(tmp_varFormatting$DataType == "%")
              {
                tmp_distrPlot <- tmp_distrPlot + scale_x_continuous(label=scales::label_percent())
              } else if(tmp_varFormatting$DataType == "$")
              {
                tmp_distrPlot <- tmp_distrPlot + scale_x_continuous(label=scales::label_dollar())
              } else
              {
                tmp_distrPlot <- tmp_distrPlot + scale_x_continuous(label=scales::label_comma())
              }


              suppressWarnings(print(tmp_distrPlot))

              # Add variable plot to workbook
              insertPlot(tmp_wb, tmp_worksheetName, width = 6, height = 4
                , startRow = tmp_distrPlotStartRow
                , startCol = (tmp_currentDistrPlotVariableNumber-1) * tmp_distrPlotColIncrease + 1
                , fileType = "png", units = "in", dpi = 300)

              if(plotQuietly)
              {
                dev.off()
              }

            } # End plotting variable distributions

          } # End [includeDistributionPlots]



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

              for(tmp_currentRadarPlotVariableNumber in 1:dim(tmp_currentSolutionMeansStorage)[1])
              {
                # print(tmp_currentSolutionMeansStorage[tmp_currentRadarPlotVariableNumber,1])

                tmp_plotDat <- tmp_currentSolutionMeansStorage[tmp_currentRadarPlotVariableNumber, -1]
                tmp_maxDat <- as.data.frame( matrix(max(tmp_plotDat, na.rm = T), 1, (dim(tmp_plotDat)[2])) )
                tmp_minDat <- as.data.frame( matrix(min(tmp_plotDat, na.rm = T), 1, (dim(tmp_plotDat)[2])) )

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
                  , title = tmp_currentSolutionMeansStorage[tmp_currentRadarPlotVariableNumber, 1]
                  , axistype = 1
                  , axislabcol = "#222222"
                  , pcol = 'black'
                  # , paxislabels
                )


                # Add plot
                insertPlot(tmp_wb, tmp_worksheetName, width = 6, height = 4
                  , startRow = tmp_radarPlotStartRow
                  , startCol = (tmp_currentRadarPlotVariableNumber-1) * tmp_radarPlotColIncrease + 1
                  , fileType = "png", units = "in", dpi = 300)

                if(plotQuietly)
                {
                  dev.off()
                }
              } # End loop for each variable to plot

            } # End loop for multivar radar plots
          } # End [includeRadarPlots]


        } # END PLOTTING #

      } # END ADD CLUSTER DESCRIPTIONS TO EXCEL #

    } # END EXPORT OF CLUSTER DESCRIPTIONS #

  } # END CREATING LIST OF CLUSTER DESCRIPTIONS #


  # EXPORT IF REQUESTED
  if(exportOutput)
  {
    print("Exporting Excel file to current working directory:")
    print(getwd())

    # Set timestamp for file name
    tmp_timestamp <- gsub(":", "-", Sys.time())
    tmp_timestamp <- gsub(" ", "_", tmp_timestamp)
    tmp_timestamp <- gsub("\\.[0-9]+", "", tmp_timestamp)
    # Export excel file
    saveWorkbook(tmp_wb, paste0("Cluster Descriptions ", tmp_timestamp, ".xlsx"), TRUE)
  }

  # Concatenate and return cluster info
  # tmp_clusterInfoOutputObjects <- c('tmp_clusterFitMetrics', 'tmp_clusterDescriptionsList')
  # print(tmp_clusterInfoOutputObjects)
  #
  # tmp_clusterInfoObjectsCheck <- sapply(tmp_clusterInfoOutputObjects, exists)
  # print(tmp_clusterInfoObjectsCheck)

  if(includeClusterFitMetrics) # If metrics were calculated...
  {
    if(includeClusterDescriptions) # If descriptions were also calculated, return both
    {
      tmp_clusterInfoOutput <- list(
        "Fit Metrics"
        , tmp_clusterFitMetrics
        , "Descriptions"
        , tmp_clusterDescriptionsList
      )

      return(tmp_clusterInfoOutput)

    } else # Else return only fit metrics
    {
      tmp_clusterInfoOutput <- list(
        "Fit Metrics"
        , tmp_clusterFitMetrics
      )

      return(tmp_clusterInfoOutput)
    }
  } else if(includeClusterDescriptions) # If only descriptions were calculated, return descriptions
  {
    tmp_clusterInfoOutput <- list(
      "Descriptions"
      , tmp_clusterDescriptionsList
    )

    return(tmp_clusterInfoOutput)
  } else # Else return blank list
  {
    tmp_clusterInfoOutput <- list("")

    return(tmp_clusterInfoOutput)
  }

} ## END FUNCTION [createClusterDescriptions] ##

#
# # -------------------------------------------------------------------------
# tmpClusterDescription <- tmp_clusterInfoOutput
# # save(tmpClusterDescription, file = "tmpClusterDescriptionObject.Rda")
# load(file = "tmpClusterDescriptionObject.Rda")
#
# describeObservation <- function(observationID, clusterID, clusterDescriptions)
# {
#
# } ## END FUNCTION [describeObservation] ##
