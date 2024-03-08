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
# Updated: 2024-03-07
# -------------------------------------------------------------------------


# TODO --------------------------------------------------------------------

# Add check for total sample size >2
# Make sure distr plots works with single observation (mean only)
# Add functionality without variance so a set of singletons can be compared
# Any way to look for missing combinations? business opportunity for gap in the market
# Add variable precision and units secondary table
# Add one-pager describing how to use product
# Create structure overview
# Test input values
# Test [clusterSolutionsToFitMetricsOn]
# Drop all vars after done using them internally?
# Try with clusters of single values for div 0 errors
# Add single variable description for R using returned descriptions list from main function
# Add confusion tables to compare movement of observations across clusters
# Add nice names for metric printing
# CHECK CLUSTER METRICS, CAN PLOT THEM ALL? NO, NO I CAN'T
# Force solutions to end up as 1:n if any clusters are missing in between

# Check for FIX comments!!
# Check for TODO comments!!
# Check for DELETE comments!!

# Change pooled variance to
#   - Hedges for <=20
# <https://www.statisticshowto.com/hedges-g/>
#   - Cohens for >30
# <https://www.statisticshowto.com/probability-and-statistics/statistics-definitions/cohens-d/>

# DOES R HAVE A DEBUG MODE LIKE PYCHARM?!


# -------------------------------------------------------------------------

describeClusters <- function(clusterData, uniqueID, clusterSolutions, dataColumns, clusterDistances = "", clusterFitMetrics = "", clusterSolutionsToFitMetricsOn = "", exportSignificantDigits = 3, includeClusterDescriptions = TRUE, includeRadarPlots = TRUE, includeDistributionPlots = TRUE, includeClusterFitMetrics = TRUE, exportOutput = TRUE, positiveNegativeSignificanceColors = c("#33F5B7", "#FCB099"), dataFormattingMatrix = "")
{
  ## LIBRARY REQUIREMENTS ##
  # - {tidyverse} for general use
  # - {fpc} for clustering fit metrics
  # - {fmsb} for radar plots
  # - {ggplot2} for distribution plots
  # - {openxlsx} for exporting to Excel

  ## FUNCTION - Cluster Descriptions ##
  # Inputs:
  #    - [clusterData]: Dataset formatted as a data.frame
  #    - [uniqueID]: Unique ID column name/number for dataset
  #    - [clusterSolutions]: List of column names/numbers containing cluster ID assignments
  #    - [dataColumns]: List of column names/numbers containing the data for describing the clusters
  #    - [clusterDistances]: List of distances between points, from 'dist' command (default = "")
  #    - [clusterFitMetrics]: List of cluster fit metrics to calculate, from 'cluster.stats' in the {fpc} library (default = "")
  #    - [clusterSolutionsToFitMetricsOn]: List of cluster solutions in [clusterSolutions] to calculate fit metrics for (default = "")
     - [exportSignificantDigits] = 3
  #    - [includeClusterDescriptions]: Bool, if cluster fit descriptions should be calculated (default = TRUE)
  #    - [includeRadarPlots]: Bool, if radar plots of variable/cluster means should be created; requires {fmsb} library (default = TRUE)
  #    - [includeDistributionPlots]: Bool, if distribution plots of variable/cluster means should be created; requires {ggplot2} library (default = TRUE)
  #    - [includeClusterFitMetrics]: Bool, if cluster fit metrics should be calculated; requires {fpc} library (default = TRUE)
  #    - [exportOutput]: Bool, if the results should be exported to a Excel file; requires {openxlsx} library (default = TRUE)
  #    - [positiveNegativeSignificanceColors]: List of two colors to replace the significance indicators in the Excel output (default = c("#33F5B7", "#FCB099"))
  #    - [dataFormattingMatrix]: Matrix of data types and data decimals for formatting in Excel export file (default = "" -> this will return 3 decimals for all variables)

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


     ## FUNCTION - Analyze Clusters
# Inputs:
#    - Data with uniqueID and cluster assignments
# Outputs:
#    - Cluster metric values
#    - Cluster metric plots
#    - Cluster descriptions if desired
#    - Excel file output if desired
# -------------------------------------------------------------------------

  require(tidyverse)
  options(scipen = 9999)

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

  # Reorder [clusterSolutions] and [clusterNamesColumns] by number of clusters (i.e. max cluster ID by column)
  clusterSolutions <- clusterSolutions[
    order(
      sapply(clusterData %>% select(all_of(clusterSolutions)), FUN = function(x) {length(unique(x))})
      )
  ]


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
  # If [includeClusterFitMetrics] is TRUE but no distances are given, then fit metrics cannot be calculated
  if(includeClusterFitMetrics & length(clusterDistances) == 1)
  {
    includeClusterFitMetrics = FALSE
    print("NOTE: Calculating fit metrics require distances given in [clusterDistances]. [includeClusterFitMetrics] changed to FALSE")
  }

  if(includeClusterFitMetrics)
  {
    require(fpc)

    if(clusterSolutionsToFitMetricsOn[1] == "")
    {
      metricClusterSolutions <- clusterSolutions
      tmp_numClustSolutions <- length(metricClusterSolutions)
    } else
    {
      metricClusterSolutions <- clusterSolutionsToFitMetricsOn
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
      print(paste0("Fitting metrics for [", tmp_clustSolution, "]"))

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
    if(length(positiveNegativeSignificanceColors) != 2 | positiveNegativeSignificanceColors == "")
    {
      # Defaults:
      tmp_style_posSig <- createStyle(bgFill = "#33F5B7")
      tmp_style_negSig <- createStyle(bgFill = "#FCB099")
    } else
    {
      tmp_style_posSig <- createStyle(bgFill = positiveNegativeSignificanceColors[1])
      tmp_style_negSig <- createStyle(bgFill = positiveNegativeSignificanceColors[2])
    }

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

      # If any cluster solutions have been excluded from the metrics calculations, then note their exclusion in the 'Cluster Metrics' tab
      if( !all(clusterSolutions %in% clusterSolutionsToFitMetricsOn) )
      {
        # Set start column for listing missing solutions from metrics:
        #   => [2 for space for metrics table]
        #      + [2 for metrics table headers (solution name and number of clusters)]
        #      + [the number metrics included (length(clusterFitMetrics))]
        #      + [3 for spacing from table and to be outside of graphs if list of exclusions is long]
        tmp_startExcludedMetricsSolutionNotesColumn <- 2 + 2 + length(clusterFitMetrics) + 3

        # Pull missing solutions
        tmp_metricsExclusionsList <- clusterSolutions[which(!(clusterSolutions %in% clusterSolutionsToFitMetricsOn))]

        # Write note of missing solutions
        writeData(tmp_wb, "Cluster Metrics", "Clusters excluded from fit metrics calculations:", startRow = 2, startCol = tmp_startExcludedMetricsSolutionNotesColumn)
        addStyle(tmp_wb, "Cluster Metrics", style=tmp_style_bold, rows = 2, cols = tmp_startExcludedMetricsSolutionNotesColumn)

        for(tmp_excludedSolutionNumber in 1:length(tmp_metricsExclusionsList))
        {
          writeData(tmp_wb, "Cluster Metrics", tmp_metricsExclusionsList[tmp_excludedSolutionNumber], startCol = tmp_startExcludedMetricsSolutionNotesColumn, startRow = 2+tmp_excludedSolutionNumber)
        }
      }


      # Set rows for spacing out metrics plots
      tmp_plotStartRow = dim(tmp_clustFitMetricStorage)[1] + 5 # 5 rows below the end of the [tmp_clustFitMetricStorage] table
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

    } # End [includeClusterFitMetrics] if statement


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
      names(tmp_clusterSolution_clusterNames) <- c('clusterName', 'tmp_plotClusterID')

      # Set current solution name
      tmp_plotClusterSolution <- clusterSolutions[s]

      # Initialize cluster colors
      # First eight colors from <https://developer.r-project.org/Blog/public/2019/11/21/a-new-palette-for-r/>
      # Next twelve from a combination of [RColorBrewer] library
      if(distributionPlotColors[1] == "" | (distributionPlotColors[1] != "" & length(distributionPlotColors) < tmp_numClustersInSolution))
      {
        tmp_allPlottingColors <- c(
        "#000000", "#df536b", "#61d04f", "#2297e6"
        , "#28e2e5", "#cd0bbc", "#f5c710", "#626262"
        , "#276419", "#2D004B", "#D53E4F", "#542788"
        , "#F46D43", "#66C2A5", "#7F3B08", "#7FBC41"
        , "#5E4FA2", "#9E0142", "#DE77AE", "#FDB863")

        if(distributionPlotColors[1] != "" & length(distributionPlotColors) < tmp_numClustersInSolution)
        {
          print(paste0("Too few colors were given for solution [", tmp_plotClusterSolution, "] (" , tmp_numClustersInSolution, " clusters). Default colors used for this solution."))
        }
      } else
      {
        tmp_allPlottingColors = distributionPlotColors
      }

      # Assign cluster colors based on number of clusters in current solution
      if(tmp_numClustersInSolution <= 20)
      {
        tmp_clusterColors <- tmp_allPlottingColors[1:tmp_numClustersInSolution]
        tmp_ifTooManyClustersForColors = FALSE
      } else
      {
        tmp_clusterColors <- tmp_allPlottingColors
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

      # Iterate over clusters and write the stories to the workbook [tmp_wb]
      for(c in 1:tmp_numClustersInSolution)
      {
        # Write cluster number
        writeData(tmp_wb, tmp_worksheetName, paste0("Cluster ", tmp_clusterDescriptionsStorage[[s]][[c]][[1]][1,1]), startCol = tmp_currentCol, startRow = tmp_titleRow)

        # Write cluster name and color to titles
        if(tmp_clusterDescriptionsStorage[[s]][[c]][[1]][1,5] != "")
        {
          # Cluster name
          writeData(tmp_wb, tmp_worksheetName, tmp_clusterDescriptionsStorage[[s]][[c]][[1]][1,5], startCol = tmp_currentCol + 1, startRow = tmp_titleRow)
          # Cluster color
          # tmp_style_clusterColor <- createStyle(fgFill = tmp_clusterColors[tmp_clusterDescriptionsStorage[[s]][[c]][[1]][1,1]])
          # addStyle(tmp_wb, tmp_worksheetName, style=tmp_style_clusterColor, cols = tmp_currentCol + 2, rows = tmp_titleRow, stack = F)
        } else # If no cluster name given, only write color
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


# STOP HERE ---------------------------------------------------------------


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

        # Set proper names for storage if given
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
          require(ggplot2)

          # If there are more clusters than colors assigned (max of 20), then print warning and change plot row to account for it
          if(tmp_ifTooManyClustersForColors)
          {
            writeData(tmp_wb, tmp_worksheetName, "NOTE: The number of clusters exceeds the plotting capability (20 cluster maximum). Clusters above this number are excluded from plots.", startRow = tmp_distrPlotStartRow, startCol = 2)
            tmp_distrPlotStartRow = tmp_distrPlotStartRow + 2
          }

          # NOTE: This function only has enough colors to make max number plots, i.e. length(tmp_allPlottingColors)
          # Set the clusters to plot to be that if actual solution has more
          tmp_numPlottableClustersInSolution <- min(tmp_numClustersInSolution, length(tmp_allPlottingColors))

          # Iterate over each variable and create distribution plots
          for(tmp_currentDistrPlotVariableNumber in 1:dim(tmp_currentSolutionMeansStorage)[1])
          {
            print(tmp_currentDistrPlotVariableNumber)
            # Get data for current var [tmp_currentDistrPlotVariableNumber] across all clusters in current solution [tmp_plotClusterSolution]
            tmp_varDensityData <- clusterData %>% select(all_of(tmp_plotClusterSolution), all_of(tmp_currentSolutionMeansStorage[tmp_currentDistrPlotVariableNumber,1]))
            names(tmp_varDensityData) <- c("tmp_plotClusterID", "tmp_plotVariable")

            # Convert cluster IDs into a factor for ggplot
            tmp_varDensityData[,"tmp_plotClusterID"] <- as.factor(tmp_varDensityData[,"tmp_plotClusterID"])

            # Set means by cluster ID
            tmp_clusterVarMeans <- tmp_varDensityData %>% group_by(tmp_plotClusterID) %>% summarize(clusterMean = mean(tmp_plotVariable, na.rm = T))
            tmp_clusterVarMeans$clusterMean <- round(tmp_clusterVarMeans$clusterMean,3)

            tmp_clusterSolution_clusterNames$clusterID <- as.factor(tmp_clusterSolution_clusterNames$clusterID)

            # Change labels to include means for plotting
            if(any(tmp_clusterSolution_clusterNames[,1] != "" ))
            {
              tmp_clusterVarMeans <- left_join(tmp_clusterVarMeans, tmp_clusterSolution_clusterNames, join_by("tmp_plotClusterID"=="clusterID"))
            } else
            {
              tmp_clusterVarMeans$clusterName <- paste0("k=", tmp_clusterVarMeans$tmp_plotClusterID)
            }

            tmp_clusterVarMeans$meanPlotLabels <- paste0(tmp_clusterVarMeans$clusterName, " - mean: ", tmp_clusterVarMeans$clusterMean)

            # Attach 'meanPlotLabels' to [tmp_varDensityData]
            tmp_varDensityData <- left_join(tmp_varDensityData, tmp_clusterVarMeans[, c("tmp_plotClusterID", "meanPlotLabels")], by = "tmp_plotClusterID")
            tmp_varDensityData$meanPlotLabels <- as.factor(tmp_varDensityData$meanPlotLabels)

            # Change density plot line colors by groups
            ggplot(tmp_varDensityData, aes(x=tmp_varDensityData[,2], fill = tmp_varDensityData[,3], color=tmp_varDensityData[,3])) +
            geom_density(alpha = 0.25) +
            geom_vline(data=tmp_clusterVarMeans, aes(xintercept=clusterMean, color=meanPlotLabels), linetype="dashed") +
            xlab(tmp_currentSolutionMeansStorage[tmp_currentDistrPlotVariableNumber,1]) +
            ggtitle( paste0("Distribution of ["
              , tmp_currentSolutionMeansStorage[tmp_currentDistrPlotVariableNumber,1]
              ,"] Values by Cluster") ) +
            theme(legend.title = element_blank(), legend.position="bottom") +
            guides(fill = guide_legend(nrow = 5))

            # annotate(x=27, y=0, label="xyz", color="red") +annotate(x=27, ymin=-1, ymax=1, color="red")



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
