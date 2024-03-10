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
# Updated: 2024-03-09
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

# Data quality checks revolve around checks for 'numeric' types for column numbers. These can be passed as doubles, and R can call at least some of them as proper column numbers even though they are not integers

#  --------------------------------------------------------------------
## STRUCTURE ##
#  - Function: [describeClusters]
#      - Handler function
#      - Sanitizes data
#      - Calls other functions as requested
#      - Exports data if requested
#      - Can call:
#          - [fitClusterMetrics]
#          - [createClusterDescriptions]
#          - [plotClusterData]

#  - Function: [fitClusterMetrics]
#      - Fits cluster metrics
#      - Requires distances
#      - Requires a list of metrics from 'cluster.stats' in the {fpc} library

#  - Function: [createClusterDescriptions]
#      - Fill out list of tables containing cluster proportions and means/std diffs
#      - Returns a list of lists of data.frames. One list for each cluster solution, made up of a list of two data.frames for

#  - Function: [plotClusterData]
#      - Create radar plots of means by cluster for each variable
#      - Create distribution plots of means and distribution by cluster for each variable




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
fitClusterMetrics <- function(clusterData, clusterSolutionsToFitMetricsOn, clusterDistances, clusterFitMetrics = c("within.cluster.ss", "avg.silwidth", "ch", "wb.ratio"))
{
  require(tidyverse)
  require(fpc) # For 'cluster.stats' command

  tmp_numClustSolutions <- length(clusterSolutionsToFitMetricsOn)

  # Set fit metrics
  # Defaults:
    # "within.cluster.ss" # Within-cluster sum of squares
    # "avg.silwidth" # Average silhouette width
    # "ch" # Calinski-Harabasz index
    # "wb.ratio" # Within/between SSE ratio

  if(clusterFitMetrics[1] == "")
  {
    print("WARNING: No fit metrics given")
    return(0)
  } else # Check that each given cluster metric is valid if not using defaults
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
  tmp_clustFitMetricStorage[,1] <- clusterSolutionsToFitMetricsOn
  # Fill in 'Cluster Number' column with max of cluster IDs for each solution set, i.e. number of clusters in given solution
  tmp_clustFitMetricStorage[,2] <- sapply(clusterData %>% select(all_of(clusterSolutionsToFitMetricsOn)), function(x) {length(unique(x))})


  # Calculate cluster metrics for each [clusterSolutionsToFitMetricsOn]
  for(tmp_clustSolution in clusterSolutionsToFitMetricsOn)
  {
    print(paste0("Fitting metrics for [", tmp_clustSolution, "]"))

    tmp_clustIDs <- unlist(clusterData %>% select(all_of(tmp_clustSolution))) # Store cluster IDs

    # If cluster IDs are not numeric, or are not ordered from 1:(number of clusters) then the 'cluster.stats()' command will not work
    # Therefore, create a new vector of the correct format that overwrites [tmp_clustIDs]
    if(class(tmp_clustIDs) == "character"
      | !all( sort(unique(tmp_clustIDs)) == c(1:length(unique(tmp_clustIDs))) )
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

      # print(table(tmp_clustIDs, clusterData[,tmp_clustSolution]))
    }

    tmp_clustStats <- cluster.stats(clusterDistances, tmp_clustIDs) # Get cluster statistics

    tmp_numberStartingCols <- 2 # The (2) starting columns are 'ClusterSolutionName' and 'NumClusters'. This is the offset for downstream column selection

    tmp_currentSolutionRow <- which(clusterSolutionsToFitMetricsOn == tmp_clustSolution) # Set row of [tmp_clustFitMetricStorage] for current cluster solution [tmp_clustSolution]

    # Confirm 'cluster.number' == tmp_clustFitMetricStorage[,2]
    if(!tmp_clustFitMetricStorage[tmp_currentSolutionRow,2] == tmp_clustStats$cluster.number)
    {
      stop("Number of clusters in solution do not match 'cluster.stats' output")
    }

    # Store desired cluster metrics
    tmp_clustFitMetricStorage[tmp_currentSolutionRow, (1:length(clusterFitMetrics) + tmp_numberStartingCols)] <- tmp_clustStats[ clusterFitMetrics ]

    rm(tmp_clustStats)
  }

  return(tmp_clustFitMetricStorage)

} ## END FUNCTION [fitClusterMetrics] ##



# -------------------------------------------------------------------------


describeClusters <- function(clusterData, uniqueID, clusterSolutions, dataColumns, runQuietly = FALSE, exportOutput = TRUE, exportDecimalPlaces = 3, exportPositiveNegativeSignificanceColors = c("#33F5B7", "#FCB099"), includeClusterFitMetrics = FALSE, clusterDistances = "", clusterFitMetrics = c("within.cluster.ss", "avg.silwidth", "ch", "wb.ratio"), clusterSolutionsToFitMetricsOn = "")
# includeClusterDescriptions = TRUE, includeRadarPlots = TRUE, includeDistributionPlots = TRUE, dataFormattingMatrix = "")
{
  ## LIBRARY REQUIREMENTS ##
  # - {tidyverse} for general use
  # - {fpc} for clustering fit metrics
  # - {fmsb} for radar plots
  # - {ggplot2} for distribution plots
  # - {openxlsx} for exporting to Excel

  ## PARAMETER REQUIREMENTS ##
  # - [clusterData] must be a data.frame
  # - [uniqueID] must be a single column number or name that is in [clusterData]
  # - [clusterSolutions] must be a list of column numbers only, or of column names only, that are in [clusterData]

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

  # Reorder [clusterSolutions] by number of clusters (i.e. max cluster ID by column)
  clusterSolutions <- clusterSolutions[
    order(
      sapply(clusterData %>% select(all_of(clusterSolutions)), FUN = function(x) {length(unique(x))})
      )
  ]


# -------------------------------------------------------------------------


  # If [exportOutput] if TRUE, then begin export process with cleaned data
  if(exportOutput)
  {
    require(openxlsx)

    # [exportDecimalPlaces] must be a single number, or a data.frame
    if( (length(exportDecimalPlaces) == 1 & (class(exportDecimalPlaces) == "numeric" | class(exportDecimalPlaces) == "integer")) )
    {
      # [exportDecimalPlaces] must be a whole number
      exportDecimalPlaces <- round(exportDecimalPlaces)

      # If [exportDecimalPlaces] is a single number, is must be greater than 0
      if(exportDecimalPlaces < 0)
      {
        print("NOTE: [exportDecimalPlaces] must be non-negative. It has been converted to the default value of 3")
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

      # Round [exportDecimalPlaces] decimals to be whole numbers if not given as such
      exportDecimalPlaces[,3] = round(exportDecimalPlaces[,3], 0)

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

    # Set Formats #

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

    tmp_style_clusterHeader <- createStyle(fontSize = 14, textDecoration = "Bold")
    tmp_style_count <- createStyle(numFmt="#,##0")
    tmp_style_pct <- createStyle(numFmt="0.0%")
    tmp_style_bold <- createStyle(textDecoration = "Bold")
    tmp_style_boldAndCenter <- createStyle(textDecoration = "Bold", halign = "Center")

    if(class(exportDecimalPlaces) == 'data.frame')
    {
      tmp_style_floatMetricsTable <- createStyle(numFmt= paste0( "#,##0.0", paste0(rep("0", (exportDecimalPlaces[1,3]-1)), collapse = "") ) )
      tmp_style_floatDescriptionTable <- createStyle(numFmt= paste0( "#,##0.0", paste0(rep("0", (exportDecimalPlaces[2,3]-1)), collapse = "") ) )
    } else
    {
      tmp_style_floatMetricsTable <- createStyle(numFmt= paste0( "#,##0.0", paste0(rep("0", (exportDecimalPlaces-1)), collapse = "") ) )
      tmp_style_floatDescriptionTable <- tmp_style_floatMetricsTable
    }

    # Create parameters workbook
    addWorksheet(tmp_wb, "Parameters")
    writeData(tmp_wb, "Parameters", "Standard difference threshold:", startCol = 2, startRow = 2)
    writeData(tmp_wb, "Parameters", 0.5, startCol = 3, startRow = 2)
  } # END EXPORT - INITIALIZATION #


# -------------------------------------------------------------------------


  # If [includeClusterFitMetrics] is TRUE, then check for necessary parameters to fit cluster metrics
  if(includeClusterFitMetrics)
  {
    if(clusterDistances[1] == "")
    {
      print("NOTE: Distances using the 'dist()' command must be provided in [clusterDistances] for fit metrics to be calculated")
      includeClusterFitMetrics = FALSE
    } else
    {
      # If no subset of clusters solutions is given specifically for metrics to be fit on, use all cluster solutions
      if(clusterSolutionsToFitMetricsOn[1] == "")
      {
        clusterSolutionsToFitMetricsOn <- clusterSolutions
      }

      # Call function [fitClusterMetrics] and store results
      tmp_clusterFitMetrics <- fitClusterMetrics(clusterData, clusterSolutionsToFitMetricsOn, clusterDistances, clusterFitMetrics)
    }

    # Remove distances for fit metrics (often a very large object)
    rm(clusterDistances)
  } # END FIT METRICS #

# -------------------------------------------------------------------------

    # If exporting, add fit metrics data to Excel file from within the [includeClusterFitMetrics] check
  if(exportOutput & includeClusterFitMetrics)
  {
    # Set [tmp_numMetrics] to be the number of metrics fit
    if(clusterFitMetrics[1] == "")
    {
      tmp_numMetrics <- 4
    } else
    {
      tmp_numMetrics <- length(clusterFitMetrics)
    }

    # Create cluster metrics and plots workbook
    addWorksheet(tmp_wb, "Cluster Metrics")
    writeData(tmp_wb, "Cluster Metrics", tmp_clusterFitMetrics, startRow = 2, startCol = 2)
    addStyle(tmp_wb, "Cluster Metrics", style = tmp_style_floatMetricsTable, rows = c(3:(3+length(clusterSolutionsToFitMetricsOn)-1)), cols = c(4:(4+tmp_numMetrics-1)), gridExpand = TRUE)



# HERE -> (tmp_numberStartingCols missing) --------------------------------


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
      writeData(tmp_wb, "Cluster Metrics", "Clusters excluded from fit metrics calculations:", startRow = 2, startCol = tmp_startExcludedMetricsSolutionNotesColumn)
      addStyle(tmp_wb, "Cluster Metrics", style=tmp_style_bold, rows = 2, cols = tmp_startExcludedMetricsSolutionNotesColumn)

      for(tmp_excludedSolutionNumber in 1:length(tmp_metricsExclusionsList))
      {
        writeData(tmp_wb, "Cluster Metrics", tmp_metricsExclusionsList[tmp_excludedSolutionNumber], startCol = tmp_startExcludedMetricsSolutionNotesColumn, startRow = 2+tmp_excludedSolutionNumber)
      }
    }

    # Set rows for spacing out metrics plots
    tmp_plotStartRow = dim(tmp_clusterFitMetrics)[1] + 5 # 5 rows below the end of the [tmp_clusterFitMetrics] table
    tmp_plotRowIncrease = 22 # Number of rows to match 4 inches of plot height plus a margin
    tmp_numPlots = 0 # Initialize plot count to 0

    # Iterate over fit metrics
    for(tmp_metricToPlot in 3:dim(tmp_clusterFitMetrics)[2])
    {
      plot(c(1:dim(tmp_clusterFitMetrics)[1]), tmp_clusterFitMetrics[,tmp_metricToPlot], type = 'l', main = names(tmp_clusterFitMetrics)[tmp_metricToPlot], xlab = "Cluster Number", xaxt = "n", ylab = "Metric Value")
      axis(side = 1, at = c(1:dim(tmp_clusterFitMetrics)[1]), labels = tmp_clusterFitMetrics[,1])
      grid()

      # Add plot
      insertPlot(tmp_wb, "Cluster Metrics", width = 6, height = 4,
      startRow = tmp_plotStartRow + tmp_plotRowIncrease * tmp_numPlots, startCol = 2, fileType = "png", units = "in", dpi = 300)

      # Iterate plot count
      tmp_numPlots = tmp_numPlots + 1
    }

  } # END EXPORT - FIT METRICS #

# -------------------------------------------------------------------------



} ## END FUNCTION [describeClusters] ##


createClusterDescriptions <- function()
{
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
  } # End loop across all solutions

} ## END FUNCTION [createClusterDescriptions] ##


