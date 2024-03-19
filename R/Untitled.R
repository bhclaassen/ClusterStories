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
 }


if(includeRadarPlots & includeDistributionPlots)
{
  print(tmp_radarPlotStartRow)
  print(tmp_distrPlotStartRow)

} else if(includeRadarPlots)
{
  print("radar")
  print(tmp_radarPlotStartRow)

} else if(includeDistributionPlots)
{
  print("distr")
  print(tmp_distrPlotStartRow)
}

