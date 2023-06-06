#' Correspondence Analysis with Geometric Frequency Interpretation
#'
#' @description This function performs Correspondence Analysis on the given data frame and plots the results in a scatterplot that emphasizes
#' the geometric interpretation aspect of the analysis (Borg-Groenen 2005; Yelland 2010). It is particularly useful for highlighting the relationships between
#' a selected row (or column) category and the column (or row) categories.\cr
#' Visit this \href{https://drive.google.com/file/d/1sZKPEqOqQ7VpfIn9DnGgdrS_K8p7cbvf/view?usp=share_link}{LINK} to access the package's vignette.\cr
#'
#' @details \strong{Overview}\cr The function follows a visualization approach outlined, e.g., by Borg-Groenen 2005 and Yelland 2010.
#' This method allows the relative frequencies of categories in the dataset to be intuitively read off the plot.
#' The function first draws a line through the origin and the point corresponding to the selected reference category.
#' Perpendicular lines are then dropped from each category’s position on the plot to the line connecting the reference category and the origin.
#'
#' \strong{Interpretation}\cr
#' The relative frequencies of the categories can be inferred by looking at the positions at which the perpendiculars from the categories intersect this line.
#' Categories with an intersection on the same side of the origin as the reference category occur more often than the average; the further from the
#' origin an intersection occurs, the higher the frequency. Conversely, categories with an intersection on the opposite
#' side occur less frequently; in this case, the further from the origin an intersection occurs, the smaller the frequency.
#'
#' \strong{Limitations}\cr
#' The method implemented here complements, but does not replace, the nuanced interpretation of a correspondence analysis output gleaned from a
#' typical scatterplot of row and column categories. While this approach may not be ideally suited for handling large tables, it provides a valuable tool
#' for the visual interpretation of small-to-medium size tables, particularly for non-expert audiences.
#' This method simplifies the understanding of some aspects of the data and can make the analytical results more accessible.
#'
#'
#' @param df A cross-tabulation (dataframe) for which the correspondence analysis is performed.
#' @param dims A numeric vector specifying the dimensions to be plotted (default: c(1,2)).
#' @param ref.category The reference category for interpreting the plot. Must be a row or column name of the input dataframe.
#' Note that, to enhance visual focus on the selected category, the other categories are rendered in grey.
#' @param dot.size A numerical value representing the size of dots in the plot (default: 2)
#' @param label.size A numerical value representing the size of labels in the plot (default: 3).
#' @param axis.title.size A numerical value specifying the size of the axis titles (default: 8).
#' @param equal.scale Logical value indicating whether to use the same scale for both axes (default: TRUE).
#'
#' @return A scatterplot visualizing the results of the Correspondence Analysis with geometric frequency interpretation.
#'
#' @references Borg, I., & Groenen, P. J. F. (2005). Modern Multidimensional Scaling: Theory and Applications.
#' Springer Science & Business Media.
#'
#' @references Yelland, P. (2010). An Introduction to Correspondence Analysis.
#'  In The Mathematica Journal (Vol. 12). Wolfram Research, Inc.
#'
#' @export
#'
#' @import ggplot2
#' @import ggrepel
#' @import ca
#'
#' @examples
#'
#' # EXAMPLE 1
#' # Build a toy dataset (the famous Greenacre's "smoke" dataset).
#'
#' mytable <- structure(list(none = c(4, 4, 25, 18, 10), light = c(2, 3, 10,
#' 24, 6), medium = c(3, 7, 12, 33, 7), heavy = c(2, 4, 4, 13, 2
#' )), row.names = c("SM", "JM", "SE", "JE", "SC"), class = "data.frame")
#'
#' # Run the function, using the "heavy" smoking as reference category
#'
#' caplot(mytable, ref.category="heavy")
#'
#' # In the returned scatterplot, it can be seen that the JM and SM categories
#' # feature a larger-than-average proportion of heavy smokers, whereas SC,
#' # SE, and JE feature a smaller-than-average proportion.
#' # Also, JM intersects the reference category line at a larger
#' # distance compared to the SM category, which indicates that the proportion
#' # of heavy smokers in JM is larger than in SM.
#' # Finally, SC features the smallest proportion of heavy smokers since it intersects the
#' # reference line at the furthest distance, on the side opposite to the plot's origin.
#' # This can be cross-checked by inspecting the table of row profiles:
#'
#' row_props <- round(prop.table(as.matrix(mytable), margin = 1),3)
#'
#'
#' # EXAMPLE 2
#' # Run the function using the "yelland" in-built dataset and "MT2" (Mark Twain 2)
#' # as reference category, so reproducing the scatterplot in Yelland 2010, figure 4.
#'
#' caplot(yelland, ref.category="MT2", label.size=2)
#'
#'
#' # EXAMPLE 3
#' # Run the function using the "borggroenen" in-built dataset and "MA"
#' # as reference category, so reproducing the scatterplot in Borg-Groenen 2005, figure 24.9.
#'
#' caplot(borggroenen, ref.category="MA", label.size=2)
#'
#' # As noted by Borg-Groenen 2005:
#' # "the projections on the line through the origin and MA (Massachusetts)
#' # show that auto theft and robbery happen more often than average.
#' # Because larceny and burglary project almost on the origin,
#' # they occur at an average rate in Massachusetts, whereas murder,
#' # rape, and assault are below average."
#'
#'
#'
caplot <- function(df, dims = c(1,2), ref.category, dot.size = 2, label.size = 3, axis.title.size = 8, equal.scale = TRUE) {

  Dim1=Dim2=ID=Type=x=xend=y=yend=Color <- NULL

  # Perform Correspondence Analysis
  ca_res <- ca(df)

  # Extract inertia and compute percentages
  summary_ca <- summary(ca_res)
  total_inertia <- sum(summary_ca$scree[,2])
  inertia1 <- summary_ca$scree[dims[1],2]
  inertia2 <- summary_ca$scree[dims[2],2]
  percentage1 <- round(inertia1 / total_inertia * 100, 2)
  percentage2 <- round(inertia2 / total_inertia * 100, 2)

  # Extract Row and Column coordinates
  row_coor <- data.frame(ca_res$rowcoord)[, dims]
  colnames(row_coor) <- c("Dim1", "Dim2")
  col_coor <- data.frame(ca_res$colcoord)[, dims]
  colnames(col_coor) <- c("Dim1", "Dim2")

  # Add Row/Column identifier
  row_coor$ID <- rownames(row_coor)
  col_coor$ID <- rownames(col_coor)

  # Add Row/Column type identifier
  row_coor$Type <- "Row"
  col_coor$Type <- "Column"

  # Combine row and column coordinates into one data frame
  coor <- rbind(row_coor, col_coor)

  # Determine whether the reference category is a row or column
  if (ref.category %in% rownames(df)) {
    ref_coor <- row_coor
    other_coor <- col_coor
    coor$Color <- ifelse(coor$Type == "Row" & !(coor$ID %in% ref.category), "grey", coor$Type)
  } else if (ref.category %in% colnames(df)) {
    ref_coor <- col_coor
    other_coor <- row_coor
    coor$Color <- ifelse(coor$Type == "Column" & !(coor$ID %in% ref.category), "grey", coor$Type)
  } else {
    stop("ref.category must be a row or column name in df")
  }

  # Calculate the angle θ
  theta <- atan2(coor$Dim2[coor$ID == ref.category], coor$Dim1[coor$ID == ref.category])

  # Rotate the other coordinates by -θ
  other_coor$Rot_Dim1 <- cos(-theta) * other_coor$Dim1 - sin(-theta) * other_coor$Dim2
  other_coor$Rot_Dim2 <- sin(-theta) * other_coor$Dim1 + cos(-theta) * other_coor$Dim2

  # Calculate the perpendicular coordinates in the rotated plane, then rotate back
  other_coor$Perp_Dim1 <- cos(theta) * other_coor$Rot_Dim1 - sin(theta) * 0
  other_coor$Perp_Dim2 <- sin(theta) * other_coor$Rot_Dim1 + cos(theta) * 0

  # Create a data frame for segments
  segments_df <- data.frame(
    x = other_coor$Dim1,
    y = other_coor$Dim2,
    xend = other_coor$Perp_Dim1,
    yend = other_coor$Perp_Dim2
  )

  # Create dimension labels
  xlab <- paste0("Dimension ", dims[1], " (Inertia: ", round(inertia1, 2), "; ", round(percentage1, 2), "%)")
  ylab <- paste0("Dimension ", dims[2], " (Inertia: ", round(inertia2, 2), "; ", round(percentage2, 2), "%)")

  # Define colors
  color_values <- c("Row" = "red", "Column" = "blue", "grey" = "darkgrey")

  # Plot
  plot <- ggplot() +
    geom_hline(yintercept=0, linetype="dashed", linewidth = 0.25, color = "gray") +
    geom_vline(xintercept=0, linetype="dashed", linewidth = 0.25, color = "gray") +
    geom_abline(slope = tan(theta), intercept=0, color="black", linewidth = 0.30) +
    geom_segment(data=segments_df, aes(x=x, y=y, xend=xend, yend=yend), linetype="dotted", linewidth = 0.30) +
    geom_point(data=coor, aes(x=Dim1, y=Dim2, color=Color), size=dot.size) +
    geom_text_repel(data=coor, aes(x=Dim1, y=Dim2, label=ID, color=Color), size = label.size) +
    theme(panel.background = element_rect(fill = "white", colour = "black"),
          axis.title = element_text(size = axis.title.size),
          plot.title = element_text(size = 10),
          legend.position = "none") + # No legend
    labs(title="Correspondence Analysis Scatterplot", x=xlab, y=ylab) +
    scale_color_manual(values=color_values)

  if (equal.scale) {
    plot <- plot + coord_equal()
  }

  return(plot)
}
