
library(ggplot2)
library(ggrepel)

# Display a Taylor Diagram
#
# This script is based on source code for the function taylor.diagram()
# from the R-package plotrix written by Olivier Eterradossi and Jim Lemon.
#
# The original source code is available here:
#     https://CRAN.R-project.org/package=plotrix
#
# Peter D. Wilson
# Adjunct Fellow
# School of Natural Sciences
# Faculty of Science and Engineering
# Macquarie University, North Ryde, NSW, Australia 2109
#
# 2022-03-03; 2022-03-17; 2022-03-19
#
# -----------------------------------------------------------------------
# TO DO:
#
#   1. Implement "normalise" option
#
#   2. Implement use of user-specified 'point_palette'; at present this
#      is not used. Comparison model symbols are coloured by sampling
#      the heat.colors() palette
#
#   3. Implement return of a ggplot2 object so users can place that object
#      into their own outputs, plus give options to render a png image to
#      a user specified file.
# -----------------------------------------------------------------------

#' Taylor Diagrams
#'
#' Plot a Taylor Diagram to visualise important differences between a reference data set and one or more comparison data sets
#'
#' @param data data.frame. A data.frame holding the data sets to be plotted. First column is the reference data set, and subsequent columns represent models/datasets for comparison.
#' @param plot_type character string. Plot type to be generated: "full" covers quadrants 1 and 2; "half" only quadrant 1 which is suitable for situations where all correlations between data sets are known to be positive.
#' @param point_palette character vector. A vector of colour names or values to be used for plot symbols.
#' @param normalise logical. Should standard deviations of data sets be normalised relative to the reference standard deviation?
#' @param plot_title character string. Title to be added to the plot.
#' @param show_labels logical. Show labels alongside plot symbols? Default is FALSE.
#' @param model_labels character vector. Character vector of names to be used when model labels re to be plotted. If show_labels == TRUE but no values are passed in this parameter, column names from the data.frame 'data' are use.d
#' @param rmsd_col character or colour value. Colour used to plot the RMSD circles centered on the reference datum. Can be a recognised R colour name or a hexadecimal RGB value.
#'
#' @details 
#' The development of this function was inspired by the function \emph{taylor.diagram} in the package \emph{plotrix}. Some aspects of the code are borrowed from the \emph{plotrix} function and are acknowledged in comments with the source code.
#'
#'
#' @return
#' A ggplot2 object representing the Taylor Diagram for the submitted data
#' 
#' @export
#'
#' @examples
taylor_diagram <- function(data,
                           plot_type = "full",
                           point_palette = NA,
                           normalise = FALSE,
                           plot_title = "",
                           show_labels = FALSE,
                           model_labels = c(),
                           rmsd_col = "blue")
{
  # Sanity checks...
  if (!(plot_type %in% c("half", "full")))
    stop("plot_type must be one of 'half' or 'full")
  
  if (length(point_palette) > 1)
  {
    if (inherits(try(col2rgb(point_palette)), "try-error"))
      stop("One or more colour names or values given in point_palette is not recognised by R")
  }
    
  if (!is.logical(normalise)) stop("normalise can only be TRUE or FALSE")
  
  if (!is.logical(show_labels)) stop("show_labels can only be TRUE or FALSE")
  
  if (show_labels)
  {
    if (length(model_labels) == 0)
    {
      model_labels <- colnames(data)
      cat("BOING!\n")
    }
    else
      if (length(model_labels) < ncol(data))
        stop("show_labels == TRUE but not enough labels have been supplied in model_labels")
  }
  else
    model_labels <- rep("", ncol(data))
  
  if (inherits(try(col2rgb(rmsd_col)), "try-error"))
    stop("Colour name or value given in rmsd_col is not recognised by R")
  
  #### Prepare data
  # Remove NA rows...
  badRows <- which(is.na(rowSums(data)))
  if (length(badRows) > 0)
  {
    cat(paste0(length(badRows)," NA records removed\n"))
    data <- data[-badRows, ]
  }
  
  stdDev <- apply(data, 2, sd)
  
  # Assemble computed statistical values...
  if (is.null(colnames(data))) colnames(data) <- paste0("m", 1:ncol(data))
  
  d <- data.frame(model = colnames(data),
                  correl = cor(data)[, 1],
                  std_dev = apply(data, 2, sd))
  
  # Make a data.frame of transformed values to make plotting a little easier...
  d_trans <- data.frame(model = d$model,
                        x = d$std_dev * cos(acos(d$correl)),
                        y = d$std_dev * sin(acos(d$correl)),
                        stringsAsFactors = FALSE)
  
  # Compute some important 'control' values...
  if (is.na(point_palette[1]))
    point_palette <- c("black", heat.colors(ncol(data) - 1))
  
  sd_max <- 1.25 * max(d$std_dev)
  
  sd_ref <- d[1, "std_dev"]
  
  if (plot_type == "half")
  {
    # Settings for a first quadrant plot when all correlations to be plotted are
    # positive
    x_left <- 0
    grad_corr_lines = c(0.2, 0.4, 0.6, 0.8, 0.9)
    max_radians <- pi/2
    x_start <- 0
    x_label_pos <- sd_max/2
    
    x_vals <- sd_max * cos(acos(seq(1, 0, -0.005)))
    y_vals <- sd_max * sin(acos(seq(1, 0, -0.005)))
    
    axis_ticks <- pretty(c(0, sd_max))
    axis_ticks <- axis_ticks[axis_ticks <= sd_max]
    #print(axis_ticks)
  } 
  else 
  {
    # Settings for the general case where the plot covers the first and second
    # quadrants
    x_left <- -sd_max 
    grad_corr_lines = c(0.2, 0.4, 0.6, 0.8, 0.9, -0.2, -0.4, -0.6, -0.8, -0.9)
    max_radians <- pi
    x_start <- -sd_max
    x_label_pos <- 0
    
    x_vals <- sd_max * cos(acos(c(seq(1, 0, -0.005), seq(0, -1, -0.005))))
    y_vals <- sd_max * sin(acos(c(seq(1, 0, -0.005), seq(0, -1, -0.005))))
    
    axis_ticks <- pretty(c(-sd_max, sd_max))
    axis_ticks <- axis_ticks[(axis_ticks <= sd_max) & (axis_ticks > -sd_max)]
    #print(axis_ticks)
  } 
  
  x_right <- sd_max
  x_limit_right <- 1.1 * x_right
  
  # Std. dev. (x-axis) scale step size; useful for a number of tasks including
  # positioning axis labels on the custom axes, and computing RMSD circles (see
  # section below)
  axis_step <- diff(axis_ticks[1:2])
  
  y_limit_lower <- -axis_step
  
  if (plot_type == "half")
    x_limit_left <- -axis_step
  else
    x_limit_left <- 1.1 * x_left
  
  axis_marks_x <- data.frame(x = axis_ticks,
                             y = rep(0, length(axis_ticks)),
                             xend = axis_ticks,
                             yend = 0.05 * axis_step)
  
  axis_marks_y <- data.frame(x = rep(0, length(axis_ticks[axis_ticks > 0])),
                             y = axis_ticks[axis_ticks > 0],
                             xend = 0.05 * diff(axis_ticks[axis_ticks > 0][1:2]),
                             yend = axis_ticks[axis_ticks > 0])
  
  outer_arc <- data.frame(x = x_vals[1:(length(x_vals) - 1)],
                          y = y_vals[1:(length(y_vals) - 1)],
                          xend = x_vals[2:length(x_vals)],
                          yend = y_vals[2:length(y_vals)])
  
  #### Start plotting:
  # First, the basic plot framework...
  baseFig <- ggplot(d_trans, aes(x = x, y = y)) +
    coord_fixed() +
    scale_x_continuous(limits = c(x_limit_left, x_limit_right), breaks = axis_ticks, labels = rep("", length(axis_ticks))) +
    scale_y_continuous(limits = c(y_limit_lower, x_limit_right), breaks = axis_ticks, labels = rep("", length(axis_ticks))) +
    geom_segment(data = outer_arc, aes(x = x , y = y, xend = xend, yend = yend)) +
    theme_minimal() + # Stop default axis production
    geom_segment(aes(x = 0, y = 0, xend = 0, yend = sd_max), size = 0.1) +
    geom_segment(aes(x = x_start, y = 0, xend = sd_max, yend = 0), size = 0.1) +
    
    # Make sensible axes for this plot style...
    geom_segment(data = axis_marks_x, aes(x = x , y= y, xend = xend, yend = yend)) +
    geom_segment(data = axis_marks_y, aes(x = x , y= y, xend = xend, yend = yend)) +
    annotate("text", # x-axis tick labels
             x = axis_ticks, 
             y = 0.15 * y_limit_lower, 
             label = format(abs(axis_ticks), nsmall = 1, justify = "centre"),
             size = 2) +
    annotate("text", # x-axis label
             x = x_label_pos,
             y = 0.35 * y_limit_lower,
             label = "Standard deviation",
             size = 2.5) +
    annotate("text", # y-axis tick labels
             x = -0.25 * axis_step,
             y = axis_ticks[axis_ticks > 0],
             label = format(axis_ticks[axis_ticks > 0], nsmall = 1, justify = "left"),
             size = 2) +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = 8)) +
    ylab("") +
    xlab("") +
    ggtitle(plot_title)
  
  # We can now add additional features
  # Add inner std_dev arcs
  arc_radius <- axis_ticks[axis_ticks > 0]
  
  for (this_radius in arc_radius)
  {
    if (plot_type == "half")
    {
      inner_x_vals <- this_radius * cos(acos(seq(1, 0, -0.005)))
      inner_y_vals <- this_radius * sin(acos(seq(1, 0, -0.005)))
    }
    else
    {
      inner_x_vals <- this_radius * cos(acos(c(seq(1, 0, -0.005), seq(0, -1, -0.005))))
      inner_y_vals <- this_radius * sin(acos(c(seq(1, 0, -0.005), seq(0, -1, -0.005))))
    }
    
    inner_arc <- data.frame(x = inner_x_vals,
                            y = inner_y_vals)
    
    baseFig <- baseFig +
      geom_line(data = inner_arc,
                aes(x = x, y = y),
                size = 0.25,
                linetype = "dashed",
                colour = "grey50")
  }
  
  # Add radial markers for correlation values
  corr_segments <- data.frame(x = rep(0, length(grad_corr_lines)),
                              y = rep(0, length(grad_corr_lines)),
                              xend = sd_max * cos(acos(grad_corr_lines)),
                              yend = sd_max * sin(acos(grad_corr_lines)))
  
  baseFig <- baseFig +
    geom_segment(data = corr_segments,
                 aes(x = x ,
                     y = y,
                     xend = xend, 
                     yend = yend),
                 size = 0.25,
                 linetype = "dashed",
                 colour = "grey50")
  
  # Add correlation tick marks 
  if (plot_type == "half")
  {
    big_tick_angles <- acos(seq(0.1, 0.9, by = 0.1))
    med_tick_angles <- acos(seq(0.05, 0.95, by = 0.1))
    sml_tick_angles <- acos(seq(0.91, 0.99, by = 0.01))
  }
  else
  {
    big_tick_angles <- c(acos(seq(-0.9, -0.1, by = 0.1)), acos(seq(0.1, 0.9, by = 0.1)))
    med_tick_angles <- c(acos(seq(-0.95, -0.05, by = 0.1)), acos(seq(0.05, 0.95, by = 0.1)))
    sml_tick_angles <- c(acos(seq(-0.99, -0.91, by = 0.01)), acos(seq(0.91, 0.99, by = 0.01)))
  }
  
  
  big_tick_segs <- data.frame(x = cos(big_tick_angles) * sd_max, 
                              y = sin(big_tick_angles) * sd_max,
                              xend = cos(big_tick_angles) * 0.97 * sd_max,
                              yend = sin(big_tick_angles) * 0.97 * sd_max)
  
  baseFig <- baseFig +
    geom_segment(data = big_tick_segs,
                 aes(x = x ,
                     y = y,
                     xend = xend, 
                     yend = yend),
                 size = 0.1)
  
  med_tick_segs <- data.frame(x = cos(med_tick_angles) * sd_max,
                              y = sin(med_tick_angles) * sd_max,
                              xend = cos(med_tick_angles) * 0.985 * sd_max,
                              yend = sin(med_tick_angles) * 0.985 * sd_max)
  baseFig <- baseFig +
    geom_segment(data = med_tick_segs,
                 aes(x = x ,
                     y = y,
                     xend = xend, 
                     yend = yend),
                 size = 0.1)
  
  sml_tick_segs <- data.frame(x = cos(sml_tick_angles) * sd_max,
                              y = sin(sml_tick_angles) * sd_max,
                              xend = cos(sml_tick_angles) * 0.99 * sd_max,
                              yend = sin(sml_tick_angles) * 0.99 * sd_max)
  
  baseFig <- baseFig +
    geom_segment(data = sml_tick_segs,
                 aes(x = x ,
                     y = y,
                     xend = xend, 
                     yend = yend),
                 size = 0.1)
  
  # Add correlation labels
  baseFig <- baseFig +
    annotate("text", x = sd_max * 0.78,
             y = sd_max * 0.78,
             label = "Correlation",
             size = 3,
             angle = 315)
  
  if (plot_type == "half")
  {
    baseFig <- baseFig +
      annotate("text",
               x = cos(c(big_tick_angles, acos(c(0.95, 0.99)))) * 1.03 * sd_max,
               y = sin(c(big_tick_angles, acos(c(0.95, 0.99)))) * 1.03 * sd_max,
               label = c(seq(0.1, 0.9, by = 0.1), 0.95, 0.99),
               size = 2,
               hjust = "left")
  }
  else
  {
    x_vals <- cos(c(acos(c(-0.99, -0.95)), big_tick_angles, acos(c(0.95, 0.99)))) * 1.03 * sd_max
    
    # Nudje text for labels so that larger values do not get too close to the outer arc of the plot
    text_adj <- c(rep("right", sum(x_vals < 0)), rep("left", sum(x_vals > 0)))
    #text_adj <- c("right", "right", rep("center", 18), "left", "left")
    
    baseFig <- baseFig +
      annotate("text",
               x = x_vals, #cos(c(acos(c(-0.99, -0.95)), big_tick_angles, acos(c(0.95, 0.99)))) * 1.03 * sd_max,
               y = sin(c(acos(c(-0.99, -0.95)), big_tick_angles, acos(c(0.95, 0.99)))) * 1.03 * sd_max,
               label = c(c(-0.99, -0.95, seq(-0.9, -0.1, by = 0.1)), c(seq(0.1, 0.9, by = 0.1), 0.95, 0.99)),
               size = 2,
               hjust = text_adj) +
      annotate("text",
               x = -sd_max * 0.78,
               y = sd_max * 0.78,
               label = "Correlation",
               size = 3,
               angle = 45)
  }
  
  # Add zero label
  baseFig <- baseFig +
    annotate("text",
             x = 0,
             y = 1.03 * sd_max,
             label = "0",
             size = 2)
  
  # Add centred RMSD circles
  # First, find maximum RMSD value we can fit into the plot
  if (plot_type == "half")
  {
    # For the half-plot, it is the distance between (sd_ref, 0) and (0, sd_max):
    max_rmsd <- sqrt(sd_ref^2 + sd_max^2)
  }
  else
  {
    # For the full-plot, it is the distance from x-left to sd_ref along the x-axis:
    max_rmsd <- sd_ref + abs(x_left)
  }
  
  # Number of axis_steps which can fit into the body of the plot
  num_rmsd_steps <- max_rmsd %/% axis_step
  
  # Compute rmsd values to be plotted as a sequence based on the values computed above 
  rmsd <- seq(axis_step, num_rmsd_steps * axis_step, by = axis_step)
  
  # Add each arc to the plot
  for (this_rmsd in rmsd)
  {
    x_curve <- cos(seq(0, pi, by = 0.01)) * this_rmsd + sd_ref
    y_curve <- sin(seq(0, pi, by = 0.01)) * this_rmsd
    
    # find where to clip the curves
    if (plot_type == "half")
    {
      # For a first quadrant-only plot, clipping occurs at the y-axis
      curve_end <- which(x_curve < 0)
      curve_end <- ifelse(length(curve_end), min(curve_end) - 1, 315)
    }
    else
    {
      # For a full, first and second quadrant plot, clipping is at the y-axis
      curve_end <- which(y_curve < 0)
      curve_end <- ifelse(length(curve_end), min(curve_end) - 1, 315)
    }
    
    max_curve <- x_curve * x_curve + y_curve * y_curve
    curve_start <- which(max_curve > sd_max * sd_max)
    curve_start <- ifelse(length(curve_start), max(curve_start) + 1, 0)
    curve_data <- data.frame(x = x_curve[curve_start:curve_end],
                             y = y_curve[curve_start:curve_end])
    
    if (plot_type == "half")
    {
      # For a half-plot, locate centred RMSD labels on the line joining the reference
      # model point on the x-axis (i.e. coords (sd_ref, 0)) and the upper limit
      # of values on the y-axis (i.e. coords (0, sd_max). This ensures that,
      # except in the most extreme case, all labels will be within the body of
      # the plot for all visible RMSD arcs. So, we compute the coordinates of
      # the intersection of this line with the current centred RMSD arc:
      
      m <- -sd_max/sd_ref
      a <- 1 + m^2
      b <- 2*(m*sd_max - sd_ref)
      c <- sd_ref^2 + sd_max^2 - this_rmsd^2
      
      label_x <- (-b - sqrt(b*b - 4 * a * c))/(2 * a)
      label_y <- m * label_x + sd_max
      
      label_data <- data.frame(x = label_x,
                               y = label_y)
    }
    else
    {
      # For a full plot, we have a much easier task...just put the labels in a
      # specified distance from the arc end which touches the x-axis
      label_data <- data.frame(x = x_curve[length(x_curve) - 25],
                               y = y_curve[length(x_curve) - 25])
    }
    
    baseFig <- baseFig +
      geom_line(data = curve_data,
                aes(x = x, y = y),
                col = rmsd_col,
                size = 0.25,
                linetype = "dashed") +
      geom_label(data = label_data, aes(x = x,
                                        y = y),
                 label = format(this_rmsd, nsmall = 1, justify = "centre"),
                 size = 2,
                 fill = "white",
                 label.size = 0) # Parameter 'label.size' controls label border width...go figure!
  }
  
  # Add points as last layer so they are not over-plotted by other "graph
  # furniture" i.e. radial grids and circles, labels, etc
  baseFig <- baseFig +
    geom_point(size = 2,
               shape = c(22, rep(24, ncol(data) - 1)),
               fill = point_palette) #c("black", heat.colors(ncol(data) - 1)))
  
  if (show_labels)
  {
    label_data <- data.frame(x = d_trans$x,
                             y = d_trans$y,
                             label = model_labels,
                             stringsAsFactors = FALSE)
    
    baseFig <- baseFig +
      ggrepel::geom_text_repel(data = label_data,
                               aes(x = x,
                                   y = y,
                                   label = label),
                               size = 3)
  }
  
  return(baseFig)
}
