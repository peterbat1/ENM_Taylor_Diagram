# Display a Taylor Diagram
#
# This script is derived from the source code for the function taylor.diagram()
# from the R-package plotrix written by Olivier Eterradossi and Jim Lemon.
#
# The original source code is available here:
#     https://CRAN.R-project.org/package=plotrix
#
# Changes made to the original taylor.diagram() function include:
#
# 1. Significant streamlining and re-organisation of the original source code
#
# 2. Ensuring that the visual style or aesthetic for the two variants are the
#    same - the plotrix function produces VERY different plots for each variant
#
# 3. Changing some parameters to enhance the ability of users to adjust aspects
#    of the plots
#
# Dependencies:
#   This function requires library plotrix to access the function boxed.labels().
#
# Peter D. Wilson
# Adjunct Fellow
# School of Natural Sciences
# Faculty of Science and Engineering
# Macquarie University, Sydney, Australia
#
# 2022-03-03 
#
# The original plotrix source is covered by a GPL-3 license. The derivative code
# is also provided under GPL-3 license.

library(plotrix)

taylor_diagram <- function(ref,
                           model,
                           add = FALSE,
                           col = "red",
                           pch = 19,
                           pos_cor = TRUE, 
                           xlab = "Standard deviation",
                           ylab = "",
                           main = "Taylor Diagram",
                           show_gamma = TRUE,
                           gamma_vals = 0,
                           #ngamma = 3,
                           gamma_col = "grey50",
                           show_sd_arcs = TRUE,
                           ref_sd = FALSE,
                           #sd.method = "sample",
                           grad_corr_lines = c(0.2, 0.4, 0.6, 0.8, 0.9), 
                           pcex = 1,
                           cex_axis = 1,
                           normalize = FALSE,
                           mar = c(4, 3, 4, 3), ...)
{
  # Conditionally instantiate grad.cor.lines and max_radians
  if (pos_cor)
  {
    # Settings for a first quadrant plot when all correlations to be plotted are
    # positive
    grad_corr_lines = c(0.2, 0.4, 0.6, 0.8, 0.9)
    max_radians <- pi/2
  }
  else
  {
    # Settings for the general case where the plot covers the first and second
    # quadrants
    grad_corr_lines = c(0.2, 0.4, 0.6, 0.8, 0.9, -0.2, -0.4, -0.6, -0.8, -0.9)
    max_radians <- pi
  }
  
  # convert any list elements or data frames to vectors
  R <- cor(ref, model, use = "pairwise")
  
  if (is.list(ref)) 
    ref <- unlist(ref)
  
  if (is.list(model)) 
    model <- unlist(model)
  
  sd_ref <- sd(ref, na.rm = TRUE)
  sd_model <- sd(model, na.rm = TRUE)
  
  if (normalize)
  {
    sd_model <- sd_model/sd_ref
    sd_ref <- 1
  }
  
  sd_max <- 1.5 * max(sd_model, sd_ref)
  oldpar <- par("mar", "xpd", "xaxs", "yaxs")
  
  if (add)
  {
    # display the points
    points(sd_model * R,
           sd_model * sin(acos(R)),
           pch = pch,
           col = col, 
           cex = pcex)
  }
  else
  {
    par(mar = mar)
    
    # display the diagram
    
    if (pos_cor)
    {
      plot(0, xlim = c(0, sd_max*1.1), ylim = c(0, sd_max*1.1), xaxs = "i", 
           yaxs = "i", axes = FALSE, main = main, xlab = "",
           ylab = ylab, type = "n", cex = cex_axis, ...)
    }
    else
    {
      plot(0, xlim = c(-sd_max*1.1, sd_max*1.1), ylim = c(0, sd_max*1.1), xaxs = "i", 
           yaxs = "i", axes = FALSE, main = main, xlab = "", 
           ylab = ylab, type = "n", cex = cex_axis, ...)
    }
    
    mtext(xlab, side = 1, line = 2.3)
    
    if (grad_corr_lines[1])
    {
      for (gcl in grad_corr_lines)
        lines(c(0, sd_max * gcl), c(0, sd_max * sqrt(1 - gcl^2)), lty = 3)
    }
    
    # add the axes
    if (pos_cor)
    {
      segments(c(0, 0), c(0, 0), c(0, sd_max), c(sd_max, 0))
      axis_ticks <- pretty(c(0, sd_max))
      axis_ticks <- axis_ticks[axis_ticks <= sd_max]
      axis(1, at = axis_ticks, cex.axis = cex_axis)
      axis(2, at = axis_ticks, cex.axis = cex_axis)
    }
    else
    {
      segments(c(0, -sd_max), c(0, 0), c(0, sd_max), c(sd_max, 0))
      
      # Make a set of positive ticks to be used for gamma arc plotting; this will
      # ensure that, in the 2 quadrant situation, all visible arcs will be
      # plotted
      pos_ticks <- pretty(c(0, sd_max))
      axis_ticks <- pretty(c(-sd_max, sd_max))
      axis_ticks <<- axis_ticks[(axis_ticks <= sd_max) & (axis_ticks > -sd_max)]
      axis(1, at = axis_ticks, cex.axis = cex_axis, labels = abs(axis_ticks))
    }
    
    if (show_sd_arcs)
    {
      if (pos_cor)
        sd.arcs <- axis_ticks[(axis_ticks > 0) & (axis_ticks < sd_max)]
      else
        sd.arcs <- pos_ticks[pos_ticks < sd_max]
      
      for (sdarc in sd.arcs)
      {
        xcurve <- cos(seq(0, max_radians, by = 0.01)) * sdarc
        ycurve <- sin(seq(0, max_radians, by = 0.01)) * sdarc
        lines(xcurve, ycurve, col = "blue", lty = 3)
      }
    }
    
    if (show_gamma)
    {
      # if the user has passed a set of gamma values, use that; otherwise make up a set
      if (gamma_vals[1] != 0)
        gamma <- gamma_vals
      else
        gamma <- axis_ticks[axis_ticks > 0]

      # do the gamma curves
      for (gindex in 1:length(gamma))
      {
        xcurve <- cos(seq(0, pi, by = 0.01)) * gamma[gindex] + sd_ref
        ycurve <- sin(seq(0, pi, by = 0.01)) * gamma[gindex]
        
        # find where to clip the curves
        if (pos_cor)
        {
          # For a first quadrant-only plot, clipping occurs at the y-axis
          endcurve <- which(xcurve < 0)
          endcurve <- ifelse(length(endcurve), min(endcurve) - 1, 315)
        }
        else
        {
          # For a full, first and second quadrant plot, clipping is at the y-axis
          endcurve <- which(ycurve < 0)
          endcurve <- ifelse(length(endcurve), min(endcurve) - 1, 315)
        }
        
        maxcurve <- xcurve * xcurve + ycurve * ycurve
        startcurve <- which(maxcurve > sd_max * sd_max)
        startcurve <- ifelse(length(startcurve), max(startcurve) + 1, 0)
        lines(xcurve[startcurve:endcurve],
              ycurve[startcurve:endcurve],
              col = gamma_col)
        
        boxed.labels(xcurve[startcurve + 40],
                     ycurve[startcurve + 40], 
                     gamma[gindex],
                     border = FALSE)
      }
    }
    
    # the outer curve for correlation
    xcurve <- cos(seq(0, max_radians, by = 0.01)) * sd_max
    ycurve <- sin(seq(0, max_radians, by = 0.01)) * sd_max
    lines(xcurve, ycurve)
    
    if (pos_cor)
    {
      bigtickangles <- acos(seq(0.1, 0.9, by = 0.1))
      medtickangles <- acos(seq(0.05, 0.95, by = 0.1))
      smltickangles <- acos(seq(0.91, 0.99, by = 0.01))
      segments(cos(bigtickangles) * sd_max, sin(bigtickangles) * 
                 sd_max, cos(bigtickangles) * 0.97 * sd_max, sin(bigtickangles) * 
                 0.97 * sd_max)
    }
    else
    {
      bigtickangles <- c(acos(seq(-0.9, -0.1, by = 0.1)), acos(seq(0.1, 0.9, by = 0.1)))
      medtickangles <- c(acos(seq(-0.95, -0.05, by = 0.1)), acos(seq(0.05, 0.95, by = 0.1)))
      smltickangles <- c(acos(seq(-0.99, -0.91, by = 0.01)), acos(seq(0.91, 0.99, by = 0.01)))
      segments(cos(bigtickangles) * sd_max, 
               sin(bigtickangles) * sd_max,
               cos(bigtickangles) * 0.97 * sd_max,
               sin(bigtickangles) * 0.97 * sd_max)
    }
    
    par(xpd = TRUE)
    
    # the inner curve for reference SD
    if (ref_sd)
    {
      xcurve <- cos(seq(0, max_radians, by = 0.01)) * sd_ref
      ycurve <- sin(seq(0, max_radians, by = 0.01)) * sd_ref
      lines(xcurve, ycurve)
    }
    
    points(sd_ref, 0, pch = 15, cex = pcex)
    
    text(sd_max * 0.8,
         sd_max * 0.8,
         "Correlation",
         srt = 315,
         cex = cex_axis)
    
    if (pos_cor)
    {
      text(cos(c(bigtickangles, acos(c(0.95, 0.99)))) * 1.05 * sd_max,
           sin(c(bigtickangles, acos(c(0.95, 0.99)))) * 1.05 * sd_max,
           c(seq(0.1, 0.9, by = 0.1), 0.95, 0.99),
           cex = cex_axis)
    }
    else
    {
      text(cos(c(acos(c(-0.99, -0.95)), bigtickangles, acos(c(0.95, 0.99)))) * 1.05 * sd_max,
           sin(c(acos(c(-0.99, -0.95)), bigtickangles, acos(c(0.95, 0.99)))) * 1.05 * sd_max,
           c(c(-0.99, -0.95, seq(-0.9, -0.1, by = 0.1)), c(seq(0.1, 0.9, by = 0.1), 0.95, 0.99)),
           cex = cex_axis)
      text(-sd_max * 0.8,
           sd_max * 0.8,
           "Correlation",
           srt = 45,
           cex = cex_axis)
    }
    
    # Add zero label
    text(0, 1.05 * sd_max, "0", cex = cex_axis)
    
    segments(cos(medtickangles) * sd_max,
             sin(medtickangles) * sd_max,
             cos(medtickangles) * 0.98 * sd_max,
             sin(medtickangles) * 0.98 * sd_max)
    segments(cos(smltickangles) * sd_max,
             sin(smltickangles) * sd_max,
             cos(smltickangles) * 0.99 * sd_max,
             sin(smltickangles) * 0.99 * sd_max)
    
    # Add the model point
    points(sd_model * R,
           sd_model * sin(acos(R)),
           pch = pch,
           col = col, 
           cex = pcex)
  }
  
  invisible(oldpar)
}
