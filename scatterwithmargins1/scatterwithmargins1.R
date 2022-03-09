##--------------------------------------------------------------
## Date    : 29-03-19
## Authors : Thomas Drury (TD) 
## Purpose : Tool to plot bivariate distributions.
##           Adapted from code by Doug Thompson (DT) and
##           cowplot help from Christina Fillamore (CF).
##
##--------------------------------------------------------------

##--------------------------------------------------------------
## Scatterwithmargin function
##--------------------------------------------------------------

scatterwithmargins1 <- function(indata=NULL           ## Input dataset
                               ,xvars=NULL            ## Variables to plot on x-axis
                               ,xrange=NULL           ## Range of values for x-axis
							   ,xlabel="xvars"        ## Label for the x-axis
                               ,xrefs=NULL            ## Reference lines for x-axis
                               ,yvars=NULL            ## Variables to plot on x-axis
                               ,yrange=NULL           ## Range of values for x-axis
							   ,ylabel="yvars"        ## Label for the y-axis
                               ,yrefs=NULL            ## Reference lines for x-axis
                               ,kdeplot=FALSE         ## Turns KDE contour plot on
                               ,kdeband=1             ## KDE smoother for contours
                               ,ptitle1=NULL){        ## Plot main title  

  
  # Handle the parameters that are not already objects
  xvars = eval(substitute(xvars), indata, parent.frame())
  yvars = eval(substitute(yvars), indata, parent.frame())
  

  # Check if axes range supplied
  xlims = !is.null(xrange)
  ylims = !is.null(yrange)


  # Get limits for the axes if supplied
  if (xlims) { xmin = min(xrange)
               xmax = max(xrange) }
  if (ylims) { ymin = min(yrange)
               ymax = max(yrange) }


  # Create main scatter
  fig1 = ggplot2::ggplot(data=indata, aes(x=xvars, y=yvars)) +
         ggplot2::geom_point(pch=21, cex=1, bg="gold", col="white") +
         ggplot2::labs(x=xlabel, y=ylabel)
		 
         # Contour plot
         if (kdeplot==T) {fig1 = fig1 + ggplot2::stat_density_2d(contour = TRUE, n = 100, h = kdeband, col="black")}
         
         # Specific axes
         if (xlims==T)   {fig1 = fig1 + ggplot2::scale_x_continuous(limits=c(xmin,xmax), breaks=xrange)}
         if (ylims==T)   {fig1 = fig1 + ggplot2::scale_y_continuous(limits=c(ymin,ymax), breaks=yrange)}
         
         # Reference lines
         if (!is.null(xrefs)==T) {fig1 = fig1 + ggplot2::geom_vline(xintercept=xrefs, linetype=2, colour="black")}
         if (!is.null(yrefs)==T) {fig1 = fig1 + ggplot2::geom_hline(yintercept=yrefs, linetype=2, colour="black")}
         
         # Theme specification
         fig1 = fig1 +
         ggplot2::theme_bw() +
         ggplot2::theme(plot.title = element_blank(),
		                axis.text  = element_text(size=18),
						axis.title = element_text(size=18))


  # Create top margin plot
  fig2 = cowplot::axis_canvas(fig1, axis="x") +          
         ggplot2::geom_histogram(data=indata, aes(x=xvars), fill="gold", col="white", bins=40) 
  
         # Reference lines
         if (!is.null(xrefs)==T) {fig2 = fig2 + ggplot2::geom_vline(xintercept=xrefs, lty=2)}
         
         # Theme specification
         fig2 = fig2 + ggplot2::theme_bw() +
         ggplot2::theme(plot.title         = element_blank(),
                        panel.border       = element_blank(),
                        panel.background   = element_blank(),
                        panel.grid.major.x = element_blank(),
                        panel.grid.minor.x = element_blank())

            
  # Create right margin plot
  fig3 = cowplot::axis_canvas(fig1, axis="y", coord_flip=TRUE) +          
         ggplot2::geom_histogram(data=indata, aes(yvars), fill="gold", col="white", bins=40) +
         coord_flip()
         
         # Reference lines
         if (!is.null(yrefs)==T) {fig3 = fig3 + ggplot2::geom_vline(xintercept=yrefs, lty=2)}
         
         # Theme specification
         fig3 = fig3 +
         ggplot2::theme_bw() +
         ggplot2::theme(plot.title         = element_blank(),
                        panel.border       = element_blank(),
                        panel.background   = element_blank(),
                        panel.grid.major.y = element_blank(),
                        panel.grid.minor.y = element_blank())
      
            
  # Add margins to main scatter   
  fig4 = cowplot::insert_xaxis_grob(fig1, fig2, position="top")
  fig5 = cowplot::insert_yaxis_grob(fig4, fig3, position="right")
  
  # Create titles
  title1 = cowplot::ggdraw() + draw_label(ptitle1, size=18, fontface='bold')
  
  # Create final plot
  plot_grid(title1, fig5, ncol=1, rel_heights=c(0.1, 1))

}





