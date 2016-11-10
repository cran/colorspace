# -------------------------------------------------------------------
# - NAME:        definitions.R
# - AUTHOR:      Reto Stauffer
# - DATE:        2016-10-24
# -------------------------------------------------------------------
# - DESCRIPTION: File which specifies the different color palettes
#                which can be accessed via choose_palette.
#                Furthermore, the example plots are defined in this
#                file as well.
# -------------------------------------------------------------------
# - EDITORIAL:   2016-10-24, RS: Created file on thinkreto.
# -------------------------------------------------------------------
# - L@ST MODIFIED: 2016-11-09 09:39 on thinkreto
# -------------------------------------------------------------------


# -------------------------------------------------------------------
# Character vector specifying the example plots. For each element
# the function Plot<Name> will be called.
# -------------------------------------------------------------------
example.plots <- c("Map","Heatmap","Scatter","Spine","Bar",
                   "Pie","Perspective","Mosaic","Lines","Spectrum")

vars.pal <- c("h1", "h2", "c1", "c2", "l1", "l2", "p1", "p2", "fixup")

qual.pals <- list()
qual.pals[["Pastel 1"]]    <- c(  0,  288,  35, NA, 85, NA,  NA,  NA, 1) # ColorBrewer.org: Pastel1
qual.pals[["Set 3"]]       <- c( 10,  320,  50, NA, 80, NA,  NA,  NA, 1) # ColorBrewer.org: Set3
qual.pals[["Set 2"]]       <- c(  0,  288,  60, NA, 70, NA,  NA,  NA, 1) # ColorBrewer.org: Set2
qual.pals[["Dark 2"]]      <- c(  0,  288,  50, NA, 60, NA,  NA,  NA, 1) # ColorBrewer.org: Dark2
qual.pals[["Even Darker"]] <- c(  0,  300,  80, NA, 60, NA,  NA,  NA, 1) # JCF/Z: Even Darker
qual.pals[["Warm"]]        <- c( 90,  -30,  50, NA, 70, NA,  NA,  NA, 1) # Z+KH+PM-09, Fig.4: Warm (based on Ihaka-03)
qual.pals[["Cold"]]        <- c(270,  150,  50, NA, 70, NA,  NA,  NA, 1) # Z+KH+PM-09, Fig.4: Cold (based on Ihaka-03)
qual.pals[["Harmonic"]]    <- c( 60,  240,  50, NA, 70, NA,  NA,  NA, 1) # Z+KH+PM-09, Fig.4: Harmonic (based on Ihaka-03)
qual.pals[["Dynamic"]]     <- c( 30,  300,  50, NA, 70, NA,  NA,  NA, 1) # Z+KH+PM-09, Fig.4: Dynamic (based on Ihaka-03)

seqs.pals <- list()
seqs.pals[["Greys"]]       <- c(  0,   NA,   0,  0, 15, 95, 1.3,  NA, 1) # ColorBrewer.org: Greys
seqs.pals[["Purples"]]     <- c(280,   NA,  60,  5, 20, 95, 0.7,  NA, 1) # ColorBrewer.org: Purples
seqs.pals[["Blues"]]       <- c(260,   NA,  80, 10, 30, 95, 0.7,  NA, 1) # ColorBrewer.org: Blues
seqs.pals[["Greens"]]      <- c(135,   NA,  50, 10, 40, 95, 0.4,  NA, 1) # ColorBrewer.org: Greens
seqs.pals[["Oranges"]]     <- c( 20,   NA,  80,  5, 35, 95, 0.6,  NA, 1) # ColorBrewer.org: Oranges
seqs.pals[["Reds"]]        <- c( 10,   NA,  80, 10, 30, 95, 0.7,  NA, 1) # JCF/Z: Reds
seqs.pals[["Blues 2"]]     <- c(260,   NA,  80,  0, 30, 90, 1.5,  NA, 1) # Z+KH+PM-09, Fig.5: Blues
seqs.pals[["Light Grays"]] <- c(260,   NA,   0,  0, 30, 90, 1.5,  NA, 1) # Z+KH+PM-09, Fig.5: Light Grays

seqm.pals <- list()
seqm.pals[["BuPu"]]        <- c(300,  200,  60,  0, 25, 95, 0.7, 1.3, 1) # ColorBrewer.org: BuPu
seqm.pals[["Pure Red"]]    <- c(370,  280,  80,  5, 25, 95, 0.7, 1.3, 1) # ColorBrewer.org: PuRd
seqm.pals[["Yellow-Green"]]<- c(140,   80,  40, 10, 35, 95, 0.7, 1.7, 1) # ColorBrewer.org: YlGn
seqm.pals[["Yellow-Blue"]] <- c(265,   80,  60, 10, 25, 95, 0.7, 2.0, 1) # ColorBrewer.org: YlGnBu
seqm.pals[["Yellow-Red"]]  <- c( 10,   85,  80, 10, 25, 95, 0.4, 1.3, 1) # ColorBrewer.org: YlOrRd
seqm.pals[["Alt Heat"]]    <- c(  0,   90,  80, 30, 30, 90, 0.2, 2.0, 1) # JCF/Z: alternative to heat_hcl
seqm.pals[["Heat HCL"]]    <- c(  0,   90, 100, 30, 50, 90, 0.2, 1.0, 1) # Z+KH+PM-09, Fig.5: heat_hcl
seqm.pals[["Alt Terrain"]] <- c(130,   30,  65,  0, 45, 90, 0.5, 1.5, 1) # JCF/Z: alternative to terrain_hcl
seqm.pals[["Terrain HCL"]] <- c(130,    0,  80,  0, 60, 95, 0.1, 1.0, 1) # Z+KH+PM-09, Fig.5: terrain_hcl
seqm.pals[["Red Blue"]]    <- c(  0, -100,  80, 40, 40, 75, 1.0, 1.0, 1) # Z+KH+PM-09, Fig.5: Red-Blue
seqm.pals[["Viridis"]]     <- c(300,   75,  35, 95, 15, 90, 0.8, 1.2, 1) # viridis::viridis
seqm.pals[["Plasma"]]      <-c(-100,  100,  60,100, 15, 95, 2.0, 0.9, 1) # viridis::plasma

dive.pals <- list()
dive.pals[["PiYg"]]        <- c(340,  128,  45, NA, 35, 95, 0.7,  NA, 1) # ColorBrewer.org: PiYG
dive.pals[["PRGn"]]        <- c(300,  128,  45, NA, 30, 95, 0.7,  NA, 1) # ColorBrewer.org: PRGn
dive.pals[["BrBG"]]        <- c( 55,  160,  30, NA, 35, 95, 0.7,  NA, 1) # ColorBrewer.org: BrBG
dive.pals[["PuOr"]]        <- c( 40,  270,  45, NA, 30, 95, 0.7,  NA, 1) # ColorBrewer.org: PuOr
dive.pals[["RdBu"]]        <- c( 12,  265,  80, NA, 25, 95, 0.7,  NA, 1) # ColorBrewer.org: RdBu
dive.pals[["Blue-Red"]]    <- c(260,    0,  80, NA, 30, 90, 1.5,  NA, 1) # Z+KH+PM-09, Fig.6: Blue-Red (high luminance contrast)
dive.pals[["Blue-Red 2"]]  <- c(260,    0, 100, NA, 50, 90, 1.0,  NA, 1) # Z+KH+PM-09, Fig.6: Blue-Red (medium luminance contrast)
dive.pals[["Green-Orange"]]<- c(130,   43, 100, NA, 70, 90, 1.0,  NA, 1) # Z+KH+PM-09, Fig.6: Green-Orange (low luminance contrast)
dive.pals[["Blue-Red 3"]]  <- c(180,  330,  59, NA, 75, 95, 1.5,  NA, 1) # Z+KH+PM-09, Fig.6: Blue-Red (low luminance contrast)

base.pals <- list()
base.pals[["rainbow"]]        <- c(NA, NA, NA, NA, NA, NA, NA, NA, 1) # Default RGB rainbow
base.pals[["heat.colors"]]    <- c(NA, NA, NA, NA, NA, NA, NA, NA, 1) # Default heatmap
base.pals[["topo.colors"]]    <- c(NA, NA, NA, NA, NA, NA, NA, NA, 1) # Default topo colors
base.pals[["terrain.colors"]] <- c(NA, NA, NA, NA, NA, NA, NA, NA, 1) # Default terrain colors
base.pals[["cm.colors"]]      <- c(NA, NA, NA, NA, NA, NA, NA, NA, 1) # Default cyan magenta colors
base.pals[["bpy"]]            <- c(NA, NA, NA, NA, NA, NA, NA, NA, 1) # Analog to sp::bpy.colors


# -------------------------------------------------------------------
# Helper function: returns a data.frame containing all
# palettes specified above. Used for hclwizard
# -------------------------------------------------------------------
GetPaletteConfig <- function() {
   res <- NULL
   palettes <- list("qual"=qual.pals,"seqs"=seqs.pals,"seqm"=seqm.pals,"dive"=dive.pals,"base"=base.pals)
   for ( i in 1:length(palettes) ) {
      tmp <- data.frame(matrix(unlist(palettes[[i]]),ncol=9,byrow=T))
      names(tmp) <- toupper(vars.pal)
      tmp$name   <- names(palettes[[i]])
      tmp$typ    <- names(palettes)[i]
      res        <- rbind(tmp,res)
   }
   res
}




