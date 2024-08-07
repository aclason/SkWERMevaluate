% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prepHabitatModel.R
\name{prep_vri_bem}
\alias{prep_vri_bem}
\title{Prep the vri-bem object}
\usage{
prep_vri_bem(vri_bem_path = NA, hab_layername, minsize = 10000, inputPath = NA)
}
\arguments{
\item{minsize}{minimum area for a polygon (m2) to be included for site assessment}

\item{gdbpath}{pathway to geodatabase containing VRI-BEM layer}

\item{layername}{name of the VRI-BEM layer}

\item{minhabscore}{maximum habitat score (numeric 1 - 5) to be included for site assessment}

\item{sec_beu}{\%(0-100) of a secondary BEU class of a polygon allowed. Default = 0\% (i.e.
no complex polygons selected for validation)}

\item{incl_hibernation}{}
}
\description{
This function imports the habitat layer (or calls it from memory), removes unnecessary columns
re-arranges rows to represent primary, secondary and tertiary BEU if multiple BEU classes/polygon
are allowed and re-organizes the data table to create columns for field assessment of the habitat model
}
\details{
The minsize parameter is included to remove right off the start numerous "sliver" polygons that are
created during the VRI-BEM combination process. Polygons smaller than this are meaningless, with a
default value of 10000 m2 (1 ha).

murar = bear, mulal = moose
minhabscore=2, sec_beu = 0, incl_hibernation = FALSE
}
