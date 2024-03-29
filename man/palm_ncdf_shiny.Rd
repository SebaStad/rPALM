% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/palm_shiny.R
\name{palm_ncdf_shiny}
\alias{palm_ncdf_shiny}
\title{R6 Class to create a static driver for the shiny app. The domain is defined via the topography file, other
data arrays have to be defined later.}
\description{
R6 Class to create a static driver for the shiny app. The domain is defined via the topography file, other
data arrays have to be defined later.

R6 Class to create a static driver for the shiny app. The domain is defined via the topography file, other
data arrays have to be defined later.
}
\examples{

## ------------------------------------------------
## Method `palm_ncdf_shiny$new`
## ------------------------------------------------

shiny_static <- palm_ncdf_shiny$new("Path/to/topfile.nc", palm_headclass, FALSE, FALSE)
}
\section{Super class}{
\code{\link[rPALM:palm_ncdf_berlin]{rPALM::palm_ncdf_berlin}} -> \code{palm_ncdf_shiny}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{palm_ncdf_shiny$new()}}
\item \href{#method-importfiles}{\code{palm_ncdf_shiny$importfiles()}}
\item \href{#method-importbuildings_DUMMY}{\code{palm_ncdf_shiny$importbuildings_DUMMY()}}
\item \href{#method-getBuildingID}{\code{palm_ncdf_shiny$getBuildingID()}}
\item \href{#method-clone}{\code{palm_ncdf_shiny$clone()}}
}
}
\if{html}{
\out{<details ><summary>Inherited methods</summary>}
\itemize{
\item \out{<span class="pkg-link" data-pkg="rPALM" data-topic="palm_ncdf_berlin" data-id="SortOverlayingdata">}\href{../../rPALM/html/palm_ncdf_berlin.html#method-SortOverlayingdata}{\code{rPALM::palm_ncdf_berlin$SortOverlayingdata()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="rPALM" data-topic="palm_ncdf_berlin" data-id="add_R6_data">}\href{../../rPALM/html/palm_ncdf_berlin.html#method-add_R6_data}{\code{rPALM::palm_ncdf_berlin$add_R6_data()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="rPALM" data-topic="palm_ncdf_berlin" data-id="add_R6_dim">}\href{../../rPALM/html/palm_ncdf_berlin.html#method-add_R6_dim}{\code{rPALM::palm_ncdf_berlin$add_R6_dim()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="rPALM" data-topic="palm_ncdf_berlin" data-id="add_any2D_variable">}\href{../../rPALM/html/palm_ncdf_berlin.html#method-add_any2D_variable}{\code{rPALM::palm_ncdf_berlin$add_any2D_variable()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="rPALM" data-topic="palm_ncdf_berlin" data-id="add_lod2_variable">}\href{../../rPALM/html/palm_ncdf_berlin.html#method-add_lod2_variable}{\code{rPALM::palm_ncdf_berlin$add_lod2_variable()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="rPALM" data-topic="palm_ncdf_berlin" data-id="add_simple_surfacefraction">}\href{../../rPALM/html/palm_ncdf_berlin.html#method-add_simple_surfacefraction}{\code{rPALM::palm_ncdf_berlin$add_simple_surfacefraction()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="rPALM" data-topic="palm_ncdf_berlin" data-id="add_soil">}\href{../../rPALM/html/palm_ncdf_berlin.html#method-add_soil}{\code{rPALM::palm_ncdf_berlin$add_soil()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="rPALM" data-topic="palm_ncdf_berlin" data-id="addsoilandsurfacefraction">}\href{../../rPALM/html/palm_ncdf_berlin.html#method-addsoilandsurfacefraction}{\code{rPALM::palm_ncdf_berlin$addsoilandsurfacefraction()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="rPALM" data-topic="palm_ncdf_berlin" data-id="correct_surface_fraction">}\href{../../rPALM/html/palm_ncdf_berlin.html#method-correct_surface_fraction}{\code{rPALM::palm_ncdf_berlin$correct_surface_fraction()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="rPALM" data-topic="palm_ncdf_berlin" data-id="countemptyfields">}\href{../../rPALM/html/palm_ncdf_berlin.html#method-countemptyfields}{\code{rPALM::palm_ncdf_berlin$countemptyfields()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="rPALM" data-topic="palm_ncdf_berlin" data-id="create_paraview_buildings">}\href{../../rPALM/html/palm_ncdf_berlin.html#method-create_paraview_buildings}{\code{rPALM::palm_ncdf_berlin$create_paraview_buildings()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="rPALM" data-topic="palm_ncdf_berlin" data-id="create_paraview_lad">}\href{../../rPALM/html/palm_ncdf_berlin.html#method-create_paraview_lad}{\code{rPALM::palm_ncdf_berlin$create_paraview_lad()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="rPALM" data-topic="palm_ncdf_berlin" data-id="createbuilding3D">}\href{../../rPALM/html/palm_ncdf_berlin.html#method-createbuilding3D}{\code{rPALM::palm_ncdf_berlin$createbuilding3D()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="rPALM" data-topic="palm_ncdf_berlin" data-id="cutout_static">}\href{../../rPALM/html/palm_ncdf_berlin.html#method-cutout_static}{\code{rPALM::palm_ncdf_berlin$cutout_static()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="rPALM" data-topic="palm_ncdf_berlin" data-id="dim">}\href{../../rPALM/html/palm_ncdf_berlin.html#method-dim}{\code{rPALM::palm_ncdf_berlin$dim()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="rPALM" data-topic="palm_ncdf_berlin" data-id="downscale_resolution">}\href{../../rPALM/html/palm_ncdf_berlin.html#method-downscale_resolution}{\code{rPALM::palm_ncdf_berlin$downscale_resolution()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="rPALM" data-topic="palm_ncdf_berlin" data-id="exportncdf">}\href{../../rPALM/html/palm_ncdf_berlin.html#method-exportncdf}{\code{rPALM::palm_ncdf_berlin$exportncdf()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="rPALM" data-topic="palm_ncdf_berlin" data-id="fill_areal">}\href{../../rPALM/html/palm_ncdf_berlin.html#method-fill_areal}{\code{rPALM::palm_ncdf_berlin$fill_areal()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="rPALM" data-topic="palm_ncdf_berlin" data-id="generate_lad_single_trees">}\href{../../rPALM/html/palm_ncdf_berlin.html#method-generate_lad_single_trees}{\code{rPALM::palm_ncdf_berlin$generate_lad_single_trees()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="rPALM" data-topic="palm_ncdf_berlin" data-id="generate_lai_array">}\href{../../rPALM/html/palm_ncdf_berlin.html#method-generate_lai_array}{\code{rPALM::palm_ncdf_berlin$generate_lai_array()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="rPALM" data-topic="palm_ncdf_berlin" data-id="generate_patches_beta">}\href{../../rPALM/html/palm_ncdf_berlin.html#method-generate_patches_beta}{\code{rPALM::palm_ncdf_berlin$generate_patches_beta()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="rPALM" data-topic="palm_ncdf_berlin" data-id="import_data">}\href{../../rPALM/html/palm_ncdf_berlin.html#method-import_data}{\code{rPALM::palm_ncdf_berlin$import_data()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="rPALM" data-topic="palm_ncdf_berlin" data-id="plot_area">}\href{../../rPALM/html/palm_ncdf_berlin.html#method-plot_area}{\code{rPALM::palm_ncdf_berlin$plot_area()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="rPALM" data-topic="palm_ncdf_berlin" data-id="print">}\href{../../rPALM/html/palm_ncdf_berlin.html#method-print}{\code{rPALM::palm_ncdf_berlin$print()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="rPALM" data-topic="palm_ncdf_berlin" data-id="quickplot">}\href{../../rPALM/html/palm_ncdf_berlin.html#method-quickplot}{\code{rPALM::palm_ncdf_berlin$quickplot()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="rPALM" data-topic="palm_ncdf_berlin" data-id="set_crs_from_epsg">}\href{../../rPALM/html/palm_ncdf_berlin.html#method-set_crs_from_epsg}{\code{rPALM::palm_ncdf_berlin$set_crs_from_epsg()}}\out{</span>}
}
\out{</details>}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{palm_ncdf_shiny$new(
  topofile,
  headclass,
  oldversion = FALSE,
  gui.arcgis = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{topofile}}{Link to a rastered ncfile with topography. May not conatin fill values!}

\item{\code{headclass}}{R6-Class.}

\item{\code{oldversion}}{Logical. Only use TRUE for PALM versions <2900}

\item{\code{gui.arcgis}}{Logical. Set to TRUE, when you want to import rastered files created by ARCGIS.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The initialize/new functions creates a R6 class that only contains the array "zt". Other information has to be
loaded via import_files()
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{shiny_static <- palm_ncdf_shiny$new("Path/to/topfile.nc", palm_headclass, FALSE, FALSE)
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-importfiles"></a>}}
\if{latex}{\out{\hypertarget{method-importfiles}{}}}
\subsection{Method \code{importfiles()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{palm_ncdf_shiny$importfiles(...)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-importbuildings_DUMMY"></a>}}
\if{latex}{\out{\hypertarget{method-importbuildings_DUMMY}{}}}
\subsection{Method \code{importbuildings_DUMMY()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{palm_ncdf_shiny$importbuildings_DUMMY(filepath)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-getBuildingID"></a>}}
\if{latex}{\out{\hypertarget{method-getBuildingID}{}}}
\subsection{Method \code{getBuildingID()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{palm_ncdf_shiny$getBuildingID(filepath, trustfile = FALSE)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{palm_ncdf_shiny$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
