\name{rornitho-news}
\alias{rornitho-news} 
\title{News and changes to the rornitho package}
\section{VERSION 1.0-2}{
  \subsection{BUG FIXES}{
    \itemize{
    \item Data import had to be adjusted to changes in the structure in the export format of ornitho.de
    }
  }
  \subsection{IMPROVED FEATURES}{
    \itemize{
    \item read.ornitho can now auto-adjust the data import to changes in the structure of the files exorted by ornitho.de.
    }
  }
}
\section{VERSION 1.0-1}{
  \subsection{BUG FIXES}{
  \itemize{
  \item There were mismatches between bird names as used in ornitho.de and internally in the rornitho package, e.g. in the Saxicola torquatus superspecies complex. Both lists have been carefully matched and additional functionality was added. Now it is possible to toggle between species-level or subspecies-level treatment for certain species. Besides, subsets of the species list can be created according to family and phenology.
    }
  }
  \subsection{NEW FEATURES}{
    \itemize{
    \item If show.hidden = FALSE in ornithoPDF() or plotOrnitho(), the number of hidden observations is now given in the summary at the upperleft corner of the first page of every species account. In addition, the locality ("Gebiet") of hidden observations in the summary tables on every second page produced by ornithoPDF is now also masked. The argument show.hidden does not have an effect on plots of rasterized distribution, phenology, or altitudinal distribution.

    \item plotOrnithoPheno(), plotOrnitho(), and ornithoPDF() have two new arguments, from and to, which can be used to subset or shift the the months on the x-axis of the plot produced by plotOrnithoPheno().
    }
  }
}  

