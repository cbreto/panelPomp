\name{NEWS}
\title{News for package `panelPomp'}
\section{Changes in \pkg{panelPomp} version 1.2.0}{
  \itemize{
    \item Change the generic \code{unitlogLik} to \code{unitLogLik} to match camel-case style of package. The original function is deprecated in this version and will be removed in future updates.
    \item Changed \code{panelPomp} S4 object slot name from \code{unit.objects} to \code{unit_objects} in order to be consistent with \code{pomp} pacakge. Corresponding getter and setter methods for this slot have also been changed from \code{unitobjects()} to \code{unit_objects()}.
    \item Added generics \code{shared()}, \code{shared()<-}, \code{specific()}, and \code{specific()<-}. These generics provide convinient access to parameters of a \code{panelPomp} object, but the setter methods also allow for changing a parameter type between shared and unit-specific.
    \item Minor typo fixes to package and function documentation.
  }
}
\section{Changes in \pkg{panelPomp} version 1.1.0.2}{
  \itemize{
    \item Added a type check for the \code{params} arguement of the
      \code{pfilter} function in order to throw more user friendly error.
    \item A more user friendly error is thrown when the user specifies a
      parameter as both shared and unit specific in the \code{mif2} function.
    \item Added a \code{match.arg} call in \code{mif2.internal}; this will
      allow the default option for the \code{cooling.type} argument in
      \code{mif2} to no longer throw an error, and mimics the behaviour of the
      \code{mif2} function in the \code{pomp} package.
    \item The \code{pParams} function no longer changes the order of the unit
      specific parameter names; this was causing an error because the
      \code{barycentric} parameter transformation requires that the parameters
      are adjacent to one another in the parameter vector.
    \item Improved documentation for \code{panel_logmeanexp}.
    \item More robust input checking for the \code{pfilter} function with
      user friendly error messages.
  }
}
\section{Changes in \pkg{panelPomp} version 1.1.0}{
  \itemize{
  \item Improved documentation and tests.
  }
}
\section{Changes in \pkg{panelPomp} version 1.0.0}{
  \itemize{
  \item Submitted to CRAN.
  }
}
\section{Changes in \pkg{panelPomp} version 0.18.0}{
  \itemize{
  \item Fixed bug in \code{fromVectorPparams()} to make it a true inverse of \code{toVectorPparams()}.
  \item Homogenized documentation; added examples.
  }
}
\section{Changes in \pkg{panelPomp} version 0.15.0}{
  \itemize{
  \item New coercion to \code{pompList} and update to list.
  \item \code{panelGompertzLikelihood} replaces \code{kalman_gompertz.R}.
  \item expunged \code{panelPompExample}.
  \item Names added to plot; \code{pfilter} matches units by name.
  }
}
\section{Changes in \pkg{panelPomp} version 0.14.0}{
  \itemize{
    \item New \code{simulate} function.
  }
}
\section{Changes in \pkg{panelPomp} version 0.13.0}{
  \itemize{
    \item New \code{panelRandomWalk} example.
    \item \code{panelPompExample} is now deprecated and will be removed in the
    near future.
  }
}
\section{Changes in \pkg{panelPomp} version 0.12.0}{
  \itemize{
    \item New \code{plot} method for \code{panelPomp} objects.
  }
}
\section{Changes in \pkg{panelPomp} version 0.11.0}{
  \itemize{
    \item Additional functions implementing Kalman filtering to panels and to the Gompertz model.
  }
}
\section{Changes in \pkg{panelPomp} version 0.10.2.0}{
  \itemize{
    \item Minor improvements to the interface.
    \item Additional examples: \pkg{contacts()} and \pkg{panelGompertz()}.
    \item The minimum version of \pkg{pomp} supported is now \code{2.7}.
  }
}
\section{Changes in \pkg{panelPomp} version 0.9.1}{
  \itemize{
    \item The minimum version of \pkg{pomp} supported is now \code{2.1}. While
    this is likely to imply substantial changes in \pkg{pomp} objects (on which
    \pkg{panelPomp} objects rely) there are only three fundamental changes that
    break panelPomp's backwards compatibility:
    \item The first argument \code{pfilter(object, ...)} changes to \code{data}.
    \item The first argument \code{mif2(object, ...)} changes to \code{data}.
    \item The ‘conv.rec’ method has been deprecated, replaced by the new
    ‘traces’ method.
  }
}
\section{Changes in \pkg{panelPomp} version 0.8.3}{
  \itemize{
    \item Improvements to the interface.
  }
}
\section{Changes in \pkg{panelPomp} version 0.7}{
  \itemize{
    \item A new \code{conv.rec} method.
  }
}
\section{Changes in \pkg{panelPomp} version 0.6}{
  \itemize{
    \item Many improvements to the interface.
  }
}
\section{Changes in \pkg{panelPomp} version 0.5.6}{
  \itemize{
    \item \code{panelPompExamples()} now allows creating \code{panelPomp} objects that come with the package.
    \item New documentation and tests.
  }
}
\section{Changes in \pkg{panelPomp} version 0.5.5}{
  \itemize{
    \item New documentation and tests.
  }
}
\section{Changes in \pkg{panelPomp} version 0.5.4}{
  \itemize{
    \item New documentation and tests.
  }
}
\section{Changes in \pkg{panelPomp} version 0.5.3}{
  \itemize{
    \item New documentation and additional tests.
  }
}
\section{Changes in \pkg{panelPomp} version 0.5}{
  \itemize{
    \item New documentation and additional tests and coverage.
  }
}
\section{Changes in \pkg{panelPomp} version 0.4}{
  \itemize{
    \item New tests added.
    \item New methods.
    \item A bug in \code{pfilter} and \code{mif2} has been fixed (\code{pomp}'s default filtering tolerance \code{tol=1e-17} was used instead of values specified by the user). Thanks to Aaron King for finding it.
  }
}
\section{Changes in \pkg{panelPomp} version 0.2}{
  \itemize{
    \item New tests added.
    \item Eliminate need to protect \code{rw.sd} from being evaluated.
  }
}
\section{Changes in \pkg{panelPomp} version 0.1}{
  \itemize{
    \item Unused variable purged.
    \item \code{pPfilter.internal} streamlined.
    \item Test for reproducibility of pfilter calculations added.
  }
}
