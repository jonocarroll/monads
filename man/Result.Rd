% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/result.R
\name{Result}
\alias{Result}
\title{Result Monad}
\description{
Creates a \code{Result} monad instance which can be used with the flatMap
operator \verb{\%>>=\%}. See the \verb{Monads in R} vignette for more details.

This container, when flatMapped, always returns a value, either of type
\code{Err} (if an error was raised or the result was \code{NULL}), or \code{Ok}.
}
\seealso{
\code{\link[=Err]{Err()}}, \code{\link[=is_err]{is_err()}}, \code{\link[=Ok]{Ok()}}, \code{\link[=is_ok]{is_ok()}}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{value}}{value contained in the instance}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-ResultMonad-new}{\code{Result$new()}}
\item \href{#method-ResultMonad-bind}{\code{Result$bind()}}
\item \href{#method-ResultMonad-clone}{\code{Result$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ResultMonad-new"></a>}}
\if{latex}{\out{\hypertarget{method-ResultMonad-new}{}}}
\subsection{Method \code{new()}}{
create a new \code{Result} instance
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Result$new(value)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{value}}{value to be contained}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
a new \code{Result} instance
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ResultMonad-bind"></a>}}
\if{latex}{\out{\hypertarget{method-ResultMonad-bind}{}}}
\subsection{Method \code{bind()}}{
bind when using flatMap (\verb{\%>>=\%})
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Result$bind(f, expr)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{f}}{function to apply}

\item{\code{expr}}{expression of the function}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ResultMonad-clone"></a>}}
\if{latex}{\out{\hypertarget{method-ResultMonad-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Result$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
