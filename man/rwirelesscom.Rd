% Generated by roxygen2 (4.1.1.9000): do not edit by hand
% Please edit documentation in R/rwirelesscom.R
\docType{package}
\name{rwirelesscom}
\alias{rwirelesscom}
\alias{rwirelesscom-package}
\title{R Wireless Communications Package}
\description{
A communications simulation package supporting common modulations formats
including BPSK, QPSK, 8-PSK, 16-PSK, 16-QAM and 64-QAM. The package includes
AWGN noise generation, and raised cosine and square root raised cosine
pulse shaping. It also includes functions for plotting
constellation diagrams, density plots, stem plots and eye diagrams. The rwirelesscom
package includes the followng functions:
\itemize{
\item fNo(),
\item fbpskmod(), fbpskdemod(),
\item f8pskmod(), f8pskdemod(),
\item f16pskmod(), f16pskdemod(),
\item f16qammod(), f16qamdemod(),
\item f64qammod(), f64qamdemod,
\item rcosine(), sqrtrcosine(), sinc(),
\item iqscatterplot(), iqdensityplot(),
\item stemplot(), eyediagram()
}
Together these functions
enable the evaluation of bit error and symbol error rates, evalutation of pulse shaping and inter-symbol
interferance and support visualization of the respective signals and noise.
}

