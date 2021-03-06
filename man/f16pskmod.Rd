% Generated by roxygen2 (4.1.1.9000): do not edit by hand
% Please edit documentation in R/rwirelesscom.R
\name{f16pskmod}
\alias{f16pskmod}
\title{16-PSK Modulator}
\usage{
f16pskmod(bits, Ns = 1, p = 1)
}
\arguments{
\item{bits}{- vector of bits (0's and 1's).}

\item{Ns}{- N samples per symbol (default, Ns = 1)}

\item{p}{- a vector defining the pulse shape of the transmitted waveform (default, p = 1)}
}
\value{
Returns a complex vector of length of 8-PSK symbols. If Ns > 1
then the returned signal is shaped with pulse shape, p.
}
\description{
Receives a vector of bits (1's and 0's). The received vector is mapped
to in-phase (real) and quadrature (imaginary) components, according to
a Binary Reflective Gray Code (BRGC, see reference). Each received pair of bits
are are mapped to 16-PSK symbols,
with Es (symbol energy) = 1.The bit to symbol mapping is illustrated in the following table.
\tabular{cc}{
input \tab output \cr
0000 \tab \eqn{    0 }  \cr
0001 \tab \eqn{   \pi/8 } \cr
0011 \tab \eqn{   \pi/4} \cr
0010 \tab \eqn{   3\pi/8} \cr
0110 \tab  \eqn{  \pi/2} \cr
0111 \tab  \eqn{ 5 \pi/8} \cr
0101 \tab  \eqn{ 3 \pi/4} \cr
0100 \tab  \eqn{ 7 \pi/8} \cr
1100 \tab \eqn{  -1 }  \cr
1101 \tab \eqn{  -7 \pi/8 } \cr
1111 \tab \eqn{  -3\pi/4} \cr
1110 \tab \eqn{  -5 \pi/8} \cr
1010 \tab  \eqn{ - \pi/2} \cr
1011 \tab  \eqn{ -3 \pi/8} \cr
1001 \tab  \eqn{ - \pi/4} \cr
1000 \tab  \eqn{ - \pi/8}
}
Reference: E. Agrell, J Lassing, E. Strom, and T. Ottosson, Gray Coding for Multilevel Constellations In Gaussian Noise, IEEE Transactions on Communications, Vol. 53, No. 1, January 2007
}
\examples{
M=16
Nsymbols=20
Nbits=log2(M)*Nsymbols
bits <- sample(0:1,Nbits, replace=TRUE)
s <- f16pskmod(bits)
}
\seealso{
Other rwirelesscom functions: \code{\link{eyediagram}};
  \code{\link{f16pskdemod}}; \code{\link{f16qamdemod}};
  \code{\link{f16qammod}}; \code{\link{f64qamdemod}};
  \code{\link{f64qammod}}; \code{\link{f8pskdemod}};
  \code{\link{f8pskmod}}; \code{\link{fNo}};
  \code{\link{fbpskdemod}}; \code{\link{fbpskmod}};
  \code{\link{fqpskdemod}}; \code{\link{iqdensityplot}};
  \code{\link{iqscatterplot}}; \code{\link{stemplot}}
}

