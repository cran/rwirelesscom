% Generated by roxygen2 (4.1.1.9000): do not edit by hand
% Please edit documentation in R/rwirelesscom.R
\name{f64qammod}
\alias{f64qammod}
\title{64-QAM Modulator}
\usage{
f64qammod(bits, Ns = 1, p = 1)
}
\arguments{
\item{bits}{- received vector of bits (0's and 1's).}

\item{Ns}{- N samples per symbol (default, Ns = 1)}

\item{p}{- a vector defining the pulse shape of the transmitted waveform (default, p = 1)}
}
\value{
Returns a complex vector of  64-QAM symbols. If Ns > 1
then the returned signal is shaped with pulse shape, p.
}
\description{
Receives a vector of bits (1's and 0's). The received vector is mapped to an
in-phase (real) and quadrature (imaginary) 64-QAM (6 bit) symbol according to
a Binary Reflective Gray Code (BRGC, see reference). In-phase and quadrature constellation points
take on values -7, -5, -3, -1, +1, +3, +5, +7, respectively, corresponding to a symbol energy Es = 42.
Mapping of bits to 64-QAM constellation points is illustrated in the following constellation.
\tabular{cccccccc}{
-7+7i  \tab -5+7i \tab +3+7i  \tab -1+7i \tab +1+7i  \tab +3+7i \tab +5+7i  \tab +7+7i \cr
 (100000) \tab (100001) \tab (100011) \tab (100010) \tab (100110) \tab (100111) \tab (100101) \tab  (100100) \cr
 \tab \tab \tab \tab \tab  \cr
  -7+5i  \tab -5+5i \tab +3+5i  \tab -1+5i \tab +1+5i  \tab +3+5i \tab +5+5i  \tab +7+5i \cr
(101000) \tab (101001) \tab (101011) \tab (101010) \tab (101110) \tab (101111) \tab (101101) \tab  (101100) \cr
  \tab \tab \tab \tab \tab  \cr
 -7+3i  \tab -5+3i \tab +3+3i  \tab -1+3i \tab +1+3i  \tab +3+3i \tab +5+3i  \tab +7+3i \cr
(111000) \tab (111001) \tab (111011) \tab (111010) \tab (111110) \tab (111111) \tab (111101) \tab (111100) \cr
  \tab \tab \tab \tab \tab   \cr
 -7+1i  \tab -5+1i \tab +3+1i  \tab -1+1i \tab +1+1i  \tab +3+1i \tab +5+1i  \tab +7+1i \cr
 (110000) \tab (110001) \tab (110011) \tab (110010) \tab (110110) \tab (110111) \tab (110101) \tab (110100) \cr
  \tab \tab \tab \tab \tab   \cr
-7-1i  \tab -5-1i \tab +3-1i  \tab -1-1i \tab +1-1i  \tab +3-1i \tab +5-1i  \tab +7-1i \cr
 (010000) \tab (010001) \tab (010011) \tab (010010) \tab (010110) \tab (010111) \tab (010101) \tab  (010100) \cr
 \tab \tab \tab \tab \tab  \cr
  -7-3i  \tab -5-3i \tab +3-3i  \tab -1-3i \tab +1-3i  \tab +3-3i \tab +5-3i  \tab +7-3i \cr
(011000) \tab (011001) \tab (011011) \tab (011010) \tab (011110) \tab (011111) \tab (011101) \tab  (011100) \cr
  \tab \tab \tab \tab \tab  \cr
 -7-5i  \tab -5-5i \tab +3-5i  \tab -1-5i \tab +1-5i  \tab +3-5i \tab +5-5i  \tab +7-5i \cr
(001000) \tab (001001) \tab (001011) \tab (001010) \tab (001110) \tab (001111) \tab (001101) \tab (001100) \cr
  \tab \tab \tab \tab \tab   \cr
 -7-7i  \tab -5-7i \tab +3-7i  \tab -1-7i \tab +1-7i  \tab +3-7i \tab +5-7i  \tab +7-7i \cr
 (000000) \tab (000001) \tab (000011) \tab (000010) \tab (000110) \tab (000111) \tab (000101) \tab (000100)
}
Reference: E. Agrell, J Lassing, E. Strom, and T. Ottosson, Gray Coding for Multilevel Constellations In Gaussian Noise, IEEE Transactions on Communications, Vol. 53, No. 1, January 2007
}
\examples{
M=64
Es=42
Eb = Es/log2(M)
Nsymbols=1000
Nbits=log2(M)*Nsymbols
bits <- sample(0:1,Nbits, replace=TRUE)
s <- f64qammod(bits)
}
\seealso{
Other rwirelesscom functions: \code{\link{eyediagram}};
  \code{\link{f16pskdemod}}; \code{\link{f16pskmod}};
  \code{\link{f16qamdemod}}; \code{\link{f16qammod}};
  \code{\link{f64qamdemod}}; \code{\link{f8pskdemod}};
  \code{\link{f8pskmod}}; \code{\link{fNo}};
  \code{\link{fbpskdemod}}; \code{\link{fbpskmod}};
  \code{\link{fqpskdemod}}; \code{\link{iqdensityplot}};
  \code{\link{iqscatterplot}}; \code{\link{stemplot}}
}

