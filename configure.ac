AC_INIT(crossingprobability, 0.0.0.9000)

# Thanks to https://unconj.ca/blog/an-autoconf-primer-for-r-package-authors.html
# Find the compiler and compiler flags used by R.
: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
  echo "could not determine R_HOME"
  exit 1
fi

# Check for PKG_CONFIG
PKG_PROG_PKG_CONFIG

CXX=`"${R_HOME}/bin/R" CMD config CXX`
CXXFLAGS=`"${R_HOME}/bin/R" CMD config CXXFLAGS`
CPPFLAGS=`"${R_HOME}/bin/R" CMD config CPPFLAGS`
AC_LANG(C++)
AC_PROG_CPP


have_fftw3=no
FFTW3_CXXFLAGS=""
FFTW3_LIBS="-lfftw3"

if test [ -n "$PKG_CONFIG" ] ; then
  AC_MSG_CHECKING([pkg-config for fftw3])
  if $PKG_CONFIG --exists fftw3; then
    have_fftw3=yes
    FFTW3_CXXFLAGS=`"${PKG_CONFIG}" --cflags fftw3`
    FFTW3_LIBS=`"${PKG_CONFIG}" --libs fftw3`
  fi
  AC_MSG_RESULT([${have_fftw3}])
fi

if test "x${have_fftw3}" = xno; then
  AC_CHECK_HEADERS(fftw3.h, [have_fftw3=yes], [])
fi

if test "x${have_fftw3}" = xno; then
  AC_MSG_FAILURE([
  ---------------------------------------------
   'fftw3' and its header files are required.

   Please install:

   * libfftw3-dev (on Debian and Ubuntu)
   * fftw-devel (on Fedora, CentOS and RHEL)
   * fftw  (via Homebrew on macOS)
   * libfftw_dev (on Solaris)

   and try again.

   If you believe this library is installed on your system but
   this script is simply unable to find it, you can specify the
   include and lib paths manually:

   R CMD INSTALL ${PACKAGE_NAME} \\
     --configure-vars='LIBS=-L/path/to/libs CPPFLAGS=-I/path/to/headers'
  ---------------------------------------------])
fi

# List of Src files for Rcpp, ignore the bin files with the command line tools
AUTOSOURCES="RcppExports.cpp common.cc ecdf1_mns2016.cc ecdf1_new.cc ecdf2.cc fftwconvolver.cc poisson_pmf.cc read_boundaries_file.cc string_utils.cc tinymt64.cc"
AC_SUBST(AUTOSOURCES)

# Write the flags into the src/Makevars file.
AC_SUBST([PKG_CPPFLAGS], ["${PKG_CPPFLAGS}"])
AC_SUBST([PKG_LIBS], ["${LIBS} ${PKG_LIBS} ${FFTW3_LIBS}"])
AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT

echo "
  --------------------------------------------------
  Configuration for ${PACKAGE_NAME} ${PACKAGE_VERSION}

    cppflags: ${CPPFLAGS} ${PKG_CPPFLAGS}
    libs:     ${PKG_LIBS}

  --------------------------------------------------
"