#! /bin/bash

# Compile the QC MATLAB functions used by hcppipe_qc into standalone binaries
# that run on the MATLAB Runtime (MCR), so hcppipe_qc can run its plotting steps
# without consuming a MATLAB license (set MATLAB_MODE=runtime in settings.sh).
#
# Builds two apps into $BCILDIR/bin/compiled/ :
#   bcil_tsplot          + run_bcil_tsplot.sh
#   bcil_motiongreyplot  + run_bcil_motiongreyplot.sh
#
# Requirements AT BUILD TIME (this machine):
#   - MATLAB with MATLAB Compiler and Statistics and Machine Learning Toolbox
#     (bcil_motiongreyplot uses quantile/norminv/prctile; bcil_tsplot is base).
#   - The mcc version MUST match the MCR version you will run with
#     (R2022b=v9.13, R2022a=v9.12, R2023a=v9.14).
#
# Usage:
#   MATLABROOT=/usr/local/MATLAB/R2022b $BCILDIR/bin/compile_qc_matlab.sh
#   (defaults to /usr/local/MATLAB/R2022b to match the requested R2022b target)

set -euo pipefail

BCILDIR=$(cd $(dirname $0); cd ..; pwd)
source $BCILDIR/bcilconf/settings.sh

MATLABROOT=${MATLABROOT:-/usr/local/MATLAB/R2022b}
MCC=$MATLABROOT/bin/mcc
OUT=$BCILDIR/bin/compiled
GLOBALMAT=$HCPPIPEDIR/global/matlab

if [ ! -x "$MCC" ] ; then
	echo "ERROR: mcc not found at $MCC" >&2
	echo "       Install MATLAB R2022b (with MATLAB Compiler + Statistics Toolbox)," >&2
	echo "       or set MATLABROOT to the install you want to build with, e.g.:" >&2
	echo "         MATLABROOT=/usr/local/MATLAB/R2022a $0" >&2
	exit 1
fi

mkdir -p "$OUT"
echo "Building with: $MCC"
echo "Output dir   : $OUT"

# --- bcil_tsplot : self-contained (only needs its own file) -------------------
echo "=== compiling bcil_tsplot ==="
"$MCC" -m -v -R -nodisplay \
	-o bcil_tsplot -d "$OUT" \
	"$BCILDIR/bin/bcil_tsplot.m"

# --- bcil_motiongreyplot : pull in DVARS + HCP global/matlab deps -------------
# -I makes mcc's dependency analyzer find the called functions
#    (MovPartextImport, FDCalc, DVARSCalc, MassAC, normalise, ciftiopen);
# -a force-includes the @gifti class/package tree (used by ciftiopen -> gifti()),
#    which the analyzer does not always pull in completely. Nifti_Util is only
#    reached for char/file inputs (this call passes an in-memory matrix), so it
#    is not needed; include it too only if you later feed file paths.
echo "=== compiling bcil_motiongreyplot ==="
"$MCC" -m -v -R -nodisplay \
	-o bcil_motiongreyplot -d "$OUT" \
	-I "$BCILDIR/bin" \
	-I "$DVARSDIR" -I "$DVARSDIR/mis" \
	-I "$GLOBALMAT" -I "$GLOBALMAT/gifti-1.6" \
	-a "$GLOBALMAT/gifti-1.6" \
	"$BCILDIR/bin/bcil_motiongreyplot.m"

echo
echo "Done. Built:"
ls -la "$OUT"/bcil_tsplot "$OUT"/run_bcil_tsplot.sh \
       "$OUT"/bcil_motiongreyplot "$OUT"/run_bcil_motiongreyplot.sh 2>/dev/null || true
echo
echo "Next:"
echo "  1) In $BCILDIR/bcilconf/settings.sh set:"
echo "       export MATLAB_MODE=runtime"
echo "       export MCRROOT=<MCR for the version you built with>   # e.g. $MATLABROOT or .../MATLAB_Runtime/v913"
echo "  2) Run hcppipe_qc as usual; the two plotting steps will use the compiled binaries."
echo
echo "NOTE: headless figure printing. These apps are built with -nodisplay and call"
echo "      print(-dpng). If a plot step errors with a display/renderer problem on a"
echo "      headless host, run under a virtual X server, e.g.:"
echo "        xvfb-run -a $OUT/run_bcil_tsplot.sh \$MCRROOT ...    (or wrap the call)"
